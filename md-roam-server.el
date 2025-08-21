;;; md-roam-server.el --- HTTP server for md-roam API

;;; Commentary:
;; HTTP server that exposes org-roam functionality via REST API

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'org-roam)

(defvar md-roam-server-port 8080
  "Port for the md-roam HTTP server.")

(defvar md-roam-server-process nil
  "Process object for the running server.")

(defvar md-roam-server-request-buffer ""
  "Buffer to accumulate request data.")

(defun md-roam-server-init-org-roam ()
  "Initialize org-roam with proper configuration."
  (unless (bound-and-true-p org-roam-directory)
    ;; Try to find org-roam directory
    (let ((possible-dirs '("~/org-roam" "~/Documents/org-roam" "~/.org-roam" "~/org")))
      (dolist (dir possible-dirs)
        (let ((expanded-dir (expand-file-name dir)))
          (when (file-directory-p expanded-dir)
            (setq org-roam-directory expanded-dir)
            (break))))))
  
  ;; Set default directory if none found
  (unless (bound-and-true-p org-roam-directory)
    (setq org-roam-directory (expand-file-name "~/org-roam")))
  
  ;; Ensure directory exists
  (unless (file-directory-p org-roam-directory)
    (make-directory org-roam-directory t))
  
  ;; Set database location
  (setq org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
  
  ;; Initialize database
  (org-roam-db-autosync-mode 1)
  
  ;; Sync database to ensure it's up to date
  (org-roam-db-sync))

(defun md-roam-server-get-files ()
  "Get list of org-roam files with metadata."
  (condition-case err
      (progn
        ;; Initialize org-roam
        (md-roam-server-init-org-roam)
        
        ;; Get all nodes
        (let ((nodes (org-roam-node-list)))
          (if nodes
              (mapcar (lambda (node)
                        `((id . ,(org-roam-node-id node))
                          (title . ,(org-roam-node-title node))
                          (file . ,(org-roam-node-file node))
                          (level . ,(org-roam-node-level node))
                          (tags . ,(org-roam-node-tags node))
                          (aliases . ,(org-roam-node-aliases node))))
                      nodes)
            ;; If no nodes, return info about the setup
            (list `((info . "No org-roam nodes found")
                   (directory . ,org-roam-directory)
                   (db-location . ,org-roam-db-location)
                   (db-exists . ,(file-exists-p org-roam-db-location)))))))
    (error
     (list `((error . ,(format "Error accessing org-roam: %s" (error-message-string err)))
           (directory . ,(if (boundp 'org-roam-directory) org-roam-directory "not set")))))))

(defun md-roam-server-sync-database ()
  "Sync org-roam database and return status information."
  (condition-case err
      (progn
        ;; Initialize org-roam
        (md-roam-server-init-org-roam)
        
        ;; Get initial node count
        (let ((initial-count (length (org-roam-node-list))))
          
          ;; Perform sync
          (org-roam-db-sync)
          
          ;; Get final node count
          (let ((final-count (length (org-roam-node-list))))
            `((status . "success")
              (message . "Database sync completed")
              (initial-count . ,initial-count)
              (final-count . ,final-count)
              (nodes-changed . ,(- final-count initial-count))
              (directory . ,org-roam-directory)
              (timestamp . ,(format-time-string "%Y-%m-%d %H:%M:%S"))))))
    (error
     `((status . "error")
       (message . ,(format "Error syncing database: %s" (error-message-string err)))
       (directory . ,(if (boundp 'org-roam-directory) org-roam-directory "not set"))
       (timestamp . ,(format-time-string "%Y-%m-%d %H:%M:%S"))))))

(defun md-roam-server-filter (proc string)
  "Process STRING from PROC."
  (setq md-roam-server-request-buffer (concat md-roam-server-request-buffer string))
  (when (string-match "\r\n\r\n" md-roam-server-request-buffer)
    (md-roam-server-handle-request proc md-roam-server-request-buffer)
    (setq md-roam-server-request-buffer "")))

(defun md-roam-server-handle-request (proc request)
  "Handle HTTP REQUEST from PROC."
  (let* ((lines (split-string request "\r\n"))
         (request-line (car lines))
         (parts (split-string request-line))
         (method (car parts))
         (path (cadr parts)))
    (cond
     ((and (string= method "GET") (string= path "/hello"))
      (md-roam-server-send-response proc 200 "application/json"
                                   (json-encode '((message . "Hello, World!")
                                                (service . "md-roam-server")
                                                (status . "running")))))
     ((and (string= method "GET") (string= path "/files"))
      (let ((files (md-roam-server-get-files)))
        (md-roam-server-send-response proc 200 "application/json"
                                     (json-encode `((files . ,files)
                                                  (count . ,(length files)))))))
     ((and (string= method "POST") (string= path "/sync"))
      (let ((sync-result (md-roam-server-sync-database)))
        (md-roam-server-send-response proc 200 "application/json"
                                     (json-encode sync-result))))
     (t
      (md-roam-server-send-response proc 404 "application/json"
                                   (json-encode '((error . "Not Found")
                                                (message . "Endpoint not found"))))))))

(defun md-roam-server-send-response (proc status content-type body)
  "Send HTTP response to PROC with STATUS, CONTENT-TYPE and BODY."
  (let* ((status-text (cond ((= status 200) "OK")
                           ((= status 404) "Not Found")
                           (t "Error")))
         (response (format "HTTP/1.1 %d %s\r\nContent-Type: %s\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
                          status status-text content-type (length body) body)))
    (process-send-string proc response)
    (delete-process proc)))

(defun md-roam-server-start (&optional port)
  "Start the md-roam HTTP server on PORT (default 8080)."
  (interactive "P")
  (let ((server-port (or port md-roam-server-port)))
    (when md-roam-server-process
      (md-roam-server-stop))
    
    (setq md-roam-server-process
          (make-network-process
           :name "md-roam-server"
           :service server-port
           :server t
           :family 'ipv4
           :filter 'md-roam-server-filter))
    
    (message "md-roam server started on port %d" server-port)))

(defun md-roam-server-stop ()
  "Stop the md-roam HTTP server."
  (interactive)
  (when md-roam-server-process
    (delete-process md-roam-server-process)
    (setq md-roam-server-process nil)
    (message "md-roam server stopped")))

(defun md-roam-server-restart (&optional port)
  "Restart the md-roam HTTP server on PORT."
  (interactive "P")
  (md-roam-server-stop)
  (sleep-for 1)
  (md-roam-server-start port))

(provide 'md-roam-server)

;;; md-roam-server.el ends here