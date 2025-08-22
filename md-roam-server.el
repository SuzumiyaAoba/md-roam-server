;;; md-roam-server.el --- HTTP server for md-roam API

;;; Commentary:
;; HTTP server that exposes org-roam functionality via REST API

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'org-roam)
(require 'org-id)
(require 'md-roam)

(defvar md-roam-server-port 8080
  "Port for the md-roam HTTP server.")

(defvar md-roam-server-process nil
  "Process object for the running server.")

(defvar md-roam-server-request-buffer ""
  "Buffer to accumulate request data.")

(defun md-roam-server--current-timestamp ()
  "Return current timestamp in standard format."
  (format-time-string "%Y-%m-%d %H:%M:%S"))

(defun md-roam-server--safe-directory ()
  "Return org-roam directory safely or 'not set' if not available."
  (if (boundp 'org-roam-directory) org-roam-directory "not set"))

(defun md-roam-server--format-list-value (value)
  "Format VALUE as space-separated string if it's a list, otherwise return as-is."
  (if (listp value)
      (mapconcat 'identity value " ")
    value))

(defun md-roam-server--create-error-response (message &optional extra-fields)
  "Create standardized error response with MESSAGE and optional EXTRA-FIELDS."
  `((status . "error")
    (message . ,message)
    (timestamp . ,(md-roam-server--current-timestamp))
    ,@extra-fields))

(defun md-roam-server--create-success-response (message &optional extra-fields)
  "Create standardized success response with MESSAGE and optional EXTRA-FIELDS."
  `((status . "success")
    (message . ,message)
    (timestamp . ,(md-roam-server--current-timestamp))
    ,@extra-fields))

(defun md-roam-server--build-yaml-front-matter (id title category aliases refs)
  "Build YAML front matter with ID, TITLE, CATEGORY, ALIASES, and REFS."
  (let ((fields (list (format "id: %s" id)
                      (format "title: %s" title))))
    (when category
      (setq fields (append fields (list (format "category: %s" (md-roam-server--format-list-value category))))))
    (when aliases
      (setq fields (append fields (list (format "roam_aliases: [%s]" 
                                               (mapconcat (lambda (alias) (format "\"%s\"" alias)) aliases ", "))))))
    (when refs
      (setq fields (append fields (list (format "roam_refs: %s" (md-roam-server--format-list-value refs))))))
    (format "---\n%s\n---\n\n" (mapconcat 'identity fields "\n"))))

(defun md-roam-server--parse-request-line (request)
  "Parse HTTP request line and return (method path) tuple."
  (let* ((lines (split-string request "\r\n"))
         (request-line (car lines))
         (parts (split-string request-line)))
    (list (car parts) (cadr parts))))

(defun md-roam-server--extract-body-params (body param-names)
  "Extract parameters from BODY JSON using PARAM-NAMES list."
  (when body
    (mapcar (lambda (param) 
              (cdr (assoc param body))) 
            param-names)))

(defun md-roam-server--extract-path-param (path pattern)
  "Extract parameter from PATH using PATTERN. 
   Pattern should use :param for parameter placeholders.
   Returns the parameter value or nil if no match."
  (let* ((pattern-parts (split-string pattern "/"))
         (path-parts (split-string path "/"))
         (param-index nil))
    (when (= (length pattern-parts) (length path-parts))
      ;; Find parameter position
      (dotimes (i (length pattern-parts))
        (when (string-prefix-p ":" (nth i pattern-parts))
          (setq param-index i)))
      ;; Return parameter value if found
      (when param-index
        (nth param-index path-parts)))))

(defun md-roam-server-init-org-roam ()
  "Initialize org-roam and md-roam with proper configuration."
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
  
  ;; Configure md-roam
  (setq org-roam-file-extensions '("org" "md"))
  (setq md-roam-file-extension "md")
  
  ;; Initialize database
  (org-roam-db-autosync-mode 1)
  
  ;; Enable md-roam mode
  (md-roam-mode 1)
  
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
     (list (md-roam-server--create-error-response 
            (format "Error accessing org-roam: %s" (error-message-string err))
            `((directory . ,(md-roam-server--safe-directory))))))))

(defun md-roam-server-get-node-by-id (node-id)
  "Get a single org-roam node by its ID."
  (condition-case err
      (progn
        ;; Initialize org-roam
        (md-roam-server-init-org-roam)
        
        ;; Find node by ID
        (let ((node (org-roam-node-from-id node-id)))
          (if node
              (md-roam-server--create-success-response
               "Node retrieved successfully"
               `((id . ,(org-roam-node-id node))
                 (title . ,(org-roam-node-title node))
                 (file . ,(org-roam-node-file node))
                 (level . ,(org-roam-node-level node))
                 (tags . ,(org-roam-node-tags node))
                 (aliases . ,(org-roam-node-aliases node))))
            (md-roam-server--create-error-response
             (format "Node with ID '%s' not found" node-id)))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving node: %s" (error-message-string err))
      `((node-id . ,node-id))))))

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
            (md-roam-server--create-success-response
             "Database sync completed"
             `((initial-count . ,initial-count)
               (final-count . ,final-count)
               (nodes-changed . ,(- final-count initial-count))
               (directory . ,org-roam-directory))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error syncing database: %s" (error-message-string err))
      `((directory . ,(md-roam-server--safe-directory)))))))

(defun md-roam-server-create-node (title &optional tags content aliases category refs)
  "Create a new org-roam node with TITLE, optional TAGS, CONTENT, ALIASES, CATEGORY, and REFS."
  (condition-case err
      (progn
        ;; Initialize org-roam
        (md-roam-server-init-org-roam)
        
        ;; Generate unique ID
        (let* ((id (org-id-new))
               (slug (org-roam-node-slug (org-roam-node-create :title title)))
               (filename (format "%s-%s.md" 
                               (format-time-string "%Y%m%d%H%M%S")
                               slug))
               (filepath (expand-file-name filename org-roam-directory))
               (tag-list (if tags (mapcar (lambda (tag) (format "#%s" tag)) tags) '()))
               (yaml-front-matter (md-roam-server--build-yaml-front-matter id title category aliases refs))
               (tag-line (if tag-list (format "%s\n\n" (mapconcat 'identity tag-list " ")) ""))
               (file-content (format "%s%s%s"
                                   yaml-front-matter
                                   tag-line
                                   (or content ""))))
          
          ;; Write file
          (with-temp-file filepath
            (insert file-content))
          
          ;; Try to register aliases manually in database if possible
          (when aliases
            (with-temp-buffer
              (insert-file-contents filepath)
              (goto-char (point-min))
              ;; Force org-roam to process this file
              (org-mode)
              (org-roam-db-update-file filepath)))
          
          ;; Sync database to register the new node
          (org-roam-db-sync)
          
          ;; Return node information
          (md-roam-server--create-success-response
           "Node created successfully"
           `((id . ,id)
             (title . ,title)
             (file . ,filepath)
             (category . ,(or category ""))
             (tags . ,(or tags []))
             (aliases . ,(or aliases []))
             (refs . ,(or refs []))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error creating node: %s" (error-message-string err))))))

(defun md-roam-server-filter (proc string)
  "Process STRING from PROC."
  (setq md-roam-server-request-buffer (concat md-roam-server-request-buffer string))
  (when (string-match "\r\n\r\n" md-roam-server-request-buffer)
    (md-roam-server-handle-request proc md-roam-server-request-buffer)
    (setq md-roam-server-request-buffer "")))

(defun md-roam-server-parse-request-body (request)
  "Parse JSON body from HTTP REQUEST."
  (let* ((lines (split-string request "\r\n"))
         (body-start (cl-position "" lines :test 'string=)))
    (when body-start
      (let ((body (mapconcat 'identity (nthcdr (1+ body-start) lines) "\r\n")))
        (when (> (length (string-trim body)) 0)
          (condition-case err
              (json-read-from-string body)
            (error nil)))))))

(defun md-roam-server-handle-request (proc request)
  "Handle HTTP REQUEST from PROC."
  (let* ((request-parts (md-roam-server--parse-request-line request))
         (method (car request-parts))
         (path (cadr request-parts))
         (body (md-roam-server-parse-request-body request)))
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
     ((and (string= method "GET") (string-prefix-p "/nodes/" path))
      (let ((node-id (md-roam-server--extract-path-param path "/nodes/:id")))
        (if node-id
            (let ((result (md-roam-server-get-node-by-id node-id)))
              (if (string= (cdr (assoc 'status result)) "success")
                  (md-roam-server-send-response proc 200 "application/json"
                                               (json-encode result))
                (md-roam-server-send-response proc 404 "application/json"
                                             (json-encode result))))
          (md-roam-server-send-response proc 400 "application/json"
                                       (json-encode '((error . "Bad Request")
                                                    (message . "Node ID is required")))))))
     ((and (string= method "POST") (string= path "/sync"))
      (let ((sync-result (md-roam-server-sync-database)))
        (md-roam-server-send-response proc 200 "application/json"
                                     (json-encode sync-result))))
     ((and (string= method "POST") (string= path "/nodes"))
      (if body
          (let* ((params (md-roam-server--extract-body-params body '(title tags content aliases category refs)))
                 (title (nth 0 params))
                 (tags (nth 1 params))
                 (content (nth 2 params))
                 (aliases (nth 3 params))
                 (category (nth 4 params))
                 (refs (nth 5 params)))
            (if title
                (let ((result (md-roam-server-create-node title tags content aliases category refs)))
                  (md-roam-server-send-response proc 200 "application/json"
                                               (json-encode result)))
              (md-roam-server-send-response proc 400 "application/json"
                                           (json-encode '((error . "Bad Request")
                                                        (message . "Title is required"))))))
        (md-roam-server-send-response proc 400 "application/json"
                                     (json-encode '((error . "Bad Request")
                                                  (message . "JSON body required"))))))
     ((and (string= method "GET") (string= path "/openapi.json"))
      (md-roam-server-send-response proc 200 "application/json"
                                   (json-encode '((openapi . "3.0.0")
                                                (info . ((title . "md-roam Server API")
                                                         (version . "1.0.0")))
                                                (paths . (("/hello" . "Health check")
                                                         ("/files" . "Get files")
                                                         ("/nodes/:id" . "Get single node")
                                                         ("/sync" . "Sync database")
                                                         ("/nodes" . "Create node")))))))
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