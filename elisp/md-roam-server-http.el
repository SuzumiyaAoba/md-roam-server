;;; md-roam-server-http.el --- HTTP server and routing for md-roam-server

;;; Commentary:
;; HTTP server implementation, request handling, and routing logic.

;;; Code:

(require 'md-roam-server-core)

;;; HTTP Server Functions

(defun md-roam-server-send-response (proc status content-type body)
  "Send HTTP response with STATUS, CONTENT-TYPE, and BODY to PROC."
  (let* ((encoded-body (if (and md-roam-server-utf-8-encoding (stringp body))
                           (md-roam-server--ensure-utf-8-encoding body)
                         body))
         (content-length (string-bytes encoded-body))
         (charset-header (if md-roam-server-utf-8-encoding
                            "; charset=utf-8"
                          ""))
         (full-content-type (concat content-type charset-header))
         (response (format "HTTP/1.1 %d %s\r\nContent-Type: %s\r\nContent-Length: %d\r\nAccess-Control-Allow-Origin: *\r\nAccess-Control-Allow-Methods: GET, POST, PUT, DELETE, OPTIONS\r\nAccess-Control-Allow-Headers: Content-Type, Authorization\r\n\r\n%s"
                          status
                          (cond ((= status 200) "OK")
                                ((= status 201) "Created")
                                ((= status 400) "Bad Request")
                                ((= status 404) "Not Found")
                                ((= status 500) "Internal Server Error")
                                (t "Unknown"))
                          full-content-type
                          content-length
                          encoded-body)))
    (process-send-string proc response)))

(defun md-roam-server-parse-request (request)
  "Parse HTTP REQUEST and return method, path, headers, and body."
  (let ((lines (split-string request "\r\n"))
        method path headers body)
    (when lines
      ;; Parse request line
      (let ((request-line (split-string (car lines) " ")))
        (when (>= (length request-line) 2)
          (setq method (nth 0 request-line))
          (setq path (nth 1 request-line))))
      
      ;; Parse headers
      (let ((header-end (cl-position "" lines :test 'string=)))
        (when header-end
          (setq headers (cl-subseq lines 1 header-end))
          (setq body (mapconcat 'identity (cl-subseq lines (1+ header-end)) "\r\n")))))
    
    (list method path headers body)))

(defun md-roam-server-parse-json-body (body)
  "Parse JSON BODY, return parsed data or nil on error."
  (condition-case nil
      (when (and body (> (length body) 0))
        (let ((decoded-body (if md-roam-server-utf-8-encoding
                               (md-roam-server--decode-utf-8-content body)
                             body)))
          (json-read-from-string decoded-body)))
    (error nil)))

(defun md-roam-server-extract-path-param (path pattern)
  "Extract parameter from PATH using PATTERN with {param} syntax."
  (let ((path-parts (split-string path "/"))
        (pattern-parts (split-string pattern "/")))
    (when (= (length path-parts) (length pattern-parts))
      (cl-loop for path-part in path-parts
               for pattern-part in pattern-parts
               when (string-prefix-p "{" pattern-part)
               collect (cons (substring pattern-part 1 -1) path-part)))))

;;; Request Processing

(defun md-roam-server-process-request (proc request)
  "Process HTTP REQUEST from PROC and route to appropriate handlers."
  (condition-case err
      (let* ((parsed (md-roam-server-parse-request request))
             (method (nth 0 parsed))
             (path (nth 1 parsed))
             (headers (nth 2 parsed))
             (body (nth 3 parsed))
             (json-data (md-roam-server-parse-json-body body)))
        
        (message "Processing: %s %s" method path)
        
        ;; Route to appropriate handler
        (md-roam-server-route-request proc method path json-data))
    (error
     (message "Error processing request: %s" (error-message-string err))
     (md-roam-server-send-response proc 500 "application/json"
                                  (json-encode (md-roam-server--create-error-response
                                               (format "Internal server error: %s" (error-message-string err))))))))

;;; Network Process Filter

(defun md-roam-server-filter (proc data)
  "Process network data from PROC."
  (setq md-roam-server-request-buffer (concat md-roam-server-request-buffer data))
  (when (string-match "\r\n\r\n" md-roam-server-request-buffer)
    (let ((content-length-match (string-match "Content-Length: \\([0-9]+\\)" md-roam-server-request-buffer))
          (header-end (match-end 0))) ; Position right after \r\n\r\n
      (if content-length-match
          (let ((content-length (string-to-number (match-string 1 md-roam-server-request-buffer))))
            (when (>= (- (length md-roam-server-request-buffer) header-end) content-length)
              (md-roam-server-process-request proc md-roam-server-request-buffer)
              (setq md-roam-server-request-buffer "")))
        (md-roam-server-process-request proc md-roam-server-request-buffer)
        (setq md-roam-server-request-buffer "")))))

;;; Server Lifecycle

(defun md-roam-server-stop ()
  "Stop the md-roam HTTP server."
  (interactive)
  (when md-roam-server-process
    (delete-process md-roam-server-process)
    (setq md-roam-server-process nil)
    (message "md-roam server stopped")))

(provide 'md-roam-server-http)
;;; md-roam-server-http.el ends here