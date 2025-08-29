;;; md-roam-server-routes.el --- Main routing logic for md-roam-server

;;; Commentary:
;; Central routing system that delegates requests to appropriate endpoint modules.

;;; Code:

(require 'md-roam-server-core)
(require 'md-roam-server-http)
(require 'md-roam-server-files)
(require 'md-roam-server-nodes)
(require 'md-roam-server-search)
(require 'md-roam-server-ui)

;;; Main Routing Function

(defun md-roam-server-route-request (proc method path json-data)
  "Route HTTP request to appropriate handler based on METHOD and PATH."
  (condition-case err
      (let (result)
        (setq result
              (cond
               ;; Health check and root endpoints
               ((string= path "/")
                (md-roam-server--create-success-response 
                 "md-roam HTTP server is running"
                 `((version . "2.0.0")
                   (server_port . ,md-roam-server-port)
                   (ui_port . ,md-roam-server-ui-port)
                   (ui_enabled . ,md-roam-server-ui-enabled)
                   (org_roam_directory . ,org-roam-directory)
                   (endpoints . ["/files" "/nodes" "/search" "/tags" "/stats" "/config" "/ui" "/sync"]))))
               
               ;; File operations
               ((string-prefix-p "/files" path)
                (md-roam-server-handle-files method path json-data))
               
               ;; Node operations  
               ((string-prefix-p "/nodes" path)
                (md-roam-server-handle-nodes method path json-data))
               
               ;; Search operations
               ((or (string-prefix-p "/search" path)
                    (string= path "/tags")
                    (string= path "/stats") 
                    (string= path "/config"))
                (md-roam-server-handle-search method path json-data))
               
               ;; UI and database operations
               ((or (string= path "/ui")
                    (string= path "/sync"))
                (md-roam-server-handle-ui method path json-data))
               
               ;; CORS preflight
               ((string= method "OPTIONS")
                (md-roam-server--create-success-response "CORS preflight OK"))
               
               ;; Default 404
               (t
                (md-roam-server--create-error-response 
                 "Endpoint not found" 
                 `((method . ,method)
                   (path . ,path))))))
        
        ;; Send response with proper HTTP status codes
        (let ((status (cond 
                       ((string= (cdr (assoc 'status result)) "success")
                        (if (string= method "POST") 201 200))
                       ((string= (cdr (assoc 'status result)) "error")
                        (if (string= (cdr (assoc 'error_type result)) "not_found") 404 400))
                       (t 404))))
          (md-roam-server-send-response proc status "application/json" (json-encode result))))
    (error
     (message "Routing error: %s" (error-message-string err))
     (md-roam-server-send-response proc 500 "application/json" 
                                  (json-encode (md-roam-server--create-error-response
                                               (format "Routing error: %s" (error-message-string err))))))))

(provide 'md-roam-server-routes)
;;; md-roam-server-routes.el ends here