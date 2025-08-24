;;; md-roam-server-ui.el --- org-roam-ui integration for md-roam-server

;;; Commentary:
;; Handles org-roam-ui integration and visual interface endpoints.

;;; Code:

(require 'md-roam-server-core)

;;; org-roam-ui Integration

(defun md-roam-server-configure-ui ()
  "Configure org-roam-ui settings."
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil  ; Don't auto-open browser in daemon mode
        org-roam-ui-port md-roam-server-ui-port))

(defun md-roam-server-start-ui ()
  "Start org-roam-ui server."
  (condition-case err
      (progn
        (md-roam-server-configure-ui)
        ;; Ensure org-roam is initialized first
        (unless org-roam-directory
          (org-roam-db-sync))
        
        ;; Load required packages
        (require 'simple-httpd)
        (require 'websocket)
        
        ;; Configure simple-httpd server for org-roam-ui
        (setq httpd-port md-roam-server-ui-port
              httpd-host "0.0.0.0")
        
        ;; Try multiple locations for org-roam-ui web assets
        (let ((possible-roots (list
                               ;; Local web assets directory (preferred)
                               (expand-file-name "org-roam-ui-web/out" default-directory)
                               ;; Package out directory
                               (expand-file-name "out" (file-name-directory (locate-library "org-roam-ui")))
                               ;; Package ui directory (fallback)
                               (expand-file-name "ui" (file-name-directory (locate-library "org-roam-ui"))))))
          (setq httpd-root nil)
          (dolist (root possible-roots)
            (when (and (not httpd-root) (file-exists-p root) (file-exists-p (expand-file-name "index.html" root)))
              (setq httpd-root root)
              (message "Found org-roam-ui web assets at: %s" httpd-root)))
          (unless httpd-root
            (setq httpd-root (car possible-roots))
            (message "Using default web root: %s (may not work)" httpd-root)))
        
        ;; Start org-roam-ui
        (setq org-roam-ui--ws-current-node nil)
        (org-roam-ui-mode 1)
        
        ;; Start the HTTP server if it's not running
        (unless (process-status "httpd")
          (httpd-start))
        
        ;; Give it time to start
        (run-with-timer 3 nil 
                       (lambda () 
                         (message "org-roam-ui web interface available at: http://localhost:%d" 
                                  md-roam-server-ui-port)))
        (message "org-roam-ui starting on port %d (root: %s)" 
                 md-roam-server-ui-port 
                 (or httpd-root "default")))
    (error
     (message "Failed to start org-roam-ui: %s" (error-message-string err)))))

(defun md-roam-server-stop-ui ()
  "Stop org-roam-ui server."
  (condition-case err
      (progn
        (when (boundp 'org-roam-ui-mode)
          (org-roam-ui-mode -1))
        (when (process-status "httpd")
          (httpd-stop))
        (message "org-roam-ui stopped"))
    (error
     (message "Failed to stop org-roam-ui: %s" (error-message-string err)))))

(defun md-roam-server-get-ui-status ()
  "Get org-roam-ui server status and information."
  (condition-case err
      (md-roam-server--create-success-response
       "org-roam-ui status retrieved successfully"
       `((ui_enabled . ,md-roam-server-ui-enabled)
         (ui_port . ,md-roam-server-ui-port)
         (ui_url . ,(format "http://localhost:%d" md-roam-server-ui-port))
         (ui_running . ,(and (boundp 'org-roam-ui-mode) org-roam-ui-mode))
         (configuration . ((sync_theme . ,(when (boundp 'org-roam-ui-sync-theme) org-roam-ui-sync-theme))
                          (follow . ,(when (boundp 'org-roam-ui-follow) org-roam-ui-follow))
                          (update_on_save . ,(when (boundp 'org-roam-ui-update-on-save) org-roam-ui-update-on-save))
                          (open_on_start . ,(when (boundp 'org-roam-ui-open-on-start) org-roam-ui-open-on-start))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving UI status: %s" (error-message-string err))))))

;;; Database Management

(defun md-roam-server-sync-database ()
  "Synchronize org-roam database."
  (condition-case err
      (let ((directory (md-roam-server--safe-directory)))
        (md-roam-server-init-org-roam)
        (let* ((initial-count (caar (org-roam-db-query [:select (funcall count *) :from nodes])))
               (sync-result (org-roam-db-sync))
               (final-count (caar (org-roam-db-query [:select (funcall count *) :from nodes]))))
          (md-roam-server--create-success-response
           "Database synchronized successfully"
           `((initial_count . ,initial-count)
             (final_count . ,final-count)
             (nodes_changed . ,(- final-count initial-count))
             (directory . ,directory)))))
    (error
     (md-roam-server--create-error-response 
      (format "Error synchronizing database: %s" (error-message-string err))
      `((directory . ,(md-roam-server--safe-directory)))))))

;;; UI Endpoint Handlers

(defun md-roam-server-handle-ui (method path json-data)
  "Handle UI and database endpoints with METHOD, PATH, and JSON-DATA."
  (cond
   ((string= method "GET")
    (cond
     ((string= path "/ui")
      (md-roam-server-get-ui-status))
     (t
      (md-roam-server--create-error-response "UI endpoint not found"))))
   ((string= method "POST")
    (cond
     ((string= path "/sync")
      (md-roam-server-sync-database))
     (t
      (md-roam-server--create-error-response "UI POST endpoint not found"))))
   (t
    (md-roam-server--create-error-response "Method not allowed for UI endpoints"))))

(provide 'md-roam-server-ui)
;;; md-roam-server-ui.el ends here