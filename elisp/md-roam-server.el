;;; md-roam-server.el --- Main HTTP server for md-roam API (Modular Version)

;;; Commentary:
;; HTTP server that exposes org-roam functionality via REST API.
;; This is the main entry point that loads all modules and starts the server.

;;; Code:

(require 'md-roam-server-core)
(require 'md-roam-server-http)
(require 'md-roam-server-routes)
(require 'md-roam-server-ui)

;;; Server Management

(defun md-roam-server-start (&optional port)
  "Start the md-roam HTTP server on PORT (default from config)."
  (interactive "P")
  ;; Load configuration first
  (md-roam-server--apply-config)
  
  (let ((server-port (or port md-roam-server-port)))
    (when md-roam-server-process
      (md-roam-server-stop))
    
    ;; Initialize org-roam
    (md-roam-server-init-org-roam)
    
    ;; Start the main HTTP server
    (setq md-roam-server-process
          (make-network-process
           :name "md-roam-server"
           :service server-port
           :server t
           :host 'local
           :family 'ipv4
           :filter 'md-roam-server-filter
           :log (lambda (server client msg)
                  (message "md-roam-server connection: %s" msg))))
    
    ;; Start org-roam-ui if enabled
    (when md-roam-server-ui-enabled
      (md-roam-server-start-ui))
    
    (message "md-roam server started on port %d%s" 
             server-port
             (if md-roam-server-ui-enabled 
                 (format " (UI on port %d)" md-roam-server-ui-port)
               ""))))

(defun md-roam-server-restart ()
  "Restart the md-roam HTTP server."
  (interactive)
  (md-roam-server-stop)
  (sleep-for 1)
  (md-roam-server-start))

(defun md-roam-server-status ()
  "Show md-roam server status."
  (interactive)
  (if md-roam-server-process
      (message "md-roam server running on port %d%s" 
               md-roam-server-port
               (if md-roam-server-ui-enabled 
                   (format " (UI on port %d)" md-roam-server-ui-port)
                 ""))
    (message "md-roam server is not running")))

;;; Module Information

(defconst md-roam-server-modules
  '(("md-roam-server-core" . "Core functionality and configuration")
    ("md-roam-server-http" . "HTTP server and request handling")
    ("md-roam-server-files" . "File operations endpoints")
    ("md-roam-server-nodes" . "Node CRUD operations")
    ("md-roam-server-search" . "Search and metadata endpoints")
    ("md-roam-server-ui" . "org-roam-ui integration")
    ("md-roam-server-routes" . "Main routing logic"))
  "List of md-roam-server modules and their descriptions.")

(defun md-roam-server-list-modules ()
  "List all md-roam-server modules."
  (interactive)
  (message "md-roam-server modules:")
  (dolist (module md-roam-server-modules)
    (message "  %s: %s" (car module) (cdr module))))

(provide 'md-roam-server)
;;; md-roam-server.el ends here