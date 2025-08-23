;;; start-server.el --- md-roam server startup script for daemon mode

;;; Commentary:
;; This script starts the md-roam server in Emacs daemon mode
;; for better stability and persistence.

;;; Code:

;; Add current directory and elisp subdirectory to load path
(add-to-list 'load-path default-directory)
(add-to-list 'load-path (expand-file-name "elisp" default-directory))

;; Load the modular server
(require 'md-roam-server)

;; Start the server
(md-roam-server-start)

;; Daemon mode messages
(message "md-roam server started in daemon mode")
(message "Access REST API at: http://localhost:8080")
(message "Access Graph UI at: http://localhost:35901")
(message "Stop server with: emacsclient -e '(md-roam-server-stop)' && pkill emacs")

;;; start-server.el ends here