;;; start-server.el --- Simple startup script for md-roam-server

;;; Commentary:
;; This file provides a simple way to start the md-roam-server
;; without loading the full modular system.

;;; Code:

;; Add current directory and elisp directory to load path
(add-to-list 'load-path default-directory)
(add-to-list 'load-path (expand-file-name "elisp" default-directory))

;; Load and start the server
(require 'md-roam-server)
(md-roam-server-start)

;; Keep the server running
(message "md-roam server started on port %d" md-roam-server-port)
(message "Access REST API at: http://localhost:%d" md-roam-server-port)
(message "Server is running. Press C-c C-c to stop.")