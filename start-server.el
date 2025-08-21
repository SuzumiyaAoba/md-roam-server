;;; start-server.el --- Script to start md-roam server

;;; Commentary:
;; Simple script to load and start the md-roam HTTP server

;;; Code:

;; Load the server
(load-file "md-roam-server.el")

;; Start the server
(md-roam-server-start)

;; Keep Emacs running
(message "md-roam server is running. Press C-c C-c to stop.")
(while t (sleep-for 1))

;;; start-server.el ends here