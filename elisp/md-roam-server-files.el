;;; md-roam-server-files.el --- File operations endpoints for md-roam-server

;;; Commentary:
;; Handles file-related API endpoints: /files, /files/raw, /files/content

;;; Code:

(require 'md-roam-server-core)

;;; File Operations

(defun md-roam-server-get-files ()
  "Get all org-roam files with metadata."
  (condition-case err
      (progn
        (md-roam-server-init-org-roam)
        (let ((nodes (org-roam-db-query [:select [id title file level] :from nodes :order-by title])))
          (md-roam-server--create-success-response
           "Files retrieved successfully"
           `((files . ,(mapcar (lambda (node)
                                 (let ((id (nth 0 node))
                                       (title (nth 1 node))
                                       (file (nth 2 node))
                                       (level (nth 3 node)))
                                   `((id . ,id)
                                     (title . ,title)
                                     (file . ,(file-relative-name file org-roam-directory))
                                     (level . ,level)
                                     (tags . ,(mapcar 'car (org-roam-db-query [:select [tag] :from tags :where (= node-id $s1)] id)))
                                     (aliases . ,(mapcar 'car (org-roam-db-query [:select [alias] :from aliases :where (= node-id $s1)] id))))))
                               nodes))
             (count . ,(length nodes))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving files: %s" (error-message-string err))
      `((directory . ,(md-roam-server--safe-directory)))))))

(defun md-roam-server-get-raw-files ()
  "Get raw file list from org-roam directory."
  (condition-case err
      (let* ((directory (md-roam-server--safe-directory))
             (files (directory-files-and-attributes directory t "\\.(md\\|org)$")))
        (md-roam-server--create-success-response
         "Raw files retrieved successfully"
         `((directory . ,directory)
           (files . ,(mapcar (lambda (file-info)
                               (let* ((name (car file-info))
                                      (attrs (cdr file-info))
                                      (size (nth 7 attrs))
                                      (mtime (nth 5 attrs))
                                      (ctime (nth 6 attrs)))
                                 `((name . ,(file-name-nondirectory name))
                                   (path . ,name)
                                   (extension . ,(file-name-extension name))
                                   (size . ,size)
                                   (modified . ,(format-time-string "%Y-%m-%d %H:%M:%S" mtime))
                                   (created . ,(format-time-string "%Y-%m-%d %H:%M:%S" ctime)))))
                             files))
           (count . ,(length files)))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving raw files: %s" (error-message-string err))
      `((directory . ,(md-roam-server--safe-directory)))))))

(defun md-roam-server-get-file-content (filepath)
  "Get file content by FILEPATH."
  (condition-case err
      (let* ((directory (md-roam-server--safe-directory))
             (full-path (expand-file-name filepath directory)))
        (if (and (file-exists-p full-path)
                 (string-prefix-p directory (expand-file-name full-path)))
            (let* ((attrs (file-attributes full-path))
                   (size (nth 7 attrs))
                   (mtime (nth 5 attrs))
                   (content (with-temp-buffer
                              (insert-file-contents full-path)
                              (buffer-string))))
              (md-roam-server--create-success-response
               "File content retrieved successfully"
               `((path . ,filepath)
                 (full_path . ,full-path)
                 (size . ,size)
                 (modified . ,(format-time-string "%Y-%m-%d %H:%M:%S" mtime))
                 (content . ,content))))
          (md-roam-server--create-error-response 
           "File not found or outside org-roam directory"
           `((path . ,filepath)
             (directory . ,directory)))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving file content: %s" (error-message-string err))
      `((path . ,filepath))))))

;;; File Endpoint Handlers

(defun md-roam-server-handle-files (method path json-data)
  "Handle /files endpoint with METHOD, PATH, and JSON-DATA."
  (cond
   ((string= method "GET")
    (cond
     ((string= path "/files")
      (md-roam-server-get-files))
     ((string= path "/files/raw")
      (md-roam-server-get-raw-files))
     ((string-prefix-p "/files/content/" path)
      (let ((filepath (substring path 15))) ; Remove "/files/content/"
        (md-roam-server-get-file-content filepath)))
     (t
      (md-roam-server--create-error-response "File endpoint not found"))))
   (t
    (md-roam-server--create-error-response "Method not allowed for files endpoint"))))

(provide 'md-roam-server-files)
;;; md-roam-server-files.el ends here