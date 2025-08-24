;;; md-roam-server-nodes.el --- Node operations endpoints for md-roam-server

;;; Commentary:
;; Handles node-related API endpoints: CRUD operations, content, parsing

;;; Code:

(require 'md-roam-server-core)

;;; Node Operations

(defun md-roam-server-get-nodes ()
  "Get all org-roam nodes."
  (condition-case err
      (progn
        ;; Ensure md-roam is properly configured
        (unless (bound-and-true-p md-roam-mode)
          (setq md-roam-file-extension "md")
          (setq org-roam-file-extensions '("org" "md"))
          (setq org-roam-title-sources '((title headline) (alias alias)))
          (setq md-roam-use-org-extract-ref-links t)
          (md-roam-mode 1))
        ;; Ensure database is in the correct location
        (setq org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
        (message "DEBUG nodes: org-roam-directory=%s, db-location=%s" org-roam-directory org-roam-db-location)
        (org-roam-db-sync)
        (let ((nodes (org-roam-db-query [:select [id title file level] :from nodes :order-by title])))
          (md-roam-server--create-success-response
           "Nodes retrieved successfully"
           `((nodes . ,(mapcar (lambda (node)
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
      (format "Error retrieving nodes: %s" (error-message-string err))))))

(defun md-roam-server-get-node (node-id)
  "Get a specific node by NODE-ID."
  (condition-case err
      (progn
        (md-roam-server-init-org-roam)
        (let ((node (org-roam-db-query [:select [id title file level] :from nodes :where (= id $s1)] node-id)))
          (if node
              (let* ((node-data (car node))
                     (id (nth 0 node-data))
                     (title (nth 1 node-data))
                     (file (nth 2 node-data))
                     (level (nth 3 node-data)))
                (md-roam-server--create-success-response
                 "Node retrieved successfully"
                 `((id . ,id)
                   (title . ,title)
                   (file . ,(file-relative-name file org-roam-directory))
                   (level . ,level)
                   (tags . ,(mapcar 'car (org-roam-db-query [:select [tag] :from tags :where (= node-id $s1)] id)))
                   (aliases . ,(mapcar 'car (org-roam-db-query [:select [alias] :from aliases :where (= node-id $s1)] id))))))
            (md-roam-server--create-error-response 
             "Node not found"
             `((node_id . ,node-id))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving node: %s" (error-message-string err))
      `((node_id . ,node-id))))))

(defun md-roam-server-get-node-content (node-id)
  "Get complete file content for NODE-ID."
  (condition-case err
      (progn
        (md-roam-server-init-org-roam)
        (let ((node (org-roam-db-query [:select [id title file level] :from nodes :where (= id $s1)] node-id)))
          (if node
              (let* ((node-data (car node))
                     (id (nth 0 node-data))
                     (title (nth 1 node-data))
                     (file (nth 2 node-data))
                     (level (nth 3 node-data))
                     (relative-path (file-relative-name file org-roam-directory))
                     (attrs (file-attributes file))
                     (size (nth 7 attrs))
                     (mtime (nth 5 attrs))
                     (content (with-temp-buffer
                                (insert-file-contents file)
                                (buffer-string))))
                (md-roam-server--create-success-response
                 "Node content retrieved successfully"
                 `((node_id . ,id)
                   (title . ,title)
                   (file_path . ,relative-path)
                   (full_path . ,file)
                   (level . ,level)
                   (size . ,size)
                   (modified . ,(format-time-string "%Y-%m-%d %H:%M:%S" mtime))
                   (tags . ,(mapcar 'car (org-roam-db-query [:select [tag] :from tags :where (= node-id $s1)] id)))
                   (aliases . ,(mapcar 'car (org-roam-db-query [:select [alias] :from aliases :where (= node-id $s1)] id)))
                   (content . ,content))))
            (md-roam-server--create-error-response 
             "Node not found"
             `((node_id . ,node-id))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving node content: %s" (error-message-string err))
      `((node_id . ,node-id))))))

(defun md-roam-server-create-node (json-data)
  "Create a new node with JSON-DATA."
  (condition-case err
      (let* ((title (cdr (assoc 'title json-data)))
             (content (or (cdr (assoc 'content json-data)) ""))
             (tags (or (cdr (assoc 'tags json-data)) []))
             (aliases (or (cdr (assoc 'aliases json-data)) []))
             (category (or (cdr (assoc 'category json-data)) ""))
             (refs (or (cdr (assoc 'refs json-data)) [])))
        
        (if (not title)
            (md-roam-server--create-error-response "Title is required")
          (progn
            (md-roam-server-init-org-roam)
            (let* ((node-id (org-id-new))
                   (filename (format "%s-%s.md" 
                                   (format-time-string "%Y%m%d%H%M%S")
                                   (replace-regexp-in-string "[^a-zA-Z0-9]" "-" (downcase title))))
                   (filepath (expand-file-name filename org-roam-directory)))
              
              ;; Create the file
              (with-temp-file filepath
                (insert (format ":PROPERTIES:\n:ID: %s\n:END:\n" node-id))
                (insert (format "#+title: %s\n" title))
                (when (and category (> (length category) 0))
                  (insert (format "#+category: %s\n" category)))
                (when (and tags (> (length tags) 0))
                  (insert (format "#+filetags: %s\n" (mapconcat 'identity tags " "))))
                (when (and aliases (> (length aliases) 0))
                  (insert (format "#+roam_alias: %s\n" (mapconcat 'identity aliases " "))))
                (when (and refs (> (length refs) 0))
                  (dolist (ref refs)
                    (insert (format "#+roam_refs: %s\n" ref))))
                (insert "\n")
                (insert content))
              
              ;; Sync database
              (org-roam-db-sync)
              
              (md-roam-server--create-success-response
               "Node created successfully"
               `((id . ,node-id)
                 (title . ,title)
                 (file . ,filename)
                 (path . ,filepath)))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error creating node: %s" (error-message-string err))))))

(defun md-roam-server-delete-node (node-id)
  "Delete a node by NODE-ID and its associated file."
  (condition-case err
      (progn
        ;; Ensure md-roam is properly configured
        (unless (bound-and-true-p md-roam-mode)
          (setq md-roam-file-extension "md")
          (setq org-roam-file-extensions '("org" "md"))
          (setq org-roam-title-sources '((title headline) (alias alias)))
          (setq md-roam-use-org-extract-ref-links t)
          (md-roam-mode 1))
        ;; Ensure database is in the correct location
        (setq org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
        (org-roam-db-sync)
        
        ;; Get node information before deletion
        (let* ((node-query (org-roam-db-query [:select [id title file level] :from nodes :where (= id $s1)] node-id))
               (node (car node-query)))
          (if (not node)
              (md-roam-server--create-error-response 
               (format "Node with ID '%s' not found" node-id))
            (let* ((title (nth 1 node))
                   (file-path (nth 2 node))
                   (relative-path (file-relative-name file-path org-roam-directory)))
              
              ;; Security check - ensure file is within org-roam directory
              (unless (string-prefix-p org-roam-directory (expand-file-name file-path))
                (error "File path outside of org-roam directory: %s" file-path))
              
              ;; Delete the file
              (when (file-exists-p file-path)
                (delete-file file-path))
              
              ;; Sync database to remove deleted node
              (org-roam-db-sync)
              
              (md-roam-server--create-success-response
               (format "Node deleted successfully: %s" title)
               `((id . ,node-id)
                 (title . ,title)
                 (file . ,relative-path)))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error deleting node: %s" (error-message-string err))))))

;;; Node Endpoint Handlers

(defun md-roam-server-handle-nodes (method path json-data)
  "Handle /nodes endpoint with METHOD, PATH, and JSON-DATA."
  (cond
   ((string= method "GET")
    (cond
     ((string= path "/nodes")
      (md-roam-server-get-nodes))
     ((string-match "^/nodes/\\([^/]+\\)$" path)
      (let ((node-id (match-string 1 path)))
        (md-roam-server-get-node node-id)))
     ((string-match "^/nodes/\\([^/]+\\)/content$" path)
      (let ((node-id (match-string 1 path)))
        (md-roam-server-get-node-content node-id)))
     (t
      (md-roam-server--create-error-response "Node endpoint not found"))))
   ((string= method "POST")
    (cond
     ((string= path "/nodes")
      (md-roam-server-create-node json-data))
     (t
      (md-roam-server--create-error-response "Node POST endpoint not found"))))
   ((string= method "DELETE")
    (cond
     ((string-match "^/nodes/\\([^/]+\\)$" path)
      (let ((node-id (match-string 1 path)))
        (md-roam-server-delete-node node-id)))
     (t
      (md-roam-server--create-error-response "Node DELETE endpoint not found"))))
   (t
    (md-roam-server--create-error-response "Method not allowed for nodes endpoint"))))

(provide 'md-roam-server-nodes)
;;; md-roam-server-nodes.el ends here