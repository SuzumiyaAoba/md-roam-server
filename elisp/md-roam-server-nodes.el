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

(defun md-roam-server-get-node-backlinks (node-id)
  "Get all nodes that link to the specified NODE-ID (backlinks)."
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
        
        ;; Check if node exists
        (let* ((target-node (car (org-roam-db-query [:select [id title] :from nodes :where (= id $s1)] node-id))))
          (if (not target-node)
              (md-roam-server--create-error-response 
               (format "Node with ID '%s' not found" node-id))
            ;; Get backlinks from links table
            (let* ((backlink-query (org-roam-db-query 
                                   [:select [nodes.id nodes.title nodes.file nodes.level links.type]
                                    :from links
                                    :inner-join nodes :on (= links.source nodes.id)
                                    :where (= links.dest $s1)]
                                   node-id))
                   (backlinks (mapcar (lambda (link)
                                       (let ((id (nth 0 link))
                                             (title (nth 1 link))
                                             (file (nth 2 link))
                                             (level (nth 3 link))
                                             (link-type (nth 4 link)))
                                         `((id . ,id)
                                           (title . ,title)
                                           (file . ,(file-relative-name file org-roam-directory))
                                           (level . ,level)
                                           (link_type . ,link-type))))
                                     backlink-query)))
              (md-roam-server--create-success-response
               (format "Found %d backlinks for node" (length backlinks))
               `((node_id . ,node-id)
                 (backlinks . ,(if backlinks (vconcat backlinks) []))
                 (count . ,(length backlinks))))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving backlinks: %s" (error-message-string err))))))

(defun md-roam-server-get-node-links (node-id)
  "Get all nodes that the specified NODE-ID links to (forward links)."
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
        
        ;; Check if node exists
        (let* ((source-node (car (org-roam-db-query [:select [id title] :from nodes :where (= id $s1)] node-id))))
          (if (not source-node)
              (md-roam-server--create-error-response 
               (format "Node with ID '%s' not found" node-id))
            ;; Get forward links from links table
            (let* ((forward-link-query (org-roam-db-query 
                                       [:select [nodes.id nodes.title nodes.file nodes.level links.type]
                                        :from links
                                        :inner-join nodes :on (= links.dest nodes.id)
                                        :where (= links.source $s1)]
                                       node-id))
                   (forward-links (mapcar (lambda (link)
                                           (let ((id (nth 0 link))
                                                 (title (nth 1 link))
                                                 (file (nth 2 link))
                                                 (level (nth 3 link))
                                                 (link-type (nth 4 link)))
                                             `((id . ,id)
                                               (title . ,title)
                                               (file . ,(file-relative-name file org-roam-directory))
                                               (level . ,level)
                                               (link_type . ,link-type))))
                                         forward-link-query)))
              (md-roam-server--create-success-response
               (format "Found %d forward links from node" (length forward-links))
               `((node_id . ,node-id)
                 (links . ,(if forward-links (vconcat forward-links) []))
                 (count . ,(length forward-links))))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving forward links: %s" (error-message-string err))))))

(defun md-roam-server-get-node-aliases (node-id)
  "Get aliases for the specified NODE-ID."
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
        
        ;; Check if node exists and get aliases
        (let* ((node-query (org-roam-db-query [:select [id title] :from nodes :where (= id $s1)] node-id))
               (node (car node-query)))
          (if (not node)
              (md-roam-server--create-error-response 
               (format "Node with ID '%s' not found" node-id))
            (let* ((title (nth 1 node))
                   (aliases-query (org-roam-db-query [:select [alias] :from aliases :where (= node-id $s1)] node-id))
                   (aliases (mapcar 'car aliases-query)))
              (md-roam-server--create-success-response
               (if (> (length aliases) 0)
                   (format "Found %d aliases for node '%s'" (length aliases) title)
                 (format "No aliases found for node '%s'" title))
               `((id . ,node-id)
                 (title . ,title)
                 (aliases . ,(if aliases (vconcat aliases) []))
                 (count . ,(length aliases))))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving aliases: %s" (error-message-string err))))))

(defun md-roam-server-get-node-refs (node-id)
  "Get refs for the specified NODE-ID."
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
        
        ;; Check if node exists and get refs
        (let* ((node-query (org-roam-db-query [:select [id title] :from nodes :where (= id $s1)] node-id))
               (node (car node-query)))
          (if (not node)
              (md-roam-server--create-error-response 
               (format "Node with ID '%s' not found" node-id))
            (let* ((title (nth 1 node))
                   (refs-query (org-roam-db-query [:select [ref type] :from refs :where (= node-id $s1)] node-id))
                   (refs (mapcar (lambda (ref-row)
                                   `((ref . ,(nth 0 ref-row))
                                     (type . ,(nth 1 ref-row))))
                                 refs-query)))
              (md-roam-server--create-success-response
               (if (> (length refs) 0)
                   (format "Found %d refs for node '%s'" (length refs) title)
                 (format "No refs found for node '%s'" title))
               `((id . ,node-id)
                 (title . ,title)
                 (refs . ,(if refs (vconcat refs) []))
                 (count . ,(length refs))))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving refs: %s" (error-message-string err))))))

(defun md-roam-server-parse-node (node-id)
  "Parse node file by NODE-ID and return separated metadata and body content."
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
        
        ;; Get node information
        (let* ((node-query (org-roam-db-query [:select [id title file level] :from nodes :where (= id $s1)] node-id))
               (node (car node-query)))
          (if (not node)
              (md-roam-server--create-error-response 
               (format "Node not found: %s" node-id)
               `((node_id . ,node-id)))
            (let* ((title (nth 1 node))
                   (file-path (nth 2 node))
                   (level (nth 3 node))
                   (relative-path (file-relative-name file-path org-roam-directory))
                   (file-extension (file-name-extension file-path))
                   (file-type (cond ((string= file-extension "md") "md")
                                   ((string= file-extension "org") "org") 
                                   (t "unknown"))))
              
              ;; Read and parse file content
              (if (not (file-exists-p file-path))
                  (md-roam-server--create-error-response 
                   (format "File not found: %s" file-path)
                   `((node_id . ,node-id)
                     (file_path . ,relative-path)))
                (with-temp-buffer
                  (insert-file-contents file-path)
                  (let* ((content (buffer-string))
                         (file-stats (file-attributes file-path))
                         (file-size (nth 7 file-stats))
                         (file-modified (format-time-string "%Y-%m-%d %H:%M:%S" (nth 5 file-stats)))
                         (aliases (mapcar 'car (org-roam-db-query [:select [alias] :from aliases :where (= node-id $s1)] node-id)))
                         (tags (mapcar 'car (org-roam-db-query [:select [tag] :from tags :where (= node-id $s1)] node-id)))
                         (parsed-data (md-roam-server--parse-file-content content file-type)))
                    
                    (md-roam-server--create-success-response
                     (format "File parsed successfully for node: %s" title)
                     `((node_id . ,node-id)
                       (title . ,title)
                       (file_path . ,relative-path)
                       (full_path . ,file-path)
                       (file_type . ,file-type)
                       (level . ,level)
                       (size . ,file-size)
                       (modified . ,file-modified)
                       (tags . ,(if tags (vconcat tags) []))
                       (aliases . ,(if aliases (vconcat aliases) []))
                       (metadata . ,(car parsed-data))
                       (body . ,(cdr parsed-data)))))))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error parsing node: %s" (error-message-string err))
      `((node_id . ,node-id))))))

(defun md-roam-server--parse-file-content (content file-type)
  "Parse CONTENT based on FILE-TYPE, return (metadata . body) cons."
  (cond
   ((string= file-type "md")
    (md-roam-server--parse-markdown content))
   ((string= file-type "org")
    (md-roam-server--parse-org content))
   (t
    (cons [] content))))

(defun md-roam-server--parse-markdown (content)
  "Parse Markdown CONTENT and return (metadata . body) cons."
  (let ((lines (split-string content "\n"))
        (metadata-list '())
        (in-frontmatter nil)
        (frontmatter-ended nil)
        (body-lines '()))
    
    (dolist (line lines)
      (cond
       ;; Start of YAML front matter
       ((and (string= line "---") (not in-frontmatter) (not frontmatter-ended))
        (setq in-frontmatter t))
       ;; End of YAML front matter
       ((and (string= line "---") in-frontmatter (not frontmatter-ended))
        (setq in-frontmatter nil)
        (setq frontmatter-ended t))
       ;; Parse front matter line
       (in-frontmatter
        (when (string-match "^\\([^:]+\\):\\s-*\\(.+\\)$" line)
          (let ((key (match-string 1 line))
                (value (match-string 2 line)))
            (push `((,key . ,value)) metadata-list))))
       ;; Body content
       (t
        (when (or frontmatter-ended (not (string= line "---")))
          (push line body-lines)))))
    
    (cons (if metadata-list (vconcat (reverse metadata-list)) [])
          (string-join (reverse body-lines) "\n"))))

(defun md-roam-server--parse-org (content)
  "Parse Org CONTENT and return (metadata . body) cons."
  (let ((lines (split-string content "\n"))
        (metadata-list '())
        (in-properties nil)
        (body-lines '()))
    
    (dolist (line lines)
      (cond
       ;; Start of properties drawer
       ((string-match "^\\s-*:PROPERTIES:" line)
        (setq in-properties t))
       ;; End of properties drawer
       ((string-match "^\\s-*:END:" line)
        (setq in-properties nil))
       ;; Property line
       ((and in-properties (string-match "^\\s-*:\\([^:]+\\):\\s-*\\(.+\\)$" line))
        (let ((key (match-string 1 line))
              (value (match-string 2 line)))
          (push `((,key . ,value)) metadata-list)))
       ;; #+KEYWORD: value format
       ((string-match "^\\s-*#\\+\\([^:]+\\):\\s-*\\(.+\\)$" line)
        (let ((key (match-string 1 line))
              (value (match-string 2 line)))
          (push `((,key . ,value)) metadata-list)))
       ;; Body content
       (t
        (push line body-lines))))
    
    (cons (if metadata-list (vconcat (reverse metadata-list)) [])
          (string-join (reverse body-lines) "\n"))))

(defun md-roam-server-update-node (node-id json-data)
  "Update a node by NODE-ID with JSON-DATA containing new content and metadata."
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
        
        ;; Check if node exists
        (let* ((node-query (org-roam-db-query [:select [id title file level] :from nodes :where (= id $s1)] node-id))
               (node (car node-query)))
          (if (not node)
              (md-roam-server--create-error-response 
               (format "Node with ID '%s' not found" node-id))
            (let* ((current-title (nth 1 node))
                   (file-path (nth 2 node))
                   (relative-path (file-relative-name file-path org-roam-directory))
                   (file-extension (file-name-extension file-path))
                   ;; Extract new data from JSON
                   (new-title (or (cdr (assoc 'title json-data)) current-title))
                   (new-category (cdr (assoc 'category json-data)))
                   (new-tags (cdr (assoc 'tags json-data)))
                   (new-aliases (cdr (assoc 'aliases json-data)))
                   (new-refs (cdr (assoc 'refs json-data)))
                   (new-content (cdr (assoc 'content json-data))))
              
              ;; Security check - ensure file is within org-roam directory
              (unless (string-prefix-p org-roam-directory (expand-file-name file-path))
                (error "File path outside of org-roam directory: %s" file-path))
              
              ;; Update file based on extension
              (cond 
               ;; Update Markdown file
               ((string= file-extension "md")
                (with-temp-file file-path
                  (insert "---\n")
                  (insert (format "id: %s\n" node-id))
                  (insert (format "title: %s\n" new-title))
                  (when new-category
                    (insert (format "category: %s\n" new-category)))
                  (when (and new-aliases (> (length new-aliases) 0))
                    (insert (format "roam_aliases: %s\n" 
                                   (json-encode (if (vectorp new-aliases) 
                                                   (append new-aliases nil)
                                                 new-aliases)))))
                  (when (and new-refs (> (length new-refs) 0))
                    (insert (format "roam_refs: %s\n" 
                                   (if (vectorp new-refs)
                                       (string-join (append new-refs nil) " ")
                                     (string-join new-refs " ")))))
                  (insert "---\n\n")
                  (when (and new-tags (> (length new-tags) 0))
                    (insert (mapconcat (lambda (tag) (format "#%s" tag)) 
                                      (if (vectorp new-tags) (append new-tags nil) new-tags) " "))
                    (insert "\n\n"))
                  (when new-content
                    (insert new-content))))
               
               ;; Update Org file
               ((string= file-extension "org")
                (with-temp-file file-path
                  (insert ":PROPERTIES:\n")
                  (insert (format ":ID: %s\n" node-id))
                  (insert ":END:\n")
                  (insert (format "#+title: %s\n" new-title))
                  (when new-category
                    (insert (format "#+category: %s\n" new-category)))
                  (when (and new-tags (> (length new-tags) 0))
                    (insert (format "#+filetags: %s\n" (string-join 
                                                       (if (vectorp new-tags) (append new-tags nil) new-tags) " "))))
                  (when (and new-aliases (> (length new-aliases) 0))
                    (insert (format "#+roam_alias: %s\n" (string-join 
                                                         (if (vectorp new-aliases) (append new-aliases nil) new-aliases) " "))))
                  (when (and new-refs (> (length new-refs) 0))
                    (dolist (ref (if (vectorp new-refs) (append new-refs nil) new-refs))
                      (insert (format "#+roam_refs: %s\n" ref))))
                  (insert "\n")
                  (when new-content
                    (insert new-content))))
               
               ;; Unsupported file type
               (t
                (error "Unsupported file type: %s" file-extension)))
              
              ;; Sync database
              (org-roam-db-sync)
              
              (md-roam-server--create-success-response
               (format "Node updated successfully: %s" new-title)
               `((id . ,node-id)
                 (title . ,new-title)
                 (file . ,relative-path)
                 (category . ,new-category)
                 (tags . ,(if new-tags (if (vectorp new-tags) new-tags (vconcat new-tags)) []))
                 (aliases . ,(if new-aliases (if (vectorp new-aliases) new-aliases (vconcat new-aliases)) []))
                 (refs . ,(if new-refs (if (vectorp new-refs) new-refs (vconcat new-refs)) []))))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error updating node: %s" (error-message-string err))))))

(defun md-roam-server-add-tag-to-node (node-id json-data)
  "Add a tag to a node by NODE-ID using JSON-DATA containing the tag."
  (condition-case err
      (let ((tag (cdr (assoc 'tag json-data))))
        (if (not tag)
            (md-roam-server--create-error-response "Tag parameter is required")
          (md-roam-server--create-success-response
           (format "Tag '%s' would be added to node '%s'" tag node-id)
           `((node_id . ,node-id)
             (tag_added . ,tag)
             (message . "Tag management not implemented - file modification required")))))
    (error
     (md-roam-server--create-error-response 
      (format "Error adding tag: %s" (error-message-string err))))))

(defun md-roam-server-remove-tag-from-node (node-id tag)
  "Remove a tag from a node by NODE-ID."
  (condition-case err
      (md-roam-server--create-success-response
       (format "Tag '%s' would be removed from node '%s'" tag node-id)
       `((node_id . ,node-id)
         (tag_removed . ,tag)
         (message . "Tag management not implemented - file modification required")))
    (error
     (md-roam-server--create-error-response 
      (format "Error removing tag: %s" (error-message-string err))))))

(defun md-roam-server-add-category-to-node (node-id json-data)
  "Add a category to a node by NODE-ID using JSON-DATA containing the category."
  (condition-case err
      (let ((category (cdr (assoc 'category json-data))))
        (if (not category)
            (md-roam-server--create-error-response "Category parameter is required")
          (md-roam-server--create-success-response
           (format "Category '%s' would be added to node '%s'" category node-id)
           `((node_id . ,node-id)
             (category_added . ,category)
             (message . "Category management not implemented - use PUT /nodes/:id instead")))))
    (error
     (md-roam-server--create-error-response 
      (format "Error adding category: %s" (error-message-string err))))))

(defun md-roam-server-remove-category-from-node (node-id category)
  "Remove a category from a node by NODE-ID."
  (condition-case err
      (md-roam-server--create-success-response
       (format "Category '%s' would be removed from node '%s'" category node-id)
       `((node_id . ,node-id)
         (category_removed . ,category)
         (message . "Category management not implemented - use PUT /nodes/:id instead")))
    (error
     (md-roam-server--create-error-response 
      (format "Error removing category: %s" (error-message-string err))))))

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
     ((string-match "^/nodes/\\([^/]+\\)/backlinks$" path)
      (let ((node-id (match-string 1 path)))
        (md-roam-server-get-node-backlinks node-id)))
     ((string-match "^/nodes/\\([^/]+\\)/links$" path)
      (let ((node-id (match-string 1 path)))
        (md-roam-server-get-node-links node-id)))
     ((string-match "^/nodes/\\([^/]+\\)/aliases$" path)
      (let ((node-id (match-string 1 path)))
        (md-roam-server-get-node-aliases node-id)))
     ((string-match "^/nodes/\\([^/]+\\)/refs$" path)
      (let ((node-id (match-string 1 path)))
        (md-roam-server-get-node-refs node-id)))
     ((string-match "^/nodes/\\([^/]+\\)/parse$" path)
      (let ((node-id (match-string 1 path)))
        (md-roam-server-parse-node node-id)))
     (t
      (md-roam-server--create-error-response "Node endpoint not found"))))
   ((string= method "POST")
    (cond
     ((string= path "/nodes")
      (md-roam-server-create-node json-data))
     ((string-match "^/nodes/\\([^/]+\\)/tags$" path)
      (let ((node-id (match-string 1 path)))
        (md-roam-server-add-tag-to-node node-id json-data)))
     ((string-match "^/nodes/\\([^/]+\\)/categories$" path)
      (let ((node-id (match-string 1 path)))
        (md-roam-server-add-category-to-node node-id json-data)))
     (t
      (md-roam-server--create-error-response "Node POST endpoint not found"))))
   ((string= method "PUT")
    (cond
     ((string-match "^/nodes/\\([^/]+\\)$" path)
      (let ((node-id (match-string 1 path)))
        (md-roam-server-update-node node-id json-data)))
     (t
      (md-roam-server--create-error-response "Node PUT endpoint not found"))))
   ((string= method "DELETE")
    (cond
     ((string-match "^/nodes/\\([^/]+\\)$" path)
      (let ((node-id (match-string 1 path)))
        (md-roam-server-delete-node node-id)))
     ((string-match "^/nodes/\\([^/]+\\)/tags/\\([^/]+\\)$" path)
      (let ((node-id (match-string 1 path))
            (tag (match-string 2 path)))
        (md-roam-server-remove-tag-from-node node-id tag)))
     ((string-match "^/nodes/\\([^/]+\\)/categories/\\([^/]+\\)$" path)
      (let ((node-id (match-string 1 path))
            (category (match-string 2 path)))
        (md-roam-server-remove-category-from-node node-id category)))
     (t
      (md-roam-server--create-error-response "Node DELETE endpoint not found"))))
   (t
    (md-roam-server--create-error-response "Method not allowed for nodes endpoint"))))

(provide 'md-roam-server-nodes)
;;; md-roam-server-nodes.el ends here