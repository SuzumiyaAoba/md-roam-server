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
        
        ;; Debug: Check if directory exists and has files
        (let ((dir-exists (file-directory-p org-roam-directory))
              (files-in-dir (when (file-directory-p org-roam-directory)
                              (directory-files org-roam-directory nil "\\.\\(md\\|org\\)$"))))
          (message "DEBUG: Directory exists: %s, Files found: %s" dir-exists (length files-in-dir))
          (when files-in-dir
            (message "DEBUG: Files in directory: %s" files-in-dir)))
        
        ;; Force database sync with additional checks
        (message "DEBUG: Starting org-roam-db-sync...")
        (condition-case sync-err
            (progn
              ;; Ensure org-roam is fully initialized
              (unless (org-roam-db-p)
                (message "DEBUG: Database not initialized, creating...")
                (org-roam-db--init))
              
              ;; Force a full rebuild if database seems empty but files exist
              (let ((db-nodes-before (condition-case nil
                                         (length (org-roam-db-query [:select [id] :from nodes]))
                                       (error 0)))
                    (files-exist (and (file-directory-p org-roam-directory)
                                      (directory-files org-roam-directory nil "\\.\\(md\\|org\\)$"))))
                (message "DEBUG: Nodes in DB before sync: %s, Files exist: %s" db-nodes-before (if files-exist (length files-exist) 0))
                
                ;; If we have files but no database entries, force a rebuild
                (if (and files-exist (> (length files-exist) 0) (= db-nodes-before 0))
                    (progn
                      (message "DEBUG: Files exist but DB empty, forcing rebuild...")
                      (org-roam-db-sync 'force))
                  (org-roam-db-sync))))
          (error (message "ERROR: Database sync failed: %s" (error-message-string sync-err))))
        (message "DEBUG: org-roam-db-sync completed")
        
        ;; Debug: Check database state after sync
        (let ((db-file-exists (file-exists-p org-roam-db-location))
              (db-nodes-count (condition-case nil
                                  (length (org-roam-db-query [:select [id] :from nodes]))
                                (error 0))))
          (message "DEBUG: Database file exists: %s, Nodes in DB: %s" db-file-exists db-nodes-count))
        (let ((nodes (org-roam-db-query [:select [id title file level] :from nodes :order-by title])))
          (md-roam-server--create-success-response
           "Nodes retrieved successfully"
           `((data . ,(if nodes
                          (mapcar (lambda (node)
                                     (let ((id (nth 0 node))
                                           (title (nth 1 node))
                                           (file (nth 2 node))
                                           (level (nth 3 node)))
                                       `((id . ,id)
                                         (title . ,title)
                                         (file . ,(file-relative-name file org-roam-directory))
                                         (level . ,level)
                                         (tags . ,(mapcar 'car (org-roam-db-query [:select [tag] :from tags :where (= node-id $s1)] id)))
                                         (file_type . ,(cond ((string= (file-name-extension file) "md") "md")
                                                             ((string= (file-name-extension file) "org") "org")
                                                             (t "unknown")))
                                         (path . ,file)
                                         (aliases . ,(mapcar 'car (org-roam-db-query [:select [alias] :from aliases :where (= node-id $s1)] id))))))
                                   nodes)
                        []))  ; Return empty vector for JSON empty array
             (count . ,(if nodes (length nodes) 0))))))
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
                   (file_type . ,(let ((ext (file-name-extension file)))
                                   (cond ((string= ext "md") "md")
                                         ((string= ext "org") "org")
                                         (t "unknown"))))
                   (path . ,file)
                   (level . ,level)
                   (tags . ,(mapcar 'car (org-roam-db-query [:select [tag] :from tags :where (= node-id $s1)] id)))
                   (aliases . ,(mapcar 'car (org-roam-db-query [:select [alias] :from aliases :where (= node-id $s1)] id))))))
            (md-roam-server--create-error-response 
             "Node not found"
             `((node_id . ,node-id)
               (error_type . "not_found"))))))
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
  "Create a new node with JSON-DATA - minimal safe implementation."
  (condition-case err
      (let* ((title-raw (cdr (assoc 'title json-data)))
             (content-raw (or (cdr (assoc 'content json-data)) ""))
             (tags-raw (cdr (assoc 'tags json-data)))
             (aliases-raw (cdr (assoc 'aliases json-data)))
             (refs-raw (cdr (assoc 'refs json-data)))
             (category-raw (cdr (assoc 'category json-data)))
             (file-type (or (cdr (assoc 'file_type json-data)) "md")) ; Default to .md
             ;; Sanitize inputs to prevent XSS while preserving Japanese content
             (title (md-roam-server--sanitize-japanese-content title-raw))
             (content (md-roam-server--sanitize-japanese-content content-raw))
             (category (md-roam-server--sanitize-japanese-content category-raw))
             ;; Convert vectors to lists for safe processing and sanitize
             (tags (when tags-raw
                     (let ((tags-list (if (vectorp tags-raw) (append tags-raw nil) tags-raw)))
                       (mapcar 'md-roam-server--sanitize-japanese-content tags-list))))
             (aliases (when aliases-raw
                        (let ((aliases-list (if (vectorp aliases-raw) (append aliases-raw nil) aliases-raw)))
                          (mapcar 'md-roam-server--sanitize-japanese-content aliases-list))))
             (refs (when refs-raw
                     (let ((refs-list (if (vectorp refs-raw) (append refs-raw nil) refs-raw)))
                       (mapcar 'md-roam-server--sanitize-japanese-content refs-list)))))
        
        (if (or (not title) (string= title ""))
            (md-roam-server--create-error-response "title is required")
          (if (and file-type (not (member file-type '("md" "org"))))
              (md-roam-server--create-error-response 
               (format "Invalid file_type: %s. Must be 'md' or 'org'" file-type))
            ;; Create simple UUID
          (let* ((node-id (format "%08X-%04X-4%03X-%04X-%012X"
                                 (random (expt 16 8))
                                 (random (expt 16 4))
                                 (random (expt 16 3))
                                 (+ 32768 (random 32768)) ; Ensure first bit is set
                                 (random (expt 16 12))))
                 (timestamp (format-time-string "%Y%m%d%H%M%S"))
                 ;; Ultra-safe filename generation - use timestamp and full node-id
                 (extension (if (string= file-type "org") "org" "md"))
                 (filename (format "%s-%s.%s" timestamp node-id extension))
                 (filepath (expand-file-name filename org-roam-directory)))
            
            ;; Test org-roam-directory first
            (unless (file-directory-p org-roam-directory)
              (make-directory org-roam-directory t))
            
            ;; Create file based on type
            (cond
             ;; Create Markdown file
             ((string= extension "md")
              (condition-case file-err
                  (let ((yaml-content (format "---\nid: %s\ntitle: %s\n" node-id title)))
                    ;; Add optional metadata
                    (when category
                      (setq yaml-content (concat yaml-content (format "category: %s\n" category))))
                    ;; Add tags efficiently
                    (when (and tags (listp tags) (> (length tags) 0))
                      (let ((tag-string (mapconcat (lambda (tag) (format "\"%s\"" tag)) tags ", ")))
                        (setq yaml-content (concat yaml-content (format "tags: [%s]\n" tag-string)))))
                    
                    ;; Complete YAML and add content
                    (setq yaml-content (concat yaml-content "---\n\n" content))
                    
                    ;; Write file
                    (write-region yaml-content nil filepath)
                    ;; Perform immediate sync for critical operations
                    (org-roam-db-sync)
                    (md-roam-server--create-success-response
                     "Markdown node created successfully"
                     `((id . ,node-id)
                       (title . ,title)  
                       (file . ,filename)
                       (file_type . "md")
                       (path . ,filepath))))
                (error
                 (md-roam-server--create-error-response
                  (format "Failed to create Markdown file: %s" (error-message-string file-err))))))
             
             ;; Create Org file 
             ((string= extension "org")
              (condition-case file-err
                  (let ((org-content (format ":PROPERTIES:\n:ID: %s\n:END:\n#+title: %s\n" node-id (or title "Untitled"))))
                    ;; Add tags if present using filetags (simplified)
                    (when (and tags (listp tags) (> (length tags) 0))
                      (setq org-content (concat org-content "#+filetags: " (car tags) "\n")))
                    
                    ;; Add content if present (simplified)
                    (when content
                      (setq org-content (concat org-content "\n" content)))
                    
                    ;; Write file efficiently
                    (write-region org-content nil filepath)
                    ;; Perform immediate sync for critical operations
                    (org-roam-db-sync)
                    (md-roam-server--create-success-response
                     "Org node created successfully"
                     `((id . ,node-id)
                       (title . ,title)
                       (file . ,filename)
                       (file_type . "org")
                       (path . ,filepath))))
                (error
                 (md-roam-server--create-error-response
                  (format "Failed to create Org file: %s" (error-message-string file-err))))))
             
             ;; Invalid extension
             (t
              (md-roam-server--create-error-response
               (format "Invalid file_type: %s. Use 'md' or 'org'." file-type))))))))
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
               (format "Node with ID '%s' not found" node-id)
               '((error_type . "not_found")))
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
  "Update a node by NODE-ID with JSON-DATA."
  (condition-case err
      (progn
        (md-roam-server-init-org-roam)
        (let* ((node-query (org-roam-db-query [:select [id title file] :from nodes :where (= id $s1)] node-id))
               (node (car node-query)))
          (if (not node)
              (md-roam-server--create-error-response 
               (format "Node with ID '%s' not found" node-id)
               `((node_id . ,node-id)
                 (error_type . "not_found")))
            (let* ((file-path (nth 2 node))
                   (file-extension (file-name-extension file-path))
                   (new-title (cdr (assoc 'title json-data)))
                   (new-content (cdr (assoc 'content json-data)))
                   (new-category (cdr (assoc 'category json-data)))
                   (new-file-type (cdr (assoc 'file_type json-data))))
              
              ;; Validation
              (when (and new-title (string= new-title ""))
                (error "Title cannot be empty"))
              
              (when (and new-file-type (not (member new-file-type '("md" "org"))))
                (error "Invalid file_type: %s. Must be 'md' or 'org'" new-file-type))
              
              ;; Use current values if not provided
              (unless new-title (setq new-title (nth 1 node)))
              
              ;; Security check
              (unless (string-prefix-p org-roam-directory (expand-file-name file-path))
                (error "File path outside of org-roam directory"))
              
              ;; Read existing file content and metadata to preserve data
              (let ((existing-content "")
                    (existing-metadata nil))
                (when (file-exists-p file-path)
                  (with-temp-buffer
                    (insert-file-contents file-path)
                    (let* ((content (buffer-string))
                           (parsed (md-roam-server--parse-file-content content file-extension)))
                      (setq existing-metadata (car parsed))
                      (setq existing-content (cdr parsed)))))
                
                ;; Use existing content if no new content provided
                (unless new-content 
                  (setq new-content existing-content))
                
                ;; Preserve existing metadata and merge with new category
                (let ((final-metadata existing-metadata)
                      (final-content new-content))
                  
                  ;; Update file with preserved content
                  (cond 
                   ((string= file-extension "md")
                    (md-roam-server--write-md-file file-path node-id new-title new-category final-metadata final-content))
                   ((string= file-extension "org")
                    (md-roam-server--write-org-file file-path node-id new-title new-category final-metadata final-content))
                   (t (error "Unsupported file type")))
                  
                  ;; Extract metadata for response
                  (let ((tags (or (md-roam-server--extract-metadata-field final-metadata "tags") []))
                        (aliases (or (md-roam-server--extract-metadata-field final-metadata "roam_aliases" "aliases") []))
                        (refs (or (md-roam-server--extract-metadata-field final-metadata "roam_refs" "refs") []))
                        (category (or new-category (md-roam-server--extract-metadata-field final-metadata "category"))))
                    
                    ;; Skip database sync to prevent blocking - rely on background sync or manual /sync
                    
                    ;; Return response with preserved metadata
                    (md-roam-server--create-success-response
                     "Node updated with content preserved"
                     `((id . ,node-id)
                       (title . ,new-title)
                       (file . ,(file-relative-name file-path org-roam-directory))
                       (file_type . ,(if (string= file-extension "md") "md" "org"))
                       (path . ,file-path)
                       (category . ,category)
                       (tags . ,tags)
                       (aliases . ,aliases)
                       (refs . ,refs)
                       (content_preserved . t)
                       (note . "Content and metadata preserved during update - use POST /sync to update database"))))))))))
              
    (error
     (md-roam-server--create-error-response 
      (format "Error updating node: %s" (error-message-string err))
      `((node_id . ,node-id))))))

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

;;; Helper Functions for Update Operations

(defun md-roam-server--write-md-file (file-path node-id title category metadata content)
  "Write markdown FILE-PATH with NODE-ID, TITLE, CATEGORY, METADATA, and CONTENT."
  (let ((yaml-header "---\n")
        (yaml-footer "---\n\n"))
    
    ;; Build YAML front matter
    (setq yaml-header (concat yaml-header (format "id: %s\n" node-id)))
    (setq yaml-header (concat yaml-header (format "title: %s\n" title)))
    
    ;; Add category if provided
    (when category
      (setq yaml-header (concat yaml-header (format "category: %s\n" category))))
    
    ;; Extract and preserve existing metadata
    (when (vectorp metadata)
      (dotimes (i (length metadata))
        (let ((item (aref metadata i)))
          (when (listp item)
            (dolist (pair item)
              (let ((key (car pair))
                    (value (cdr pair)))
                (unless (member key '("id" "title" "category"))
                  (setq yaml-header (concat yaml-header (format "%s: %s\n" key value))))))))))
    
    ;; Write complete file
    (write-region (concat yaml-header yaml-footer content) nil file-path)))

(defun md-roam-server--write-org-file (file-path node-id title category metadata content)
  "Write org FILE-PATH with NODE-ID, TITLE, CATEGORY, METADATA, and CONTENT."
  (let ((org-header (format ":PROPERTIES:\n:ID: %s\n:END:\n#+title: %s\n" node-id title)))
    
    ;; Add category if provided
    (when category
      (setq org-header (concat org-header (format "#+category: %s\n" category))))
    
    ;; Extract and preserve existing metadata (simplified for org mode)
    (when (vectorp metadata)
      (dotimes (i (length metadata))
        (let ((item (aref metadata i)))
          (when (listp item)
            (dolist (pair item)
              (let ((key (car pair))
                    (value (cdr pair)))
                (unless (member key '("ID" "id" "title" "category"))
                  (cond
                   ((string-match-p "^[A-Z]+$" key)
                    ;; Property drawer format
                    (setq org-header (concat org-header (format ":%s: %s\n" key value))))
                   (t
                    ;; Keyword format
                    (setq org-header (concat org-header (format "#+%s: %s\n" key value))))))))))))
    
    ;; Write complete file
    (write-region (concat org-header "\n" content) nil file-path)))

(defun md-roam-server--extract-metadata-field (metadata field-name &optional alt-field-name)
  "Extract FIELD-NAME from METADATA vector, with optional ALT-FIELD-NAME fallback."
  (when (vectorp metadata)
    (let ((result nil))
      (dotimes (i (length metadata))
        (let ((item (aref metadata i)))
          (when (listp item)
            (dolist (pair item)
              (let ((key (car pair))
                    (value (cdr pair)))
                (when (or (string= key field-name)
                          (and alt-field-name (string= key alt-field-name)))
                  (setq result value)))))))
      
      ;; Handle array-like values (tags, aliases, refs)
      (when result
        (cond
         ((string-match "^\\[\\(.*\\)\\]$" result)
          ;; Parse array format like "[tag1, tag2]"
          (let ((array-content (match-string 1 result)))
            (when (and array-content (> (length array-content) 0))
              (vconcat (mapcar 'string-trim 
                              (split-string array-content "," t "\\s-*\"?\\s-*"))))))
         (t
          ;; Return as single value or string
          result))))))

(provide 'md-roam-server-nodes)
;;; md-roam-server-nodes.el ends here