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



;;; Node Endpoint Handlers

(defun md-roam-server-handle-nodes (method path json-data)
  "Handle /nodes endpoint with METHOD, PATH, and JSON-DATA. CUD operations removed - now handled by Hono API server."
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
   ((or (string= method "POST") (string= method "PUT") (string= method "DELETE"))
    ;; CUD operations are now handled by Hono API server on port 3001
    (md-roam-server--create-error-response 
     "CUD operations (Create/Update/Delete) have been moved to Hono API server on port 3001"
     `((method . ,method)
       (path . ,path)
       (hono_api_url . "http://localhost:3001/api/nodes")
       (migration_note . "All POST, PUT, DELETE operations for nodes are now handled by the Hono API server"))))
   (t
    (md-roam-server--create-error-response "Method not allowed for nodes endpoint"))))


(provide 'md-roam-server-nodes)
;;; md-roam-server-nodes.el ends here