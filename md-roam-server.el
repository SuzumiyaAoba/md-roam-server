;;; md-roam-server.el --- HTTP server for md-roam API

;;; Commentary:
;; HTTP server that exposes org-roam functionality via REST API

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'org-roam)
(require 'org-id)
(require 'md-roam)

(defvar md-roam-server-port 8080
  "Port for the md-roam HTTP server.")

(defvar md-roam-server-process nil
  "Process object for the running server.")

(defvar md-roam-server-request-buffer ""
  "Buffer to accumulate request data.")

(defun md-roam-server--current-timestamp ()
  "Return current timestamp in standard format."
  (format-time-string "%Y-%m-%d %H:%M:%S"))

(defun md-roam-server--safe-directory ()
  "Return org-roam directory safely or 'not set' if not available."
  (if (boundp 'org-roam-directory) org-roam-directory "not set"))

(defun md-roam-server--format-list-value (value)
  "Format VALUE as space-separated string if it's a list, otherwise return as-is."
  (if (listp value)
      (mapconcat 'identity value " ")
    value))

(defun md-roam-server--create-error-response (message &optional extra-fields)
  "Create standardized error response with MESSAGE and optional EXTRA-FIELDS."
  `((status . "error")
    (message . ,message)
    (timestamp . ,(md-roam-server--current-timestamp))
    ,@extra-fields))

(defun md-roam-server--create-success-response (message &optional extra-fields)
  "Create standardized success response with MESSAGE and optional EXTRA-FIELDS."
  `((status . "success")
    (message . ,message)
    (timestamp . ,(md-roam-server--current-timestamp))
    ,@extra-fields))

(defun md-roam-server--build-yaml-front-matter (id title category aliases refs)
  "Build YAML front matter with ID, TITLE, CATEGORY, ALIASES, and REFS."
  (let ((fields (list (format "id: %s" id)
                      (format "title: %s" title))))
    (when category
      (setq fields (append fields (list (format "category: %s" (md-roam-server--format-list-value category))))))
    (when aliases
      (setq fields (append fields (list (format "roam_aliases: [%s]" 
                                               (mapconcat (lambda (alias) (format "\"%s\"" alias)) aliases ", "))))))
    (when refs
      (setq fields (append fields (list (format "roam_refs: %s" (md-roam-server--format-list-value refs))))))
    (format "---\n%s\n---\n\n" (mapconcat 'identity fields "\n"))))

(defun md-roam-server--parse-request-line (request)
  "Parse HTTP request line and return (method path) tuple."
  (let* ((lines (split-string request "\r\n"))
         (request-line (car lines))
         (parts (split-string request-line)))
    (list (car parts) (cadr parts))))

(defun md-roam-server--extract-body-params (body param-names)
  "Extract parameters from BODY JSON using PARAM-NAMES list."
  (when body
    (mapcar (lambda (param) 
              (cdr (assoc param body))) 
            param-names)))

(defun md-roam-server--extract-path-param (path pattern)
  "Extract parameter from PATH using PATTERN. 
   Pattern should use :param for parameter placeholders.
   Returns the parameter value or nil if no match."
  (let* ((pattern-parts (split-string pattern "/"))
         (path-parts (split-string path "/"))
         (param-index nil))
    (when (= (length pattern-parts) (length path-parts))
      ;; Find parameter position
      (dotimes (i (length pattern-parts))
        (when (string-prefix-p ":" (nth i pattern-parts))
          (setq param-index i)))
      ;; Return parameter value if found
      (when param-index
        (nth param-index path-parts)))))

(defun md-roam-server--match-path-pattern (path pattern)
  "Check if PATH matches PATTERN and extract parameters.
   Returns alist of parameter names and values, or nil if no match."
  (let* ((pattern-parts (split-string pattern "/"))
         (path-parts (split-string path "/"))
         (params '()))
    (when (= (length pattern-parts) (length path-parts))
      (let ((match t))
        (dotimes (i (length pattern-parts))
          (let ((pattern-part (nth i pattern-parts))
                (path-part (nth i path-parts)))
            (if (string-prefix-p ":" pattern-part)
                ;; Parameter - extract name and value
                (let ((param-name (substring pattern-part 1)))
                  (push (cons (intern param-name) path-part) params))
              ;; Literal - must match exactly
              (unless (string= pattern-part path-part)
                (setq match nil)))))
        (when match params)))))

(defun md-roam-server-init-org-roam ()
  "Initialize org-roam and md-roam with proper configuration."
  (unless (bound-and-true-p org-roam-directory)
    ;; Try to find org-roam directory
    (let ((possible-dirs '("~/org-roam" "~/Documents/org-roam" "~/.org-roam" "~/org")))
      (dolist (dir possible-dirs)
        (let ((expanded-dir (expand-file-name dir)))
          (when (file-directory-p expanded-dir)
            (setq org-roam-directory expanded-dir)
            (break))))))
  
  ;; Set default directory if none found
  (unless (bound-and-true-p org-roam-directory)
    (setq org-roam-directory (expand-file-name "~/org-roam")))
  
  ;; Ensure directory exists
  (unless (file-directory-p org-roam-directory)
    (make-directory org-roam-directory t))
  
  ;; Set database location
  (setq org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
  
  ;; Configure md-roam
  (setq org-roam-file-extensions '("org" "md"))
  (setq md-roam-file-extension "md")
  
  ;; Initialize database
  (org-roam-db-autosync-mode 1)
  
  ;; Enable md-roam mode
  (md-roam-mode 1)
  
  ;; Sync database to ensure it's up to date
  (org-roam-db-sync))

(defun md-roam-server-get-files ()
  "Get list of org-roam files with metadata."
  (condition-case err
      (progn
        ;; Initialize org-roam
        (md-roam-server-init-org-roam)
        
        ;; Get all nodes
        (let ((nodes (org-roam-node-list)))
          (if nodes
              (mapcar (lambda (node)
                        `((id . ,(org-roam-node-id node))
                          (title . ,(org-roam-node-title node))
                          (file . ,(org-roam-node-file node))
                          (level . ,(org-roam-node-level node))
                          (tags . ,(org-roam-node-tags node))
                          (aliases . ,(org-roam-node-aliases node))))
                      nodes)
            ;; If no nodes, return info about the setup
            (list `((info . "No org-roam nodes found")
                   (directory . ,org-roam-directory)
                   (db-location . ,org-roam-db-location)
                   (db-exists . ,(file-exists-p org-roam-db-location)))))))
    (error
     (list (md-roam-server--create-error-response 
            (format "Error accessing org-roam: %s" (error-message-string err))
            `((directory . ,(md-roam-server--safe-directory))))))))

(defun md-roam-server-get-raw-files ()
  "Get list of physical files in the org-roam directory."
  (condition-case err
      (progn
        ;; Initialize org-roam
        (md-roam-server-init-org-roam)
        
        ;; Get directory contents
        (let* ((directory org-roam-directory)
               (file-extensions '("md" "org"))
               (files '()))
          
          ;; Find all matching files
          (dolist (extension file-extensions)
            (let ((pattern (concat "\\." extension "$")))
              (dolist (file (directory-files directory t pattern))
                (when (file-regular-p file)
                  (let* ((relative-path (file-relative-name file directory))
                         (file-attrs (file-attributes file))
                         (size (nth 7 file-attrs))
                         (modified (nth 5 file-attrs))
                         (created (nth 6 file-attrs)))
                    (push `((name . ,relative-path)
                           (path . ,file)
                           (extension . ,extension)
                           (size . ,size)
                           (modified . ,(format-time-string "%Y-%m-%d %H:%M:%S" modified))
                           (created . ,(format-time-string "%Y-%m-%d %H:%M:%S" created)))
                          files))))))
          
          ;; Sort by modified time (newest first)
          (setq files (sort files (lambda (a b)
                                   (let ((mod-a (cdr (assoc 'modified a)))
                                         (mod-b (cdr (assoc 'modified b))))
                                     (string> mod-a mod-b)))))
          
          (md-roam-server--create-success-response
           (format "Found %d files in org-roam directory" (length files))
           `((directory . ,directory)
             (files . ,files)
             (count . ,(length files))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error listing files: %s" (error-message-string err))
      `((directory . ,(md-roam-server--safe-directory)))))))

(defun md-roam-server-get-file-content (filepath)
  "Get content of a specific file by filepath."
  (condition-case err
      (progn
        ;; Initialize org-roam
        (md-roam-server-init-org-roam)
        
        ;; Validate and resolve filepath
        (let* ((directory org-roam-directory)
               (full-path (if (file-name-absolute-p filepath)
                             filepath
                           (expand-file-name filepath directory)))
               (relative-path (file-relative-name full-path directory)))
          
          ;; Security check - ensure file is within org-roam directory
          (unless (and (file-exists-p full-path)
                      (file-regular-p full-path)
                      (string-prefix-p (file-truename directory)
                                      (file-truename full-path)))
            (error "File not found or access denied"))
          
          ;; Check file extension
          (unless (string-match-p "\\.\\(md\\|org\\)$" full-path)
            (error "Only .md and .org files are supported"))
          
          ;; Read file content
          (let* ((content (with-temp-buffer
                           (insert-file-contents full-path)
                           (buffer-string)))
                 (file-attrs (file-attributes full-path))
                 (size (nth 7 file-attrs))
                 (modified (nth 5 file-attrs)))
            
            (md-roam-server--create-success-response
             (format "File content retrieved successfully")
             `((path . ,relative-path)
               (full_path . ,full-path)
               (size . ,size)
               (modified . ,(format-time-string "%Y-%m-%d %H:%M:%S" modified))
               (content . ,content))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error reading file '%s': %s" filepath (error-message-string err))
      `((filepath . ,filepath)
        (directory . ,(md-roam-server--safe-directory)))))))

(defun md-roam-server-get-file-content-by-node-id (node-id)
  "Get file content of a specific node by its node ID."
  (condition-case err
      (progn
        ;; Initialize org-roam
        (md-roam-server-init-org-roam)
        
        ;; Get node information from database
        (let ((node-data (org-roam-db-query 
                          [:select [id title file level todo pos]
                           :from nodes
                           :where (= id $s1)]
                          node-id)))
          (if node-data
              (let* ((node-info (car node-data))
                     (id (nth 0 node-info))
                     (title (nth 1 node-info))
                     (file-path (nth 2 node-info))
                     (level (nth 3 node-info)))
                
                ;; Validate file exists and is readable
                (unless (and file-path 
                            (file-exists-p file-path)
                            (file-regular-p file-path))
                  (error "Node file not found or not accessible"))
                
                ;; Security check - ensure file is within org-roam directory
                (let ((directory org-roam-directory))
                  (unless (string-prefix-p (file-truename directory)
                                          (file-truename file-path))
                    (error "File access denied - not within org-roam directory")))
                
                ;; Check file extension
                (unless (string-match-p "\\.\\(md\\|org\\)$" file-path)
                  (error "Only .md and .org files are supported"))
                
                ;; Read file content
                (let* ((content (with-temp-buffer
                                 (insert-file-contents file-path)
                                 (buffer-string)))
                       (file-attrs (file-attributes file-path))
                       (size (nth 7 file-attrs))
                       (modified (nth 5 file-attrs))
                       (relative-path (file-relative-name file-path org-roam-directory))
                       (tags (mapcar #'car (org-roam-db-query 
                                            [:select [tag] :from tags :where (= node-id $s1)] 
                                            id)))
                       (aliases (mapcar #'car (org-roam-db-query 
                                              [:select [alias] :from aliases :where (= node-id $s1)] 
                                              id))))
                  
                  (md-roam-server--create-success-response
                   (format "File content retrieved successfully for node: %s" title)
                   `((node_id . ,id)
                     (title . ,title)
                     (file_path . ,relative-path)
                     (full_path . ,file-path)
                     (level . ,level)
                     (size . ,size)
                     (modified . ,(format-time-string "%Y-%m-%d %H:%M:%S" modified))
                     (tags . ,(or tags []))
                     (aliases . ,(or aliases []))
                     (content . ,content)))))
            
            ;; Node not found
            (md-roam-server--create-error-response
             (format "Node not found: %s" node-id)
             `((node_id . ,node-id))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error reading file content for node '%s': %s" node-id (error-message-string err))
      `((node_id . ,node-id)
        (directory . ,(md-roam-server--safe-directory)))))))

(defun md-roam-server-parse-file-by-node-id (node-id)
  "Parse file of a specific node by its node ID, separating metadata and body."
  (condition-case err
      (progn
        ;; Initialize org-roam
        (md-roam-server-init-org-roam)
        
        ;; Get node information from database
        (let ((node-data (org-roam-db-query 
                          [:select [id title file level todo pos]
                           :from nodes
                           :where (= id $s1)]
                          node-id)))
          (if node-data
              (let* ((node-info (car node-data))
                     (id (nth 0 node-info))
                     (title (nth 1 node-info))
                     (file-path (nth 2 node-info))
                     (level (nth 3 node-info)))
                
                ;; Validate file exists and is readable
                (unless (and file-path 
                            (file-exists-p file-path)
                            (file-regular-p file-path))
                  (error "Node file not found or not accessible"))
                
                ;; Security check - ensure file is within org-roam directory
                (let ((directory org-roam-directory))
                  (unless (string-prefix-p (file-truename directory)
                                          (file-truename file-path))
                    (error "File access denied - not within org-roam directory")))
                
                ;; Check file extension
                (unless (string-match-p "\\.\\(md\\|org\\)$" file-path)
                  (error "Only .md and .org files are supported"))
                
                ;; Read and parse file content
                (let* ((content (with-temp-buffer
                                 (insert-file-contents file-path)
                                 (buffer-string)))
                       (file-attrs (file-attributes file-path))
                       (size (nth 7 file-attrs))
                       (modified (nth 5 file-attrs))
                       (relative-path (file-relative-name file-path org-roam-directory))
                       (tags (mapcar #'car (org-roam-db-query 
                                            [:select [tag] :from tags :where (= node-id $s1)] 
                                            id)))
                       (aliases (mapcar #'car (org-roam-db-query 
                                              [:select [alias] :from aliases :where (= node-id $s1)] 
                                              id))))
                  
                  ;; Parse metadata and body based on file type
                  (let ((metadata nil)
                        (body content)
                        (parsed-metadata nil)
                        (file-type (cond
                                   ((string-match-p "\\.md$" file-path) "md")
                                   ((string-match-p "\\.org$" file-path) "org")
                                   (t "unknown"))))
                    
                    (cond
                     ;; Parse Markdown YAML front matter
                     ((string= file-type "md")
                      (when (string-match "\\`---\n\\(\\(?:.\\|\n\\)*?\\)\n---\n\\(\\(?:.\\|\n\\)*\\)\\'" content)
                        (let ((yaml-content (match-string 1 content))
                              (body-content (match-string 2 content)))
                          (setq body body-content)
                          
                          ;; Parse YAML metadata
                          (let ((yaml-lines (split-string yaml-content "\n")))
                            (dolist (line yaml-lines)
                              (when (string-match "^\\s-*\\([^:]+\\):\\s-*\\(.*\\)$" line)
                                (let ((key (string-trim (match-string 1 line)))
                                      (value (string-trim (match-string 2 line))))
                                  ;; Handle special cases for arrays
                                  (cond
                                   ((string-match "^\\[\\(.*\\)\\]$" value)
                                    ;; Array format like ["item1", "item2"]
                                    (let ((array-content (match-string 1 value)))
                                      (if (string-empty-p array-content)
                                          (push (cons key []) parsed-metadata)
                                        (let ((items (split-string array-content "," t)))
                                          (setq items (mapcar (lambda (item)
                                                               (string-trim item "[ \t\n\r\"']"))
                                                             items))
                                          (push (cons key items) parsed-metadata)))))
                                   ((string-match "^\"\\(.*\\)\"$" value)
                                    ;; Quoted string
                                    (push (cons key (match-string 1 value)) parsed-metadata))
                                   (t
                                    ;; Regular value
                                    (push (cons key value) parsed-metadata)))))))
                          
                          (setq metadata (nreverse parsed-metadata)))))
                     
                     ;; Parse Org mode properties
                     ((string= file-type "org")
                      (let ((lines (split-string content "\n"))
                            (in-properties nil)
                            (body-lines '())
                            (property-end-found nil))
                        (dolist (line lines)
                          (cond
                           ;; Org property drawer start
                           ((string-match "^:PROPERTIES:$" line)
                            (setq in-properties t))
                           ;; Org property drawer end
                           ((string-match "^:END:$" line)
                            (setq in-properties nil)
                            (setq property-end-found t))
                           ;; Inside properties drawer
                           (in-properties
                            (when (string-match "^:\\([^:]+\\):\\s-*\\(.*\\)$" line)
                              (let ((key (match-string 1 line))
                                    (value (string-trim (match-string 2 line))))
                                (push (cons key value) parsed-metadata))))
                           ;; #+KEYWORD: style properties (anywhere in file)
                           ((string-match "^#\\+\\([^:]+\\):\\s-*\\(.*\\)$" line)
                            (let ((key (downcase (match-string 1 line)))
                                  (value (string-trim (match-string 2 line))))
                              (push (cons key value) parsed-metadata)))
                           ;; Body content (after properties or regular lines)
                           ((or property-end-found (not in-properties))
                            (push line body-lines))))
                        
                        (setq metadata (nreverse parsed-metadata))
                        (setq body (string-join (nreverse body-lines) "\n"))))))
                    
                    (md-roam-server--create-success-response
                     (format "File parsed successfully for node: %s" title)
                     `((node_id . ,id)
                       (title . ,title)
                       (file_path . ,relative-path)
                       (full_path . ,file-path)
                       (file_type . ,(cond
                                     ((string-match-p "\\.md$" file-path) "md")
                                     ((string-match-p "\\.org$" file-path) "org")
                                     (t "unknown")))
                       (level . ,level)
                       (size . ,size)
                       (modified . ,(format-time-string "%Y-%m-%d %H:%M:%S" modified))
                       (tags . ,(or tags []))
                       (aliases . ,(or aliases []))
                       (metadata . ,(or metadata []))
                       (body . ,body)))))
            
            ;; Node not found
            (md-roam-server--create-error-response
             (format "Node not found: %s" node-id)
             `((node_id . ,node-id))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error parsing file for node '%s': %s" node-id (error-message-string err))
      `((node_id . ,node-id)
        (directory . ,(md-roam-server--safe-directory)))))))

(defun md-roam-server-get-node-by-id (node-id)
  "Get a single org-roam node by its ID."
  (condition-case err
      (progn
        ;; Initialize org-roam
        (md-roam-server-init-org-roam)
        
        ;; Find node by ID
        (let ((node (org-roam-node-from-id node-id)))
          (if node
              (md-roam-server--create-success-response
               "Node retrieved successfully"
               `((id . ,(org-roam-node-id node))
                 (title . ,(org-roam-node-title node))
                 (file . ,(org-roam-node-file node))
                 (level . ,(org-roam-node-level node))
                 (tags . ,(org-roam-node-tags node))
                 (aliases . ,(org-roam-node-aliases node))))
            (md-roam-server--create-error-response
             (format "Node with ID '%s' not found" node-id)))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving node: %s" (error-message-string err))
      `((node-id . ,node-id))))))

(defun md-roam-server-get-all-nodes ()
  "Get list of all org-roam nodes with metadata."
  (condition-case err
      (progn
        (md-roam-server-init-org-roam)
        (let ((nodes (org-roam-db-query 
                      [:select [id title file level todo pos]
                       :from nodes
                       :order-by [(asc title)]])))
          (if nodes
              (let ((node-list '()))
                (dolist (node-row nodes)
                  (let* ((id (nth 0 node-row))
                         (title (nth 1 node-row))
                         (file (nth 2 node-row))
                         (level (nth 3 node-row))
                         (tags (mapcar #'car (org-roam-db-query 
                                              [:select [tag] :from tags :where (= node-id $s1)] 
                                              id)))
                         (aliases (mapcar #'car (org-roam-db-query 
                                                 [:select [alias] :from aliases :where (= node-id $s1)] 
                                                 id))))
                    (push `((id . ,id)
                           (title . ,title)
                           (file . ,(file-relative-name file org-roam-directory))
                           (level . ,level)
                           (tags . ,(or tags []))
                           (aliases . ,(or aliases [])))
                          node-list)))
                
                (md-roam-server--create-success-response
                 (format "Retrieved %d nodes successfully" (length node-list))
                 `((nodes . ,(nreverse node-list))
                   (count . ,(length node-list)))))
            
            (md-roam-server--create-success-response
             "No nodes found"
             `((nodes . [])
               (count . 0))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving nodes: %s" (error-message-string err))
      `((directory . ,(md-roam-server--safe-directory)))))))

(defun md-roam-server-get-tags ()
  "Get list of all unique tags from org-roam nodes with node IDs."
  (condition-case err
      (progn
        ;; Initialize org-roam
        (md-roam-server-init-org-roam)
        
        ;; Get all nodes and collect tags with node IDs
        (let ((nodes (org-roam-node-list))
              (tag-data (make-hash-table :test 'equal)))
          (if nodes
              (progn
                ;; Collect all tags from all nodes with node IDs
                (dolist (node nodes)
                  (let ((node-tags (org-roam-node-tags node))
                        (node-id (org-roam-node-id node)))
                    (when node-tags
                      (dolist (tag node-tags)
                        (when tag
                          (let ((existing (gethash tag tag-data)))
                            (if existing
                                (push node-id (cdr existing))
                              (puthash tag (list 1 node-id) tag-data))))))))
                
                ;; Create result list
                (let ((tag-list '()))
                  (maphash (lambda (tag data)
                             (let ((count (car data))
                                   (node-ids (cdr data)))
                               (push `((tag . ,tag)
                                      (count . ,(length node-ids))
                                      (node_ids . ,(nreverse (delete-dups node-ids))))
                                     tag-list)))
                           tag-data)
                  
                  (md-roam-server--create-success-response
                   "Tags retrieved successfully"
                   `((tags . ,(nreverse tag-list))
                     (total_tags . ,(hash-table-count tag-data))))))
            
            ;; If no nodes, return empty list
            (md-roam-server--create-success-response
             "No tags found"
             `((tags . [])
               (total_tags . 0))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving tags: %s" (error-message-string err))
      `((directory . ,(md-roam-server--safe-directory)))))))


(defun md-roam-server-get-aliases ()
  "Get list of all unique aliases from org-roam nodes with node IDs."
  (condition-case err
      (progn
        ;; Initialize org-roam
        (md-roam-server-init-org-roam)
        
        ;; Get all nodes and collect aliases with node IDs
        (let ((nodes (org-roam-node-list))
              (alias-data (make-hash-table :test 'equal)))
          (if nodes
              (progn
                ;; Collect all aliases from all nodes with node IDs
                (dolist (node nodes)
                  (let ((node-aliases (org-roam-node-aliases node))
                        (node-id (org-roam-node-id node)))
                    (when node-aliases
                      (dolist (alias node-aliases)
                        (when alias
                          (let ((existing (gethash alias alias-data)))
                            (if existing
                                (push node-id (cdr existing))
                              (puthash alias (list 1 node-id) alias-data))))))))
                
                ;; Create result list
                (let ((alias-list '()))
                  (maphash (lambda (alias data)
                             (let ((count (car data))
                                   (node-ids (cdr data)))
                               (push `((alias . ,alias)
                                      (count . ,(length node-ids))
                                      (node_ids . ,(nreverse (delete-dups node-ids))))
                                     alias-list)))
                           alias-data)
                  
                  (md-roam-server--create-success-response
                   "Aliases retrieved successfully"
                   `((aliases . ,(nreverse alias-list))
                     (total_aliases . ,(hash-table-count alias-data))))))
            
            ;; If no nodes, return empty list
            (md-roam-server--create-success-response
             "No aliases found"
             `((aliases . [])
               (total_aliases . 0))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving aliases: %s" (error-message-string err))
      `((directory . ,(md-roam-server--safe-directory)))))))


(defun md-roam-server-get-refs ()
  "Get list of all unique refs from org-roam nodes with node IDs."
  (condition-case err
      (progn
        ;; Initialize org-roam
        (md-roam-server-init-org-roam)
        
        ;; Get all nodes and collect refs with node IDs
        (let ((nodes (org-roam-node-list))
              (ref-data (make-hash-table :test 'equal)))
          (if nodes
              (progn
                ;; Collect all refs from all nodes with node IDs
                (dolist (node nodes)
                  (let ((node-refs (org-roam-node-refs node))
                        (node-id (org-roam-node-id node)))
                    (when node-refs
                      (dolist (ref node-refs)
                        (when ref
                          (let ((existing (gethash ref ref-data)))
                            (if existing
                                (push node-id (cdr existing))
                              (puthash ref (list 1 node-id) ref-data))))))))
                
                ;; Create result list
                (let ((ref-list '()))
                  (maphash (lambda (ref data)
                             (let ((count (car data))
                                   (node-ids (cdr data)))
                               (push `((ref . ,ref)
                                      (count . ,(length node-ids))
                                      (node_ids . ,(nreverse (delete-dups node-ids))))
                                     ref-list)))
                           ref-data)
                  
                  (md-roam-server--create-success-response
                   "Refs retrieved successfully"
                   `((refs . ,(nreverse ref-list))
                     (total_refs . ,(hash-table-count ref-data))))))
            
            ;; If no nodes, return empty list
            (md-roam-server--create-success-response
             "No refs found"
             `((refs . [])
               (total_refs . 0))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving refs: %s" (error-message-string err))
      `((directory . ,(md-roam-server--safe-directory)))))))


(defun md-roam-server-get-node-refs (node-id)
  "Get refs for the org-roam node with the specified NODE-ID."
  (condition-case err
      (progn
        ;; Initialize org-roam
        (md-roam-server-init-org-roam)
        
        ;; Find node by ID
        (let ((node (org-roam-node-from-id node-id)))
          (if node
              (let ((refs (org-roam-node-refs node)))
                (md-roam-server--create-success-response
                 (if refs
                     (format "Found %d refs for node '%s'" (length refs) (org-roam-node-title node))
                   (format "No refs found for node '%s'" (org-roam-node-title node)))
                 `((id . ,node-id)
                   (title . ,(org-roam-node-title node))
                   (refs . ,(or refs []))
                   (count . ,(if refs (length refs) 0)))))
            ;; Node not found
            (md-roam-server--create-error-response
             (format "Node with ID '%s' not found" node-id)))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving refs for node '%s': %s" node-id (error-message-string err))
      `((id . ,node-id)
        (directory . ,(md-roam-server--safe-directory)))))))

(defun md-roam-server-get-node-aliases (node-id)
  "Get aliases for the org-roam node with the specified NODE-ID."
  (condition-case err
      (progn
        ;; Initialize org-roam
        (md-roam-server-init-org-roam)
        
        ;; Find node by ID
        (let ((node (org-roam-node-from-id node-id)))
          (if node
              (let ((aliases (org-roam-node-aliases node)))
                (md-roam-server--create-success-response
                 (if aliases
                     (format "Found %d aliases for node '%s'" (length aliases) (org-roam-node-title node))
                   (format "No aliases found for node '%s'" (org-roam-node-title node)))
                 `((id . ,node-id)
                   (title . ,(org-roam-node-title node))
                   (aliases . ,(or aliases []))
                   (count . ,(if aliases (length aliases) 0)))))
            ;; Node not found
            (md-roam-server--create-error-response
             (format "Node with ID '%s' not found" node-id)))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving aliases for node '%s': %s" node-id (error-message-string err))
      `((id . ,node-id)
        (directory . ,(md-roam-server--safe-directory)))))))

(defun md-roam-server-get-nodes-by-tag (tag)
  "Get list of org-roam nodes that have the specified TAG."
  (condition-case err
      (progn
        ;; Initialize org-roam
        (md-roam-server-init-org-roam)
        
        ;; Get all nodes and filter by tag
        (let ((nodes (org-roam-node-list))
              (matching-nodes '()))
          (if nodes
              (progn
                ;; Filter nodes that contain the specified tag
                (dolist (node nodes)
                  (let ((node-tags (org-roam-node-tags node)))
                    (when (and node-tags (member tag node-tags))
                      (push `((id . ,(org-roam-node-id node))
                             (title . ,(org-roam-node-title node))
                             (file . ,(org-roam-node-file node))
                             (level . ,(org-roam-node-level node))
                             (tags . ,(org-roam-node-tags node))
                             (aliases . ,(org-roam-node-aliases node)))
                            matching-nodes))))
                
                ;; Return results
                (if matching-nodes
                    (md-roam-server--create-success-response
                     (format "Found %d nodes with tag '%s'" (length matching-nodes) tag)
                     `((tag . ,tag)
                       (nodes . ,(nreverse matching-nodes))
                       (count . ,(length matching-nodes))))
                  (md-roam-server--create-success-response
                   (format "No nodes found with tag '%s'" tag)
                   `((tag . ,tag)
                     (nodes . [])
                     (count . 0)))))
            
            ;; If no nodes exist at all
            (md-roam-server--create-success-response
             "No nodes found in database"
             `((tag . ,tag)
               (nodes . [])
               (count . 0))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving nodes for tag '%s': %s" tag (error-message-string err))
      `((tag . ,tag)
        (directory . ,(md-roam-server--safe-directory)))))))

(defun md-roam-server-get-citations ()
  "Get list of all unique citations from org-roam database."
  (condition-case err
      (progn
        (md-roam-server-init-org-roam)
        (let ((citations (org-roam-db-query [:select [cite-key node-id] :from citations]))
              (citation-data (make-hash-table :test 'equal)))
          (if citations
              (progn
                ;; Process citations from database
                (dolist (citation-row citations)
                  (let ((citation-key (nth 0 citation-row))
                        (node-id (nth 1 citation-row)))
                    (when (and citation-key (> (length citation-key) 0))
                      (let ((existing (gethash citation-key citation-data)))
                        (if existing
                            (push node-id (cdr existing))
                          (puthash citation-key (list 1 node-id) citation-data))))))
                
                ;; Create result list
                (let ((citation-list '()))
                  (maphash (lambda (citation data)
                             (let ((count (car data))
                                   (node-ids (cdr data)))
                               (push `((citation . ,citation)
                                      (count . ,(length node-ids))
                                      (node_ids . ,(nreverse (delete-dups node-ids))))
                                     citation-list)))
                           citation-data)
                  
                  (md-roam-server--create-success-response
                   "Citations retrieved successfully"
                   `((citations . ,(if citation-list (nreverse citation-list) []))
                     (total_citations . ,(hash-table-count citation-data))))))
            
            ;; If no citations, return empty list
            (md-roam-server--create-success-response
             "No citations found"
             `((citations . [])
               (total_citations . 0))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving citations: %s" (error-message-string err))
      `((directory . ,(md-roam-server--safe-directory)))))))

(defun md-roam-server-search-nodes-by-title-or-alias (query)
  "Search for nodes by title or alias matching QUERY using database queries."
  (condition-case err
      (progn
        (md-roam-server-init-org-roam)
        (let ((matches '())
              (query-pattern (concat "%" (downcase query) "%"))
              (node-ids-found (make-hash-table :test 'equal)))
          
          ;; Search by title using database query
          (let ((title-matches (org-roam-db-query 
                                [:select [id title file level todo pos]
                                 :from nodes
                                 :where (like (lower title) $s1)]
                                query-pattern)))
            (dolist (match title-matches)
              (let* ((id (nth 0 match))
                     (title (nth 1 match))
                     (file (nth 2 match))
                     (level (nth 3 match)))
                (unless (gethash id node-ids-found)
                  (puthash id t node-ids-found)
                  (let ((tags (mapcar #'car (org-roam-db-query 
                                            [:select [tag] :from tags :where (= node-id $s1)] 
                                            id)))
                        (aliases (mapcar #'car (org-roam-db-query 
                                               [:select [alias] :from aliases :where (= node-id $s1)] 
                                               id))))
                    (push `((id . ,id)
                           (title . ,title)
                           (file . ,file)
                           (level . ,level)
                           (tags . ,(or tags []))
                           (aliases . ,(or aliases [])))
                          matches))))))
          
          ;; Search by alias using database query
          (let ((alias-matches (org-roam-db-query 
                               [:select :distinct [nodes:id nodes:title nodes:file nodes:level aliases:alias]
                                :from nodes
                                :inner-join aliases
                                :on (= nodes:id aliases:node-id)
                                :where (like (lower aliases:alias) $s1)]
                               query-pattern)))
            (dolist (match alias-matches)
              (let* ((id (nth 0 match))
                     (title (nth 1 match))
                     (file (nth 2 match))
                     (level (nth 3 match)))
                (unless (gethash id node-ids-found)
                  (puthash id t node-ids-found)
                  (let ((tags (mapcar #'car (org-roam-db-query 
                                            [:select [tag] :from tags :where (= node-id $s1)] 
                                            id)))
                        (aliases (mapcar #'car (org-roam-db-query 
                                               [:select [alias] :from aliases :where (= node-id $s1)] 
                                               id))))
                    (push `((id . ,id)
                           (title . ,title)
                           (file . ,file)
                           (level . ,level)
                           (tags . ,(or tags []))
                           (aliases . ,(or aliases [])))
                          matches))))))
          
          (if matches
              (md-roam-server--create-success-response
               (format "Found %d nodes matching '%s'" (length matches) query)
               `((query . ,query)
                 (nodes . ,(nreverse matches))
                 (count . ,(length matches))))
            (md-roam-server--create-success-response
             (format "No nodes found matching '%s'" query)
             `((query . ,query)
               (nodes . [])
               (count . 0))))))
    (error
     (md-roam-server--create-error-response
      (format "Error searching nodes: %s" (error-message-string err))
      `((query . ,query))))))

(defun md-roam-server-sync-database ()
  "Sync org-roam database and return status information."
  (condition-case err
      (progn
        ;; Initialize org-roam
        (md-roam-server-init-org-roam)
        
        ;; Get initial node count
        (let ((initial-count (length (org-roam-node-list))))
          
          ;; Perform sync
          (org-roam-db-sync)
          
          ;; Get final node count
          (let ((final-count (length (org-roam-node-list))))
            (md-roam-server--create-success-response
             "Database sync completed"
             `((initial_count . ,initial-count)
               (final_count . ,final-count)
               (nodes_changed . ,(- final-count initial-count))
               (directory . ,org-roam-directory))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error syncing database: %s" (error-message-string err))
      `((directory . ,(md-roam-server--safe-directory)))))))

(defun md-roam-server-create-node (title &optional tags content aliases category refs)
  "Create a new org-roam node with TITLE, optional TAGS, CONTENT, ALIASES, CATEGORY, and REFS."
  (condition-case err
      (progn
        ;; Initialize org-roam
        (md-roam-server-init-org-roam)
        
        ;; Generate unique ID
        (let* ((id (org-id-new))
               (slug (org-roam-node-slug (org-roam-node-create :title title)))
               (filename (format "%s-%s.md" 
                               (format-time-string "%Y%m%d%H%M%S")
                               slug))
               (filepath (expand-file-name filename org-roam-directory))
               (tag-list (if tags (mapcar (lambda (tag) (format "#%s" tag)) tags) '()))
               (yaml-front-matter (md-roam-server--build-yaml-front-matter id title category aliases refs))
               (tag-line (if tag-list (format "%s\n\n" (mapconcat 'identity tag-list " ")) ""))
               (file-content (format "%s%s%s"
                                   yaml-front-matter
                                   tag-line
                                   (or content ""))))
          
          ;; Write file
          (with-temp-file filepath
            (insert file-content))
          
          ;; Try to register aliases manually in database if possible
          (when aliases
            (with-temp-buffer
              (insert-file-contents filepath)
              (goto-char (point-min))
              ;; Force org-roam to process this file
              (org-mode)
              (org-roam-db-update-file filepath)))
          
          ;; Sync database to register the new node
          (org-roam-db-sync)
          
          ;; Return node information
          (md-roam-server--create-success-response
           "Node created successfully"
           `((id . ,id)
             (title . ,title)
             (file . ,filepath)
             (category . ,(or category ""))
             (tags . ,(or tags []))
             (aliases . ,(or aliases []))
             (refs . ,(or refs []))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error creating node: %s" (error-message-string err))))))

(defun md-roam-server-update-node (node-id title &optional tags content aliases category refs)
  "Update an existing org-roam node with NODE-ID, TITLE, optional TAGS, CONTENT, ALIASES, CATEGORY, and REFS."
  (condition-case err
      (progn
        ;; Initialize org-roam
        (md-roam-server-init-org-roam)
        
        ;; Get existing node information
        (let ((node-data (org-roam-db-query 
                          [:select [id title file level todo pos]
                           :from nodes
                           :where (= id $s1)]
                          node-id)))
          (if node-data
              (let* ((node-info (car node-data))
                     (existing-file (nth 2 node-info))
                     (filepath existing-file))
                
                ;; Validate file exists and is writable
                (unless (and filepath 
                            (file-exists-p filepath)
                            (file-writable-p filepath))
                  (error "Node file not found or not writable"))
                
                ;; Security check - ensure file is within org-roam directory
                (let ((directory org-roam-directory))
                  (unless (string-prefix-p (file-truename directory)
                                          (file-truename filepath))
                    (error "File access denied - not within org-roam directory")))
                
                ;; Update file content
                (let* ((yaml-front-matter
                        (concat "---\n"
                               (format "id: %s\n" node-id)
                               (format "title: %s\n" title)
                               (when category (format "category: %s\n" category))
                               (when (and aliases (> (length aliases) 0))
                                 (format "roam_aliases: [%s]\n"
                                         (mapconcat (lambda (alias) (format "\"%s\"" alias)) aliases ", ")))
                               (when (and refs (> (length refs) 0))
                                 (format "roam_refs: %s\n" (if (listp refs) (string-join refs " ") refs)))
                               "---\n\n"))
                       (full-content (concat yaml-front-matter (or content ""))))
                  
                  ;; Write updated content to file
                  (with-temp-file filepath
                    (insert full-content))
                  
                  ;; Update org-roam database
                  (org-roam-update-org-id-locations)
                  (org-roam-db-sync)
                  
                  ;; Return success response
                  (md-roam-server--create-success-response
                   (format "Node updated successfully: %s" title)
                   `((id . ,node-id)
                     (title . ,title)
                     (file . ,(file-relative-name filepath org-roam-directory))
                     (category . ,(or category ""))
                     (tags . ,(or tags []))
                     (aliases . ,(or aliases []))
                     (refs . ,(or refs []))))))
            
            ;; Node not found
            (md-roam-server--create-error-response
             (format "Node not found: %s" node-id)
             `((node_id . ,node-id))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error updating node: %s" (error-message-string err))
      `((node_id . ,node-id))))))

(defun md-roam-server-delete-node (node-id)
  "Delete an org-roam node by NODE-ID."
  (condition-case err
      (progn
        ;; Initialize org-roam
        (md-roam-server-init-org-roam)
        
        ;; Get existing node information
        (let ((node-data (org-roam-db-query 
                          [:select [id title file level todo pos]
                           :from nodes
                           :where (= id $s1)]
                          node-id)))
          (if node-data
              (let* ((node-info (car node-data))
                     (title (nth 1 node-info))
                     (filepath (nth 2 node-info)))
                
                ;; Validate file exists
                (unless (and filepath (file-exists-p filepath))
                  (error "Node file not found"))
                
                ;; Security check - ensure file is within org-roam directory
                (let ((directory org-roam-directory))
                  (unless (string-prefix-p (file-truename directory)
                                          (file-truename filepath))
                    (error "File access denied - not within org-roam directory")))
                
                ;; Delete file
                (delete-file filepath)
                
                ;; Update org-roam database
                (org-roam-db-sync)
                
                ;; Return success response
                (md-roam-server--create-success-response
                 (format "Node deleted successfully: %s" title)
                 `((id . ,node-id)
                   (title . ,title)
                   (file . ,(file-relative-name filepath org-roam-directory)))))
            
            ;; Node not found
            (md-roam-server--create-error-response
             (format "Node not found: %s" node-id)
             `((node_id . ,node-id))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error deleting node: %s" (error-message-string err))
      `((node_id . ,node-id))))))

(defun md-roam-server-get-node-backlinks (node-id)
  "Get all nodes that link to NODE-ID (backlinks)."
  (condition-case err
      (progn
        (md-roam-server-init-org-roam)
        (let ((backlinks (org-roam-db-query 
                          [:select [source dest type]
                           :from links
                           :where (= dest $s1)]
                          node-id)))
          (if backlinks
              (let ((backlink-nodes '()))
                (dolist (link-row backlinks)
                  (let* ((source-id (nth 0 link-row))
                         (link-type (nth 2 link-row))
                         (node-data (org-roam-db-query 
                                     [:select [id title file level]
                                      :from nodes
                                      :where (= id $s1)]
                                     source-id)))
                    (when node-data
                      (let* ((node-info (car node-data))
                             (title (nth 1 node-info))
                             (file (nth 2 node-info))
                             (level (nth 3 node-info)))
                        (push `((id . ,source-id)
                               (title . ,title)
                               (file . ,(file-relative-name file org-roam-directory))
                               (level . ,level)
                               (link_type . ,(or link-type "id")))
                              backlink-nodes)))))
                
                (md-roam-server--create-success-response
                 (format "Found %d backlinks for node" (length backlink-nodes))
                 `((node_id . ,node-id)
                   (backlinks . ,(nreverse backlink-nodes))
                   (count . ,(length backlink-nodes)))))
            
            (md-roam-server--create-success-response
             "No backlinks found"
             `((node_id . ,node-id)
               (backlinks . [])
               (count . 0))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving backlinks: %s" (error-message-string err))
      `((node_id . ,node-id))))))

(defun md-roam-server-get-node-links (node-id)
  "Get all nodes that NODE-ID links to (forward links)."
  (condition-case err
      (progn
        (md-roam-server-init-org-roam)
        (let ((links (org-roam-db-query 
                      [:select [source dest type]
                       :from links
                       :where (= source $s1)]
                      node-id)))
          (if links
              (let ((linked-nodes '()))
                (dolist (link-row links)
                  (let* ((dest-id (nth 1 link-row))
                         (link-type (nth 2 link-row))
                         (node-data (org-roam-db-query 
                                     [:select [id title file level]
                                      :from nodes
                                      :where (= id $s1)]
                                     dest-id)))
                    (when node-data
                      (let* ((node-info (car node-data))
                             (title (nth 1 node-info))
                             (file (nth 2 node-info))
                             (level (nth 3 node-info)))
                        (push `((id . ,dest-id)
                               (title . ,title)
                               (file . ,(file-relative-name file org-roam-directory))
                               (level . ,level)
                               (link_type . ,(or link-type "id")))
                              linked-nodes)))))
                
                (md-roam-server--create-success-response
                 (format "Found %d forward links from node" (length linked-nodes))
                 `((node_id . ,node-id)
                   (links . ,(nreverse linked-nodes))
                   (count . ,(length linked-nodes)))))
            
            (md-roam-server--create-success-response
             "No forward links found"
             `((node_id . ,node-id)
               (links . [])
               (count . 0))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving forward links: %s" (error-message-string err))
      `((node_id . ,node-id))))))

(defun md-roam-server-get-nodes-by-alias (alias)
  "Get all nodes that have the specified ALIAS."
  (condition-case err
      (progn
        (md-roam-server-init-org-roam)
        (let ((alias-matches (org-roam-db-query 
                              [:select [node-id]
                               :from aliases
                               :where (= alias $s1)]
                              alias)))
          (if alias-matches
              (let ((nodes '()))
                (dolist (match alias-matches)
                  (let* ((node-id (nth 0 match))
                         (node-data (org-roam-db-query 
                                     [:select [id title file level]
                                      :from nodes
                                      :where (= id $s1)]
                                     node-id)))
                    (when node-data
                      (let* ((node-info (car node-data))
                             (title (nth 1 node-info))
                             (file (nth 2 node-info))
                             (level (nth 3 node-info))
                             (tags (mapcar #'car (org-roam-db-query 
                                                  [:select [tag] :from tags :where (= node-id $s1)] 
                                                  node-id)))
                             (aliases (mapcar #'car (org-roam-db-query 
                                                     [:select [alias] :from aliases :where (= node-id $s1)] 
                                                     node-id))))
                        (push `((id . ,node-id)
                               (title . ,title)
                               (file . ,(file-relative-name file org-roam-directory))
                               (level . ,level)
                               (tags . ,(or tags []))
                               (aliases . ,(or aliases [])))
                              nodes)))))
                
                (md-roam-server--create-success-response
                 (format "Found %d nodes with alias '%s'" (length nodes) alias)
                 `((alias . ,alias)
                   (nodes . ,(nreverse nodes))
                   (count . ,(length nodes)))))
            
            (md-roam-server--create-success-response
             (format "No nodes found with alias '%s'" alias)
             `((alias . ,alias)
               (nodes . [])
               (count . 0))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error searching nodes by alias: %s" (error-message-string err))
      `((alias . ,alias))))))

(defun md-roam-server-get-nodes-by-ref (ref)
  "Get all nodes that have the specified REF."
  (condition-case err
      (progn
        (md-roam-server-init-org-roam)
        (let ((ref-matches (org-roam-db-query 
                            [:select [node-id]
                             :from refs
                             :where (= ref $s1)]
                            ref)))
          (if ref-matches
              (let ((nodes '()))
                (dolist (match ref-matches)
                  (let* ((node-id (nth 0 match))
                         (node-data (org-roam-db-query 
                                     [:select [id title file level]
                                      :from nodes
                                      :where (= id $s1)]
                                     node-id)))
                    (when node-data
                      (let* ((node-info (car node-data))
                             (title (nth 1 node-info))
                             (file (nth 2 node-info))
                             (level (nth 3 node-info))
                             (tags (mapcar #'car (org-roam-db-query 
                                                  [:select [tag] :from tags :where (= node-id $s1)] 
                                                  node-id)))
                             (aliases (mapcar #'car (org-roam-db-query 
                                                     [:select [alias] :from aliases :where (= node-id $s1)] 
                                                     node-id))))
                        (push `((id . ,node-id)
                               (title . ,title)
                               (file . ,(file-relative-name file org-roam-directory))
                               (level . ,level)
                               (tags . ,(or tags []))
                               (aliases . ,(or aliases [])))
                              nodes)))))
                
                (md-roam-server--create-success-response
                 (format "Found %d nodes with ref '%s'" (length nodes) ref)
                 `((ref . ,ref)
                   (nodes . ,(nreverse nodes))
                   (count . ,(length nodes)))))
            
            (md-roam-server--create-success-response
             (format "No nodes found with ref '%s'" ref)
             `((ref . ,ref)
               (nodes . [])
               (count . 0))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error searching nodes by ref: %s" (error-message-string err))
      `((ref . ,ref))))))

(defun md-roam-server-get-nodes-by-citation (citation)
  "Get all nodes that have the specified CITATION."
  (condition-case err
      (progn
        (md-roam-server-init-org-roam)
        (let ((citation-matches (org-roam-db-query 
                                 [:select [node-id]
                                  :from citations
                                  :where (= cite-key $s1)]
                                 citation)))
          (if citation-matches
              (let ((nodes '()))
                (dolist (match citation-matches)
                  (let* ((node-id (nth 0 match))
                         (node-data (org-roam-db-query 
                                     [:select [id title file level]
                                      :from nodes
                                      :where (= id $s1)]
                                     node-id)))
                    (when node-data
                      (let* ((node-info (car node-data))
                             (title (nth 1 node-info))
                             (file (nth 2 node-info))
                             (level (nth 3 node-info))
                             (tags (mapcar #'car (org-roam-db-query 
                                                  [:select [tag] :from tags :where (= node-id $s1)] 
                                                  node-id)))
                             (aliases (mapcar #'car (org-roam-db-query 
                                                     [:select [alias] :from aliases :where (= node-id $s1)] 
                                                     node-id))))
                        (push `((id . ,node-id)
                               (title . ,title)
                               (file . ,(file-relative-name file org-roam-directory))
                               (level . ,level)
                               (tags . ,(or tags []))
                               (aliases . ,(or aliases [])))
                              nodes)))))
                
                (md-roam-server--create-success-response
                 (format "Found %d nodes with citation '%s'" (length nodes) citation)
                 `((citation . ,citation)
                   (nodes . ,(nreverse nodes))
                   (count . ,(length nodes)))))
            
            (md-roam-server--create-success-response
             (format "No nodes found with citation '%s'" citation)
             `((citation . ,citation)
               (nodes . [])
               (count . 0))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error searching nodes by citation: %s" (error-message-string err))
      `((citation . ,citation))))))

(defun md-roam-server-get-stats ()
  "Get statistics about the org-roam database."
  (condition-case err
      (progn
        (md-roam-server-init-org-roam)
        (let* ((total-nodes (caar (org-roam-db-query [:select (funcall count *) :from nodes])))
               (total-links (caar (org-roam-db-query [:select (funcall count *) :from links])))
               (total-tags (caar (org-roam-db-query [:select (funcall count :distinct tag) :from tags])))
               (total-aliases (caar (org-roam-db-query [:select (funcall count :distinct alias) :from aliases])))
               (total-refs (caar (org-roam-db-query [:select (funcall count :distinct ref) :from refs])))
               (total-citations (caar (org-roam-db-query [:select (funcall count :distinct cite-key) :from citations])))
               (files-md (caar (org-roam-db-query [:select (funcall count *) :from nodes :where (like file "%.md")])))
               (files-org (caar (org-roam-db-query [:select (funcall count *) :from nodes :where (like file "%.org")]))))
          
          (md-roam-server--create-success-response
           "Statistics retrieved successfully"
           `((total_nodes . ,total-nodes)
             (total_links . ,total-links)
             (total_tags . ,total-tags)
             (total_aliases . ,total-aliases)
             (total_refs . ,total-refs)
             (total_citations . ,total-citations)
             (file_types . ((md . ,files-md)
                           (org . ,files-org)))
             (avg_links_per_node . ,(if (> total-nodes 0) (/ (float total-links) total-nodes) 0))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving statistics: %s" (error-message-string err))
      `((directory . ,(md-roam-server--safe-directory)))))))

(defun md-roam-server-filter (proc string)
  "Process STRING from PROC."
  (setq md-roam-server-request-buffer (concat md-roam-server-request-buffer string))
  (when (string-match "\r\n\r\n" md-roam-server-request-buffer)
    (md-roam-server-handle-request proc md-roam-server-request-buffer)
    (setq md-roam-server-request-buffer "")))

(defun md-roam-server-parse-request-body (request)
  "Parse JSON body from HTTP REQUEST."
  (let* ((lines (split-string request "\r\n"))
         (body-start (cl-position "" lines :test 'string=)))
    (when body-start
      (let ((body (mapconcat 'identity (nthcdr (1+ body-start) lines) "\r\n")))
        (when (> (length (string-trim body)) 0)
          (condition-case err
              (json-read-from-string body)
            (error nil)))))))

(defun md-roam-server-handle-request (proc request)
  "Handle HTTP REQUEST from PROC."
  (let* ((request-parts (md-roam-server--parse-request-line request))
         (method (car request-parts))
         (path (cadr request-parts))
         (body (md-roam-server-parse-request-body request)))
    (cond
     ((and (string= method "GET") (string= path "/files"))
      (let ((files (md-roam-server-get-files)))
        (md-roam-server-send-response proc 200 "application/json"
                                     (json-encode `((files . ,files)
                                                  (count . ,(length files)))))))
     ((and (string= method "GET") (string= path "/files/raw"))
      (let ((result (md-roam-server-get-raw-files)))
        (md-roam-server-send-response proc 200 "application/json"
                                     (json-encode result))))
     ((and (string= method "GET") (string-match "^/files/content/.+" path))
      (let ((filepath (substring path (length "/files/content/"))))
        (let ((result (md-roam-server-get-file-content filepath)))
          (if (string= (cdr (assoc 'status result)) "success")
              (md-roam-server-send-response proc 200 "application/json"
                                           (json-encode result))
            (md-roam-server-send-response proc 404 "application/json"
                                         (json-encode result))))))
     ((and (string= method "GET") (string= path "/tags"))
      (let ((result (md-roam-server-get-tags)))
        (md-roam-server-send-response proc 200 "application/json"
                                     (json-encode result))))
     ((and (string= method "GET") (string= path "/aliases"))
      (let ((result (md-roam-server-get-aliases)))
        (md-roam-server-send-response proc 200 "application/json"
                                     (json-encode result))))
     ((and (string= method "GET") (string= path "/refs"))
      (let ((result (md-roam-server-get-refs)))
        (md-roam-server-send-response proc 200 "application/json"
                                     (json-encode result))))
     ((and (string= method "GET") (string= path "/citations"))
      (let ((result (md-roam-server-get-citations)))
        (md-roam-server-send-response proc 200 "application/json"
                                     (json-encode result))))
     ((and (string= method "GET") (string= path "/stats"))
      (let ((result (md-roam-server-get-stats)))
        (md-roam-server-send-response proc 200 "application/json"
                                     (json-encode result))))
     ((and (string= method "GET") (string-match "^/aliases/.*/nodes$" path))
      (let ((params (md-roam-server--match-path-pattern path "/aliases/:alias/nodes")))
        (if params
            (let* ((alias (cdr (assoc 'alias params)))
                   (result (md-roam-server-get-nodes-by-alias alias)))
              (md-roam-server-send-response proc 200 "application/json"
                                           (json-encode result)))
          (md-roam-server-send-response proc 400 "application/json"
                                       (json-encode '((error . "Bad Request")
                                                    (message . "Invalid alias parameter")))))))
     ((and (string= method "GET") (string-match "^/refs/.*/nodes$" path))
      (let ((params (md-roam-server--match-path-pattern path "/refs/:ref/nodes")))
        (if params
            (let* ((ref (cdr (assoc 'ref params)))
                   (result (md-roam-server-get-nodes-by-ref ref)))
              (md-roam-server-send-response proc 200 "application/json"
                                           (json-encode result)))
          (md-roam-server-send-response proc 400 "application/json"
                                       (json-encode '((error . "Bad Request")
                                                    (message . "Invalid ref parameter")))))))
     ((and (string= method "GET") (string-match "^/citations/.*/nodes$" path))
      (let ((params (md-roam-server--match-path-pattern path "/citations/:citation/nodes")))
        (if params
            (let* ((citation (cdr (assoc 'citation params)))
                   (result (md-roam-server-get-nodes-by-citation citation)))
              (md-roam-server-send-response proc 200 "application/json"
                                           (json-encode result)))
          (md-roam-server-send-response proc 400 "application/json"
                                       (json-encode '((error . "Bad Request")
                                                    (message . "Invalid citation parameter")))))))
     ((and (string= method "GET") (string-match "^/tags/.*/nodes$" path))
      (let ((params (md-roam-server--match-path-pattern path "/tags/:tag/nodes")))
        (if params
            (let* ((tag (cdr (assoc 'tag params)))
                   (result (md-roam-server-get-nodes-by-tag tag)))
              (md-roam-server-send-response proc 200 "application/json"
                                           (json-encode result)))
          (md-roam-server-send-response proc 400 "application/json"
                                       (json-encode '((error . "Bad Request")
                                                    (message . "Invalid tag parameter")))))))
     ((and (string= method "GET") (string-match "^/nodes/.*/aliases$" path))
      (let ((params (md-roam-server--match-path-pattern path "/nodes/:id/aliases")))
        (if params
            (let* ((node-id (cdr (assoc 'id params)))
                   (result (md-roam-server-get-node-aliases node-id)))
              (if (string= (cdr (assoc 'status result)) "success")
                  (md-roam-server-send-response proc 200 "application/json"
                                               (json-encode result))
                (md-roam-server-send-response proc 404 "application/json"
                                             (json-encode result))))
          (md-roam-server-send-response proc 400 "application/json"
                                       (json-encode '((error . "Bad Request")
                                                    (message . "Invalid node ID parameter")))))))
     ((and (string= method "GET") (string-match "^/nodes/.*/refs$" path))
      (let ((params (md-roam-server--match-path-pattern path "/nodes/:id/refs")))
        (if params
            (let* ((node-id (cdr (assoc 'id params)))
                   (result (md-roam-server-get-node-refs node-id)))
              (if (string= (cdr (assoc 'status result)) "success")
                  (md-roam-server-send-response proc 200 "application/json"
                                               (json-encode result))
                (md-roam-server-send-response proc 404 "application/json"
                                             (json-encode result))))
          (md-roam-server-send-response proc 400 "application/json"
                                       (json-encode '((error . "Bad Request")
                                                    (message . "Invalid node ID parameter")))))))
     ((and (string= method "GET") (string-match "^/search/.*" path))
      (let ((query (substring path (length "/search/"))))
        (if (and query (> (length query) 0))
            (let ((result (md-roam-server-search-nodes-by-title-or-alias (url-unhex-string query))))
              (md-roam-server-send-response proc 200 "application/json"
                                           (json-encode result)))
          (md-roam-server-send-response proc 400 "application/json"
                                       (json-encode '((error . "Bad Request")
                                                    (message . "Search query is required")))))))
     ((and (string= method "GET") (string-match "^/nodes/.*/backlinks$" path))
      (let ((node-id (md-roam-server--extract-path-param path "/nodes/:id/backlinks")))
        (if node-id
            (let ((result (md-roam-server-get-node-backlinks node-id)))
              (if (string= (cdr (assoc 'status result)) "success")
                  (md-roam-server-send-response proc 200 "application/json"
                                               (json-encode result))
                (md-roam-server-send-response proc 404 "application/json"
                                             (json-encode result))))
          (md-roam-server-send-response proc 400 "application/json"
                                       (json-encode '((error . "Bad Request")
                                                    (message . "Node ID is required")))))))
     ((and (string= method "GET") (string-match "^/nodes/.*/links$" path))
      (let ((node-id (md-roam-server--extract-path-param path "/nodes/:id/links")))
        (if node-id
            (let ((result (md-roam-server-get-node-links node-id)))
              (if (string= (cdr (assoc 'status result)) "success")
                  (md-roam-server-send-response proc 200 "application/json"
                                               (json-encode result))
                (md-roam-server-send-response proc 404 "application/json"
                                             (json-encode result))))
          (md-roam-server-send-response proc 400 "application/json"
                                       (json-encode '((error . "Bad Request")
                                                    (message . "Node ID is required")))))))
     ((and (string= method "GET") (string-match "^/nodes/.*/parse$" path))
      (let ((node-id (md-roam-server--extract-path-param path "/nodes/:id/parse")))
        (if node-id
            (let ((result (md-roam-server-parse-file-by-node-id node-id)))
              (if (string= (cdr (assoc 'status result)) "success")
                  (md-roam-server-send-response proc 200 "application/json"
                                               (json-encode result))
                (md-roam-server-send-response proc 404 "application/json"
                                             (json-encode result))))
          (md-roam-server-send-response proc 400 "application/json"
                                       (json-encode '((error . "Bad Request")
                                                    (message . "Node ID is required")))))))
     ((and (string= method "GET") (string-match "^/nodes/.*/content$" path))
      (let ((node-id (md-roam-server--extract-path-param path "/nodes/:id/content")))
        (if node-id
            (let ((result (md-roam-server-get-file-content-by-node-id node-id)))
              (if (string= (cdr (assoc 'status result)) "success")
                  (md-roam-server-send-response proc 200 "application/json"
                                               (json-encode result))
                (md-roam-server-send-response proc 404 "application/json"
                                             (json-encode result))))
          (md-roam-server-send-response proc 400 "application/json"
                                       (json-encode '((error . "Bad Request")
                                                    (message . "Node ID is required")))))))
     ((and (string= method "GET") (string-prefix-p "/nodes/" path))
      (let ((node-id (md-roam-server--extract-path-param path "/nodes/:id")))
        (if node-id
            (let ((result (md-roam-server-get-node-by-id node-id)))
              (if (string= (cdr (assoc 'status result)) "success")
                  (md-roam-server-send-response proc 200 "application/json"
                                               (json-encode result))
                (md-roam-server-send-response proc 404 "application/json"
                                             (json-encode result))))
          (md-roam-server-send-response proc 400 "application/json"
                                       (json-encode '((error . "Bad Request")
                                                    (message . "Node ID is required")))))))
     ((and (string= method "POST") (string= path "/sync"))
      (let ((sync-result (md-roam-server-sync-database)))
        (md-roam-server-send-response proc 200 "application/json"
                                     (json-encode sync-result))))
     ((and (string= method "GET") (string= path "/nodes"))
      (let ((result (md-roam-server-get-all-nodes)))
        (md-roam-server-send-response proc 200 "application/json"
                                     (json-encode result))))
     ((and (string= method "PUT") (string-prefix-p "/nodes/" path))
      (let ((node-id (md-roam-server--extract-path-param path "/nodes/:id")))
        (if (and node-id body)
            (let* ((params (md-roam-server--extract-body-params body '(title tags content aliases category refs)))
                   (title (nth 0 params))
                   (tags (nth 1 params))
                   (content (nth 2 params))
                   (aliases (nth 3 params))
                   (category (nth 4 params))
                   (refs (nth 5 params)))
              (if title
                  (let ((result (md-roam-server-update-node node-id title tags content aliases category refs)))
                    (if (string= (cdr (assoc 'status result)) "success")
                        (md-roam-server-send-response proc 200 "application/json"
                                                     (json-encode result))
                      (md-roam-server-send-response proc 404 "application/json"
                                                   (json-encode result))))
                (md-roam-server-send-response proc 400 "application/json"
                                             (json-encode '((error . "Bad Request")
                                                          (message . "Title is required"))))))
          (md-roam-server-send-response proc 400 "application/json"
                                       (json-encode '((error . "Bad Request")
                                                    (message . "Node ID and JSON body required")))))))
     ((and (string= method "DELETE") (string-prefix-p "/nodes/" path))
      (let ((node-id (md-roam-server--extract-path-param path "/nodes/:id")))
        (if node-id
            (let ((result (md-roam-server-delete-node node-id)))
              (if (string= (cdr (assoc 'status result)) "success")
                  (md-roam-server-send-response proc 200 "application/json"
                                               (json-encode result))
                (md-roam-server-send-response proc 404 "application/json"
                                             (json-encode result))))
          (md-roam-server-send-response proc 400 "application/json"
                                       (json-encode '((error . "Bad Request")
                                                    (message . "Node ID is required")))))))
     ((and (string= method "POST") (string= path "/nodes"))
      (if body
          (let* ((params (md-roam-server--extract-body-params body '(title tags content aliases category refs)))
                 (title (nth 0 params))
                 (tags (nth 1 params))
                 (content (nth 2 params))
                 (aliases (nth 3 params))
                 (category (nth 4 params))
                 (refs (nth 5 params)))
            (if title
                (let ((result (md-roam-server-create-node title tags content aliases category refs)))
                  (md-roam-server-send-response proc 200 "application/json"
                                               (json-encode result)))
              (md-roam-server-send-response proc 400 "application/json"
                                           (json-encode '((error . "Bad Request")
                                                        (message . "Title is required"))))))
        (md-roam-server-send-response proc 400 "application/json"
                                     (json-encode '((error . "Bad Request")
                                                  (message . "JSON body required"))))))
     ((and (string= method "GET") (string= path "/openapi.json"))
      (md-roam-server-send-response proc 200 "application/json"
                                   (json-encode '((openapi . "3.0.0")
                                                (info . ((title . "md-roam Server API")
                                                         (version . "1.0.0")))
                                                (paths . (("/files" . "Get files")
                                                         ("/files/raw" . "Get raw file listing")
                                                         ("/files/content/:filepath" . "Get file content")
                                                         ("/tags" . "Get tags list with node IDs")
                                                         ("/aliases" . "Get aliases list with node IDs")
                                                         ("/refs" . "Get refs list with node IDs")
                                                         ("/citations" . "Get citations list with node IDs")
                                                         ("/stats" . "Get database statistics")
                                                         ("/aliases/:alias/nodes" . "Get nodes by alias")
                                                         ("/refs/:ref/nodes" . "Get nodes by ref")
                                                         ("/citations/:citation/nodes" . "Get nodes by citation")
                                                         ("/tags/:tag/nodes" . "Get nodes by tag")
                                                         ("/search/:query" . "Search nodes by title or alias")
                                                         ("/nodes" . "Get all nodes")
                                                         ("/nodes/:id" . "Get single node")
                                                         ("/nodes/:id/content" . "Get node file content")
                                                         ("/nodes/:id/parse" . "Parse node file (metadata and body)")
                                                         ("/nodes/:id/backlinks" . "Get node backlinks")
                                                         ("/nodes/:id/links" . "Get node forward links")
                                                         ("/nodes/:id/aliases" . "Get node aliases")
                                                         ("/nodes/:id/refs" . "Get node refs")
                                                         ("/sync" . "Sync database")
                                                         ("/nodes" . "Create node")))))))
     (t
      (md-roam-server-send-response proc 404 "application/json"
                                   (json-encode '((error . "Not Found")
                                                (message . "Endpoint not found"))))))))

(defun md-roam-server-send-response (proc status content-type body)
  "Send HTTP response to PROC with STATUS, CONTENT-TYPE and BODY."
  (let* ((status-text (cond ((= status 200) "OK")
                           ((= status 404) "Not Found")
                           (t "Error")))
         (response (format "HTTP/1.1 %d %s\r\nContent-Type: %s\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
                          status status-text content-type (length body) body)))
    (process-send-string proc response)
    (delete-process proc)))

(defun md-roam-server-start (&optional port)
  "Start the md-roam HTTP server on PORT (default 8080)."
  (interactive "P")
  (let ((server-port (or port md-roam-server-port)))
    (when md-roam-server-process
      (md-roam-server-stop))
    
    (setq md-roam-server-process
          (make-network-process
           :name "md-roam-server"
           :service server-port
           :server t
           :family 'ipv4
           :filter 'md-roam-server-filter))
    
    (message "md-roam server started on port %d" server-port)))

(defun md-roam-server-stop ()
  "Stop the md-roam HTTP server."
  (interactive)
  (when md-roam-server-process
    (delete-process md-roam-server-process)
    (setq md-roam-server-process nil)
    (message "md-roam server stopped")))

(defun md-roam-server-restart (&optional port)
  "Restart the md-roam HTTP server on PORT."
  (interactive "P")
  (md-roam-server-stop)
  (sleep-for 1)
  (md-roam-server-start port))

(provide 'md-roam-server)

;;; md-roam-server.el ends here