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
               (full-path . ,full-path)
               (size . ,size)
               (modified . ,(format-time-string "%Y-%m-%d %H:%M:%S" modified))
               (content . ,content))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error reading file '%s': %s" filepath (error-message-string err))
      `((filepath . ,filepath)
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

(defun md-roam-server-get-tags ()
  "Get list of all unique tags from org-roam nodes."
  (condition-case err
      (progn
        ;; Initialize org-roam
        (md-roam-server-init-org-roam)
        
        ;; Get all nodes and collect tags
        (let ((nodes (org-roam-node-list))
              (all-tags '())
              (tag-counts (make-hash-table :test 'equal)))
          (if nodes
              (progn
                ;; Collect all tags from all nodes
                (dolist (node nodes)
                  (let ((node-tags (org-roam-node-tags node)))
                    (when node-tags
                      (dolist (tag node-tags)
                        (when tag
                          (setq all-tags (cons tag all-tags))
                          (puthash tag (1+ (gethash tag tag-counts 0)) tag-counts))))))
                
                ;; Remove duplicates and create result
                (let ((unique-tags (delete-dups all-tags))
                      (tag-list '()))
                  (dolist (tag unique-tags)
                    (push `((tag . ,tag)
                           (count . ,(gethash tag tag-counts 0)))
                          tag-list))
                  
                  (md-roam-server--create-success-response
                   "Tags retrieved successfully"
                   `((tags . ,(nreverse tag-list))
                     (total-tags . ,(length unique-tags))
                     (total-usage . ,(length all-tags))))))
            
            ;; If no nodes, return empty list
            (md-roam-server--create-success-response
             "No tags found"
             `((tags . [])
               (total-tags . 0)
               (total-usage . 0))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving tags: %s" (error-message-string err))
      `((directory . ,(md-roam-server--safe-directory)))))))

(defun md-roam-server-get-tags-detailed ()
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
                                      (node-ids . ,(nreverse (delete-dups node-ids))))
                                     tag-list)))
                           tag-data)
                  
                  (md-roam-server--create-success-response
                   "Tags with node IDs retrieved successfully"
                   `((tags . ,(nreverse tag-list))
                     (total-tags . ,(hash-table-count tag-data))))))
            
            ;; If no nodes, return empty list
            (md-roam-server--create-success-response
             "No tags found"
             `((tags . [])
               (total-tags . 0))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving detailed tags: %s" (error-message-string err))
      `((directory . ,(md-roam-server--safe-directory)))))))

(defun md-roam-server-get-aliases ()
  "Get list of all unique aliases from org-roam nodes."
  (condition-case err
      (progn
        ;; Initialize org-roam
        (md-roam-server-init-org-roam)
        
        ;; Get all nodes and collect aliases
        (let ((nodes (org-roam-node-list))
              (all-aliases '())
              (alias-counts (make-hash-table :test 'equal)))
          (if nodes
              (progn
                ;; Collect all aliases from all nodes
                (dolist (node nodes)
                  (let ((node-aliases (org-roam-node-aliases node)))
                    (when node-aliases
                      (dolist (alias node-aliases)
                        (when alias
                          (setq all-aliases (cons alias all-aliases))
                          (puthash alias (1+ (gethash alias alias-counts 0)) alias-counts))))))
                
                ;; Remove duplicates and create result
                (let ((unique-aliases (delete-dups all-aliases))
                      (alias-list '()))
                  (dolist (alias unique-aliases)
                    (push `((alias . ,alias)
                           (count . ,(gethash alias alias-counts 0)))
                          alias-list))
                  
                  (md-roam-server--create-success-response
                   "Aliases retrieved successfully"
                   `((aliases . ,(nreverse alias-list))
                     (total-aliases . ,(length unique-aliases))
                     (total-usage . ,(length all-aliases))))))
            
            ;; If no nodes, return empty list
            (md-roam-server--create-success-response
             "No aliases found"
             `((aliases . [])
               (total-aliases . 0)
               (total-usage . 0))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving aliases: %s" (error-message-string err))
      `((directory . ,(md-roam-server--safe-directory)))))))

(defun md-roam-server-get-aliases-detailed ()
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
                                      (node-ids . ,(nreverse (delete-dups node-ids))))
                                     alias-list)))
                           alias-data)
                  
                  (md-roam-server--create-success-response
                   "Aliases with node IDs retrieved successfully"
                   `((aliases . ,(nreverse alias-list))
                     (total-aliases . ,(hash-table-count alias-data))))))
            
            ;; If no nodes, return empty list
            (md-roam-server--create-success-response
             "No aliases found"
             `((aliases . [])
               (total-aliases . 0))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving detailed aliases: %s" (error-message-string err))
      `((directory . ,(md-roam-server--safe-directory)))))))

(defun md-roam-server-get-refs ()
  "Get list of all unique refs from org-roam nodes."
  (condition-case err
      (progn
        ;; Initialize org-roam
        (md-roam-server-init-org-roam)
        
        ;; Get all nodes and collect refs
        (let ((nodes (org-roam-node-list))
              (all-refs '())
              (ref-counts (make-hash-table :test 'equal)))
          (if nodes
              (progn
                ;; Collect all refs from all nodes
                (dolist (node nodes)
                  (let ((node-refs (org-roam-node-refs node)))
                    (when node-refs
                      (dolist (ref node-refs)
                        (when ref
                          (setq all-refs (cons ref all-refs))
                          (puthash ref (1+ (gethash ref ref-counts 0)) ref-counts))))))
                
                ;; Remove duplicates and create result
                (let ((unique-refs (delete-dups all-refs))
                      (ref-list '()))
                  (dolist (ref unique-refs)
                    (push `((ref . ,ref)
                           (count . ,(gethash ref ref-counts 0)))
                          ref-list))
                  
                  (md-roam-server--create-success-response
                   "Refs retrieved successfully"
                   `((refs . ,(nreverse ref-list))
                     (total-refs . ,(length unique-refs))
                     (total-usage . ,(length all-refs))))))
            
            ;; If no nodes, return empty list
            (md-roam-server--create-success-response
             "No refs found"
             `((refs . [])
               (total-refs . 0)
               (total-usage . 0))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving refs: %s" (error-message-string err))
      `((directory . ,(md-roam-server--safe-directory)))))))

(defun md-roam-server-get-refs-detailed ()
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
                                      (node-ids . ,(nreverse (delete-dups node-ids))))
                                     ref-list)))
                           ref-data)
                  
                  (md-roam-server--create-success-response
                   "Refs with node IDs retrieved successfully"
                   `((refs . ,(nreverse ref-list))
                     (total-refs . ,(hash-table-count ref-data))))))
            
            ;; If no nodes, return empty list
            (md-roam-server--create-success-response
             "No refs found"
             `((refs . [])
               (total-refs . 0))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving detailed refs: %s" (error-message-string err))
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
             `((initial-count . ,initial-count)
               (final-count . ,final-count)
               (nodes-changed . ,(- final-count initial-count))
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
     ((and (string= method "GET") (string= path "/hello"))
      (md-roam-server-send-response proc 200 "application/json"
                                   (json-encode '((message . "Hello, World!")
                                                (service . "md-roam-server")
                                                (status . "running")))))
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
     ((and (string= method "GET") (string= path "/tags/detailed"))
      (let ((result (md-roam-server-get-tags-detailed)))
        (md-roam-server-send-response proc 200 "application/json"
                                     (json-encode result))))
     ((and (string= method "GET") (string= path "/aliases"))
      (let ((result (md-roam-server-get-aliases)))
        (md-roam-server-send-response proc 200 "application/json"
                                     (json-encode result))))
     ((and (string= method "GET") (string= path "/aliases/detailed"))
      (let ((result (md-roam-server-get-aliases-detailed)))
        (md-roam-server-send-response proc 200 "application/json"
                                     (json-encode result))))
     ((and (string= method "GET") (string= path "/refs"))
      (let ((result (md-roam-server-get-refs)))
        (md-roam-server-send-response proc 200 "application/json"
                                     (json-encode result))))
     ((and (string= method "GET") (string= path "/refs/detailed"))
      (let ((result (md-roam-server-get-refs-detailed)))
        (md-roam-server-send-response proc 200 "application/json"
                                     (json-encode result))))
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
                                                (paths . (("/hello" . "Health check")
                                                         ("/files" . "Get files")
                                                         ("/files/raw" . "Get raw file listing")
                                                         ("/files/content/:filepath" . "Get file content")
                                                         ("/tags" . "Get tags list")
                                                         ("/tags/detailed" . "Get tags with node IDs")
                                                         ("/aliases" . "Get aliases list")
                                                         ("/aliases/detailed" . "Get aliases with node IDs")
                                                         ("/refs" . "Get refs list")
                                                         ("/refs/detailed" . "Get refs with node IDs")
                                                         ("/tags/:tag/nodes" . "Get nodes by tag")
                                                         ("/nodes/:id" . "Get single node")
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