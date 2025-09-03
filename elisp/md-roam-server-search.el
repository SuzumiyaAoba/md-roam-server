;;; md-roam-server-search.el --- Search and metadata endpoints for md-roam-server

;;; Commentary:
;; Handles search and metadata collection endpoints: /search, /tags, /stats, /config

;;; Code:

(require 'md-roam-server-core)

;;; Search Operations

(defun md-roam-server-search-nodes (query)
  "Search nodes by QUERY in title or alias."
  (condition-case err
      (progn
        (md-roam-server-init-org-roam)
        (let* ((decoded-query (url-unhex-string query))
               (nodes (org-roam-db-query 
                      [:select [nodes:id nodes:title nodes:file nodes:level]
                       :from nodes
                       :left-join aliases :on (= nodes:id aliases:node-id)
                       :left-join tags :on (= nodes:id tags:node-id)
                       :where (or (like nodes:title $r1) (like aliases:alias $r1) (like tags:tag $r1))
                       :group-by nodes:id
                       :order-by nodes:title]
                      (concat "%" decoded-query "%"))))
          (md-roam-server--create-success-response
           "Search completed successfully"
           `((query . ,decoded-query)
             (results . ,(if nodes
                             (mapcar (lambda (node)
                                       (let ((id (nth 0 node))
                                             (title (nth 1 node))
                                             (file (nth 2 node))
                                             (level (nth 3 node)))
                                         `((id . ,id)
                                           (title . ,title)
                                           (file . ,(file-relative-name file org-roam-directory))
                                           (level . ,level)
                                           (tags . ,(let ((tag-results (org-roam-db-query [:select [tag] :from tags :where (= node-id $s1)] id)))
                                                      (if tag-results (mapcar 'car tag-results) [])))
                                           (aliases . ,(let ((alias-results (org-roam-db-query [:select [alias] :from aliases :where (= node-id $s1)] id)))
                                                         (if alias-results (mapcar 'car alias-results) []))))))
                                     nodes)
                           []))
             (count . ,(length nodes))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error searching nodes: %s" (error-message-string err))
      `((query . ,query))))))

;;; Metadata Collections

(defun md-roam-server-get-tags ()
  "Get all unique tags with usage counts."
  (condition-case err
      (progn
        (md-roam-server-init-org-roam)
        (let ((tag-data (org-roam-db-query [:select [tag (funcall count tag)] :from tags :group-by tag :order-by tag])))
          (md-roam-server--create-success-response
           "Tags retrieved successfully"
           `((tags . ,(mapcar (lambda (tag-info)
                                (let ((tag (nth 0 tag-info))
                                      (count (nth 1 tag-info)))
                                  `((tag . ,tag)
                                    (count . ,count)
                                    (node_ids . ,(mapcar 'car (org-roam-db-query [:select [node-id] :from tags :where (= tag $s1)] tag))))))
                              tag-data))
             (total_tags . ,(length tag-data))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving tags: %s" (error-message-string err))))))

(defun md-roam-server-get-aliases ()
  "Get all unique aliases with usage counts."
  (condition-case err
      (progn
        (md-roam-server-init-org-roam)
        (let ((alias-data (org-roam-db-query [:select [alias (funcall count alias)] :from aliases :group-by alias :order-by alias])))
          (md-roam-server--create-success-response
           "Aliases retrieved successfully"
           `((aliases . ,(mapcar (lambda (alias-info)
                                  (let ((alias (nth 0 alias-info))
                                        (count (nth 1 alias-info)))
                                    `((alias . ,alias)
                                      (count . ,count)
                                      (node_ids . ,(mapcar 'car (org-roam-db-query [:select [node-id] :from aliases :where (= alias $s1)] alias))))))
                                alias-data))
             (total_aliases . ,(length alias-data))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving aliases: %s" (error-message-string err))))))

(defun md-roam-server-get-refs ()
  "Get all unique refs with usage counts."
  (condition-case err
      (progn
        (md-roam-server-init-org-roam)
        (let ((refs-data (org-roam-db-query [:select [ref type (funcall count ref)] :from refs :group-by [ref type] :order-by ref])))
          (md-roam-server--create-success-response
           "Refs retrieved successfully"
           `((refs . ,(mapcar (lambda (ref-info)
                               (let ((ref (nth 0 ref-info))
                                     (type (nth 1 ref-info))
                                     (count (nth 2 ref-info)))
                                 `((ref . ,ref)
                                   (type . ,type)
                                   (count . ,count)
                                   (node_ids . ,(mapcar 'car (org-roam-db-query [:select [node-id] :from refs :where (and (= ref $s1) (= type $s2))] ref type))))))
                             refs-data))
             (total_refs . ,(length refs-data))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving refs: %s" (error-message-string err))))))

(defun md-roam-server-get-citations ()
  "Get all unique citations with usage counts."
  (condition-case err
      (progn
        (md-roam-server-init-org-roam)
        (let ((citations-data (org-roam-db-query [:select [cite-key (funcall count cite-key)] :from citations :group-by cite-key :order-by cite-key])))
          (md-roam-server--create-success-response
           "Citations retrieved successfully"
           `((citations . ,(mapcar (lambda (citation-info)
                                    (let ((citation (nth 0 citation-info))
                                          (count (nth 1 citation-info)))
                                      `((citation . ,citation)
                                        (count . ,count)
                                        (node_ids . ,(mapcar 'car (org-roam-db-query [:select [node-id] :from citations :where (= cite-key $s1)] citation))))))
                                  citations-data))
             (total_citations . ,(length citations-data))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving citations: %s" (error-message-string err))))))

(defun md-roam-server-get-nodes-by-tag (tag)
  "Get nodes that have the specified TAG."
  (condition-case err
      (progn
        (md-roam-server-init-org-roam)
        (let* ((decoded-tag (url-unhex-string tag))
               (nodes (org-roam-db-query 
                      [:select [nodes:id nodes:title nodes:file nodes:level]
                       :from tags
                       :inner-join nodes :on (= tags:node-id nodes:id)
                       :where (= tags:tag $s1)
                       :order-by nodes:title]
                      decoded-tag)))
          (md-roam-server--create-success-response
           (if (> (length nodes) 0)
               (format "Found %d nodes with tag '%s'" (length nodes) decoded-tag)
             (format "No nodes found with tag '%s'" decoded-tag))
           `((tag . ,decoded-tag)
             (nodes . ,(mapcar (lambda (node)
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
      (format "Error retrieving nodes by tag: %s" (error-message-string err))
      `((tag . ,tag))))))

(defun md-roam-server-get-nodes-by-alias (alias)
  "Get nodes that have the specified ALIAS."
  (condition-case err
      (progn
        (md-roam-server-init-org-roam)
        (let* ((decoded-alias (url-unhex-string alias))
               (nodes (org-roam-db-query 
                      [:select [nodes:id nodes:title nodes:file nodes:level]
                       :from aliases
                       :inner-join nodes :on (= aliases:node-id nodes:id)
                       :where (= aliases:alias $s1)
                       :order-by nodes:title]
                      decoded-alias)))
          (md-roam-server--create-success-response
           (if (> (length nodes) 0)
               (format "Found %d nodes with alias '%s'" (length nodes) decoded-alias)
             (format "No nodes found with alias '%s'" decoded-alias))
           `((alias . ,decoded-alias)
             (nodes . ,(mapcar (lambda (node)
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
      (format "Error retrieving nodes by alias: %s" (error-message-string err))
      `((alias . ,alias))))))

(defun md-roam-server-get-nodes-by-ref (ref)
  "Get nodes that have the specified REF."
  (condition-case err
      (progn
        (md-roam-server-init-org-roam)
        (let* ((decoded-ref (url-unhex-string ref))
               (nodes (org-roam-db-query 
                      [:select [nodes:id nodes:title nodes:file nodes:level refs:type]
                       :from refs
                       :inner-join nodes :on (= refs:node-id nodes:id)
                       :where (= refs:ref $s1)
                       :order-by nodes:title]
                      decoded-ref)))
          (md-roam-server--create-success-response
           (if (> (length nodes) 0)
               (format "Found %d nodes with ref '%s'" (length nodes) decoded-ref)
             (format "No nodes found with ref '%s'" decoded-ref))
           `((ref . ,decoded-ref)
             (nodes . ,(mapcar (lambda (node)
                                (let ((id (nth 0 node))
                                      (title (nth 1 node))
                                      (file (nth 2 node))
                                      (level (nth 3 node))
                                      (ref-type (nth 4 node)))
                                  `((id . ,id)
                                    (title . ,title)
                                    (file . ,(file-relative-name file org-roam-directory))
                                    (level . ,level)
                                    (ref_type . ,ref-type)
                                    (tags . ,(mapcar 'car (org-roam-db-query [:select [tag] :from tags :where (= node-id $s1)] id)))
                                    (aliases . ,(mapcar 'car (org-roam-db-query [:select [alias] :from aliases :where (= node-id $s1)] id))))))
                              nodes))
             (count . ,(length nodes))))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving nodes by ref: %s" (error-message-string err))
      `((ref . ,ref))))))

(defun md-roam-server-get-nodes-by-citation (citation)
  "Get nodes that have the specified CITATION."
  (condition-case err
      (progn
        (md-roam-server-init-org-roam)
        (let* ((decoded-citation (url-unhex-string citation))
               (nodes (org-roam-db-query 
                      [:select [nodes:id nodes:title nodes:file nodes:level]
                       :from citations
                       :inner-join nodes :on (= citations:node-id nodes:id)
                       :where (= citations:cite-key $s1)
                       :order-by nodes:title]
                      decoded-citation)))
          (md-roam-server--create-success-response
           (if (> (length nodes) 0)
               (format "Found %d nodes with citation '%s'" (length nodes) decoded-citation)
             (format "No nodes found with citation '%s'" decoded-citation))
           `((citation . ,decoded-citation)
             (nodes . ,(mapcar (lambda (node)
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
      (format "Error retrieving nodes by citation: %s" (error-message-string err))
      `((citation . ,citation))))))

(defun md-roam-server-get-stats ()
  "Get statistics about the org-roam database."
  (condition-case err
      (progn
        ;; Ensure md-roam is properly configured and database is up to date
        (unless (bound-and-true-p md-roam-mode)
          (setq md-roam-file-extension "md")
          (setq org-roam-file-extensions '("org" "md"))
          (setq org-roam-title-sources '((title headline) (alias alias)))
          (setq md-roam-use-org-extract-ref-links t)
          (md-roam-mode 1))
        ;; Ensure database is in the correct location
        (setq org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
        (message "DEBUG stats: org-roam-directory=%s, db-location=%s" org-roam-directory org-roam-db-location)
        ;; Skip database sync to prevent blocking during stats requests
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

(defun md-roam-server-get-config ()
  "Get current server configuration."
  (condition-case err
      (let ((config (md-roam-server--load-config)))
        (md-roam-server--create-success-response
         "Configuration retrieved successfully"
         `((config_file . ,md-roam-server-config-file)
           (server . ((port . ,md-roam-server-port)))
           (org_roam . ((directory . ,org-roam-directory)
                       (db_location . ,(or org-roam-db-location "default"))
                       (db_exists . ,(condition-case nil 
                                         (file-exists-p (org-roam-db-location))
                                       (error nil)))))
           (config_source . ,config))))
    (error
     (md-roam-server--create-error-response 
      (format "Error retrieving configuration: %s" (error-message-string err))
      `((config_file . ,md-roam-server-config-file))))))

;;; Search and Metadata Endpoint Handlers

(defun md-roam-server-handle-search (method path json-data)
  "Handle search and metadata endpoints with METHOD, PATH, and JSON-DATA."
  (cond
   ((string= method "GET")
    (cond
     ((string-match "^/search/\\(.+\\)$" path)
      (let ((query (match-string 1 path)))
        (md-roam-server-search-nodes query)))
     ((string= path "/tags")
      (md-roam-server-get-tags))
     ((string= path "/aliases")
      (md-roam-server-get-aliases))
     ((string= path "/refs")
      (md-roam-server-get-refs))
     ((string= path "/citations")
      (md-roam-server-get-citations))
     ((string-match "^/tags/\\([^/]+\\)/nodes$" path)
      (let ((tag (match-string 1 path)))
        (md-roam-server-get-nodes-by-tag tag)))
     ((string-match "^/aliases/\\([^/]+\\)/nodes$" path)
      (let ((alias (match-string 1 path)))
        (md-roam-server-get-nodes-by-alias alias)))
     ((string-match "^/refs/\\([^/]+\\)/nodes$" path)
      (let ((ref (match-string 1 path)))
        (md-roam-server-get-nodes-by-ref ref)))
     ((string-match "^/citations/\\([^/]+\\)/nodes$" path)
      (let ((citation (match-string 1 path)))
        (md-roam-server-get-nodes-by-citation citation)))
     ((string= path "/stats")
      (md-roam-server-get-stats))
     ((string= path "/config")
      (md-roam-server-get-config))
     (t
      (md-roam-server--create-error-response "Search/metadata endpoint not found"))))
   (t
    (md-roam-server--create-error-response "Method not allowed for search/metadata endpoints"))))

(provide 'md-roam-server-search)
;;; md-roam-server-search.el ends here