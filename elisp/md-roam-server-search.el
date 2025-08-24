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
                       :where (or (like nodes:title $r1) (like aliases:alias $r1))
                       :group-by nodes:id
                       :order-by nodes:title]
                      (concat "%" decoded-query "%"))))
          (md-roam-server--create-success-response
           "Search completed successfully"
           `((query . ,decoded-query)
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
        (org-roam-db-sync)
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
           (server . ((port . ,md-roam-server-port)
                     (ui_port . ,md-roam-server-ui-port)
                     (ui_enabled . ,md-roam-server-ui-enabled)))
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