;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'org)
(require 'seq)
(require 'subr-x)

(defun org-req-buffer-export ()
  "Parse the current buffer and create a new buffer with cleaned requirements.
This cleaned buffer uses generated `CUSTOM_ID''s with links.
Also checks for consistency in item traceability."
  (interactive)
  (let ((items-lists (org-req--parse-buffer)))
    (org-req--consistency-check-items (apply 'append items-lists))
    (let ((category-elements (org-req--items-lists-to-headlines items-lists))
          (new-buffer (get-buffer-create "output.org")))
      (set-buffer new-buffer)
      (erase-buffer)
      (mapc (lambda (category-element)
              (insert (org-element-interpret-data category-element)))
            category-elements)
      (display-buffer new-buffer))))

(defun org-req-buffer-traceability ()
  "Parse the current buffer and create a new buffer with a traceability matrix."
  (interactive)
  (let* ((items (org-req--parse-buffer))
         (item-chains (org-req--chain-items items))
         (table (org-req--chains-to-table item-chains))
         (filename (buffer-file-name))
         (new-buffer (get-buffer-create "output.org")))
    (set-buffer new-buffer)
    (erase-buffer)
    (insert (org-element-interpret-data table))
    (display-buffer new-buffer)))

(defun org-req--parse-buffer ()
  "Parse the current `org-mode' buffer to produce a set of plist items.
Each item has the following properties:
- `:title' -- a list of the words in the title
- `:CUSTOM_ID' -- an incrementing number after the category to form a UID
- `:TRACES_TO' -- a list of custom-id's from the source TRACES_TO property
- `:TRACES_FROM' -- a list of custom-id's from the source TRACES_FROM property"
  (let* ((parsed-buffer (org-element-parse-buffer))
         (headlines (org-req--get-sub-headlines parsed-buffer))
         (items (mapcar 'org-req--parse-category headlines)))
    (org-req--link-traces (apply 'append items))
    items))

(defun org-req--parse-category (headline)
  "Return a simplified data-structure for items under the given HEADLINE."
  (let* ((category-title (car (org-element-property :title headline)))
         (headlines (org-req--get-sub-headlines headline)))
    (org-req--parse-items headlines category-title 1)))

(defun org-req--parse-items (items category index)
  "Return a list of simplified data-structures for ITEMS.
CATEGORY is the category of item this is.  INDEX is the 1-based
index of the item under its parent CATEGORY."
  (if (zerop (length items)) '()
    (let* ((item (car items))
           (item-plist '())
           (put-prop (lambda (prop)
                       (plist-put item-plist prop
                                  (let ((value (org-element-property prop item)))
                                    (if value
                                        (split-string value)
                                      nil))))))
      (setq item-plist (plist-put item-plist :TITLE
                                  (org-element-property :title item)))
      (plist-put item-plist :ID
                 (org-element-property :ID item))
      (mapc put-prop (list :TRACES_TO :TRACES_FROM))
      (plist-put item-plist :CUSTOM_ID
                 (format "%s-%02d" category index))
      (plist-put item-plist :CATEGORY_ID category)
      (cons item-plist (org-req--parse-items (cdr items)
                                        category
                                        (+ 1 index))))))

(defun org-req--get-sub-headlines (element)
  "Return a list of all elements contained in ELEMENT that are headlines."
  (let ((contents (org-element-contents element)))
    (seq-filter (lambda (element) (equal 'headline (org-element-type element)))
                contents)))

(defun org-req--link-traces (items)
  "Replace org-generated id's in `:TRACES_FROM' and `:TRACES_TO' with CUSTOM_ID's in all ITEMS."
  (mapcar (lambda (item) (org-req--link-item-traces item items)) items))

(defun org-req--link-item-traces (item items)
  "Replace org-generated id's in `:TRACES_FROM' and `:TRACES_TO' with CUSTOM_ID's in this ITEM to link them to other ITEMS."
  (mapc (lambda (prop)
          (let* ((traces-from-ids (plist-get item prop))
                 (matching-items (mapcan (lambda (id)
                                           (seq-filter
                                            (lambda (other-item)
                                              (equal (plist-get other-item :ID)
                                                     id))
                                            items))
                                         traces-from-ids))
                 (custom-ids (mapcar (lambda (matching-item)
                                       (plist-get matching-item :CUSTOM_ID))
                                     matching-items)))
            (plist-put item prop custom-ids)))
        (list :TRACES_FROM :TRACES_TO)))

(defun org-req--chain-items (items)
  "Create a chain for each combination from source to destination in ITEMS.
This returns a list of all created chains."
  (when items
    (let* ((sources (org-req--find-sources (car items)))
           (next-chains (mapcan (lambda (source-item)
                                  (org-req--trace-from-source source-item
                                                              (cdr items)))
                                sources)))
      (append next-chains (org-req--chain-items (cdr items))))))

(defun org-req--find-sources (items)
  "Find all source items in ITEMS.
A source item is an item whose `:TRACES_FROM' property is nil."
  (seq-filter (lambda (item)
                (not (plist-get item :TRACES_FROM)))
              items))

(defun org-req--trace-from-source (source-item destination-lists)
  "For a given SOURCE-ITEM, create a chain using the given DESTINATION-LISTS.
DESTINATION-LISTS is a list of list of items."
  (let* ((traces-to-ids (plist-get source-item :TRACES_TO))
         (traces-to-items (mapcar (lambda (custom-id)
                                    (org-req--find-item (apply 'append
                                                               destination-lists)
                                                        custom-id))
                                  traces-to-ids))
         (traces-to-chains (mapcan (lambda (traces-to-item)
                                     (org-req--trace-from-source traces-to-item
                                                                 destination-lists))
                                   traces-to-items)))
    (if traces-to-chains
        (mapcar (lambda (traces-to-chain)
                  (cons source-item traces-to-chain))
                traces-to-chains)
      (list (list source-item)))))

(defun org-req--consistency-check-items (items)
  "Check that each tracing link in ITEMS is bi-directional.
If one is discovered, a placeholder it added to make the link
bi-directional, and a warning is added to the title showing that
it was altered."
  (mapc (lambda (matching-keywords)
          (let ((starting-keyword (car matching-keywords))
                (ending-keyword (cdr matching-keywords)))
            (mapc (lambda (item)
                    (let* ((item-id (plist-get item :CUSTOM_ID))
                           (traces-to-ids (plist-get item starting-keyword))
                           (traces-to-items (mapcar (lambda (id)
                                                      (org-req--find-item items id))
                                                    traces-to-ids)))
                      (mapc (lambda (traces-to-item)
                              (org-req--consistency-check-item traces-to-item
                                                               ending-keyword
                                                               item-id))
                            traces-to-items)))
                  items)))
        '((:TRACES_FROM . :TRACES_TO)
          (:TRACES_TO . :TRACES_FROM))))

(defun org-req--consistency-check-item (item keyword requester-id)
  "Check that there is a link for KEYWORD to REQUESTER-ID in ITEM.
If one is not discovered, a placeholder it added to make the link
bi-directional, and a warning is added to the title showing that
it was altered."
  (unless (member requester-id
                  (plist-get item keyword))
    (plist-put item keyword item)
    (let ((title (plist-get item :TITLE)))
      (plist-put item :TITLE
                 (format "(Missing %s link to %s) %s"
                         keyword requester-id title)))))

(defun org-req--find-item (items custom-id)
  "Return the first item with the given CUSTOM-ID in ITEMS."
  (if-let (found-items (seq-filter (lambda (item)
                                     (equal custom-id (plist-get item :CUSTOM_ID)))
                                   items))
      (car found-items)
    nil))

(defun org-req--chains-to-table (chains)
  "Translate list of CHAINS to an org table element."
  (apply 'org-element-create
         (cons* 'table nil
                (mapcar (lambda (chain)
                          (apply 'org-element-create
                                 (cons* 'table-row nil
                                        (mapcar (lambda (item)
                                                  (org-element-create
                                                   'table-cell nil
                                                   (plist-get item :CUSTOM_ID)))
                                                chain))))
                        chains))))

(defun org-req--items-lists-to-headlines (items-lists)
  "Translate the given ITEMS-LISTS into an org element tree."
  (mapcar (lambda (items)
            (let ((category-name (plist-get (car items) :CATEGORY_ID)))
              (apply 'org-element-create
                     (append (list 'headline (list :level 1
                                                   :title category-name))
                             (mapcar 'org-req--item-to-headline items)))))
          items-lists))

(defun org-req--item-to-headline (item)
  "Translate the ITEM into an org headline element."
  (let* ((base-traces-to (plist-get item :TRACES_TO))
         (base-traces-from (plist-get item :TRACES_FROM))
         (id (plist-get item :CUSTOM_ID))
         (base-title (plist-get item :TITLE))
         (title (format "%s: %s" id base-title)))
    (let* ((traces-to-links
            (mapcar (lambda (traces-to-id)
                      (org-element-create 'link (list :type "custom-id"
                                                      :path traces-to-id)
                                          traces-to-id))
                    base-traces-to))
           (traces-from-links
            (mapcar (lambda (traces-from-id)
                      (org-element-create 'link (list :type "custom-id"
                                                      :path traces-from-id)
                                          traces-from-id))
                    base-traces-from))
           (property-drawer
            (org-element-create 'property-drawer nil
                                ;; (org-element-create 'node-property
                                ;;                     (list :key 'TRACES_TO
                                ;;                           :value traces-to-links))
                                ;; (org-element-create 'node-property
                                ;;                     (list :key 'TRACES_FROM
                                ;;                           :value traces-from-links))
                                (org-element-create 'node-property
                                                    (list :key 'CUSTOM_ID
                                                          :value id))))
           (links
            (org-element-create
             'plain-list (list :type 'descriptive)
             (apply 'org-element-create
                    (append (list 'item (list :bullet "- ") "Traces To: ")
                            traces-to-links)))))
      (org-element-create 'headline (list :level 2
                                          :title title)
                          property-drawer
                          links))))

(defun cons* (item &rest items)
  "Recursive cons on arbitrary number of ITEMS.
In the base case with only one argument passed, ITEM will be
returned."
  (if items
      (cons item (apply 'cons* items))
    item))

(defun ormap (items)
  "Apply or to the list of ITEMS."
  (or (not items)
      (car items)
      (ormap (cdr items))))

(provide 'org-req)
;;; org-req.el ends here
