;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'org)
(require 'seq)
(require 'subr-x)

(defun org-req-translate-buffer ()
  "Parse the current `org-mode' buffer and create a new buffer with a traceability matrix."
  (interactive)
  (let* ((items (org-req--parse-buffer))
         (item-chains (org-req--chain-items items))
         (table (org-req--chains-to-table item-chains))
         (filename (buffer-file-name))
         (new-buffer (get-buffer-create (format "output.org"))))
    (set-buffer new-buffer)
    (erase-buffer)
    (insert (org-element-interpret-data table))
    (display-buffer new-buffer)))

(defun org-req--parse-buffer ()
  "Parse the current `org-mode' buffer to produce a set of plist items.
Each item has the following properties:
- `:title' -- a list of the words in the title
- `:tags' -- a list of tags as strings
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
    (unless (org-element-property :commentedp headline)
      (org-req--parse-items headlines category-title 1))))

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

(defun cons* (item &rest items)
  "Recursive cons on arbitrary number of ITEMS.
In the base case with only one argument passed, ITEM will be
returned."
  (if items
      (cons item (apply 'cons* items))
    item))

(provide 'org-req)
;;; org-req.el ends here
