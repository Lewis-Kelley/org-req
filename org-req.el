;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'org)
(require 'seq)
(require 'subr-x)

(defun or/translate-buffer ()
  "Parse the current `org-mode' buffer and create a new buffer with a traceability matrix."
  (interactive)
  (let* ((items (or/-parse-buffer))
         (item-chains (or/-chain-items items))
         (table (or/-chains-to-table item-chains))
         (filename (buffer-file-name))
         (new-buffer (get-buffer-create (format "output.org"))))
    (set-buffer new-buffer)
    (erase-buffer)
    (insert (org-element-interpret-data table))
    (display-buffer new-buffer)))

(defun or/-parse-buffer ()
  "Parse the current `org-mode' buffer to produce a set of plist items.
Each item has the following properties:
- `:title' -- a list of the words in the title
- `:CUSTOM_ID' -- an incrementing number after the category to form a UID
- `:TRACES_TO' -- a list of custom-id's from the source TRACES_TO property
- `:TRACES_FROM' -- a list of custom-id's from the source TRACES_FROM property"
  (interactive)
  (let* ((parsed-buffer (org-element-parse-buffer))
         (headlines (or/-get-sub-headlines parsed-buffer))
         (items (mapcar 'or/-parse-category headlines)))
    (or/-link-traces (apply 'append items))
    items))

(defun or/-parse-category (headline)
  "Return a simplified data-structure for items under the given HEADLINE."
  (let* ((category-title (car (org-element-property :title headline)))
         (headlines (or/-get-sub-headlines headline)))
    (or/-parse-items headlines category-title 1)))

(defun or/-parse-items (items category index)
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
      (cons item-plist (or/-parse-items (cdr items)
                                        category
                                        (+ 1 index))))))

(defun or/-get-sub-headlines (element)
  "Return a list of all elements contained in ELEMENT that are headlines."
  (let ((contents (org-element-contents element)))
    (seq-filter (lambda (element) (equal 'headline (org-element-type element)))
                contents)))

(defun or/-link-traces (items)
  "Replace org-generated id's in `:TRACES_FROM' and `:TRACES_TO' with CUSTOM_ID's in all ITEMS."
  (mapcar (lambda (item) (or/-link-item-traces item items)) items))

(defun or/-link-item-traces (item items)
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

(defun or/-chain-items (items)
  "Create a chain for each combination from source to destination in ITEMS.
This returns a list of all created chains."
  (when items
    (let* ((sources (or/-find-sources (car items)))
           (next-chains (mapcan (lambda (source-item)
                                  (or/-trace-from-source source-item (cdr items)))
                                sources)))
      (or/-union-chains next-chains (or/-chain-items (cdr items))))))

(defun or/-find-sources (items)
  "Find all source items in ITEMS.
A source item is an item whose `:TRACES_FROM' property is nil."
  (seq-filter (lambda (item)
                (not (plist-get item :TRACES_FROM)))
              items))

(defun or/-trace-from-source (source-item destination-lists)
  "For a given SOURCE-ITEM, create a chain using the given DESTINATION-LISTS.
DESTINATION-LISTS is a list of list of items."
  (let* ((traces-to-ids (plist-get source-item :TRACES_TO))
         (traces-to-items (mapcar (lambda (custom-id)
                                    (or/-find-item (apply 'append destination-lists)
                                                   custom-id))
                                  traces-to-ids)))
    (if traces-to-items
        (mapcar (lambda (traces-to-item)
                  (list source-item traces-to-item))
                traces-to-items)
      (list (list source-item)))))

(defun or/-union-chains (chain-list-1 chain-list-2)
  "Union CHAIN-LIST-1 and CHAIN-LIST-2 together so that no chains (or tails of chains) are repeated."
  (if (not chain-list-2) chain-list-1
    (let ((next-chain (car chain-list-2)))
      (if (mapcan (lambda (chain-1)
                    (has-cdr next-chain chain-1))
                  chain-list-1)
          (or/-union-chains chain-list-1 (cdr chain-list-2))
        (cons next-chain (or/-union-chains chain-list-1 (cdr chain-list-2)))))))

(defun has-cdr (needle haystack)
  "Return non-nil if the NEEDLE is present as a tail of HAYSTACK.
For example, `(has-cdr '(1 2 3) '(-1 0 1 2 3)) would return non-nil."
  (when haystack
    (or (equal needle haystack)
        (has-cdr needle (cdr haystack)))))

(defun or/-find-item (items custom-id)
  "Return the first item with the given CUSTOM-ID in ITEMS."
  (if-let (found-items (seq-filter (lambda (item)
                                     (equal custom-id (plist-get item :CUSTOM_ID)))
                                   items))
      (car found-items)
    nil))

(defun or/-chains-to-table (chains)
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
