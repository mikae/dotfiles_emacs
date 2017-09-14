;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/org-wiki/create ()
  "Create page in same directory with correct relative path to `org-wiki-location'."
  (interactive)
  (unless (f-dir-p org-wiki-location)
    (error "`org-wiki-location' is not a directory."))
  (let ((page-name (read-string "Page path: ")))
    (save-excursion (insert (org-make-link-string (concat "wiki:"
                                                          (f-join (f-dirname (f-relative buffer-file-name
                                                                                         org-wiki-location))
                                                                  page-name))
                                                  (concat "wiki:"
                                                          page-name))))))


;; Init
(defun init ()
  "Configure `org-wiki'."
  (serika-c/eg/add-install :type 'git
                           :name 'org-wiki
                           :src  "https://github.com/mikae/org-wiki")

  (serika-c/eg/add-many-by-name 'org-wiki
                                ("require org")
                                ;; (func/func/requirer 'org-wiki)
                                (lambda ()
                                  (require 'org-wiki))

                                ("settings org")
                                (lambda ()
                                  (setq org-wiki-location (f-join org-directory
                                                                  "wiki"))

                                  ;; NODE_PATH
                                  (setenv "NODE_PATH"
                                          (concat
                                           (f-join org-wiki-location
                                                   "programming"
                                                   "javascript"
                                                   "node_modules"
                                                   )
                                           ":"
                                           (getenv "NODE_PATH")))
                                  )

                                ("keymap org")
                                (lambda ()
                                  (func/keymap/define org-mode-map
                                                      "C-c C-z w" #'serika-f/org-wiki/create))))
