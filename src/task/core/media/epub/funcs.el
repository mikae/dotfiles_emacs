;;; package --- Summary
;;; Commentary:
;;; Code:
;;; Todo:
;;; - setup org-mode integration

;; Global
(defun serika-g/epub//require ()
  "Require modules for `epub'."
  (require 'ereader))

(defun serika-g/epub//settings ()
  "Configure `epub'."
  nil)

(defun serika-g/epub//auto-mode-alist ()
  "Configure `auto-mode-alist'."
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . ereader-mode)))

(defun serika-g/epub//keymap ()
  "Configure `ereader-mode-map'."
  (setq --serika-ereader-mode-map ereader-mode-map)
  (setq ereader-mode-map (let ((map (make-sparse-keymap)))
                           ;; Searching
                           (define-key map (kbd "A-1")   #'evil-search-forward)
                           (define-key map (kbd "A-2")   #'evil-search-backward)
                           (define-key map (kbd "A-z")   #'evil-search-next)
                           (define-key map (kbd "A-Z")   #'evil-search-previous)

                           ;; Scrolling
                           (define-key map (kbd "A-e")   #'evil-scroll-down)
                           (define-key map (kbd "A-i")   #'evil-scroll-up)
                           (define-key map (kbd "A-p")   #'evil-scroll-page-down)
                           (define-key map (kbd "A-P")   #'evil-scroll-page-up)

                           ;; Defaults
                           (define-key map (kbd "C-c A") #'ereader-hide-annotation)
                           (define-key map (kbd "C-c G") #'ereader-goto-chapter)
                           (define-key map (kbd "C-c M") #'ereader-hide-all-annotations)
                           (define-key map (kbd "C-c R") #'ereader-load-annotations)
                           (define-key map (kbd "C-c a") #'ereader-show-annotation)
                           (define-key map (kbd "C-c c") #'ereader-message-chapter)
                           (define-key map (kbd "C-c g") #'ereader-goto-chapter)
                           (define-key map (kbd "C-c m") #'ereader-show-all-annotations)

                           ;; (define-key map (kbd "C-c l") 'org-store-link)
                           map)))

;; Init
(defun init ()
  "Configure `epub'."
  (serika-c/eg/add-install :package-list '(xml+ s)
                           :name         'epub)

  (serika-c/eg/add :parents '("require")
                   :name    'epub
                   :func    #'serika-g/epub//require)

  (serika-c/eg/add :parents '("settings")
                   :name    'epub
                   :func    #'serika-g/epub//settings)

  (serika-c/eg/add :parents '("settings epub")
                   :name    'auto-mode-alist
                   :func    #'serika-g/epub//auto-mode-alist)

  (serika-c/eg/add :parents '("keymap")
                   :name    'epub
                   :func    #'serika-g/epub//keymap))
