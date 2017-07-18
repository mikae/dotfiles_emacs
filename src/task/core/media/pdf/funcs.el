;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/pdf//require ()
  "Require modules for `pdf'."
  (require 'pdf-tools))

(defun serika-g/pdf//install ()
  "Extra configuration for `pdf-tools'."
  (pdf-tools-install))

(defun serika-g/pdf//settings ()
  "Configure `pdf'."
  nil)

(defun serika-g/pdf//auto-mode-alist ()
  "Configure `auto-mode-alist' for `pdf-view-mode'."
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode)))

(defun serika-g/pdf//keymap ()
  "Configure `pdf-view-mode-map'."
  (setq pdf-view-mode-map (let ((map (make-sparse-keymap)))
                            ;; Save old keymap
                            (setq --serika-pdf-view-mode-map pdf-view-mode-map)

                            ;; Kill
                            (define-key map (kbd "q")     #'serika-f/buffer/kill-current)

                            ;; Move
                            (define-key map (kbd "A-j")   #'pdf-view-next-line-or-next-page)
                            (define-key map (kbd "A-k")   #'pdf-view-previous-line-or-previous-page)
                            (define-key map (kbd "A-J")   #'pdf-view-scroll-up-or-next-page)
                            (define-key map (kbd "A-K")   #'pdf-view-scroll-down-or-previous-page)
                            (define-key map (kbd "A-H-j") #'pdf-view-next-page)
                            (define-key map (kbd "A-H-k") #'pdf-view-previous-page)
                            (define-key map (kbd "A-g")   #'pdf-view-first-page)
                            (define-key map (kbd "A-G")   #'pdf-view-last-page)
                            (define-key map (kbd "C-c p") #'pdf-view-goto-page)
                            (define-key map (kbd "C-c l") #'pdf-view-goto-label)

                            ;; Scale
                            (define-key map (kbd "-")     #'pdf-view-shrink)
                            (define-key map (kbd "+")     #'pdf-view-enlarge)
                            (define-key map (kbd "0")     #'pdf-view-scale-reset)

                            ;; Use new keymap
                            map)))

;; Init
(defun init ()
  "Configure `pdf'."
  (serika-c/eg/add-install :package-list '(pdf-tools)
                           :name         'pdf)

  (serika-c/eg/add :parents '("require")
                   :name    'pdf
                   :func    #'serika-g/pdf//require)

  (serika-c/eg/add :parents '("install pdf")
                   :name    'extra
                   :func    #'serika-g/pdf//install)

  (serika-c/eg/add :parents '("settings")
                   :name    'pdf
                   :func    #'serika-g/pdf//settings)

  (serika-c/eg/add :parents '("settings pdf")
                   :name    'auto-mode-alist
                   :func    #'serika-g/pdf//auto-mode-alist)

  (serika-c/eg/add :parents '("keymap")
                   :name    'pdf
                   :func    #'serika-g/pdf//keymap))
