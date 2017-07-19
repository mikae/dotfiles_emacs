;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/helm//require ()
  "Require modules for `helm'."
  (require 'helm-lib)
  (require 'helm-config))

(defun serika-g/helm//global-keymap ()
  "Configure global keymap for using `helm'."
  (global-set-key (kbd "A-x")   #'helm-M-x)

  (global-set-key (kbd "C-x f") #'helm-find-files)
  (global-set-key (kbd "C-x y") #'helm-show-kill-ring))

(defun serika-g/helm//settings ()
  "Configure `helm' variables."
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (setq helm-split-window-in-side-p           t)
  (setq helm-buffers-fuzzy-matching           t)
  (setq helm-move-to-line-cycle-in-source     t)
  (setq helm-ff-search-library-in-sexp        t)
  (setq helm-ff-file-name-history-use-recentf t))

(defun serika-g/helm//keymap ()
  "Configure `helm' keymaps."
  (flet ((cl (ind)
             ;; Create lambda => cl
             (lexical-let ((-ind ind))
               (lambda ()
                 (interactive)
                 (helm-select-nth-action -ind))))
         (cm (map)
             ;; Configure map => cm
             (define-key map (kbd "A-j") 'helm-next-line)
             (define-key map (kbd "A-k") 'helm-previous-line)
             (define-key map (kbd "RET") 'helm-maybe-exit-minibuffer)
             (define-key map (kbd "TAB") 'helm-select-action)
             (dolist (ind '(1 2 3 4 5 6 7 8 9))
               (define-key map (kbd (format "A-%d" ind)) (cl (1- ind))))
             (define-key map (kbd "A-0") (cl 9))
             )
         )
    (setq helm-map (make-sparse-keymap))
    (cm helm-map)

    (setq helm-buffer-map (make-sparse-keymap))
    ()))

(defun serika-g/helm//activate ()
  "Activate `helm'."
  (helm-mode +1))

;; Init
(defun init ()
  "Configure `helm'."
  (serika-c/eg/add-install :package-list '(helm)
                           :name         'helm)

  (serika-c/eg/add :parents '("require")
                   :name    'helm
                   :func    #'serika-g/helm//require)

  (serika-c/eg/add :parents '("settings")
                   :name    'helm
                   :func    #'serika-g/helm//settings)

  (serika-c/eg/add :parents '("keymap")
                   :name    'helm
                   :func    #'serika-g/helm//keymap)

  (serika-c/eg/add :parents '("global-keymap")
                   :name    'helm
                   :func    #'serika-g/helm//global-keymap)

  (serika-c/eg/add :parents '("post activate")
                   :name    'helm
                   :func    #'serika-g/helm//activate))
