;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'helm-lib)
(require 'helm-config)

(defun serika/helm//global-keymap ()
  "Configure global keymap for using `helm'."
  (global-set-key (kbd "A-x")     #'helm-M-x)

  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "C-x C-y") #'helm-show-kill-ring))

(defun serika/helm//variables ()
  "Configure `helm' variables."
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (setq helm-split-window-in-side-p           t)
  (setq helm-buffers-fuzzy-matching           t)
  (setq helm-move-to-line-cycle-in-source     t)
  (setq helm-ff-search-library-in-sexp        t)
  (setq helm-ff-file-name-history-use-recentf t))

(defun serika/helm//keymap ()
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

(defun init ()
  "Configure `helm'."
  (serika/helm//variables)
  (serika/helm//keymap)
  (serika/helm//global-keymap)

  (helm-mode +1))
