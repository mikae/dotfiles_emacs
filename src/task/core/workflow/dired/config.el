;;; package --- Summary
;;; Commentary:
;;; Code:

;; Load dired
(require 'dired)

;; Configure dired options
(setq dired-recursive-deletes 'always
      dired-dwim-target       t)

;; Configure dired hook
(add-hook 'dired-mode-hook (lambda ()
                             ;; Enable auto reverting
                             (auto-revert-mode 1)

                             ;; Enable line truncations
                             (setq truncate-lines t)

                             ;; Disable attempt to save dired buffer
                             (make-local-variable 'serika-buffer-save-function)
                             (setq serika-buffer-save-function 'serika-misc-silence)
                             ))

;; Configure dired map
(progn
  (setq dired-mode-map (make-sparse-keymap))

  (dolist (key '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0" ))
    (define-key dired-mode-map (kbd key) 'digit-argument))

  ;; Dired actions
  (define-key dired-mode-map (kbd "d d")   'dired-do-delete)
  (define-key dired-mode-map (kbd "d r")   'dired-do-rename)
  (define-key dired-mode-map (kbd "d c")   'dired-do-copy)

  (define-key dired-mode-map (kbd "* m")   'dired-mark)
  (define-key dired-mode-map (kbd "* u")   'dired-unmark)

  (define-key dired-mode-map (kbd "n d")   'dired-create-directory)
  (define-key dired-mode-map (kbd "n f")   'helm-find-files)
  (define-key dired-mode-map (kbd "n h")   'dired-do-hardlink)
  (define-key dired-mode-map (kbd "n s")   'dired-do-symlink)

  (define-key dired-mode-map (kbd "c o")   'dired-do-chown)
  (define-key dired-mode-map (kbd "c g")   'dired-do-chgrp)

  (define-key dired-mode-map (kbd "h")     'dired-up-directory)
  (define-key dired-mode-map (kbd "l")     'dired-find-file)

  ;; Kill all dired buffers mapping
  (define-key dired-mode-map (kbd "q")     (lambda ()
                                             (interactive)
                                             (serika/buffer/kill-by-major-mode 'dired-mode)))

  ;; Add search functions
  (define-key dired-mode-map (kbd "/")    'evil-search-forward)
  (define-key dired-mode-map (kbd "?")    'evil-search-backward)
  (define-key dired-mode-map (kbd "A-n")  'evil-search-next)
  (define-key dired-mode-map (kbd "A-N")  'evil-search-previous)

  (define-key dired-mode-map (kbd "j")    'serika/dired/next-visual-line)
  (define-key dired-mode-map (kbd "k")    'serika/dired/previous-visual-line)

  (define-key dired-mode-map (kbd "g")    'serika/dired/move-to-beginning)
  (define-key dired-mode-map (kbd "G")    'serika/dired/move-to-end)

  (define-key dired-mode-map (kbd "A-k")  'serika/dired/move-to-window-top)
  (define-key dired-mode-map (kbd "A-m")  'serika/dired/move-to-window-middle)
  (define-key dired-mode-map (kbd "A-j")  'serika/dired/move-to-window-bottom)

  (define-key dired-mode-map (kbd "A-f")  'serika/dired/scroll-page-down)
  (define-key dired-mode-map (kbd "A-b")  'serika/dired/scroll-page-up)
  (define-key dired-mode-map (kbd "A-d")  'serika/dired/scroll-half-page-down)
  (define-key dired-mode-map (kbd "A-u")  'serika/dired/scroll-half-page-up)

  (define-key dired-mode-map (kbd "RET")  'dired-run-associated-program)
  ())

;; Add global mapping to invoke dired
(global-set-key (kbd "C-x C-d") 'serika/dired/open-this-directory)
