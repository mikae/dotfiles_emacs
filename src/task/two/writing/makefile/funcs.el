;;; package --- Summary
;;; Commentary:
;;; Code:

;; Funcs
(defun serika-f/makefile/setup-buffer ()
  "Configure `makefile' buffers."
  (when (eq major-mode
            'makefile-mode)
    (setq tab-width        2
          truncate-lines   t)

    (serika-f/evil/activate :evil-shift-width 2
                            :evil-state       'normal)
    ;; merge it into `serika-f/evil/activate'
    (evil-local-set-key 'insert (kbd "TAB") #'self-insert-command)

    (serika-f/smartparens/activate)

    (serika-f/whitespace/enable)
    (serika-f/linum-relative/activate)
    (serika-f/highlight-symbol/activate)))

;; Init
(defun init ()
  "Configure `makefile' buffers."
  (serika-c/eg/add-many-by-name 'makefile
    ("settings")
    (serika-f/settings/register-ft 'makefile-mode
                                   "Makefile$")

    ("hook")
    (func/hook/add 'makefile-mode-hook
                   #'serika-f/makefile/setup-buffer)))
