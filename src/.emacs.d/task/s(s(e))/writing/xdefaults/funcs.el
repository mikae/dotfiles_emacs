;;; package --- Summary
;;; Commentary:
;;; Code:

;; Local
(defun serika-f/xdefaults/setup-buffer ()
  "Setup buffer in `xdefaults-mode'"
  (when (eq major-mode
            'conf-xdefaults-mode)
    (func/var/ensure-local tab-width      4
                           truncate-lines t)

    (serika-f/evil/activate :evil-shift-width 4
                            :evil-state       'normal)

    (serika-f/settings/show-trailing-whitespaces)
    (serika-f/linum-relative/activate)
    (serika-f/rainbow-delimiters/activate)
    (serika-f/rainbow-mode/activate)
    (serika-f/highlight-symbol/activate)))

(defun init ()
  "Configure `xdefaults'."
  (serika-c/eg/add-many-by-name 'xdefaults
    ("settings")
    (serika-f/settings/register-ft 'conf-xdefaults-mode "\\.Xresources\\'")

    ("keymap")
    (progn
      (func/keymap/create conf-xdefaults-mode-map)
      (func/keymap/define conf-xdefaults-mode-map
        "C-t =" 'evil-indent
        "C-t /" 'evilnc-comment-or-uncomment-lines))

    ("hook")
    (func/hook/add 'conf-xdefaults-mode-hook
                   #'serika-f/xdefaults/setup-buffer)))
