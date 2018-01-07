;;; package --- Summary
;;; Commentary:
;;; Code:

;; Local
(defun serika-f/vimperator/setup-buffer ()
  "Setup vimperator buffer."
  (when (eq major-mode
            'vimperator-mode)
    (func/var/ensure-local tab-width      2
                           truncate-lines t)

    (serika-f/evil/activate :evil-shift-width 2
                            :evil-state       'normal)

    (serika-f/settings/show-trailing-whitespaces)
    (serika-f/linum-relative/activate)
    (serika-f/rainbow-delimiters/activate)
    (serika-f/highlight-symbol/activate)))

(defun init ()
  "Configure `vimperator'."
  (serika-c/eg/add-install :type 'download
                           :name 'vimperator
                           :src  "https://raw.githubusercontent.com/shinkiley/vimperator-mode/master/vimperator-mode.el")

  (serika-c/eg/add-many-by-name 'vimperator
    ("require")
    (func/func/require 'vimperator-mode)

    ("settings")
    (serika-f/settings/register-ft 'vimperator-mode
                                   "\\.vimperatorrc\\'"
                                   "\\.vimperatorrc\\.after\\'"
                                   "\\.vimp\\'"
                                   "\\.vimp\\.after\\'")

    ("hook")
    (func/hook/add 'vimperator-mode-hook
                   #'serika-f/vimperator/setup-buffer)))
