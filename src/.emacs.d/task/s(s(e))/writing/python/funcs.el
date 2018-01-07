;;; package --- Summary
;;; Commentary:
;;; Code:

;; Funcs
(defun serika-f/anaconda/activate ()
  "Activate anaconda mode"
  (interactive)
  (anaconda-mode +1))

(defun serika-f/anaconda/eldoc-activate ()
  "Activate `anaconda-eldoc-mode'."
  (interactive)
  (anaconda-eldoc-mode))

(defun serika-f/python/setup-buffer ()
  "Setup python buffers"
  (when (eq major-mode
            'python-mode)
    (let ((--current-buffer (current-buffer)))
      (setq tab-width      4
            truncate-lines t)

      (serika-f/evil/activate :evil-shift-width 4
                              :evil-state       'normal)
      (serika-f/smartparens/activate)
      (serika-f/aggressive-indent/activate)
      (serika-f/yasnippet/activate)

      (serika-f/ycmd/activate)
      (serika-f/flycheck/activate)
      (serika-f/company/activate :backends-set '(company-ycmd))

      (serika-f/eldoc/activate)

      (serika-f/settings/show-trailing-whitespaces)
      (serika-f/linum-relative/activate)
      (serika-f/rainbow-delimiters/activate)
      (serika-f/highlight-symbol/activate)

      ;; autofocus to previous buffers
      (when (not (func/buffer/check-modes 'python-mode))
        (switch-to-buffer --current-buffer)
        (func/buffer/focus-to 'python-mode)))))

;; Init
(defun init ()
  "Configure `python-mode'."
  (serika-c/eg/add-many-by-name 'python
    ("require")
    (require 'python)

    ("settings")
    (serika-f/settings/register-ft 'python-mode
                                   "\\.py\\'")

    ("settings smartparens")
    (progn
      (sp-local-pair 'python-mode "("    ")")
      (sp-local-pair 'python-mode "{"    "}")
      (sp-local-pair 'python-mode "["    "]")
      (sp-local-pair 'python-mode "\""   "\"")
      (sp-local-pair 'python-mode "`"    "'")
      (sp-local-pair 'python-mode "\\\"" "\\\""))

    ("keymap")
    (progn
      (func/keymap/save python-mode-map)

      (func/keymap/create python-mode-map
        "TAB" #'yas-expand

        "C-t =" #'evil-indent
        "C-t /" #'evilnc-comment-or-uncomment-lines))

    ("hook")
    (func/hook/add 'python-mode-hook
                   #'serika-f/python/setup-buffer)))
