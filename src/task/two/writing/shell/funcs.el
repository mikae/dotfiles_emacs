;;; package --- Summary
;;; Commentary:
;;; Code:

;; Funcs
(defun serika-f/sh/execute ()
  "Execute marked text or line."
  (interactive)
  (let ((--proc        (get-process "shell"))
        (--proc-buffer  nil)
        (--min          nil)
        (--max          nil)
        (--command      nil))
    (unless --proc
      (let ((--current-buffer (current-buffer)))
        (shell)
        (switch-to-buffer --current-buffer)
        (setq --proc
              (get-process "shell"))))
    (setq --proc-buffer (process-buffer --proc))
    (if (not (use-region-p))
        (setq --min (point-at-bol)
              --max (point-at-eol))
      (setq --min (region-beginning)
            --max (region-end))
      (deactivate-mark))
    (setq --command (concat (buffer-substring --min
                                              --max)
                            "\n"))
    (with-current-buffer --proc-buffer
      (goto-char (process-mark --proc))
      (insert --command)
      (move-marker (process-mark --proc)
                   (point)))
    (process-send-string --proc
                         --command)
    (other-window 1)
    (display-buffer (process-buffer --proc) t)))

(defun serika-f/sh/setup-buffer ()
  "Configure sh buffers."
  (when (eq major-mode
            'sh-mode)
    (func/var/ensure-local tab-width      2
                           truncate-lines t)
    (serika-f/evil/activate :evil-shift-width 2
                            :evil-state       'normal)
    (serika-f/smartparens/activate)
    (serika-f/aggressive-indent/activate)

    (serika-f/yasnippet/activate)
    (serika-f/flycheck/activate)

    (serika-f/eldoc/activate)
    (serika-f/company/activate :backends-set '(company-shell
                                               company-shell-env))

    (serika-f/settings/show-trailing-whitespaces)
    (serika-f/linum-relative/activate)
    (serika-f/rainbow-delimiters/activate)
    (serika-f/highlight-symbol/activate)
    (serika-f/prettify-symbols/activate :name "sh")))

;; Init
(defun init ()
  "Configure `sh-mode'."
  (serika-c/eg/add-many-by-name 'sh
                                ("settings")
                                (lambda ()
                                  (serika-f/settings/register-ft 'sh-mode
                                                                 "\\.sh\\'"
                                                                 "\\.zsh\\'"
                                                                 "\\.zshrc\\'"
                                                                 "\\.zshenv\\'"
                                                                 "\\.zshprofile\\'"
                                                                 "\\.xinit\\'"))

                                ("settings multi-compile")
                                (lambda ()
                                  (add-to-list 'multi-compile-alist '(sh-mode . (("bash" . "bash  %path")
                                                                                 ("zsh"  . "zsh   %path")))))

                                ("settings smartparens")
                                (lambda ()
                                  (sp-local-pair 'sh-mode "("    ")")
                                  (sp-local-pair 'sh-mode "{"    "}")
                                  (sp-local-pair 'sh-mode "["    "]")
                                  (sp-local-pair 'sh-mode "\""   "\"")
                                  (sp-local-pair 'sh-mode "'"    "'")
                                  (sp-local-pair 'sh-mode "\\\"" "\\\""))

                                ("keymap")
                                (lambda ()
                                  (func/keymap/create sh-mode-map
                                                      "TAB" #'yas-expand

                                                      "C-c a" #'serika-f/sh/execute
                                                      "C-c A" #'multi-compile-run

                                                      "C-t =" #'evil-indent
                                                      "C-t /" #'evilnc-comment-or-uncomment-lines))

                                ("hook")
                                (lambda ()
                                  (func/hook/add 'sh-mode-hook
                                                 #'serika-f/sh/setup-buffer))))
