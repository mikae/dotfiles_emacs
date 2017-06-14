;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'web-beautify)

;; Functions
(defun serika/html//evil ()
  "Configure `evil' for `html'."
  (setq evil-shift-width 2)
  (evil-local-mode +1)
  (evil-normal-state))

(defun serika/html//buffer-local-variables ()
  "Configure buffer-local variables for `html' files."
  (setq evil-shift-width 2)
  (setq truncate-lines t))

(defun serika/html//buffer-local-mappings ()
  "Configure buffer-local mappings for `html' files."
  (evil-local-set-key 'normal (kbd "=") 'indent-for-tab-command)
  (evil-local-set-key 'normal (kbd "A-=") 'web-beautify-html))

(defun serika/html//snippet-engine ()
  "Configure snippet engine for `web-mode' buffers with `html' engine."
  (emmet-mode              +1))

(defun serika/html//auto-completion ()
  "Configure auto completion for `web-mode' buffers with `html' engine."
  (auto-complete-mode      +1)

  (setq ac-sources '(
                     ac-source-abbrev
                     ac-source-dictionary
                     ac-source-words-in-same-mode-buffers
                     )))

(defun serika/html//syntax-checking ()
  "Configure syntax checking for `web-mode' buffers with `html' engine."
  ;; (flycheck-mode +1)
  ())


(defun serika/html//interface ()
  "Configure interface for `web-mode' buffers with `html' engine."
  (setq show-trailing-whitespace +1)

  (rainbow-delimiters-mode       +1)
  (rainbow-mode                  +1)
  (linum-mode                    +1))

(defun serika/html//prettify-symbols ()
  "Configure prettify-symbols for `web-mode' buffers with `html' engine."
  (prettify-symbols-mode   +1)

  (setq prettify-symbols-alist ()))

(defun serika/html//hook (f)
  "Add hook that invokes F only if `web-mode' engine is `html'."
  (lexical-let ((f f))
    (add-hook 'web-mode-hook
              (lambda ()
                (when (serika/web/buffer-is-html-p)
                  (funcall f)
                  )))))

(defun init ()
  "Configure `html'."
  (serika/html//hook 'serika/html//evil)
  (serika/html//hook 'serika/html//buffer-local-variables)
  (serika/html//hook 'serika/html//buffer-local-mappings)

  (serika/html//hook 'serika/html//snippet-engine)
  (serika/html//hook 'serika/html//auto-completion)
  (serika/html//hook 'serika/html//syntax-checking)

  (serika/html//hook 'serika/html//interface)
  (serika/html//hook 'serika/html//prettify-symbols))
