;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'web-beautify)

;; Global
(defun serika/html//auto-mode-alist ()
  "Configure `auto-mode-alist'."
  (add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode)))

(defun serika/html//mmm-mode ()
  "Add support in `mmm-mode'."
  (serika/mmm-mode/add 'html-mode
                       'js2-mode
                       "<script[ a-zA-Z=]*type=\"text\/javascript\"[ a-zA-Z=]*>"
                       "<\/script>"))

;; Local
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
  (evil-local-set-key 'normal (kbd "=")   'indent-for-tab-command)
  (evil-local-set-key 'normal (kbd "A-=") 'web-beautify-html))

(defun serika/html//snippet-engine ()
  "Configure snippet engine for `web-mode' buffers with `html' engine."
  (emmet-mode      +1)

  (yas-minor-mode  +1)
  (yas-recompile-all)
  (yas-reload-all))

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

;; Init
(defun init ()
  "Configure `html'."
  (serika/html//auto-mode-alist)
  (serika/html//mmm-mode)

  ;; Clear hooks
  (setq html-mode-hook nil)

  ;; Add hooks
  (add-hook 'html-mode-hook 'serika/html//evil)
  (add-hook 'html-mode-hook 'serika/html//buffer-local-variables)
  (add-hook 'html-mode-hook 'serika/html//buffer-local-mappings)

  (add-hook 'html-mode-hook 'serika/html//snippet-engine)
  (add-hook 'html-mode-hook 'serika/html//auto-completion)
  (add-hook 'html-mode-hook 'serika/html//syntax-checking)

  (add-hook 'html-mode-hook 'serika/html//interface)
  (add-hook 'html-mode-hook 'serika/html//prettify-symbols))
