;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika/html//require ()
  "Require modules for `html'."
  (require 'web-beautify)
  (require 'sgml-mode))

(defun serika/html//auto-mode-alist ()
  "Configure `auto-mode-alist'."
  (add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode)))

(defun serika/html//keymap ()
  "Configure buffer-local mappings for `html' files."
  (setq --serika-html-mode-map html-mode-map)
  (setq html-mode-map (let ((map (make-sparse-keymap)))
                        (define-key map (kbd "C-t e") #'yas-expand)
                        (define-key map (kbd "C-t E") #'serika/emmet/expand)
                        (define-key map (kbd "C-t =") #'evil-indent)
                        (define-key map (kbd "C-t +") #'web-beautify-html)
                        (define-key map (kbd "C-t /") #'evilnc-comment-or-uncomment-lines)
                        map)))

(defun serika/html//mmm-mode ()
  "Add support in `mmm-mode'."
  (mmm-add-classes '((html-js-1
                      :submode js-mode
                      :front   "<script[^>]*>[ \t]*\n?"
                      :back    "[ \t]*</script>")))
  (mmm-add-mode-ext-class 'html-mode nil 'html-js-1))

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

(defun serika/html//snippet-engine ()
  "Configure snippet engine for `web-mode' buffers with `html' engine."
  (serika/emmet/activate)
  (serika/yasnippet/activate))

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
  (linum-mode                    +1)

  (prettify-symbols-mode   +1)

  (setq prettify-symbols-alist ()))

;; Init
(defun init ()
  "Configure `html'."
  (serika/html//require)
  (serika/html//auto-mode-alist)
  (serika/html//keymap)
  (serika/html//mmm-mode)

  ;; Add hooks
  (add-hook 'html-mode-hook 'serika/html//evil)
  (add-hook 'html-mode-hook 'serika/html//buffer-local-variables)

  (add-hook 'html-mode-hook 'serika/html//snippet-engine)
  (add-hook 'html-mode-hook 'serika/html//auto-completion)
  (add-hook 'html-mode-hook 'serika/html//syntax-checking)
  (add-hook 'html-mode-hook 'serika/mmm-mode//activate)

  (add-hook 'html-mode-hook 'serika/html//interface))
