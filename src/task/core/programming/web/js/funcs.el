;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/js2//require ()
  "Require modules for `js'."
  (require 'js2-mode)
  (require 'ac-js2))

(defun serika-g/js2//auto-mode-alist ()
  "Configure `auto-mode-alist'."
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(defun serika-g/js2//keymap ()
  "Configure `js2-mode-map'."
  (setq --serika-js2-mode-map js2-mode-map)
  (setq js2-mode-map (let ((map (make-sparse-keymap)))
                       (define-key map (kbd "C-c c") 'multi-compile-run)
                       map)))

;; Local
(defun serika-l/js2//evil ()
  "Configure `evil' for `js'."
  (setq evil-shift-width 2)
  (evil-local-mode +1)
  (evil-normal-state))

(defun serika-l/js2//buffer-local-variables ()
  "Configure snippet engine for `js' mode."
  (setq tab-width 2)
  (setq js-indent-level 2)
  (setq truncate-lines t))

(defun serika-l/js2//buffer-local-mappings ()
  "Configure keymap for `js' mode."
  (evil-local-set-key 'normal (kbd "C-t =") 'evil-indent)
  (evil-local-set-key 'normal (kbd "C-t +") 'web-beautify-js)
  (evil-local-set-key 'normal (kbd "C-t /") 'evilnc-comment-or-uncomment-lines)
  (evil-local-set-key 'normal (kbd "C-t e") 'yas-expand)
  (evil-local-set-key 'normal (kbd "C-t E") 'serika-f/emmet/expand))

(defun serika-l/js2//syntax-checking ()
  "Configure syntax checking for `js' mode."
  (flycheck-mode           +1))

(defun serika-l/js2//snippet-engine ()
  "Configure snippet engine for `js' mode."
  (serika-f/yasnippet/activate)
  (serika-f/emmet/activate))

(defun serika-l/js2//auto-completion ()
  "Configure auto completion for `js' mode."
  (setq ac-sources '(
                      ac-source-filename
                      ac-source-dictionary
                      ac-source-files-in-current-dir))
  (ac-js2-mode))

(defun serika-l/js2//auto-pairing ()
  "Configure auto completion for `js' mode."
  (electric-pair-mode +1))

(defun serika-l/js2//multi-compile ()
  "Configure `multi-compile' for `js2-mode'."
  (add-to-list 'multi-compile-alist '(js2-mode . (("Execute" . "node %path")))))

(defun serika-l/js2//interface ()
  "Configure interface for `js' mode."
  (setq show-trailing-whitespace +1)

  (rainbow-delimiters-mode       +1)
  (linum-mode                    +1))

(defun serika-l/js2//prettify-symbols ()
  "Configure prettify symbols for `js' mode."
  (prettify-symbols-mode +1)

  (setq prettify-symbols-alist ()))

(defun init ()
  "Configure Emacs for `js'-programming."
  (serika-g/js2//require)
  (serika-g/js2//auto-mode-alist)
  (serika-g/js2//keymap)

  (add-hook 'js2-mode-hook 'serika-l/js2//evil)
  (add-hook 'js2-mode-hook 'serika-l/js2//buffer-local-variables)
  (add-hook 'js2-mode-hook 'serika-l/js2//buffer-local-mappings)

  (add-hook 'js2-mode-hook 'serika-l/js2//syntax-checking)
  (add-hook 'js2-mode-hook 'serika-l/js2//snippet-engine)
  (add-hook 'js2-mode-hook 'serika-l/js2//auto-completion)
  (add-hook 'js2-mode-hook 'serika-l/js2//auto-pairing)
  (add-hook 'js2-mode-hook 'serika-l/js2//multi-compile)

  (add-hook 'js2-mode-hook 'serika-l/js2//interface)
  (add-hook 'js2-mode-hook 'serika-l/js2//prettify-symbols))
