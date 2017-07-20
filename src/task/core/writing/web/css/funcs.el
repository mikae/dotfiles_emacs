;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/css//require ()
  "Require modules for css"
  (require 'css-mode))

(defun serika-g/css//settings ()
  "Configure `css'."
  (add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode)))

(defun serika-g/css//auto-mode-alist ()
  "Configure `auto-mode-alist' for `css'."
  )

(defun serika-g/css//keymap ()
  "Configure `css-mode-map'"
  (setq --serika-css-mode-map css-mode-map)
  (setq css-mode-map (let ((map (make-sparse-keymap)))
                       (define-key map (kbd "C-t e") #'yas-expand)
                       (define-key map (kbd "C-t E") #'serika-f/emmet/expand)
                       (define-key map (kbd "C-t =") #'evil-indent)
                       (define-key map (kbd "C-t +") #'web-beautify-css)
                       (define-key map (kbd "C-t /") #'evilnc-comment-or-uncomment-lines)
                       map)))

;; Local
(defun serika-l/css//evil ()
  "Configure `evil' for `css'."
  (setq evil-shift-width 2)
  (evil-local-mode +1)
  (evil-normal-state))

(defun serika-l/css//buffer-local-variables ()
  "Configure buffer-local variables for `css' files."
  (setq evil-shift-width 2)
  (setq truncate-lines t))

(defun serika-l/css//snippet-engine ()
  "Configure snippet engine for `web-mode' buffers with `css' engine."
  (serika-f/emmet/activate)
  (serika-f/yasnippet/activate))

(defun serika-l/css//syntax-checking ()
  "Configure syntax checking for `web-mode' buffers with `css' engine."
  (flycheck-mode +1))

(defun serika-l/css//interface ()
  "Configure interface for `web-mode' buffers with `css' engine."
  (setq show-trailing-whitespace +1)

  (rainbow-delimiters-mode       +1)
  (rainbow-mode                  +1)
  (serika-f/linum-relative/activate)

  (prettify-symbols-mode   +1)

  (setq prettify-symbols-alist ()))

(defun init ()
  "Configure `css'."
  (serika-c/eg/add :parents '("require")
                   :name    'css
                   :func    #'serika-g/css//require)

  (serika-c/eg/add :parents '("settings")
                   :name    'css
                   :func    #'serika-g/css//settings)

  (serika-c/eg/add :parents '("settings css")
                   :name    'auto-mode-alist
                   :func    #'serika-g/css//auto-mode-alist)

  (serika-c/eg/add :parents '("hook")
                   :name    'css
                   :func    (lambda ()
			      (add-hook 'css-mode-hook 'serika-l/css//evil)
			      (add-hook 'css-mode-hook 'serika-l/css//buffer-local-variables)
			      (add-hook 'css-mode-hook 'serika-l/css//snippet-engine)
			      (add-hook 'css-mode-hook 'serika-l/css//syntax-checking)
			      (add-hook 'css-mode-hook 'serika-l/css//interface))))
