;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-g/lisp-interaction//keymap ()
  "Configure keymap for `lisp-interaction' mode."
  (setq --serika-lisp-interaction-mode-map lisp-interaction-mode-map)
  (setq lisp-interaction-mode-map (let ((map (make-sparse-keymap)))
                              (define-key map (kbd "C-t =") #'evil-indent)
                              (define-key map (kbd "C-t /") #'evilnc-comment-or-uncomment-lines)
                              (define-key map (kbd "C-c e") #'eval-last-sexp)
                              map)))

;; Local
(defun serika-l/lisp-interaction//save-function ()
  "Ignore saving."
  (make-local-variable 'serika-buffer-save-function)
  (setq serika-buffer-save-function 'ignore))

(defun serika-l/lisp-interaction//buffer-local-variables ()
  "Configure snippet engine for `lisp-interaction' mode."
  (setq tab-width 2)
  (setq truncate-lines t))


(defun serika-l/lisp-interaction//evil ()
  "Configure `evil' for `lisp-interaction-mode'."
  (setq evil-shift-width 2)
  (evil-local-mode +1)
  (evil-normal-state))

(defun serika-l/lisp-interaction//auto-pairing ()
  "Configure auto completion for `lisp-interaction' mode."
  (electric-pair-mode +1))

(defun serika-l/lisp-interaction//interface ()
  "Configure interface for `lisp-interaction' mode."
  (setq show-trailing-whitespace +1)

  (rainbow-delimiters-mode       +1)
  (linum-mode                    +1))

(defun serika-l/lisp-interaction//prettify-symbols ()
  "Configure prettify symbols for `lisp-interaction' mode."
  (prettify-symbols-mode +1)

  (setq prettify-symbols-alist ())

  (push '("lambda" . ?λ) prettify-symbols-alist)
  (push '(">="     . ?≤) prettify-symbols-alist)
  (push '("<="     . ?≥) prettify-symbols-alist))

(defun init ()
  "Configure `lisp-interaction-mode'."
  (serika-g/lisp-interaction//keymap)

  (add-hook 'lisp-interaction-mode-hook 'serika-l/lisp-interaction//evil)
  (add-hook 'lisp-interaction-mode-hook 'serika-l/lisp-interaction//buffer-local-variables)
  (add-hook 'lisp-interaction-mode-hook 'serika-l/lisp-interaction//save-function)

  (add-hook 'lisp-interaction-mode-hook 'serika-l/lisp-interaction//auto-pairing)

  (add-hook 'lisp-interaction-mode-hook 'serika-l/lisp-interaction//interface)
  (add-hook 'lisp-interaction-mode-hook 'serika-l/lisp-interaction//prettify-symbols))
