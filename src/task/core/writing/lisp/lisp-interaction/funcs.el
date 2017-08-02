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
  (set (make-local-variable 'serika-buffer-save-function)
       'ignore))

(defun serika-l/lisp-interaction//buffer-local-variables ()
  "Configure snippet engine for `lisp-interaction' mode."
  (setq tab-width 2)
  (setq truncate-lines t))


(defun serika-l/lisp-interaction//evil ()
  "Configure `evil' for `lisp-interaction-mode'."
  (setq evil-shift-width 2)
  (evil-local-mode +1)
  (evil-normal-state))

(defun serika-l/lisp-interaction//interface ()
  "Configure interface for `lisp-interaction' mode."
  (setq show-trailing-whitespace +1)

  (rainbow-delimiters-mode       +1)
  (serika-f/linum-relative/activate))

(defun serika-l/lisp-interaction//prettify-symbols ()
  "Configure prettify symbols for `lisp-interaction' mode."
  (prettify-symbols-mode +1)

  (setq prettify-symbols-alist ())

  (push '("lambda" . ?λ) prettify-symbols-alist)
  (push '(">="     . ?≤) prettify-symbols-alist)
  (push '("<="     . ?≥) prettify-symbols-alist))

(defun init ()
  "Configure `lisp-interaction-mode'."
  (serika-c/eg/add-many 'lisp-interaction
                        ("keymap") #'serika-g/lisp-interaction//keymap
                        ("hook")   (lambda ()
                                     (dolist (callback (list
                                                        'serika-l/lisp-interaction//evil
                                                        'serika-l/lisp-interaction//buffer-local-variables
                                                        'serika-l/lisp-interaction//save-function

                                                        'serika-f/eldoc/activate

                                                        'serika-l/lisp-interaction//interface
                                                        'serika-l/lisp-interaction//prettify-symbols))
                                       (serika-f/hook/add 'lisp-interaction-mode-hook callback)))))
