;;; package --- Summary
;;; Commentary:
;;; Code:

;; Local
(defun serika-l/lisp-interaction//save-function ()
  "Ignore saving."
  (set (make-local-variable 'serika-buffer-save-function)
       #'ignore))

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
                        ("keymap")
                        (lambda ()
                          (serika-f/keymap/create lisp-interaction-mode-map
                                                  "C-t =" #'evil-indent
                                                  "C-t /" #'evilnc-comment-or-uncomment-lines
                                                  "C-c e" #'eval-last-sexp))
                        ("hook")
                        (lambda ()
                          (dolist (callback (list
                                             'serika-l/lisp-interaction//evil
                                             'serika-l/lisp-interaction//buffer-local-variables
                                             'serika-l/lisp-interaction//save-function

                                             'serika-l/lisp-interaction//interface
                                             'serika-l/lisp-interaction//prettify-symbols))
                            (serika-f/hook/add 'lisp-interaction-mode-hook callback)))))
