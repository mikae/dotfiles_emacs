;;; package --- Summary
;;; Commentary:
;;; Code:

;; Funcs
(defun serika-f/multi-compile/run ()
  "Saves current buffer and invokes `multi-compile-run'."
  (interactive)
  (save-buffer)
  (multi-compile-run))

(defmacro serika-f/multi-compile/configure (m-mode &rest args)
  "Create lambda that configures `multi-compile'."
  `(let ((--major-mode  ,m-mode)
         (--args        ',args)
         (--alist-right ()))
     (cl-loop for --name in --args       by #'cddr
              for --cmd  in (cdr --args) by #'cddr
              do
              (setq --alist-right (cons `(,--name . ,--cmd) --alist-right)))
     (add-to-list 'multi-compile-alist `(,--major-mode . ,--alist-right))))

;; Init
(defun init ()
  "Configure `multi-compile'."
  (serika-c/eg/add-install :type    'git
                           :name    'multi-compile
                           :src     "https://github.com/shinkiley/emacs-multi-compile")

  (serika-c/eg/add-many-by-name 'multi-compile
    ("require")
    (func/func/require 'multi-compile)

    ("settings")
    (setq multi-compile-alist             ()
          multi-compile-completion-system 'helm)))
