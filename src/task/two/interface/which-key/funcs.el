;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defmacro serika-f/which-key//define-keys (mode keymap-name &rest bindings)
  "Define keys in KEYMAP-NAME with `which-key' descriptions in MODE.
Usage:
(serika-f/which-key//define-keys
 major-mode
 keymap
 \"keystroke sequence\" executable \"which-key description\").
`executable' is something that can be executed by `func/keymap/define'."
  `(let ((--mode                 ',mode)
         (--keymap-name          ',keymap-name)
         (--bindings             ',bindings)
         (--keymap-bindings      ())
         (--binding-descriptions ()))
     (setq --keymap-bindings
           (cl-loop for --kbd  in --bindings        by #'cdddr
                    for --func in (cdr --bindings)  by #'cdddr
                    for --desc in (cddr --bindings) by #'cdddr
                    collect --kbd
                    collect --func))
     (setq --binding-descriptions
           (cl-loop for --kbd  in --bindings        by #'cdddr
                    for --func in (cdr --bindings)  by #'cdddr
                    for --desc in (cddr --bindings) by #'cdddr
                    collect --kbd
                    collect --desc))
     (eval `(func/keymap/define ,--keymap-name
                                ,@--keymap-bindings))
     (if --mode
         (eval `(which-key-add-major-mode-key-based-replacements ',--mode
                  ,@--binding-descriptions))
       (eval `(which-key-add-key-based-replacements
                ,@--binding-descriptions)))))

(defmacro serika-f/which-key/create-keymap (mode keymap-name &rest bindings)
  "Create new keymap KEYMAP-NAME with keys in BINDINGS with `which-key' descriptions in MODE.
Usage:
(serika-f/which-key/create-keymap
 major-mode
 keymap
 \"keystroke sequence\" executable \"which-key description\").
`executable' is something that can be executed by `func/keymap/define'."
  `(setq ,keymap-name (make-sparse-keymap))
  `(serika-f/which-key//define-keys ,mode
                                    ,keymap-name
                                    ,@bindings))

(defmacro serika-f/which-key/define-keys (mode keymap-name &rest bindings)
  "Define keys in KEYMAP-NAME with `which-key' descriptions in MODE.
Usage:
(serika-f/which-key/define-keys
 major-mode
 keymap
 \"keystroke sequence\" executable \"which-key description\").
`executable' is something that can be executed by `func/keymap/define'."
  `(serika-f/which-key//define-keys ,mode
                                    ,keymap-name
                                    ,@bindings))

(defmacro serika-f/which-key/define-global-keys (&rest bindings)
  `(serika-f/which-key//define-keys nil
                                    global-map
                                    ,@bindings))

(defun serika-f/which-key/activate ()
  "Activate `which-key-mode'."
  (which-key-mode +1))

;; Init
(defun init ()
  "Configure `which-key'."
  (serika-c/eg/add-install :type    'git
                           :name    'which-key
                           :src     "https://github.com/shinkiley/emacs-which-key")

  (serika-c/eg/add-many-by-name 'which-key
    ("require")
    (progn
      (func/func/require 'which-key)
      ;; Indentation setup
      (put 'serika-f/which-key/create-keymap 'lisp-indent-function 'defun))

    ("settings")
    (progn
      (setq which-key-idle-delay 0.2)
      (which-key-setup-side-window-bottom))

    ("post activate")
    (serika-f/which-key/activate)))
