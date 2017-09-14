;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/which-key/activate ()
  "Activate `which-key'."
  (interactive)
  (which-key-mode))


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
     (eval `(which-key-add-major-mode-key-based-replacements ',--mode
                                                             ,@--binding-descriptions))))

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

;; Init
(defun init ()
  "Configure `which-key'."
  (serika-c/eg/add-install :type 'package
                           :name 'which-key
                           :package-list '(which-key))

  (serika-c/eg/add-many-by-name 'which-key
                                ("require")
                                (lambda ()
                                  (require 'which-key))

                                ("settings")
                                (lambda ()
                                  (setq which-key-popup-type 'side-window)
                                  (setq which-key-side-window-location 'bottom)
                                  (setq which-key-side-window-max-width 0.33)
                                  (setq which-key-side-window-max-height 0.25))))
