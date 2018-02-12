;;; package --- Summary
;;; Commentary:
;;; Code:

;; Funcs
(defun serika-f/ibuffer/mark ()
  ""
  (interactive))

(defun serika-f/ibuffer/mark-all ()
  ""
  (interactive))

(defun serika-f/ibuffer/unmark ()
  ""
  (interactive))

;; Hook
(defun serika-f/ibuffer/hook ()
  "Hook for `ibuffer-mode'."
  (ibuffer-switch-to-saved-filter-groups "default"))

;; Init
(defun init ()
  "Configure `ibuffer'."
  (serika-c/eg/add-many-by-name 'ibuffer
    ("require")
    (func/func/require 'ibuffer
                       'ibuf-ext)

    ("settings")
    (progn
      (setq ibuffer-saved-filter-groups
            '(("default"
               ("dired" (mode . dired-mode)))))
      (setq ibuffer-never-show-predicates '("^\\*")))

    ("keymap")
    (progn
      (func/keymap/save   ibuffer-mode-map)
      (func/keymap/create ibuffer-mode-map
        ;; neio
        "e"          #'ibuffer-forward-line
        "i"          #'ibuffer-backward-line
        "E"          #'ibuffer-forward-filter-group
        "I"          #'ibuffer-backward-filter-group

        ;; arstd
        "a a"        #'serika-f/ibuffer/mark
        "a A"        #'serika-f/ibuffer/mark-all
        "a r"        #'serika-f/ibuffer/unmark
        "a R"        #'ibuffer-unmark-all
        "a s"        #'ibuffer-mark-unsaved-buffers
        "a S"        #'ibuffer-mark-read-only-buffers
        "a t"        #'ibuffer-mark-by-mode
        "a d"        #'ibuffer-mark-help-buffers
        "a D"        #'ibuffer-mark-dired-buffers

        "r a"        #'ibuffer-do-delete
        "r A"        #'ibuffer-do-save

        "s a"        #'ibuffer-do-sort-by-alphabetic
        "s f"        #'ibuffer-do-sort-by-filename/process
        "s i"        #'ibuffer-invert-sorting
        "s m"        #'ibuffer-do-sort-by-major-mode
        "s s"        #'ibuffer-do-sort-by-size

        ;; qwfpg
        "q"          #'kill-buffer

        "RET"        #'ibuffer-visit-buffer
        "<C-return>" #'ibuffer-visit-buffer-other-window)
      (func/keymap/bind-digits ibuffer-mode-map #'digit-argument))

    ("global-keymap")
    (func/keymap/define-global
      "C-x b l" 'ibuffer)

    ("hook")
    (func/hook/add 'ibuffer-mode-hook
                   #'serika-f/ibuffer/hook)))
