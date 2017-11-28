;;; package --- Summary
;;; Commentary:
;;; Code:

;; Funcs

;; Init
(defun init ()
  "Configure `ibuffer'."
  (serika-c/eg/add-many-by-name 'ibuffer
    ("require")
    (func/func/require 'ibuffer
                       'ibuf-ext)

    ("settings")
    (progn
      (add-to-list 'ibuffer-never-show-predicates "^\\*")
      (setq ibuffer-saved-filter-groups nil))

    ("keymap")
    (progn
      (func/keymap/save   ibuffer-mode-map)
      (func/keymap/create ibuffer-mode-map
                          "A-e"        #'ibuffer-forward-line
                          "A-i"        #'ibuffer-backward-line
                          "A-E"        #'ibuffer-forward-filter-group
                          "A-I"        #'ibuffer-backward-filter-group

                          ;; arstd

                          ;; "a a"        #'serika-f/ibuffer/mark
                          ;; "a A"        #'serika-f/ibuffer/mark-all
                          ;; "a r"        #'serika-f/ibuffer/unmark
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
                          "q"          #'ibuffer-find-file

                          "RET"        #'ibuffer-visit-buffer
                          "<C-return>" #'ibuffer-visit-buffer-other-window)
      (func/keymap/bind-digits ibuffer-mode-map #'digit-argument))

    ("global-keymap")
    (progn
      (func/keymap/define-global "C-x b l" 'ibuffer))))
