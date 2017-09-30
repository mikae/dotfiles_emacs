;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/auto-complete/activate ()
  "Activate `auto-complete' in current buffer."
  (interactive)
  (auto-complete-mode +1))

;; Init
(defun init ()
  "Configure `auto-complete'."
  (serika-c/eg/add-install :type 'package
                           :package-list '(auto-complete)
                           :name         'auto-complete)

  (serika-c/eg/add-many-by-name 'auto-complete
                                ("require")
                                (lambda ()
                                  (require 'auto-complete))

                                ("settings")
                                (lambda ()
                                  (setq ac-delay 0.1)
                                  (setq ac-quick-help-delay 0.1)
                                  (setq ac-auto-start 1)
                                  (setq ac-use-quick-help t)
                                  (setq ac-menu-height 20)
                                  (setq ac-ignore-case t)
                                  (setq ac-use-fuzzy t)

                                  (setq ac-comphist-file (f-join serika-tmp-directory
                                                                 "ac-comphist.dat")))

                                ("keymap")
                                (lambda ()
                                  (func/keymap/save ac-mode-map)
                                  (func/keymap/save ac-completing-map)

                                  (func/keymap/create ac-completing-map
                                                      "A-n" #'ac-stop
                                                      "A-e" #'ac-next
                                                      "A-o" #'ac-complete
                                                      "A-i" #'ac-previous))))
