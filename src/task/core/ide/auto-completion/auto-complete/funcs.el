;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/auto-complete/activate ()
  "Activate `auto-complete' in current buffer."
  (auto-complete-mode +1))

(defun serika-f/auto-complete/create-activator (&rest forms)
  "Create lambda that activates `auto-complete', and
executes FORMS after."
  `(lambda ()
     (auto-complete-mode +1)
     (progn ,@forms)))

;; Init
(defun init ()
  "Configure `auto-complete'."
  (serika-c/eg/add-install :package-list '(auto-complete)
                           :name         'auto-complete)

  (serika-c/eg/add-many 'auto-complete
                        ("require")
                        (lambda ()
                          (require 'auto-complete))

                        ("settings")
                        (lambda ()
                          (setq ac-delay 0.1)
                          (setq ac-quick-help-delay 2)
                          (setq ac-auto-start 2)
                          (setq ac-use-quick-help t)
                          (setq ac-menu-height 20)
                          (setq ac-ignore-case t)
                          (setq ac-use-fuzzy t)

                          (setq ac-comphist-file (func/path/join serika-tmp-directory
                                                                     "ac-comphist.dat")))

                        ("keymap")
                        (lambda ()
                          (setq --ac-mode-map       ac-mode-map)
                          (setq --ac-completing-map ac-completing-map)

                          (func/keymap/create ac-completing-map
                                                  "A-n" #'ac-stop
                                                  "A-e" #'ac-next
                                                  "A-o" #'ac-complete
                                                  "A-i" #'ac-previous))))
