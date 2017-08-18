;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(cl-defun serika-f/company/activate (&key (backends nil backends-provided-p))
  "Activate `company-mode' in current buffer."
  (when backends-provided-p
    (setq-local company-backends backends))
  (company-mode +1))

;; Init
(defun init ()
  "Configure `company'."
  (serika-c/eg/add-install :package-list '(company)
                           :name         'company)

  (serika-c/eg/add-many 'company
                        ("require")
                        (lambda ()
                          (require 'company))

                        ("settings")
                        (lambda ()
                          (setq company-minimum-prefix-length 2)
                          (setq company-mode/enable-yas t)
                          (setq company-idle-delay 0.1))

                        ("keymap")
                        (lambda ()
                          (func/keymap/create company-active-map
                                              "A-n" #'company-abort
                                              "A-e" #'company-select-next
                                              "A-o" #'company-complete-selection
                                              "A-i" #'company-select-previous))))
