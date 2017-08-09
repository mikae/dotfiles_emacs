;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/company/activate ()
  "Activate `company-mode' in current buffer."
  (company-mode +1))

(defmacro serika-f/company/create-activator (&rest forms)
  "Return lambda that activater `company-mode' in current buffer.
Executer FORMS after."
  `(lambda ()
     (company-mode +1)

     (progn ,@forms)))

;; Init
(defun init ()
  "Configure `company'."
  (serika-c/eg/add-install :package-list '(company)
                           :name         'company)

  (serika-c/eg/add-many 'company
                        ("require")
                        (lambda ()
                          ()
                          (require 'company))

                        ("settings")
                        (lambda ()
                          (setq company-minimum-prefix-length 2)
                          (setq company-mode/enable-yas t))

                        ("keymap")
                        (lambda ()
                          (func/keymap/create company-active-map
                                                  "A-n" #'company-abort
                                                  "A-e" #'company-select-next
                                                  "A-o" #'company-complete-selection
                                                  "A-i" #'company-select-previous))))
