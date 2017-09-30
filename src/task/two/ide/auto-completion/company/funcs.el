;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(cl-defun serika-f/company/activate (&key (backends nil backends-provided-p))
  "Activate `company-mode' in current buffer.
Supported keys:
  - `backends': add backends to `company-backends' and make it local variable."
  (when backends-provided-p
    (set (make-local-variable 'company-backends)
         backends))
  (company-mode            +1)
  (company-statistics-mode +1)
  (company-quickhelp-mode  +1))

;; Init
(defun init ()
  "Configure `company'."
  (serika-c/eg/add-install :package-list '(company
                                           company-statistics
                                           company-quickhelp)
                           :name         'company)

  (serika-c/eg/add-many-by-name 'company
                                ("require")
                                (func/func/requirer 'company
                                                    'company-quickhelp
                                                    'company-statistics)

                                ("interface theme")
                                (lambda ()
                                  (setq company-quickhelp-color-background "#282c34")
                                  (setq company-quickhelp-color-foreground "#bbc2cf"))

                                ("settings")
                                (lambda ()
                                  ;; company
                                  (setq company-minimum-prefix-length 1)
                                  (setq company-mode/enable-yas t)
                                  (setq company-idle-delay 0.1)
                                  (setq company-backends
                                        '((company-files
                                           company-keywords
                                           company-capf
                                           :separate
                                           company-yasnippet)
                                          (company-abbrev company-dabbrev)))
                                  (setq company-show-numbers t)

                                  ;;company-quickhelp
                                  (setq company-quickhelp-delay 0.1))

                                ("keymap")
                                (lambda ()
                                  (func/keymap/create company-active-map
                                                      "A-n" #'company-abort
                                                      "A-e" #'company-select-next
                                                      "A-o" #'company-complete-selection
                                                      "A-i" #'company-select-previous)
                                  (func/keymap//bind company-active-map
                                                     ;; todo: change it to mapcar style
                                                     '("A-1"
                                                       "A-2"
                                                       "A-3"
                                                       "A-4"
                                                       "A-5"
                                                       "A-6"
                                                       "A-7"
                                                       "A-8"
                                                       "A-9"
                                                       "A-0")
                                                     #'company-complete-number))))
