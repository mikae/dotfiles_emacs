;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(cl-defun serika-f/company/activate (&key (backends-add nil backends-add-p)
                                          (backends-set nil backends-set-p))
  ""
  (when (and backends-add-p
             backends-set-p)
    (error "`backends-add' and `backends-set' was provided simultaneously"))

  (when backends-add-p
    (set (make-local-variable 'company-backends)
         (cons backends-add company-backends)))

  (when backends-set-p
    (set (make-local-variable 'company-backends)
         backends-set))

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

  (dolist (--package '((company-mode       . "https://github.com/shinkiley/company-mode")
                       (company-statistics . "https://github.com/shinkiley/company-statistics")
                       (company-quickhelp  . "https://github.com/shinkiley/company-quickhelp")))
    (serika-c/eg/add-install :type 'git
                             :name (car --package)
                             :src  (cdr --package)))

  (serika-c/eg/add-many-by-name 'company
    ("require")
    (func/func/require 'company
                       'company-quickhelp
                       'company-statistics)

    ("interface theme")
    (setq company-quickhelp-color-background "#282c34"
          company-quickhelp-color-foreground "#bbc2cf")

    ("settings")
    (progn
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
    (progn
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
