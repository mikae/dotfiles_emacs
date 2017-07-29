;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/company//require ()
  "Require modules for `company'."
  (require 'func-package)
  (require 'company))

(defun serika-g/company//settings ()
  "Configure `company-mode'."
  (setq company-minimum-prefix-length 2)
  (setq company-mode/enable-yas t))

(defun serika-g/company//keymap ()
  "Configure `company-active-map'."
  (setq company-active-map (make-sparse-keymap))

  (let ((map company-active-map))
    (define-key company-active-map (kbd "A-n") 'company-abort)
    (define-key company-active-map (kbd "A-e") 'company-select-next)
    (define-key company-active-map (kbd "A-o") 'company-complete-selection)
    (define-key company-active-map (kbd "A-i") 'company-select-previous)))

(defun init ()
  "Configure `company'."
  (serika-c/eg/add-install :package-list '(company)
                           :name         'company)

  (serika-c/eg/add :parents '("require")
                   :name    'company
                   :func    #'serika-g/company//require)

  (serika-c/eg/add :parents '("settings")
                   :name    'company
                   :func    #'serika-g/company//settings)

  (serika-c/eg/add :parents '("keymap")
                   :name    'company
                   :func    #'serika-g/company//keymap))
