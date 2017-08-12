;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/smartparens/activate ()
  "Enable `smartparens' in the current buffer."
  (smartparens-mode +1))

;; Init
(defun init ()
  "Configure smartparens."
  (serika-c/eg/add-install :package-list '(smartparens)
                           :name 'smartparens)

  (serika-c/eg/add-many 'smartparens
                        ("require")
                        (lambda ()
                          (require 'smartparens))

                        ("settings")
                        (lambda ()
                          (sp-pair "\\\\(" nil :actions :rem)
                          (sp-pair "\\{"   nil :actions :rem)
                          (sp-pair "\\("   nil :actions :rem)
                          (sp-pair "\\\""  nil :actions :rem)
                          (sp-pair "/*"    nil :actions :rem)
                          (sp-pair "\""    nil :actions :rem)
                          (sp-pair "'"     nil :actions :rem)
                          (sp-pair "("     nil :actions :rem)
                          (sp-pair "["     nil :actions :rem)
                          (sp-pair "{"     nil :actions :rem)
                          (sp-pair "`"     nil :actions :rem))))
