;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/smartparens/activate ()
  "Enable `smartparens' in the current buffer."
  (smartparens-mode +1))

(defun serika-f/smartparens/load (mode)
  (let* ((--conf (func/file/read-as-string (func/path/join serika-conf-directory
                                                                   "smartparens"
                                                                   (concat (symbol-name mode)
                                                                           ".smartparens-pairs"))))
         (--parts (split-string --conf)))
    (when (cl-oddp (length --parts))
      (error "Incorrect prettify configuration file."))
    (cl-loop for --begin in --parts       by #'cddr
             for --end   in (cdr --parts) by #'cddr
             do
             (sp-local-pair mode --begin --end))))

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
