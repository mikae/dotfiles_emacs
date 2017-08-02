;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/smartparens/enable ()
  "Enable `smartparens' in the current buffer."
  (smartparens-mode +1))

;; Configuration
(defun serika-g/smartparens//require ()
  "Require modules for `smartparens'."
  (require 'smartparens))

(defun serika-g/smartparens//settings ()
  "Configure smartparens keymaps."
  ;; Remove default pairs
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
  (sp-pair "`"     nil :actions :rem))

(defun serika-g/smartparens//keymap ()
  "Configure smartparens keymaps."
  )

(defun serika-g/smartparens//global-keymap ()
  "Configure global keymap."
  )

;; Init
(defun init ()
  "Configure smartparens."
  (serika-c/eg/add-install :package-list '(smartparens)
                           :name 'smartparens)

  (serika-c/eg/add :parents '("require")
                   :name    'smartparens
                   :func    #'serika-g/smartparens//require)

  (serika-c/eg/add :parents '("settings")
                   :name    'smartparens
                   :func    #'serika-g/smartparens//settings)

  (serika-c/eg/add :parents '("keymap")
                   :name    'smartparens
                   :func    #'serika-g/smartparens//keymap)

  (serika-c/eg/add :parents '("global-keymap")
                   :name    'smartparens
                   :func    #'serika-g/smartparens//global-keymap)
  )
