;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/neotree/create ()
  "Create `neotree' buffer."
  (neotree-show))

(defun serika-f/neotree/remove ()
  "Require modules for `neotree'."
  (neotree-hide))

(defun serika-f/neotree/exists-p ()
  "Require modules for `neotree'."
  (serika-f/buffer/exists-p 'neotree-mode))

(defun serika-f/neotree/not-exists-p ()
  "Require modules for `neotree'."
  (serika-f/buffer/not-exists-p 'neotree-mode))

;; Global
(defun serika-g/neotree//require ()
  "Require modules for `neotree'."
  (require 'neotree))

;; Init
(defun init ()
  "Configure `neotree'."
  (serika-c/eg/add-install :package-list '(neotree)
                           :name         'neotree)

  (serika-c/eg/add :parents '("require")
                   :name    'neotree
                   :func    #'serika-g/neotree//require)
  )
