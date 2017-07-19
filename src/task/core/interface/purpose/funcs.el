;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/purpose//require ()
  "Require modules for `purpose'."
  (require 'func-path)
	(setq purpose-use-default-configuration nil)
  (require 'window-purpose)
  (require 's-helm-window-purpose))

(defun serika-g/purpose//settings ()
  "Require modules for `purpose'."
  ;; `purpose'
  (setq purpose-preferred-prompt 'helm)
  (add-to-list 'purpose-user-mode-purposes '("mingus-playlist-mode" . "music"))
  (add-to-list 'purpose-user-mode-purposes '("mingus-help-mode"     . "music"))
  (add-to-list 'purpose-user-mode-purposes '("mingus-browse-mode"   . "music"))

  (add-to-list 'purpose-user-mode-purposes '("help-mode"            . "help"))

  ;; `s-helm-window-purpose'
  (add-to-list 's-purpose-layout-dirs (serika-f/path/join serika-conf-directory
                                                          "layouts")))

(defun serika-g/purpose//keymap ()
  "Require modules for `purpose'."
  (setq purpose-mode-map (let ((map (make-sparse-keymap)))
                           (define-key map (kbd "C-x p l") #'s//purpose-layout-helm)
													 map)))

(defun serika-l/purpose//activate ()
  "Require modules for `purpose'."
  (purpose-mode))

;; Local
(defun init ()
	"Configure `purpose'."
	(serika-c/eg/add-install :package-list '(window-purpose)
                           :name         'w-purpose)

	(serika-c/eg/add :parents '("require")
									 :name    'w-purpose
									 :func    #'serika-g/purpose//require)

	(serika-c/eg/add :parents '("settings")
									 :name    'w-purpose
									 :func    #'serika-g/purpose//settings)

	(serika-c/eg/add :parents '("keymap")
									 :name    'w-purpose
									 :func    #'serika-g/purpose//keymap)

	(serika-c/eg/add :parents '("post activate")
									 :name    'w-purpose
									 :func    #'serika-l/purpose//activate)
	)
