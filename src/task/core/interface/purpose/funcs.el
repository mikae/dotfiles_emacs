;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/purpose/layoutp (layout-path)
  "Return t if LAYOUT-PATH is correct window purpose layout file."
  (and (file-exists-p layout-path)))

(defun serika-f/purpose/use-layout (layout-name)
  (let ((layout-path (serika-f/path/join serika-layouts-directory
                                         layout-name)))
    (if (serika-f/purpose/layoutp layout-path)
        (lambda ()
          (purpose-x-code1-setup)
          (purpose-load-window-layout-file layout-path))
      (error "Invalid file path to window purpose layout."))))

;; Global
(defun serika-g/purpose//require ()
  "Require modules for `purpose'."
  (require 'func-path)
	(setq purpose-use-default-configuration nil)
  (require 'window-purpose)
  (require 's-helm-window-purpose))

(defun serika-g/purpose//settings ()
  "Require modules for `purpose'."
  ;; `auto-mode-alist'
  (add-to-list 'auto-mode-alist '("\\.purpose-layout\\'" . emacs-lisp-mode))

  (setq purpose-preferred-prompt 'helm)

  ;; `edit' purpose
  (add-to-list 'purpose-user-mode-purposes '(lua-mode . edit))
  (add-to-list 'purpose-user-mode-purposes '(sh-mode  . edit))
  (add-to-list 'purpose-user-mode-purposes '(js2-mode . edit))
  (add-to-list 'purpose-user-mode-purposes '(emacs-lisp-mode . edit))

  ;; `check' purpose
  (add-to-list 'purpose-user-mode-purposes '(flycheck-error-list-mode . check))

  ;; `neotree' purpose
  (add-to-list 'purpose-user-mode-purposes '(neotree-mode . neotree))

  (purpose-compile-user-configuration)

  ;; `s-helm-window-purpose'
  (add-to-list 's-purpose-layout-dirs (serika-f/path/join serika-conf-directory
                                                          "layouts")))

(defun serika-g/purpose//keymap ()
  "Require modules for `purpose'."
  (setq purpose-mode-map (let ((map (make-sparse-keymap)))
                           (define-key map (kbd "C-x p l") #'s/purpose-layout-helm)
													 map)))

(defun serika-g/purpose//global-keymap ()
  "Require modules for `purpose'."
  (global-set-key (kbd "C-x p l") #'s/purpose-layout-helm)
  (global-set-key (kbd "C-x p k") #'purpose-delete-non-dedicated-windows))

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

	(serika-c/eg/add :parents '("global-keymap")
									 :name    'w-purpose
									 :func    #'serika-g/purpose//global-keymap)

	(serika-c/eg/add :parents '("post activate")
									 :name    'w-purpose
									 :func    #'serika-l/purpose//activate)
  )
