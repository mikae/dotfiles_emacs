;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/purpose/layoutp (layout-path)
  "Return t if LAYOUT-PATH is correct window purpose layout file."
  (and (file-exists-p layout-path)))

(defun serika-f/purpose/load-layout (layout-name)
  (let ((layout-path (f-join serika-layouts-directory
                             layout-name)))
    (when (serika-f/purpose/layoutp layout-path)
      (purpose-x-code1-setup)
      (purpose-load-window-layout-file layout-path))))

(defun serika-f/purpose/use-layout (layout-name)
  (let ((layout-path (f-join serika-layouts-directory
                             layout-name)))
    (if (serika-f/purpose/layoutp layout-path)
        (lambda ()
          (purpose-x-code1-setup)
          (purpose-load-window-layout-file layout-path))
      (error "Invalid file path to window purpose layout."))))

(defun serika-f/purpose/activate ()
  "Activate `window-purpose'."
  (purpose-mode +1)
  (purpose-compile-user-configuration))

(defmacro serika-f/purpose/add (mode purpose)
  "Register MODE with PURPOSE."
  `(add-to-list 'purpose-user-mode-purposes (cons ,mode ,purpose)))

;; Local
(defun init ()
	"Configure `purpose'."
	(serika-c/eg/add-install :package-list '(window-purpose)
                           :name         'w-purpose)

  (serika-c/eg/add-many 'w-purpose
                        ("require")
                        (lambda ()
                          (setq purpose-use-default-configuration nil)
                          (require 'window-purpose)
                          (require 's-helm-window-purpose))

                        ("settings")
                        (lambda ()
                          ;; `auto-mode-alist'
                          (add-to-list 'auto-mode-alist '("\\.purpose-layout\\'" . emacs-lisp-mode))

                          (setq purpose-preferred-prompt 'helm)

                          ;; `output' purpose
                          (add-to-list 'purpose-user-mode-purposes '(compilation-mode . output))

                          ;; `s-helm-window-purpose'
                          (add-to-list 's-purpose-layout-dirs (func/path/join serika-conf-directory
                                                                                  "layouts")))

                        ("keymap")
                        (lambda ()
                          (func/keymap/save purpose-mode-map)
                          (func/keymap/create purpose-mode-map
                                                  "C-x p l" #'s/purpose-layout-helm))

                        ("global-keymap")
                        (lambda ()
                          (func/keymap/define-global "C-x p l" #'s/purpose-layout-helm
                                                         "C-x p k" #'purpose-delete-non-dedicated-windows))
                        ("post activate")
                        #'serika-f/purpose/activate))
