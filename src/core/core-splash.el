;;; package --- Summary
;;; Commentary:
;;; Code:
;;; Package-Requires:

(require 'func-path)
(require 'func-list)

;; Declare variables
(defvar serika-splash-enable t
  "If t then greetings will be shown.")

(defvar serika-splash-greeting-list (list "Hello there!"
                                                       "Glad to see you here!"
                                                       "Welcome!"))

;; Declare functions
(defun serika/splash/image-file ()
  "Return path to greetings file."
  (when (boundp 'serika-images-directory)
    (serika/path/join serika-images-directory
                      "greetings.png")))

(defun serika/splash/insert-header ()
  "Insert the head part of the splash screen into the current buffer."
  (let* ((image-file (serika/splash/image-file))
         (img (create-image image-file))
         (image-width (and img (car (image-size img))))
         (window-width (window-width)))
    (message image-file)
    (when img
      (insert (propertize " " 'display
                          `(space :align-to (+ center (-0.5 . ,img)))))
      (insert-image img)
      (insert "\n")
      (insert (serika/list/random serika-splash-greeting-list))
      (insert "\n"))))

(defun serika/splash/insert-content ()
  "Insert content to greetings buffer."
  ())

(defun serika/splash/insert-footer ()
  "Insert footer to greetings buffer."
  (insert "Have a nice session :3")
  (insert "\n"))

(defun serika/splash/configure-splash-screen ()
  "Configure splash screen."
  (setq inhibit-startup-screen (not serika-splash-enable))

  (setq initial-buffer-choice
        (lambda ()
          (let ((splash-buffer (get-buffer-create "*Serika's welcome message*")))
            (with-current-buffer splash-buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (setq default-directory command-line-default-directory)
                (make-local-variable 'startup-screen-inhibit-startup-screen)
                (if pure-space-overflow
                    (insert pure-space-overflow-message))

                (serika/splash/insert-header)
                (serika/splash/insert-content)
                (serika/splash/insert-footer)

                (use-local-map splash-screen-keymap)
                (setq buffer-read-only t)
                (set-buffer-modified-p nil)
                (if (and view-read-only (not view-mode))
                    (view-mode-enter nil 'kill-buffer))
                (goto-char (point-max)))
              splash-buffer)))))

(defun serika/splash/configure-keymap ()
  "Configure `splash-screen-keymap'."
  (setq splash-screen-keymap (make-sparse-keymap))

  (define-key splash-screen-keymap (kbd "q") 'exit-splash-screen))

(defun serika/splash/configure ()
  "Configure splash screen."
  (when serika-splash-enable
    (progn
      (serika/splash/configure-splash-screen)
      (serika/splash/configure-keymap))))

(provide 'core-splash)
;;; core-splash.el ends here
