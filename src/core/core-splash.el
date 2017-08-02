;;; package --- Summary
;;; Commentary:
;;; Code:
;;; Package-Requires:

(require 'func-path)
(require 'func-list)
(require 'func-keymap)

;; Declare variables
(defvar serika-splash-enable t
  "If t then greetings will be shown.")

(defvar serika-splash-greeting-list (list "Hello there!"
                                          "Glad to see you here!"
                                          "Welcome!"))

(defvar serika-splash-buffer-name "*Welcome message :3*")

;; Configuration functions
(defun serika-c/splash/image-file ()
  "Return path to greetings file."
  (when (boundp 'serika-images-directory)
    (serika-f/path/join serika-images-directory
                        "greetings.png")))

(defun serika-c/splash/insert-header ()
  "Insert the head part of the splash screen into the current buffer."
  (let* ((image-file (serika-c/splash/image-file))
         (img (create-image image-file))
         (image-width (and img (car (image-size img))))
         (window-width (window-width)))
    (message image-file)
    (when img
      (insert (propertize " " 'display
                          `(space :align-to (+ center (-0.5 . ,img)))))
      (insert-image img)
      (insert "\n")
      (insert (serika-f/list/random serika-splash-greeting-list))
      (insert "\n"))))

(defun serika-c/splash/insert-content ()
  "Insert content to greetings buffer."
  ())

(defun serika-c/splash/insert-footer ()
  "Insert footer to greetings buffer."
  (insert "Have a nice session :3")
  (insert "\n"))

(defun serika-c/splash/configure-splash-screen ()
  "Configure splash screen."
  (setq inhibit-startup-screen (not serika-splash-enable))

  (setq initial-buffer-choice
        (lambda ()
          (let ((splash-buffer (get-buffer-create serika-splash-buffer-name)))
            (with-current-buffer splash-buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (setq default-directory command-line-default-directory)
                (set (make-local-variable 'startup-screen-inhibit-startup-screen)
                     (when pure-space-overflow
                       (insert pure-space-overflow-message)))

                (serika-c/splash/insert-header)
                (serika-c/splash/insert-content)
                (serika-c/splash/insert-footer)

                (use-local-map splash-screen-keymap)
                (setq buffer-read-only t)
                (set-buffer-modified-p nil)
                (if (and view-read-only (not view-mode))
                    (view-mode-enter nil 'kill-buffer))
                (goto-char (point-max)))
              splash-buffer)))))

(defun serika-c/splash/configure-keymap ()
  "Configure `splash-screen-keymap'."
  (setq --serika-splash-screen-keymap splash-screen-keymap)
  (serika-f/keymap/create splash-screen-keymap
                          "q" #'exit-splash-screen))

(defun serika-c/splash/configure ()
  "Configure splash screen."
  (when serika-splash-enable
    (progn
      (serika-c/splash/configure-splash-screen)
      (serika-c/splash/configure-keymap))))

;; Functions
(defun serika-f/splash/show ()
  "Show splash screen."
  (interactive)
  (mapc (serika-f/func/lambda 'serika-f/splash/show
                              (buffer)
                              (with-current-buffer buffer
                                (serika-f/buffer/kill)))
        (buffer-list))
  (switch-to-buffer (funcall initial-buffer-choice))
  (delete-other-windows))

(provide 'core-splash)
;;; core-splash.el ends here
