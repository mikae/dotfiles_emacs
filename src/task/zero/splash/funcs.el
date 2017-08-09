;;; package --- Summary
;;; Commentary:
;;; Code:

;; Declare variables
(defvar serika-splash-enable t
  "If t then greetings will be shown.")

(defvar serika-splash-greeting-list (list "Hello there!"
                                          "Glad to see you here!"
                                          "Welcome!"))

(defvar serika-splash-buffer-name "*Welcome message :3*")

(defun serika-f/splash/image-file ()
  "Return path to greetings file."
  (when (boundp 'serika-images-directory)
    (func/path/join serika-images-directory
                        "greetings.png")))

(defun serika-f/splash/insert-header ()
  "Insert the head part of the splash screen into the current buffer."
  (let* ((image-file (serika-f/splash/image-file))
         (img (create-image image-file))
         (image-width (and img (car (image-size img))))
         (window-width (window-width)))
    (message image-file)
    (when img
      (insert (propertize " " 'display
                          `(space :align-to (+ center (-0.5 . ,img)))))
      (insert-image img)
      (insert "\n")
      (insert (func/list/random serika-splash-greeting-list))
      (insert "\n"))))

(defun serika-f/splash/insert-content ()
  "Insert content to greetings buffer."
  ())

(defun serika-f/splash/insert-footer ()
  "Insert footer to greetings buffer."
  (insert "Have a nice session :3")
  (insert "\n"))

(defun serika-f/splash/configure-splash-screen ()
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

                (serika-f/splash/insert-header)
                (serika-f/splash/insert-content)
                (serika-f/splash/insert-footer)

                (use-local-map splash-screen-keymap)
                (setq buffer-read-only t)
                (set-buffer-modified-p nil)
                (if (and view-read-only (not view-mode))
                    (view-mode-enter nil 'kill-buffer))
                (goto-char (point-max)))
              splash-buffer)))))

(defun serika-f/splash/configure-keymap ()
  "Configure `splash-screen-keymap'."
  (func/keymap/save splash-screen-keymap)
  (func/keymap/create splash-screen-keymap
                          "q" #'exit-splash-screen))

(defun serika-f/splash/configure ()
  "Configure splash screen."
  (when serika-splash-enable
    (progn
      (serika-f/splash/configure-splash-screen)
      (serika-f/splash/configure-keymap))))

;; Functions
(defun serika-f/splash/show ()
  "Show splash screen."
  (interactive)
  (mapc (func/func/lambda 'serika-f/splash/show
                              (buffer)
                              (with-current-buffer buffer
                                (func/buffer/kill)))
        (buffer-list))
  (switch-to-buffer (funcall initial-buffer-choice))
  (delete-other-windows))

(defun init ()
  (serika-c/eg/add-many 'splash
                        ("post")
                        #'serika-f/splash/configure))
