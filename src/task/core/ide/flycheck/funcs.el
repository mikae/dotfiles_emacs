;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(cl-defun serika-f/flycheck/activate (&key ((:disabled-checkers --fdc) '() --fdc-p))
  "Enable flycheck in current buffer."
  (when --fdc-p
    (if (listp --fdc)
        (setq flycheck-disabled-checkers --fdc)
      (error "Attempt to set non-list to `flycheck-disabled-checkers'.")
      ))
  (flycheck-mode +1))

(defmacro serika-f/flycheck/create-activator (&rest forms)
  "Create lambda, that activates yasnippet in current buffer.
Executes FORMS after."
  `(lambda ()
    (flycheck-mode +1)
    (progn ,@forms)))

(defun serika-f/flycheck/create ()
  "Create `flycheck' buffer."
  (flycheck-list-errors))

(defun serika-f/flycheck/remove ()
  "Remove `flycheck' buffers."
  (func/buffer/kill-by-major-mode 'flycheck-error-list-mode))

(defun serika-f/flycheck/exists-p ()
  "Return t if any flycheck buffer is created."
  (func/buffer/exists-p 'flycheck-error-list-mode))

(defun serika-f/flycheck/not-exists-p ()
  "Return t if any flycheck buffer is created."
  (func/buffer/not-exists-p 'flycheck-error-list-mode))

;; Init
(defun init ()
  "Configure `flycheck'."
  (serika-c/eg/add-install :package-list '(flycheck)
                           :name         'flycheck)

  (serika-c/eg/add-many 'flycheck
                        ("require")
                        (lambda ()
                          (require 'flycheck))

                        ("settings")
                        (lambda ()
                          (add-to-list 'display-buffer-alist
                                       `(,(rx bos "*Flycheck errors*" eos)
                                         (display-buffer-reuse-window display-buffer-in-side-window)
                                         (side            . bottom)
                                         (reusable-frames . visible)
                                         (window-height   . 0.33)))
                          (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change))
                          (setq flycheck-idle-change-delay 1))

                        ("settings w-purpose")
                        (lambda ()
                          (add-to-list 'purpose-user-mode-purposes '(flycheck-error-list-mode . output)))

                        ("keymap")
                        (lambda ()
                          (func/keymap/save flycheck-error-list-mode-map)
                          (func/keymap/create flycheck-error-list-mode-map
                                                  "A-n" #'evil-backward-char
                                                  "A-e" #'evil-next-visual-line
                                                  "A-i" #'evil-previous-visual-line
                                                  "A-o" #'evil-forward-char))

                        ("global-keymap")
                        (lambda ()
                          (func/keymap/define-global "C-, f s" 'serika-f/flycheck/create
                                                         "C-, f h" 'serika-f/flycheck/remove))))
