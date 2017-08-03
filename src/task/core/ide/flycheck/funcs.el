;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/flycheck/activate ()
  "Enable flycheck in current buffer."
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
  (serika-f/buffer/kill-by-major-mode 'flycheck-error-list-mode))

(defun serika-f/flycheck/exists-p ()
  "Return t if any flycheck buffer is created."
  (serika-f/buffer/exists-p 'flycheck-error-list-mode))

(defun serika-f/flycheck/not-exists-p ()
  "Return t if any flycheck buffer is created."
  (serika-f/buffer/not-exists-p 'flycheck-error-list-mode))

;; Global
(defun serika-g/flycheck//require ()
  "Require modules for `flycheck'."
  (require 'func-buffer)
  (require 'flycheck))

(defun serika-g/flycheck//settings ()
  "Configure `flycheck' settings."
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.33)))

  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change))

  (setq flycheck-idle-change-delay 1))

(defun serika-g/flycheck//keymap ()
  "Configure `flycheck' keymaps."
  (setq flycheck-error-list-mode-map (make-sparse-keymap))

  (define-key flycheck-error-list-mode-map "n" 'evil-backward-char)
  (define-key flycheck-error-list-mode-map "e" 'evil-next-visual-line)
  (define-key flycheck-error-list-mode-map "i" 'evil-previous-visual-line)
  (define-key flycheck-error-list-mode-map "o" 'evil-forward-char))

(defun serika-g/flycheck//global-keymap ()
  "Configure global keymap for using `flycheck'."
  (global-set-key (kbd "C-, f s") 'serika-f/flycheck/create)
  (global-set-key (kbd "C-, f h") 'serika-f/flycheck/remove))

;; Init
(defun init ()
  "Configure `flycheck'."
  (serika-c/eg/add-install :package-list '(flycheck)
                           :name         'flycheck)

  (serika-c/eg/add :parents '("require")
                   :name    'flycheck
                   :func    #'serika-g/flycheck//require)

  (serika-c/eg/add :parents '("settings")
                   :name    'flycheck
                   :func    #'serika-g/flycheck//settings)

  (serika-c/eg/add :parents '("keymap")
                   :name    'flycheck
                   :func    #'serika-g/flycheck//keymap)

  (serika-c/eg/add :parents '("global-keymap")
                   :name    'flycheck
                   :func    #'serika-g/flycheck//global-keymap))
