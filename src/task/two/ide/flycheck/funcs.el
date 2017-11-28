;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(cl-defun serika-f/flycheck/activate (&key ((:disabled-checkers --fdc) '() --fdc-p)
                                           ((:eval --eval) nil --eval-p))
  "Enable flycheck in current buffer."
  (when --fdc-p
    (setq flycheck-disabled-checkers
          (cond
           ((listp --fdc)
            (setq flycheck-disabled-checkers --fdc))
           ((symbolp --fdc-p)
            (setq flycheck-disabled-checkers (list --fdc)))
           (t (error "Unexpected disabled checkers declaration.")))))

  (flycheck-mode +1)

  (when --eval-p
    (eval --eval)))

(defun serika-f/flycheck/create ()
  "Create `flycheck' buffer."
  (interactive)
  (flycheck-list-errors))

(defun serika-f/flycheck/remove ()
  "Remove `flycheck' buffers."
  (interactive)
  (func/buffer/kill-by-major-mode 'flycheck-error-list-mode))

(defun serika-f/flycheck/exists-p ()
  "Return t if any flycheck buffer is created."
  (func/buffer/exists-p 'flycheck-error-list-mode))

(defun serika-f/flycheck/not-exists-p ()
  "Return t if any flycheck buffer is created."
  (func/buffer/not-exists-p 'flycheck-error-list-mode))

(defun serika-f/flycheck/activated-p ()
  "Return t if flycheck is activated in current buffer"
  flycheck-mode)

;; Init
(defun init ()
  "Configure `flycheck'."
  (serika-c/eg/add-install :package-list '(flycheck)
                           :name         'flycheck)

  (serika-c/eg/add-many-by-name 'flycheck
    ("require")
    (func/func/require 'flycheck)

    ("settings")
    (progn
      (add-to-list 'display-buffer-alist
                   `(,(rx bos "*Flycheck errors*" eos)
                     (display-buffer-reuse-window display-buffer-in-side-window)
                     (side            . bottom)
                     (reusable-frames . visible)
                     (window-height   . 0.2)))
      (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change))
      (setq flycheck-idle-change-delay 1))

    ("keymap")
    (progn
      (func/keymap/save flycheck-error-list-mode-map)
      (func/keymap/save flycheck-mode-map)
      (func/keymap/create flycheck-error-list-mode-map
                          "A-n" #'evil-backward-char
                          "A-e" #'evil-next-line
                          "A-i" #'evil-previous-line
                          "A-o" #'evil-forward-char)
      (func/keymap/create flycheck-mode-map))

    ("global-keymap")
    (func/keymap/define-global "C-x s s" 'serika-f/flycheck/create
                               "C-x s h" 'serika-f/flycheck/remove)))
