;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'func-package)
(require 'func-path)

;; Install packages
(serika/package/make-sure-installed 'flx-ido)
(serika/package/make-sure-installed 'ido-ubiquitous)

;; Requires
(require 'ido)
(require 'flx-ido)
(require 'ido-ubiquitous)

;; Configuration
(ido-mode       t)
(ido-everywhere t)
(ido-ubiquitous-mode 1)

(setq ido-enable-flex-matching t)

(setq ido-save-directory-list-file (serika/path/join serika-tmp-directory
                                                     "ido.hist"))

;; Keymaps
(setq ido-buffer-completion-map      (make-sparse-keymap))
(setq ido-file-dir-completion-map    (make-sparse-keymap))
(setq ido-file-completion-map        (make-sparse-keymap))
(setq ido-common-completion-map      (make-sparse-keymap))

(add-hook 'ido-setup-hook (lambda ()
  (let ((lambda-define-keys
         (lambda (target-map)
           (define-key target-map (kbd "A-j")        'ido-next-match)
           (define-key target-map (kbd "A-k")        'ido-prev-match)
           (define-key target-map (kbd "TAB")        'ido-complete)
           (define-key target-map (kbd "RET")        'ido-exit-minibuffer)
           (define-key target-map (kbd "<C-return>") 'ido-select-text)
         )
        ))
    (funcall lambda-define-keys ido-common-completion-map)

    (funcall lambda-define-keys ido-buffer-completion-map)

    (funcall lambda-define-keys ido-file-dir-completion-map)
    (define-key ido-file-dir-completion-map (kbd "C-f")    'ido-magic-forward-char)

    (funcall lambda-define-keys ido-file-completion-map)
    (define-key ido-file-completion-map (kbd "C-f")        'ido-magic-forward-char))))
