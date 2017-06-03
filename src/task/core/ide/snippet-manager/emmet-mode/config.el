;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'func-package)

;; Install `emmet-mode'
(serika/package/make-sure-installed 'emmet-mode)

;; Configure `emmet-mode-keymap'.
;; It's placed before emmet-mode because otherwise it doesn't work
(progn
  (defvar emmet-mode-keymap (make-sparse-keymap))
  (define-key emmet-mode-keymap (kbd "<C-tab>") 'emmet-expand-line)
  ())

;; Require
(require 'emmet-mode)

;; Configure `emmet-mode'
;; Disable preview
(setq emmet-preview-default nil)


(provide 'serika-emacs-plugin-configuration-emmet-mode)
;;; serika-emacs-plugin-configuration-emmet-mode.el ends here
