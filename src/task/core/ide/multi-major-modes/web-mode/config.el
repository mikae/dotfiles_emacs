;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'func-package)

;; Install `web-mode'
(serika/package/make-sure-installed 'web-mode)

;; Require `web-mode'
(require 'web-mode)

;; Configure `auto-mode-alist'
(progn
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  ())

;; Configure `evil' for `web-mode'
(evil-set-initial-state 'web-mode 'normal)

;; Configure indentation
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;; Configure paddings
(setq web-mode-style-padding 1)
(setq web-mode-script-padding 1)

;; Features
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-block-face t)
(setq web-mode-enable-part-face t)
(setq web-mode-enable-comment-keywords t)
(setq web-mode-enable-heredoc-fontification t)

(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)

(provide 'serika-emacs-plugin-configuration-web-mode)
;;; serika-emacs-plugin-configuration-web-mode.el ends here
