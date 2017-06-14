;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'web-mode)

(defun serika/web/top-language ()
  "Return the main language of `web-mode' buffer."
  (save-excursion
    (goto-char (point-min))
    (web-mode-language-at-pos)))

(defun serika/web/buffer-is-html-p ()
  "Return t if `web-mode' buffer type is html."
  (string= (serika/web/top-language) "html"))

(defun serika/web//settings ()
  "Configure `web-mode' variables."
;; Configure `auto-mode-alist'
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

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
  (setq web-mode-enable-current-column-highlight t))

(defun init ()
  "Configure `web-mode'."
  (serika/web//settings))
