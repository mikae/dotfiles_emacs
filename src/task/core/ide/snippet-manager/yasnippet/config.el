;;; package --- Summary
;;; Commentary:
;;; Code:

(setq yas-minor-mode-map (make-sparse-keymap))

(require 'yasnippet)

(setq yas-snippet-dirs
      (serika/path/join serika-conf-directory
                        "yasnippet"
                        "snippets"))

(define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)

(setq snippet-mode-map (make-sparse-keymap))

;; Configure `evil'
(evil-set-initial-state 'snippet-mode 'normal)
