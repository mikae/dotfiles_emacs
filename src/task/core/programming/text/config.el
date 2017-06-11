;;; package --- Summary
;;; Commentary:
;;; Code:

;; Configuration
(serika/text/evil)

;; Hooks
(add-hook 'text-mode-hook 'serika/text/buffer-local-variables)
(add-hook 'text-mode-hook 'serika/text/interface)
