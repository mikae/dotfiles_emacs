;;; package --- Summary
;;; Commentary:
;;; Code:

(serika/xdefaults//evil)
(serika/xdefaults//keymap)

;; Hooks
(add-hook 'conf-xdefaults-mode-hook 'serika/xdefaults//buffer-local-variables)
(add-hook 'conf-xdefaults-mode-hook 'serika/xdefaults//buffer-local-mappings)

(add-hook 'conf-xdefaults-mode-hook 'serika/xdefaults//interface)
