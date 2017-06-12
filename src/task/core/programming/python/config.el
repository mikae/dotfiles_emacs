;;; package --- Summary
;;; Commentary:
;;; Code:

;; Hooks
(add-hook 'python-mode-hook 'serika/python/evil)
(add-hook 'python-mode-hook 'serika/python/buffer-local-variables)
(add-hook 'python-mode-hook 'serika/python/buffer-local-mappings)
(add-hook 'python-mode-hook 'serika/python/minor-modes)

(add-hook 'python-mode-hook 'serika/python/snippet-engine)
(add-hook 'python-mode-hook 'serika/python/syntax-checking)
(add-hook 'python-mode-hook 'serika/python/auto-completion)

(add-hook 'python-mode-hook 'serika/python/interface)
