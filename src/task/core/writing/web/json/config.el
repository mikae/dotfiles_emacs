;;; package --- Summary
;;; Commentary:
;;; Code:

;; Configuration
(serika/json/evil)
(serika/json/keymap)

;; Hooks
(add-hook 'json-mode-hook 'serika/json/buffer-local-variables)
(add-hook 'json-mode-hook 'serika/json/buffer-local-mappings)

(add-hook 'json-mode-hook 'serika/json/syntax-checking)
(add-hook 'json-mode-hook 'serika/json/snippet-engine)
(add-hook 'json-mode-hook 'serika/json/auto-completion)

(add-hook 'json-mode-hook 'serika/json/interface)
(add-hook 'json-mode-hook 'serika/json/prettify-symbols)
