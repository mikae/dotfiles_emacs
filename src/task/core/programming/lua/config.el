;;; package --- Summary
;;; Commentary:
;;; Code:

;; Configuration
(serika/lua/keymap)

;; Hooks
(add-hook 'lua-mode-hook 'serika/lua/evil)
(add-hook 'lua-mode-hook 'serika/lua/buffer-local-variables)
(add-hook 'lua-mode-hook 'serika/lua/buffer-local-mappings)

(add-hook 'lua-mode-hook 'serika/lua/syntax-checking)
(add-hook 'lua-mode-hook 'serika/lua/snippet-engine)
(add-hook 'lua-mode-hook 'serika/lua/auto-completion)

(add-hook 'lua-mode-hook 'serika/lua/interface)
(add-hook 'lua-mode-hook 'serika/lua/prettify-symbols)
