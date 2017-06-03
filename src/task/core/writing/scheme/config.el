;;; package --- Summary
;;; Commentary:
;;; Code:

;;; package --- Summary
;;; Commentary:
;;; Code:

;; Configuration
(serika/scheme/evil)

;; Hooks
(add-hook 'scheme-mode-hook 'serika/scheme/buffer-local-variables)
(add-hook 'scheme-mode-hook 'serika/scheme/buffer-local-mappings)

(add-hook 'scheme-mode-hook 'serika/scheme/snippet-engine)
(add-hook 'scheme-mode-hook 'serika/scheme/syntax-checking)
(add-hook 'scheme-mode-hook 'serika/scheme/auto-completion)

(add-hook 'scheme-mode-hook 'serika/scheme/interface)
(add-hook 'scheme-mode-hook 'serika/scheme/prettify-symbols)
