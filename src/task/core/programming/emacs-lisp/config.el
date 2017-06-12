;;; package --- Summary
;;; Commentary:
;;; Code:

;; Hooks
(add-hook 'emacs-lisp-mode-hook 'serika/emacs-lisp/evil)
(add-hook 'emacs-lisp-mode-hook 'serika/emacs-lisp/buffer-local-variables)
(add-hook 'emacs-lisp-mode-hook 'serika/emacs-lisp/buffer-local-mappings)

(add-hook 'emacs-lisp-mode-hook 'serika/emacs-lisp/snippet-engine)
(add-hook 'emacs-lisp-mode-hook 'serika/emacs-lisp/syntax-checking)
(add-hook 'emacs-lisp-mode-hook 'serika/emacs-lisp/auto-completion)
(add-hook 'emacs-lisp-mode-hook 'serika/emacs-lisp/auto-pairing)

(add-hook 'emacs-lisp-mode-hook 'serika/emacs-lisp/interface)
(add-hook 'emacs-lisp-mode-hook 'serika/emacs-lisp/prettify-symbols)
