;;; package --- Summary
;;; Commentary:
;;; Code:

;; Variables
(defvar serika-eshell-intro-messages '("Serika hopes that this session of eshell will be helpful for you :3\n"
                                       "Serika greets you in eshell :3\n")
  "List of messages, that can be displayed on Emacs shell banner.")


;; Configuration
(serika/eshell//variables)
(serika/eshell//global-keymap)

;; Hooks
(add-hook 'eshell-mode-hook 'serika/eshell//local-mappings)
(add-hook 'eshell-mode-hook 'serika/eshell//kill-function)

(provide 'serika-emacs-mode-configuration-eshell)
;;; serika-emacs-mode-configuration-eshell.el ends here
