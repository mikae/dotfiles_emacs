;;; package --- serika entry point -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(debug-on-entry 'projectile-mode)
(defconst serika-emacs-min-version   "25.1.1"
  "Minimal version of Emacs.")

(if (not (version<= serika-emacs-min-version emacs-version))
    (error (concat "Your version of Emacs (%s) is too old.\n"
                   "Serika requires Emacs version %s or above.")
           emacs-version serika-emacs-min-version)
  (progn
    (load-file (concat (file-name-directory load-file-name)
                       (file-name-as-directory "core")
                       "core-load-path.el"))
    (require 'core-serika)
    (serika-c/init)))

(provide 'init)
;;; init.el ends here
