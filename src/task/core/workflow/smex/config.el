;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'func-package)

(serika/package/make-sure-installed 'smex)

(message "smex")

(require 'smex)

(setq smex-save-file (serika/path/join serika-tmp-directory "smex-items"))

(global-set-key (kbd "A-x") 'smex)
