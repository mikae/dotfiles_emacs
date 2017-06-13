;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'func-package)

(serika/package/repository-clear)

(serika/package/repository-add "gnu"   "http://elpa.gnu.org/packages/")
(serika/package/repository-add "melpa" "http://melpa.org/packages/")

(serika/package/initialize)
(serika/package/list-update)
