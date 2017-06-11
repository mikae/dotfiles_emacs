;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'func-package)

(serika/package/make-sure-installed 'ace-window)

(require 'ace-window)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-dispatch-always t)

(global-set-key (kbd "C-, w s") 'ace-window)
