;;; package --- Summary
;;; Commentary:
;;; Code:

;; Init
(defun init ()
  "Configure `moe-theme'."
  (serika-c/eg/add-install :type 'package
                           :package-list '(moe-theme)
                           :name         'moe-theme
                           :parents      '("base interface install"))

  (serika-c/eg/add :parents '("base interface install moe-theme")
                   :name    'require
                   :func    (lambda ()
                              (require 'moe-theme)))

  (serika-c/eg/add :parents '("base interface install moe-theme")
                   :name    'configure
                   :func    (lambda ()
                              (setq moe-theme-highlight-buffer-id t)

                              (moe-theme-random-color)
                              (moe-dark))))
