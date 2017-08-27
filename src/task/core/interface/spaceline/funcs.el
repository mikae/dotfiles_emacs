;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `spaceline'."
  (serika-c/eg/add-install :type 'package
                           :name 'spaceline
                           :package-list '(spaceline))

  (serika-c/eg/add-many-by-name 'spaceline
                                ("require")
                                (func/func/requirer spaceline)

                                ("settings")
                                (lambda ()
                                  (spaceline-compile 'main
                                                     '()
                                                     '())
                                  (setq-default mode-line-format
                                                '("%e" (:eval (spaceline-ml-main)))))))
