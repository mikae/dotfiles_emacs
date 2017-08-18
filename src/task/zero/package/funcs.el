;;; package --- Summary
;;; Commentary:
;;; Code:

;; Init
(defun init ()
  (serika-c/eg/add-many-by-name 'package-manager
                        ("zero require")
                        (lambda ()
                          ())

                        ("zero configure")
                        (lambda ()
                          (serika-f/package/configure)

                          (serika-f/package/repository-add "melpa-stable"
                                                           "https://stable.melpa.org/packages/"
                                                           10)
                          (serika-f/package/repository-add "gnu"
                                                           "http://elpa.gnu.org/packages/"
                                                           5)
                          (serika-f/package/repository-add "melpa"
                                                           "http://melpa.org/packages/"
                                                           0)

                          (serika-f/package/initialize)
                          (serika-f/package/list-update))))
