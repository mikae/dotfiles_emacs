;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `ace-jump-mode'."
  (serika-c/eg/add-install :type 'package
                           :name 'ace-jump-mode
                           :package-list '(ace-jump-mode)
                           :parents '("install evil"))

  (serika-c/eg/add-many 'ace-jump-mode
                        ("require")
                        (func/func/requirer ace-jump-mode)

                        ("global-map evil")
                        (lambda ()
                          (func/keymap/define-global "A-c"  'ace-jump-word-mode
                                                     "A-C"  'ace-jump-char-mode
                                                     "A-v"  'ace-jump-line-mode)))
  )