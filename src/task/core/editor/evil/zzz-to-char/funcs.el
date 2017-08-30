;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `zzz-to-char'."
  (serika-c/eg/add-install :type 'package
                           :name 'zzz-to-char
                           :package-list '(zzz-to-char)
                           :parents '("install evil"))

  (serika-c/eg/add-many-by-name 'zzz-to-char
                                ("require evil")
                                (func/func/requirer 'zzz-to-char)

                                ("keymap evil normal")
                                (lambda ()
                                  (func/keymap/define evil-normal-state-map
                                                      "g"   #'zzz-to-char
                                                      "G"   #'zzz-up-to-char))))
