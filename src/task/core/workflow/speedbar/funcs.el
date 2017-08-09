;;; package --- Summary
;;; Commentary:
;;; Code:

;; Funcs

;; Init
(defun init ()
  "Configure `sr-speedbar'."
  (serika-c/eg/add-many 'sr-speedbar
                        ("settings w-purpose")
                        (lambda ()
                          (serika-f/purpose/add 'speedbar-mode
                                                'file-manager)))
  )
