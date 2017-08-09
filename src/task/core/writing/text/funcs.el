;;; package --- Summary
;;; Commentary:
;;; Code:

;; Local
(defun serika-l/text//evil ()
  "Configure `evil' for `text-mode'."
  (setq evil-shift-width 4)
  (evil-local-mode +1)
  (evil-normal-state))

(defun serika-l/text//interface ()
  "Configure interface for `text-mode'."
  (serika-f/linum-relative/activate))

(defun serika-l/text//buffer-local-variables ()
  "Configure buffer-local variables for `text-mode'."
  (setq tab-width 4))

;; Init
(defun init ()
  "Configure `text-mode'."
  (serika-c/eg/add-many 'text
                        ("settings")
                        (lambda ()
                          (add-to-list 'auto-mode-alist '("\\.txt\\'" . text-mode)))

                        ("hook")
                        (lambda ()
                          (dolist (callback (list #'serika-l/text//evil
                                                  #'serika-l/text//buffer-local-variables
                                                  #'serika-l/text//interface))
                            (func/hook/add 'text-mode-hook
                                               callback)))))
