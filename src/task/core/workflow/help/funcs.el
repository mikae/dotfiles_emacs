;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/help//keymap ()
  "Configure `help-mode-map'."
  (setq help-mode-map (make-sparse-keymap)))

;; Local
(defun serika-l/help//evil ()
  "Configure `evil' for `help-mode'."
  (evil-local-mode +1)
  (evil-motion-state))

;; Init
(defun init ()
  "Configure `help-mode'."
  (serika-g/help//keymap)

  (serika-c/eg/add :parents '("keymap")
                   :name    'help
                   :func    #'serika-g/help//keymap)

  (serika-c/eg/add :parents '("hook")
                   :name    'help
                   :func    (lambda ()
                              (add-hook 'help-mode-hook #'serika-l/help//evil))))
