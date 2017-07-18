;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/sh//keymap ()
  "Configure `sh-mode-map'."
  (setq sh-mode-map (make-sparse-keymap)))

(defun serika-g/sh//settings ()
  "Configure `sh-mode'."
  nil)

(defun serika-g/sh//auto-mode-alist ()
  "Configure `auto-mode-alist'."
  (add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode)))

;; Local
(defun serika-l/sh//evil ()
  "Configure evil for `sh-mode'."
  (evil-local-mode +1)
  (evil-normal-state))

(defun init ()
  "Configure `sh-mode'."
  (serika-c/eg/add :parents '("keymap")
                   :name    'sh
                   :func    #'serika-g/sh//keymap)

  (serika-c/eg/add :parents '("settings")
                   :name    'sh
                   :func    #'serika-g/sh//settings)

  (serika-c/eg/add :parents '("settings sh")
                   :name    'auto-mode-alist
                   :func    #'serika-g/sh//auto-mode-alist)

  (serika-c/eg/add :parents '("hook")
                   :name    'sh
		   :func    (lambda ()
			      (add-hook 'sh-mode-hook 'serika-l/sh//evil))))
