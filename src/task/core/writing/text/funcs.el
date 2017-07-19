;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/text//settings ()
  "Configure `text-mode'."
  (add-to-list 'auto-mode-alist '("\\.txt\\'" . text-mode)))

;; Local
(defun serika-l/text//evil ()
  "Configure `evil' for `text-mode'."
  (setq evil-shift-width 4)
  (evil-local-mode +1)
  (evil-normal-state))

(defun serika-l/text//interface ()
  "Configure interface for `text-mode'."
  (linum-mode  1))

(defun serika-l/text//buffer-local-variables ()
  "Configure buffer-local variables for `text-mode'."
  (setq tab-width 4))

;; Init
(defun init ()
  "Configure `text-mode'."
  (serika-c/eg/add :parents '("settings")
		   :name    'text
		   :func    #'serika-g/text//settings)

  (serika-c/eg/add :parents '("hook")
		   :name    'text
		   :func    (lambda ()
			      (add-hook 'text-mode-hook 'serika-l/text//evil)
                              (add-hook 'text-mode-hook 'serika-l/text//buffer-local-variables)
                              (add-hook 'text-mode-hook 'serika-l/text//interface)
			      )))
