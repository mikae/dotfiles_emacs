;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'func-path)

;; Public
(defun serika/yasnippet/activate ()
  "Activate `yasnippet' in current buffer."
  (yas-minor-mode          +1)

  (yas-recompile-all)
  (yas-reload-all))

;; Local
(defun serika/yasnippet/snippet-mode//evil ()
  "Configure `evil' for `snippet-mode'."
  (evil-local-mode +1)
  (evil-normal-state))

;; Global
(defun serika/yasnippet//yasnippet ()
  "Configure `yasnippet'."
  (setq yas-minor-mode-map (make-sparse-keymap))

  (require 'yasnippet)

  (setq yas-snippet-dirs
        (serika/path/join serika-conf-directory
                          "yasnippet"
                          "snippets"))

  (define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)

  (setq snippet-mode-map (make-sparse-keymap)))

(defun serika/yasnippet//snippet-mode ()
  "Configure `snippet-mode'."
  (add-hook 'snippet-mode-hook #'serika/yasnippet/snippet-mode//evil))

(defun init ()
  "Configure `yasnippet'."
  (serika/yasnippet//yasnippet)
  (serika/yasnippet//snippet-mode))
