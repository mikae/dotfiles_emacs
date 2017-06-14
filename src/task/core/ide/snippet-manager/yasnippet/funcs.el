;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'func-path)

(defun serika/yasnippet/snippet-mode//evil ()
  "Configure `evil' for `snippet-mode'."
  (evil-local-mode +1)
  (evil-normal-state))

(defun init ()
  "Configure `yasnippet'."
  (setq yas-minor-mode-map (make-sparse-keymap))

  (require 'yasnippet)

  (setq yas-snippet-dirs
        (serika/path/join serika-conf-directory
                          "yasnippet"
                          "snippets"))

  (define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)

  (setq snippet-mode-map (make-sparse-keymap))

  (add-hook 'snippet-mode-hook #'serika/yasnippet/snippet-mode//evil))
