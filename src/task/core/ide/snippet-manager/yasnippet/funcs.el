;;; package --- Summary
;;; Commentary:
;;; Code:

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
(defun serika/yasnippet//require ()
  "Require modules for `yasnippet'."
  (require 'func-path))

(defun serika/yasnippet//yasnippet ()
  "Configure `yasnippet'."
  ;; `yas-minor-mode-map'
  (setq yas-minor-mode-map (let ((map (make-sparse-keymap)))
                             map))

  ;; `yas-keymap'
  (setq yas-keymap (let ((map (make-sparse-keymap)))
                     map))

  (require 'yasnippet)

  (setq yas-snippet-dirs
        (serika/path/join serika-conf-directory
                          "yasnippet"
                          "snippets")))

(defun serika/yasnippet//snippet-mode ()
  "Configure `snippet-mode'."
  ;; `snippet-mode-map'
  (setq snippet-mode-map (let ((map (make-sparse-keymap)))
                           map))

  (add-hook 'snippet-mode-hook #'serika/yasnippet/snippet-mode//evil))

;; Init
(defun init ()
  "Configure `yasnippet'."
  (serika/yasnippet//require)
  (serika/yasnippet//yasnippet)
  (serika/yasnippet//snippet-mode))
