;;; package --- Summary
;;; Commentary:
;;; Code:

;; Public
(defun serika-f/yasnippet/activate ()
  "Activate `yasnippet' in current buffer."
  (yas-minor-mode          +1)

  (yas-recompile-all)
  (yas-reload-all))

;; Local
(defun serika-l/yasnippet/snippet-mode//evil ()
  "Configure `evil' for `snippet-mode'."
  (evil-local-mode +1)
  (evil-normal-state))

;; Global
(defun serika-g/yasnippet//require ()
  "Require modules for `yasnippet'."
  (require 'func-path))

(defun serika-g/yasnippet//settings ()
  "Configure `yasnippet'."
  ;; `yas-minor-mode-map'
  (setq yas-minor-mode-map (let ((map (make-sparse-keymap)))
                             map))

  ;; `yas-keymap'
  (setq yas-keymap (let ((map (make-sparse-keymap)))
                     map))

  (require 'yasnippet)

  (setq yas-snippet-dirs
        (serika-f/path/join serika-conf-directory
                          "yasnippet"
                          "snippets")))

(defun serika-g/yasnippet//snippet-mode-hook ()
  "Configure `snippet-mode'."
  ;; `snippet-mode-map'
  (setq snippet-mode-map (let ((map (make-sparse-keymap)))
                           map))

  (add-hook 'snippet-mode-hook #'serika-l/yasnippet/snippet-mode//evil))

;; Init
(defun init ()
  "Configure `yasnippet'."
  (serika-c/eg/add-install :type      'git
                           :name      'yasnippet
                           :src       "https://github.com/joaotavora/yasnippet"
                           :post-hook "rake")

  (serika-c/eg/add :parents '("require")
                   :name    'yasnippet
                   :func    #'serika-g/yasnippet//require)

  (serika-c/eg/add :parents '("settings")
                   :name    'yasnippet
                   :func    #'serika-g/yasnippet//settings)

  (serika-c/eg/add :parents '("hook")
                   :name    'yasnippet
                   :func    #'serika-g/yasnippet//snippet-mode-hook))
