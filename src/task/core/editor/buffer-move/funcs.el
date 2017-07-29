;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/buffer-move/require ()
  "Require modules for `buffer-move'."
  (require 'func-package)
  (require 'buffer-move))

(defun serika-g/buffer-move/settings ()
  "Configure `buffer-move' settings."
  (setq buffer-move-behavior 'swap)
  (setq buffer-move-stay-after-swap nil))


(defun serika-g/buffer-move/global-keymap ()
  "Configure `buffer-move' settings."
  (global-set-key (kbd "C-, w I") 'buf-move-up)
  (global-set-key (kbd "C-, w O") 'buf-move-right)
  (global-set-key (kbd "C-, w N") 'buf-move-down)
  (global-set-key (kbd "C-, w H") 'buf-move-left))

;; Init
(defun init ()
  "Configure `buffer-move'."
  (serika-c/eg/add-install :package-list '(buffer-move)
                           :name 'buffer-move)

  (serika-c/eg/add :parents '("require")
                   :name    'buffer-move
                   :func    #'serika-g/buffer-move/require)

  (serika-c/eg/add :parents '("settings")
                   :name    'buffer-move
                   :func    #'serika-g/buffer-move/settings)

  (serika-c/eg/add :parents '("global-keymap")
                   :name    'buffer-move
                   :func    #'serika-g/buffer-move/global-keymap)
  )
