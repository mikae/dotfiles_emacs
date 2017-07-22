;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/pomidor//require ()
  "Require modules for `pomidor'."
  (require 'pomidor))

(defun serika-g/pomidor//settings ()
  "Configure `pomidor' settings."
  (setq pomidor-sound-tick nil)
  (setq pomidor-sound-tack nil)
  (setq pomidor-sound-overwork t)
  (setq pomidor-seconds 3000))

(defun serika-g/pomidor//keymap ()
  "Configure `pomidor' keymap."
  (setq pomidor-mode-map (make-sparse-keymap))
  (define-key pomidor-mode-map "n" 'pomidor-stop)
  (define-key pomidor-mode-map "b" 'pomidor-break)
  (define-key pomidor-mode-map "r" 'pomidor-reset)
  (define-key pomidor-mode-map "q" 'quit-window)
  (define-key pomidor-mode-map "s" 'pomidor-quit))

(defun serika-g/pomidor//global-keymap ()
  "Configure `pomidor' global keymap."
  (global-set-key (kbd "<C-m> p") 'pomidor))

;; Init
(defun init ()
  "Configure `pomidor'."
  (serika-c/eg/add :parents '("require")
                   :name    'pomidor
                   :func    #'serika-g/pomidor//require)

  (serika-c/eg/add :parents '("settings")
                   :name    'pomidor
                   :func    #'serika-g/pomidor//settings)

  (serika-c/eg/add :parents '("keymap")
                   :name    'pomidor
                   :func    #'serika-g/pomidor//keymap)

  (serika-c/eg/add :parents '("global-keymap")
                   :name    'pomidor
                   :func    #'serika-g/pomidor//global-keymap))
