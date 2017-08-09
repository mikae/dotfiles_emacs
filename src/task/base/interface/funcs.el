;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/interface//require ()
  "Require modules."
  ())

(defun serika-g/interface//hide-gui ()
  "Hide menu, toolbar, scrollbar, tooltips elements."
  (when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
    (tool-bar-mode -1))
  (unless (func/system/mac-p)
    (when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
      (menu-bar-mode -1)))
  (when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
    (scroll-bar-mode -1))
  ;; tooltips in echo-area
  (when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
    (tooltip-mode -1)))

(defun serika-g/interface//font ()
  "Configure font."
  (set-face-attribute 'default nil
                      :font (concat serika-interface-font-default
                                    " "
                                    (number-to-string serika-interface-font-power)))
  (set-frame-font (concat serika-interface-font-default
                          " "
                          (number-to-string serika-interface-font-power)) nil t))

;; Init
(defun init ()
  "Configure interface."
  (serika-c/eg/add :parents '("base require")
                   :name    'interface
                   :func    #'serika-g/interface//require)
  (serika-c/eg/add :parents '("base interface")
                   :name    'hide-gui
                   :func    #'serika-g/interface//hide-gui)
  (serika-c/eg/add :parents '("base interface")
                   :name    'set-font
                   :func    #'serika-g/interface//font))
