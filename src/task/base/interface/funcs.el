;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/interface//require ()
  "Require modules."
  (require 'func-system))

(defun serika-g/interface//hide-gui ()
  "Hide menu, toolbar, scrollbar, tooltips elements."
  (when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
    (tool-bar-mode -1))
  (unless (serika-f/system/mac-p)
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
  (serika-g/interface//require)
  (serika-g/interface//hide-gui)
  (serika-g/interface//font))
