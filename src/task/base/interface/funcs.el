;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'func-system)

(defun serika/interface//preferred-cursor-color ()
  "Return preferred cursor color."
  "white")

(defun serika/interface//update-cursor-color ()
  "Change cursor color according to some minor modes."
  (let ((color (serika/interface//preferred-cursor-color)))
    (unless (and
             (string= color serika-interface--color)
             (string= (buffer-name) serika-interface--color-buffer))
      (set-cursor-color (setq serika-interface--color color))
      (setq serika-interface--color-buffer (buffer-name)))))

(defun serika/interface//hide-gui ()
  "Hide menu, toolbar, scrollbar, tooltips elements."
  (when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
    (tool-bar-mode -1))
  (unless (serika/system-mac-p)
    (when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
      (menu-bar-mode -1)))
  (when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
    (scroll-bar-mode -1))
  ;; tooltips in echo-area
  (when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
    (tooltip-mode -1)))

(defun serika/interface//configure-cursor ()
  "Configure cursor."
  ;; (add-hook 'post-command-hook #'serika/interface//update-cursor-color)
  )

(defun serika/interface//font ()
  "Configure font."
  (set-face-attribute 'default nil
                      :font (concat serika-interface-font-default
                                    " "
                                    (number-to-string serika-interface-font-power)))
  (set-frame-font (concat serika-interface-font-default
                          " "
                          (number-to-string serika-interface-font-power)) nil t))

(defun init ()
  "Configure interface."
  (serika/interface//hide-gui)
  (serika/interface//configure-cursor)
  (serika/interface//font))
