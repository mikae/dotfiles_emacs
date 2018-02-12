;;; package --- Summary
;;; Commentary:
;;; Code:

;; Vars
(defvar serika-interface-font-default "DejaVu Sans Mono"
  "This variable contains font name that will be used by default.")

(defvar serika-interface-font-power 12
  "This variable contains the power of the fonts.")

;; Init
(defun init ()
  "Configure interface."
  (serika-c/eg/add-many-by-parents ("base interface")
    'hide-gui-parts
    (progn
      (when (and (fboundp 'tool-bar-mode)
                 (not (eq tool-bar-mode -1)))
        (tool-bar-mode -1))
      (unless (func/system/mac-p)
        (when (and (fboundp 'menu-bar-mode)
                   (not (eq menu-bar-mode -1)))
          (menu-bar-mode -1)))
      (when (and (fboundp 'scroll-bar-mode)
                 (not (eq scroll-bar-mode -1)))
        (scroll-bar-mode -1))
      ;; tooltips in echo-area
      (when (and (fboundp 'tooltip-mode)
                 (not (eq tooltip-mode -1)))
        (tooltip-mode -1)))

    'set-variables
    (progn
      (setq-default bidi-display-reordering        nil
                    cursor-in-non-selected-windows nil)

      (setq blink-matching-paren           nil ;; candidate to move
            show-help-function             nil
            indicate-empty-lines           nil
            highlight-nonselected-windows  nil
            indicate-buffer-boundaries     nil

            resize-mini-windows            'grow-only
            max-mini-window-height         0.3
            mode-line-default-help-echo    nil ;; candidate to move

            use-dialog-box                 nil
            visible-cursor                 nil
            x-stretch-cursor               nil
            ring-bell-function             #'ignore
            visible-bell                   nil)

      (blink-cursor-mode -1))

    'set-font
    (progn
      (set-face-attribute 'default nil
                          :font (concat serika-interface-font-default
                                        " "
                                        (number-to-string serika-interface-font-power)))
      (set-frame-font (concat serika-interface-font-default
                              " "
                              (number-to-string serika-interface-font-power))
                      nil t))))
