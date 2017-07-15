;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/auto-complete//require ()
  "Require modules for `auto-complete'."
  (require 'func-package)
  (require 'func-path)
  (require 'auto-complete))

(defun serika-g/auto-complete//settings ()
  "Configure `auto-complete' settings."
  (setq ac-delay 0.2)
  (setq ac-quick-help-delay 2)
  (setq ac-auto-start 2)
  (setq ac-use-quick-help t)
  (setq ac-menu-height 20)
  (setq ac-ignore-case t)
  (setq ac-use-fuzzy t)

  (setq ac-comphist-file (serika-f/path/join serika-tmp-directory
                                           "ac-comphist.dat"))

  (setq ac-mode-map       (make-sparse-keymap))
  (setq ac-completing-map (make-sparse-keymap)))

(defun serika-g/auto-complete//keymap ()
  "Configure `auto-complete' keymaps."
  (define-key ac-completing-map (kbd "A-j") 'ac-next)
  (define-key ac-completing-map (kbd "A-k") 'ac-previous)
  (define-key ac-completing-map (kbd "A-l") 'ac-complete)
  (define-key ac-completing-map (kbd "A-h") 'ac-stop))

(defun init ()
  "Configure `auto-complete'."
  (serika-g/auto-complete//require)
  (serika-g/auto-complete//settings)
  (serika-g/auto-complete//keymap))
