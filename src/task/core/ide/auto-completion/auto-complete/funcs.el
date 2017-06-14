;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'func-package)
(require 'func-path)

(require 'auto-complete)

(defun serika/auto-complete//settings ()
  "Configure `auto-complete' settings."
  (setq ac-delay 0.2)
  (setq ac-quick-help-delay 2)
  (setq ac-auto-start 2)
  (setq ac-use-quick-help t)
  (setq ac-menu-height 20)
  (setq ac-ignore-case t)
  (setq ac-use-fuzzy t)

  (setq ac-comphist-file (serika/path/join serika-tmp-directory
                                           "ac-comphist.dat"))

  (setq ac-mode-map       (make-sparse-keymap))
  (setq ac-completing-map (make-sparse-keymap)))

(defun serika/auto-complete//keymap ()
  "Configure `auto-complete' keymaps."
  (define-key ac-completing-map (kbd "A-j") 'ac-next)
  (define-key ac-completing-map (kbd "A-k") 'ac-previous)
  (define-key ac-completing-map (kbd "A-l") 'ac-complete)
  (define-key ac-completing-map (kbd "A-h") 'ac-stop))

(defun init ()
  "Configure `auto-complete'."
  (serika/auto-complete//settings)
  (serika/auto-complete//keymap))
