;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'func-package)
(require 'func-path)

(serika/package/make-sure-installed 'auto-complete)

(require 'auto-complete)

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
(setq ac-completing-map (make-sparse-keymap))

(define-key ac-completing-map (kbd "A-j") 'ac-next)
(define-key ac-completing-map (kbd "A-k") 'ac-previous)
(define-key ac-completing-map (kbd "A-l") 'ac-complete)
(define-key ac-completing-map (kbd "A-h") 'ac-stop)
