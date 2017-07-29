;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/auto-complete//require ()
  "Require modules for `auto-complete'."
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
                                             "ac-comphist.dat")))

(defun serika-g/auto-complete//keymap ()
  "Configure `auto-complete' keymaps."
  (setq ac-mode-map       (make-sparse-keymap))
  (setq ac-completing-map (make-sparse-keymap))

  (define-key ac-completing-map (kbd "A-n") 'ac-stop)
  (define-key ac-completing-map (kbd "A-e") 'ac-next)
  (define-key ac-completing-map (kbd "A-o") 'ac-complete)
  (define-key ac-completing-map (kbd "A-i") 'ac-previous))

(defun init ()
  "Configure `auto-complete'."
  (serika-c/eg/add-install :package-list '(auto-complete)
                           :name         'auto-complete)

  (serika-c/eg/add :parents '("require")
                   :name    'auto-complete
                   :func    #'serika-g/auto-complete//require)

  (serika-c/eg/add :parents '("settings")
                   :name    'auto-complete
                   :func    #'serika-g/auto-complete//settings)

  (serika-c/eg/add :parents '("keymap")
                   :name    'auto-complete
                   :func    #'serika-g/auto-complete//keymap))
