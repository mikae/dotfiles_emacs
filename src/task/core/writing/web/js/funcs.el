;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/js2//require ()
  "Require modules for `js'."
  (require 'js2-mode)
  (require 'skewer-mode)
  (require 'ac-js2))

(defun serika-g/js2//settings ()
  "Configure `js2'."
  ;; `auto-mode-alist'
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

  ;; `multi-compile'
  (add-to-list 'multi-compile-alist '(js2-mode . (("Execute" . "node %path")))))

(defun serika-g/js2//keymap ()
  "Configure `js2-mode-map'."
  (setq --serika-js2-mode-map js2-mode-map)
  (setq js2-mode-map (let ((map (make-sparse-keymap)))
                       (define-key map (kbd "C-c e e") 'skewer-eval-last-expression)
                       (define-key map (kbd "C-c e d") 'skewer-eval-defun)
                       (define-key map (kbd "C-c e a") 'skewer-load-buffer)
                       (define-key map (kbd "C-c e r") 'run-skewer)

                       (define-key map (kbd "C-c c c") 'multi-compile-run)
                       (define-key map (kbd "C-c c d") (lambda ()
							 (interactive)
                                                         (serika-f/buffer/kill-by-major-mode 'compilation-mode)))

                       (define-key map (kbd "C-t =")   'evil-indent)
                       (define-key map (kbd "C-t +")   'web-beautify-js)
                       (define-key map (kbd "C-t /")   'evilnc-comment-or-uncomment-lines)
                       (define-key map (kbd "C-t e")   'yas-expand)
                       (define-key map (kbd "C-t E")   'serika-f/emmet/expand)
                       map)))

;; Local
(defun serika-l/js2//evil ()
  "Configure `evil' for `js'."
  (setq evil-shift-width 2)
  (evil-local-mode +1)
  (evil-normal-state))

(defun serika-l/js2//buffer-local-variables ()
  "Configure snippet engine for `js' mode."
  (setq tab-width 2)
  (setq js-indent-level 2)
  (setq truncate-lines t))

(defun serika-l/js2//syntax-checking ()
  "Configure syntax checking for `js' mode."
  (flycheck-mode           +1))

(defun serika-l/js2//snippet-engine ()
  "Configure snippet engine for `js' mode."
  (serika-f/yasnippet/activate)
  (serika-f/emmet/activate))

(defun serika-l/js2//auto-completion ()
  "Configure auto completion for `js' mode."
  (setq ac-sources '(
                      ac-source-filename
                      ac-source-dictionary
                      ac-source-files-in-current-dir))
  (ac-js2-mode))

(defun serika-l/js2//auto-pairing ()
  "Configure auto completion for `js' mode."
  (electric-pair-mode +1))

;; Interface
(defun serika-l/js2//interface ()
  "Configure interface for `js' mode."
  (setq show-trailing-whitespace +1)

  (rainbow-delimiters-mode       +1)
  (serika-f/linum-relative/activate))

(defun serika-l/js2//prettify-symbols ()
  "Configure prettify symbols for `js' mode."
  (prettify-symbols-mode +1)

  (setq prettify-symbols-alist ()))

(defun init ()
  "Configure Emacs for `js'-programming."
  (serika-c/eg/add-install :package-list '(js2-mode
                                           ac-js2)
                           :name         'js2)

  (serika-c/eg/add :parents '("require")
                   :name    'js2
                   :func    #'serika-g/js2//require)

  (serika-c/eg/add :parents '("settings"
                              "settings w-purpose"
                              "settings multi-compile")
                   :name    'js2
                   :func    #'serika-g/js2//settings)

  (serika-c/eg/add :parents '("keymap")
                   :name    'js2
                   :func    #'serika-g/js2//keymap)

  (serika-c/eg/add :parents '("hook")
                   :name    'js2
                   :func    (lambda ()
                              (add-hook 'js2-mode-hook #'serika-l/js2//evil)
                              (add-hook 'js2-mode-hook #'serika-l/js2//buffer-local-variables)

                              (add-hook 'js2-mode-hook #'serika-l/js2//syntax-checking)
                              (add-hook 'js2-mode-hook #'serika-l/js2//snippet-engine)
                              (add-hook 'js2-mode-hook #'serika-f/skewer/activate)
                              (add-hook 'js2-mode-hook #'serika-f/eldoc/activate)
                              ;; bug:
                              ;; `https://github.com/ScottyB/ac-js2/issues/18'
                              ;; (add-hook 'js2-mode-hook #'serika-l/js2//auto-completion)
                              (add-hook 'js2-mode-hook #'serika-l/js2//auto-pairing)

                              (add-hook 'js2-mode-hook #'serika-l/js2//interface)
                              (add-hook 'js2-mode-hook #'serika-l/js2//prettify-symbols)
                              (add-hook 'js2-mode-hook (serika-f/purpose/use-layout "js.purpose-layout"))

                              (serika-f/add-hook-predicated 'js2-mode-hook
                                                            #'serika-f/flycheck/create
                                                            #'serika-f/flycheck/not-exists-p)
                              (serika-f/add-hook-predicated 'js2-mode-hook
                                                            #'serika-f/neotree/create
                                                            #'serika-f/neotree/not-exists-p))))
