;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/emacs-lisp/p ()
  "Return t if major mode is `emacs-lisp-mode'."
  (eq 'emacs-lisp-mode
      major-mode))

;; Global
(defun serika-g/emacs-lisp//require ()
  "Require modules for `emacs-lisp'."
  (require 'func-keymap)
  (require 'func-buffer)
  (require 'flycheck-cask))

(defun serika-g/emacs-lisp//settings ()
  "Configure `emacs-lisp'."
  (add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode)))

(defun serika-g/emacs-lisp//keymap ()
  "Configure keymap for `emacs-lisp' mode."
  (setq --serika-emacs-lisp-mode-map emacs-lisp-mode-map)
  (setq emacs-lisp-mode-map (serika-f/keymap/create "C-t =" #'evil-indent
                                                    "C-t /" #'evilnc-comment-or-uncomment-lines
                                                    "C-t e" #'yas-expand)))

;; Local
(defun serika-l/emacs-lisp//buffer-local-variables ()
  "Configure snippet engine for `emacs-lisp' mode."
  (setq tab-width 2)
  (setq truncate-lines t))

(defun serika-l/emacs-lisp//evil ()
  "Configure `evil' for `emacs-lisp-mode'."
  (setq evil-shift-width 2)
  (evil-local-mode +1)
  (evil-normal-state))

(defun serika-l/emacs-lisp//snippet-engine ()
  "Configure snippet engine for `emacs-lisp' mode."
  (serika-f/yasnippet/activate))

(defun serika-l/emacs-lisp//syntax-checking ()
  "Configure syntax checking for `emacs-lisp' mode."
  (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (flycheck-mode           +1)

  (flycheck-cask-setup))

(defun serika-l/emacs-lisp//auto-completion ()
  "Configure auto completion for `emacs-lisp' mode."
  (setq-local company-backends '(company-elisp))

  (company-mode +1))

(defun serika-l/emacs-lisp//auto-pairing ()
  "Configure auto completion for `emacs-lisp' mode."
  (electric-pair-mode +1))

(defun serika-l/emacs-lisp//interface ()
  "Configure interface for `emacs-lisp' mode."
  (setq show-trailing-whitespace +1)

  (rainbow-delimiters-mode       +1)
  (serika-f/linum-relative/activate))

(defun serika-l/emacs-lisp//prettify-symbols ()
  "Configure prettify symbols for `emacs-lisp' mode."
  (prettify-symbols-mode +1)

  (setq prettify-symbols-alist ())

  (push '("lambda" . ?λ) prettify-symbols-alist)
  (push '(">="     . ?≤) prettify-symbols-alist)
  (push '("<="     . ?≥) prettify-symbols-alist))

(defun init ()
  "Configure `emacs-lisp-mode'."
  (serika-c/eg/add-install :package-list '(flycheck-cask)
                           :name         'emacs-lisp)

  (serika-c/eg/add :parents '("require")
                   :name    'emacs-lisp
                   :func    #'serika-g/emacs-lisp//require)

  (serika-c/eg/add :parents '("settings")
                   :name    'emacs-lisp
                   :func    #'serika-g/emacs-lisp//settings)

  (serika-c/eg/add :parents '("keymap")
                   :name    'emacs-lisp
                   :func    #'serika-g/emacs-lisp//keymap)

  (serika-c/eg/add :parents '("hook")
                   :name    'emacs-lisp
                   :func    (lambda ()
                              (dolist (callback (list
                                                 #'serika-l/emacs-lisp//evil
                                                 #'serika-l/emacs-lisp//buffer-local-variables

                                                 #'serika-l/emacs-lisp//snippet-engine
                                                 #'serika-l/emacs-lisp//syntax-checking
                                                 #'serika-l/emacs-lisp//auto-completion
                                                 #'serika-l/emacs-lisp//auto-pairing
                                                 #'serika-f/eldoc/activate
                                                 #'serika-f/flycheck/activate

                                                 #'serika-l/emacs-lisp//interface
                                                 #'serika-l/emacs-lisp//prettify-symbols
                                                 (serika-f/purpose/use-layout "emacs-lisp.purpose-layout")

                                                 #'serika-f/flycheck/create))
                                ;; `lisp-interaction-mode' is inherited from `emacs-lisp-mode',
                                ;; so, predicate was added
                                (serika-f/hook/add-predicated 'emacs-lisp-mode-hook
                                                              callback
                                                              #'serika-f/emacs-lisp/p))
                              (serika-f/hook/add-predicated 'emacs-lisp-mode-hook
                                                            #'serika-f/neotree/create
                                                            (serika-f/func/create-ander #'serika-f/neotree/not-exists-p
                                                                                        #'serika-f/emacs-lisp/p))
                              (serika-f/hook/add-predicated 'emacs-lisp-mode-hook
                                                            (serika-f/func/bind 'serika-f/buffer/focus-to
                                                                                'emacs-lisp-mode)
                                                            (lambda ()
                                                              (and (not (eq major-mode
                                                                            'emacs-lisp-mode))
                                                                   (not (eq major-mode
                                                                            'lisp-interaction-mode)))))
                              )))
