;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/html//require ()
  "Require modules for `html'."
  (require 'web-beautify)
  (require 'sgml-mode))

(defun serika-g/html//settings ()
  "Configure `html'."
  ;; `auto-mode-alist'
  (add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))

  ;; `multi-compile'
  (add-to-list 'multi-compile-alist '(html-mode . (("Firefox"     . "firefox     %path")
                                                   ("Firefox-esr" . "firefox-esr %path")
                                                   ("Chromium"    . "chromium    %path"))))
  ;; `mmm-mode'
  (mmm-add-classes '((html-js-1
                      :submode js-mode
                      :front   "<script[^>]*>[ \t]*\n?"
                      :back    "[ \t]*</script>")))
  (mmm-add-mode-ext-class 'html-mode nil 'html-js-1)
  )

(defun serika-g/html//keymap ()
  "Configure `html-mode-map'."
  (setq --serika-html-mode-map html-mode-map)
  (setq html-mode-map (let ((map (make-sparse-keymap)))
                        (define-key map (kbd "C-c c") #'multi-compile-run)
                        (define-key map (kbd "C-t e") #'yas-expand)
                        (define-key map (kbd "C-t E") #'serika-f/emmet/expand)
                        (define-key map (kbd "C-t =") #'evil-indent)
                        (define-key map (kbd "C-t +") #'web-beautify-html)
                        (define-key map (kbd "C-t /") #'evilnc-comment-or-uncomment-lines)
                        map)))

;; Local
(defun serika-l/html//evil ()
  "Configure `evil' for `html'."
  (setq evil-shift-width 2)
  (evil-local-mode +1)
  (evil-normal-state))

(defun serika-l/html//buffer-local-variables ()
  "Configure buffer-local variables for `html' files."
  (setq evil-shift-width 2)
  (setq truncate-lines t))

(defun serika-l/html//snippet-engine ()
  "Configure snippet engine for `web-mode' buffers with `html' engine."
  (serika-f/emmet/activate)
  (serika-f/yasnippet/activate))

(defun serika-l/html//auto-completion ()
  "Configure auto completion for `web-mode' buffers with `html' engine."
  (auto-complete-mode      +1)

  (setq ac-sources '(
                     ac-source-abbrev
                     ac-source-dictionary
                     ac-source-words-in-same-mode-buffers
                     )))

(defun serika-l/html//syntax-checking ()
  "Configure syntax checking for `web-mode' buffers with `html' engine."
  ;; (flycheck-mode +1)
  ())


(defun serika-l/html//interface ()
  "Configure interface for `web-mode' buffers with `html' engine."
  (setq show-trailing-whitespace +1)

  (rainbow-delimiters-mode       +1)
  (rainbow-mode                  +1)
  (serika-f/linum-relative/activate)

  (prettify-symbols-mode   +1)

  (setq prettify-symbols-alist ()))

;; Init
(defun init ()
  "Configure `html'."
  (serika-c/eg/add :parents '("require")
                   :name    'html
                   :func    #'serika-g/html//require)

  (serika-c/eg/add :parents '("settings"
                              "settings mmm-mode"
                              "settings multi-compile")
                   :name    'html
                   :func    #'serika-g/html//settings)

  (serika-c/eg/add :parents '("keymap")
                   :name    'html
                   :func    #'serika-g/html//keymap)

  (serika-c/eg/add :parents '("hook")
                   :name    'html
                   :func    (lambda ()
                              (add-hook 'html-mode-hook 'serika-l/html//evil)
                              (add-hook 'html-mode-hook 'serika-l/html//buffer-local-variables)

                              (add-hook 'html-mode-hook 'serika-l/html//snippet-engine)
                              (add-hook 'html-mode-hook 'serika-l/html//auto-completion)
                              (add-hook 'html-mode-hook 'serika-l/html//syntax-checking)
                              (add-hook 'html-mode-hook 'serika-l/mmm-mode//activate)

                              (add-hook 'html-mode-hook 'serika-l/html//interface))))
