;;; package --- Summary
;;; Commentary:
;;; Code:

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
  (serika-c/eg/add-many 'html
                        ("require")
                        (lambda ()
                          (require 'web-beautify)
                          (require 'sgml-mode))

                        ("settings")
                        (lambda ()
                          (add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode)))

                        ("settings multi-compile")
                        (lambda ()
                          (add-to-list 'multi-compile-alist '(html-mode . (("Firefox"     . "firefox     %path")
                                                                           ("Firefox-esr" . "firefox-esr %path")
                                                                           ("Chromium"    . "chromium    %path")))))

                        ("settings mmm-mode")
                        (lambda ()
                          (mmm-add-classes '((html-js-1
                                              :submode js-mode
                                              :front   "<script[^>]*>[ \t]*\n?"
                                              :back    "[ \t]*</script>")))
                          (mmm-add-mode-ext-class 'html-mode nil 'html-js-1))

                        ("keymap")
                        (lambda ()
                          (setq --serika-html-mode-map html-mode-map)
                          (serika-f/keymap/create html-mode-map
                                                  "C-c c" #'multi-compile-run
                                                  "C-t e" #'yas-expand
                                                  "C-t E" #'serika-f/emmet/expand
                                                  "C-t =" #'evil-indent
                                                  "C-t +" #'web-beautify-html
                                                  "C-t /" #'evilnc-comment-or-uncomment-lines))

                        ("hook")
                        (lambda ()
                          (dolist (callback (list #'serika-l/html//evil
                                                  #'serika-l/html//buffer-local-variables

                                                  #'serika-l/html//snippet-engine
                                                  #'serika-l/html//auto-completion
                                                  #'serika-l/html//syntax-checking
                                                  #'serika-f/eldoc/activate
                                                  #'serika-l/mmm-mode//activate

                                                  #'serika-l/html//interface))
                            (serika-f/hook/add 'html-mode-hook
                                               callback)))))
