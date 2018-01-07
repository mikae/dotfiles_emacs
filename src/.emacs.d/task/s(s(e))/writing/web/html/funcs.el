;;; package --- Summary
;;; Commentary:
;;; Code:

;; Local
(defun serika-f/html/setup-buffer ()
  "Setup html buffer."
  (when (eq major-mode
            'html-mode)
    (func/var/ensure-local tab-width 2
                           truncate-lines t)
    (serika-f/evil/activate :evil-shift-width 2
                            :evil-state 'normal)
    (serika-f/emmet/activate)
    (serika-f/yasnippet/activate)
    (serika-f/company/activate :backends-set '((company-html)
                                               (company-files)))

    (serika-f/mmm-mode//activate)
    (serika-f/settings/show-trailing-whitespaces)
    (serika-f/aggressive-indent/activate)
    (serika-f/rainbow-delimiters/activate)
    (serika-f/rainbow-mode/activate)
    (serika-f/linum-relative/activate)))

;; Init
(defun init ()
  "Configure `html'."
  (serika-c/eg/add-many-by-name 'html
    ("require")
    (func/func/require 'web-beautify
                       'sgml-mode)

    ("settings")
    (serika-f/settings/register-ft 'html-mode "\\.html\\'")

    ("settings multi-compile")
    (serika-f/multi-compile/configure 'html-mode
                                      "Firefox"     "firefox     %path"
                                      "Firefox-esr" "firefox-esr %path"
                                      "Chromium"    "chromium    %path")

    ("settings mmm-mode")
    (progn
      (mmm-add-classes '((html-js-1
                          :submode js-mode
                          :front   "<script[^>]*>[ \t]*\n?"
                          :back    "[ \t]*</script>")))
      (mmm-add-mode-ext-class 'html-mode nil 'html-js-1))

    ("keymap")
    (progn
      (func/keymap/save html-mode-map)
      (func/keymap/create html-mode-map
        "TAB" #'yas-expand

        "C-c c" #'multi-compile-run
        "C-t E" #'serika-f/emmet/expand
        "C-t =" #'evil-indent
        "C-t +" #'web-beautify-html
        "C-t /" #'evilnc-comment-or-uncomment-lines))

    ("hook")
    (func/hook/add 'html-mode-hook
                   #'serika-f/html/setup-buffer)))
