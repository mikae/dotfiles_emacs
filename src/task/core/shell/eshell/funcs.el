;;; package --- Summary
;;; Commentary:
;;; Code:

;; Vars
(defvar serika-eshell-intro-messages
  '("Serika hopes that this session of eshell will be helpful for you :3\n"
    "Serika greets you in eshell :3\n")
  "List of messages, that can be displayed on Emacs shell banner :3.")

;; Local
(defun serika-l/eshell//local-mappings ()
  "Configure `eshell-mode-map'."
  (setq-local eshell-mode-map (make-sparse-keymap))
  (use-local-map eshell-mode-map)

  (func/keymap/define eshell-mode-map
                      "A-e"     #'eshell-next-matching-input-from-input
                      "A-i"     #'eshell-previous-matching-input-from-input
                      "A-E"     #'eshell-next-matching-input
                      "A-I"     #'eshell-previous-matching-input
                      "RET"     #'eshell-send-input
                      "C-x C-c" #'previous-buffer))

(defun init ()
  "Configure `eshell'."
  (serika-c/eg/add-many-by-name 'eshell
                                ("settings")
                                (lambda ()
                                  (setq eshell-visual-commands '(
                                                                 "screen" "top" "less" "more" "lynx"
                                                                 "vi" "vim" "emacsclient" "emacs"))

                                  (setq eshell-history-file-name
                                        (f-join serika-tmp-directory
                                                "eshell"
                                                "history"))

                                  (setq eshell-last-dir-ring-file-name
                                        (f-join serika-tmp-directory
                                                "eshell"
                                                "lastdir"))

                                  (setq eshell-aliases-file
                                        (f-join serika-conf-directory
                                                "eshell"
                                                "aliases"))

                                  (setq eshell-banner-message
                                        '(car (nthcdr
                                               (random (length serika-eshell-intro-messages))
                                               serika-eshell-intro-messages)))

                                  (setq eshell-prompt-function (lambda ()
                                                                 (concat "{"
                                                                         (current-time-string)
                                                                         "}"
                                                                         system-name
                                                                         ":"
                                                                         (eshell/pwd)
                                                                         " "
                                                                         (eshell/whoami)
                                                                         " $ ")))

                                  (setq eshell-prompt-regexp "^[^$#\n]* [#$] "))

                                ("global-keymap")
                                (lambda ()
                                  (func/keymap/define-global  "<C-m> s h" 'eshell))

                                ("hook")
                                (lambda ()
                                  (add-hook 'eshell-mode-hook 'serika-l/eshell//local-mappings))))
