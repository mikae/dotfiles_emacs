;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'func-path)

;; Private
(defun serika/eshell//local-mappings ()
  "Configure `eshell-mode-map'."
  (setq-local eshell-mode-map (make-sparse-keymap))
  (use-local-map eshell-mode-map)

  (define-key eshell-mode-map (kbd "A-j")   'eshell-next-matching-input-from-input)
  (define-key eshell-mode-map (kbd "A-k")   'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map (kbd "A-s")   'eshell-next-matching-input)
  (define-key eshell-mode-map (kbd "A-C-s") 'eshell-previous-matching-input)
  (define-key eshell-mode-map (kbd "RET")   'eshell-send-input))

(defun serika/eshell//kill-function ()
  "Configure `serika-buffer-kill-function'."
  (make-local-variable 'serika-buffer-kill-function)
  (setq serika-buffer-kill-function (lambda ()
                                      (previous-buffer))))

;; Configuration unctions
(defun serika/eshell//variables ()
  "Configure `eshell' variables."
  (setq eshell-visual-commands '(
                                 "screen" "top" "less" "more" "lynx"
                                 "vi" "vim" "emacsclient" "emacs"))

  ;; Set path to eshell history file
  (setq eshell-history-file-name
        (serika/path/join serika-tmp-directory
                          "eshell"
                          "history"))

  ;; Set path to eshell last-dir-ring file
  (setq eshell-last-dir-ring-file-name
        (serika/path/join serika-tmp-directory
                          "eshell"
                          "lastdir"))

  ;; Set path to eshell aliases file
  (setq eshell-aliases-file
        (serika/path/join serika-conf-directory
                          "eshell"
                          "aliases"))

  ;; Set banner message to random from `serika-eshell-intro-messages'
  (setq eshell-banner-message
        '(car (nthcdr
               (random (length serika-eshell-intro-messages))
               serika-eshell-intro-messages)))

  ;; Set eshell prompt
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

(defun serika/eshell//global-keymap ()
  "Add mapping to invoke eshell."
  (global-set-key (kbd "<C-m> s h") 'eshell))

(defun init ()
  "Configure `eshell'."
  (serika/eshell//variables)
  (serika/eshell//global-keymap)

  (add-hook 'eshell-mode-hook 'serika/eshell//local-mappings)
  (add-hook 'eshell-mode-hook 'serika/eshell//kill-function))
