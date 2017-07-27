;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/eshell//require ()
  "Require modules for `eshell'."
  (require 'func-path))

(defun serika-g/eshell//settings ()
  "Configure `eshell' variables."
  (setq eshell-visual-commands '(
                                 "screen" "top" "less" "more" "lynx"
                                 "vi" "vim" "emacsclient" "emacs"))

  ;; Set path to eshell history file
  (setq eshell-history-file-name
        (serika-f/path/join serika-tmp-directory
                          "eshell"
                          "history"))

  ;; Set path to eshell last-dir-ring file
  (setq eshell-last-dir-ring-file-name
        (serika-f/path/join serika-tmp-directory
                          "eshell"
                          "lastdir"))

  ;; Set path to eshell aliases file
  (setq eshell-aliases-file
        (serika-f/path/join serika-conf-directory
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

(defun serika-g/eshell//global-keymap ()
  "Add mapping to invoke eshell."
  (global-set-key (kbd "<C-m> s h") 'eshell))

;; Local
(defun serika-l/eshell//local-mappings ()
  "Configure `eshell-mode-map'."
  (setq-local eshell-mode-map (make-sparse-keymap))
  (use-local-map eshell-mode-map)

  (define-key eshell-mode-map (kbd "A-j")   'eshell-next-matching-input-from-input)
  (define-key eshell-mode-map (kbd "A-k")   'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map (kbd "A-s")   'eshell-next-matching-input)
  (define-key eshell-mode-map (kbd "A-C-s") 'eshell-previous-matching-input)
  (define-key eshell-mode-map (kbd "RET")   'eshell-send-input))

(defun serika-l/eshell//kill-function ()
  "Configure `serika-buffer-kill-function'."
  (set (make-local-variable 'serika-buffer-kill-function)
       (lambda ()
         (previous-buffer))))

(defun init ()
  "Configure `eshell'."
  (serika-c/eg/add :parents '("require")
                   :name    'eshell
                   :func    #'serika-g/eshell//require)

  (serika-c/eg/add :parents '("settings")
                   :name    'eshell
                   :func    #'serika-g/eshell//settings)

  (serika-c/eg/add :parents '("global-keymap")
                   :name    'eshell
                   :func    #'serika-g/eshell//global-keymap)

  (serika-c/eg/add :parents '("hook")
                   :name    'eshell
                   :func    (lambda ()
                              (add-hook 'eshell-mode-hook 'serika-l/eshell//local-mappings)
                              (add-hook 'eshell-mode-hook 'serika-l/eshell//kill-function))))
