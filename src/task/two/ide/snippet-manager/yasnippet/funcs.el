;;; package --- Summary
;;; Commentary:
;;; Code:

;; Public
(defun serika-f/yasnippet/activate ()
  "Activate `yasnippet' in current buffer."
  (yas-minor-mode          +1)

  (yas-recompile-all)
  (yas-reload-all))

(defun serika-f/snippet-mode/setup-buffer ()
  "Setup `snippet-mode' buffer."
  (when (eq major-mode
            'snippet-mode)
    (setq tab-width      2
          truncate-lines t)
    (serika-f/evil/activate :evil-shift-width 2
                            :evil-state       'normal)
    (serika-f/yasnippet/activate)))

;; Init
(defun init ()
  "Configure `yasnippet'."
  (serika-c/eg/add-install :type         'package
                           :name         'yasnippet
                           :package-list '(yasnippet))

  (serika-c/eg/add-many-by-name 'yasnippet
                                ("settings")
                                (lambda ()
                                  (func/keymap/create yas-minor-mode-map)
                                  (func/keymap/create yas-keymap
                                                      "A-O"   #'yas-next-field
                                                      "A-N"   #'yas-prev-field)
                                  (func/keymap/create snippet-mode-map
                                                      "C-t e" #'yas-expand)

                                  (require 'yasnippet)

                                  (setq yas-snippet-dirs
                                        (f-join serika-conf-directory
                                                "yasnippet"
                                                "snippets"))))

  (serika-c/eg/add-many-by-name 'yasnippet-snippet
                                ("settings")
                                (lambda ()
                                  (serika-f/settings/register-ft 'snippet-mode
                                                                 "\\.yasnippet\\'"))

                                ("hook")
                                (lambda ()
                                  (add-hook 'snippet-mode-hook #'serika-f/snippet-mode/setup-buffer))))
