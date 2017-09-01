;;; package --- Summary
;;; Commentary:
;;; Code:

;; Public
(defun serika-f/yasnippet/activate ()
  "Activate `yasnippet' in current buffer."
  (yas-minor-mode          +1)

  (yas-recompile-all)
  (yas-reload-all))

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
                                                      "A-O" 'yas-next-field
                                                      "A-N" 'yas-prev-field)

                                  (require 'yasnippet)

                                  (setq yas-snippet-dirs
                                        (f-join serika-conf-directory
                                                "yasnippet"
                                                "snippets"))))

  (serika-c/eg/add-many-by-name 'yasnippet-snippet
                                ("settings")
                                (lambda ()
                                  (serika-f/settings/register-ft 'snippet-mode "\\.yasnippet\\'"))

                                ("keymap")
                                (lambda ()
                                  (setq snippet-mode-map (let ((map (make-sparse-keymap)))
                                                           map)))

                                ("hook")
                                (lambda ()
                                  (add-hook 'snippet-mode-hook #'serika-f/evil/activate))))
