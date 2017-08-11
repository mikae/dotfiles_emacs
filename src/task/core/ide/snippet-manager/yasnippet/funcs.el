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
  (serika-c/eg/add-install :type      'git
                           :name      'yasnippet
                           :src       "https://github.com/joaotavora/yasnippet"
                           :post-hook "rake")

  (serika-c/eg/add-many 'yasnippet
                        ("settings")
                        (lambda ()
                          (func/keymap/create yas-minor-mode-map)
                          (func/keymap/create yas-keymap
                                                  "A-O" 'yas-next-field
                                                  "A-N" 'yas-prev-field)

                          (require 'yasnippet)

                          (setq yas-snippet-dirs
                                (func/path/join serika-conf-directory
                                                    "yasnippet"
                                                    "snippets"))))

  (serika-c/eg/add-many 'yasnippet-snippet
                        ("settings")
                        (lambda ()
                          (add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode)))

                        ("keymap")
                        (lambda ()
                          (setq snippet-mode-map (let ((map (make-sparse-keymap)))
                                                   map)))

                        ("hook")
                        (lambda ()
                          (add-hook 'snippet-mode-hook #'serika-f/evil/activate))))
