;;; package --- Summary
;;; Commentary:
;;; Code:

;; Funcs

(defun serika-f/text/setup-buffer ()
  "Configure `text-mode' buffers."
  (setq tab-width 4)
  (serika-f/linum-relative/activate)
  (serika-f/evil/activate :evil-shift-width 4
                          :evil-state 'normal))

;; Init
(defun init ()
  "Configure `text-mode'."
  (serika-c/eg/add-many-by-name 'text
                                ("settings")
                                (lambda ()
                                  (add-to-list 'auto-mode-alist '("\\.txt\\'" . text-mode)))

                                ("keymap")
                                (lambda ()
                                  (func/keymap/save text-mode-map)
                                  (func/keymap/create text-mode-map
                                                      "C-c C-z |" 'table-insert

                                                      "C-c C-n"   'table-backward-cell
                                                      "C-c <C-o>" 'table-forward-cell

                                                      "C-c N"   'table-narrow-cell
                                                      "C-c E"   'table-heighten-cell
                                                      "C-c I"   'table-shorten-cell
                                                      "C-c O"   'table-widen-cell

                                                      "C-c t r" 'table-insert-row
                                                      "C-c t c" 'table-insert-column
                                                      "C-c t R" 'table-delete-row
                                                      "C-c t C" 'table-delete-column
                                                      "C-c t _" 'table-split-cell-vertically
                                                      "C-c t |" 'table-split-cell-horizontally
                                                      "C-c t j" 'table-justify
                                                      ))

                                ("hook")
                                (lambda ()
                                  (func/hook/add 'text-mode-hook #'serika-f/text/setup-buffer))))
