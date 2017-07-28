;;; package --- Summary
;;; Commentary:
;;; Code:

;; Functions
(defun serika-f/neotree/create ()
  "Create `neotree' buffer."
  (neotree-show))

(defun serika-f/neotree/remove ()
  "Require modules for `neotree'."
  (neotree-hide))

(defun serika-f/neotree/exists-p ()
  "Require modules for `neotree'."
  (serika-f/buffer/exists-p 'neotree-mode))

(defun serika-f/neotree/not-exists-p ()
  "Require modules for `neotree'."
  (serika-f/buffer/not-exists-p 'neotree-mode))

;; Global
(defun serika-gc/neotree//require ()
  "Require modules for `neotree'."
  (require 'neotree))

(defun serika-gc/neotree//keymap ()
  "Configure keymap for `neotree'."
  (setq --serika-neotree-mode-map
        neotree-mode-map)
  (setq neotree-mode-map
        (let ((map (make-sparse-keymap)))
          (serika-f/keymap/bind-digits map 'digit-argument)

          (define-key map (kbd "A-j") 'neotree-next-line)
          (define-key map (kbd "A-k") 'neotree-previous-line)
          (define-key map (kbd "A-l") 'neotree-change-root)

          (define-key map (kbd "d n") 'neotree-create-node)
          (define-key map (kbd "d d") 'neotree-delete-node)
          (define-key map (kbd "d c") 'neotree-copy-node)
          (define-key map (kbd "d r") 'neotree-copy-node)

          (define-key map (kbd "t h") 'neotree-hidden-file-toggle)

          (define-key map (kbd "q")   'neotree-hide)
          (define-key map (kbd "RET") (lookup-key --serika-neotree-mode-map (kbd "RET")))
          map)))

;; Init
(defun init ()
  "Configure `neotree'."
  (serika-c/eg/add-install :package-list '(neotree)
                           :name         'neotree)

  (serika-c/eg/add :parents '("require")
                   :name    'neotree
                   :func    #'serika-gc/neotree//require)
  (serika-c/eg/add :parents '("keymap")
                   :name    'neotree
                   :func    #'serika-gc/neotree//keymap)
)
