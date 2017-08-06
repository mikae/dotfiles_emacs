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

(defun serika-f/neotree/p ()
  "Return t if current buffer is neotree buffer."
  (eq major-mode
      'neotree-mode))

(defun serika-f/neotree/parent-directory ()
  "Go to parent directory."
  (interactive)
  (when (serika-f/neotree/p)
    (goto-char (point-min))
    (neotree-change-root)))

;; Global
(defun serika-gc/neotree//require ()
  "Require modules for `neotree'."
  (require 'neotree))

(defun serika-gc/neotree//keymap ()
  "Configure keymap for `neotree'."
  (serika-f/keymap/save neotree-mode-map)

  (setq neotree-mode-map
        (let ((map (make-sparse-keymap)))
          (serika-f/keymap/bind-digits map 'digit-argument)

          (define-key map (kbd "A-n") 'serika-f/neotree/parent-directory)
          (define-key map (kbd "A-e") 'neotree-next-line)
          (define-key map (kbd "A-i") 'neotree-previous-line)
          (define-key map (kbd "A-o") 'neotree-change-root)

          (define-key map (kbd "d n") 'neotree-create-node)
          (define-key map (kbd "d d") 'neotree-delete-node)
          (define-key map (kbd "d c") 'neotree-copy-node)
          (define-key map (kbd "d r") 'neotree-copy-node)

          (define-key map (kbd "t h") 'neotree-hidden-file-toggle)

          (define-key map (kbd "q")   'neotree-hide)
          ;; todo: fix long name
          (define-key map (kbd "RET") (lookup-key --serika-saved-keymap-neotree-mode-map (kbd "RET")))
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
