;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika-g/helm/projectile//require ()
  "Require modules for `projectile'."
  (require 'helm-projectile))

(defun serika-g/helm/projectile//add-projectile-bindings ()
  "Add bindings to `projectile-mode-map'."
  (define-key projectile-mode-map (kbd "C-p h") 'helm-projectile)
  (define-key projectile-mode-map (kbd "C-p f") 'helm-projectile-find-file-dwim)
  (define-key projectile-mode-map (kbd "C-p g") 'helm-projectile-find-file)
  (define-key projectile-mode-map (kbd "C-p b") 'helm-projectile-switch-to-buffer)
  (define-key projectile-mode-map (kbd "C-p o") 'helm-projectile-find-other-file))

;; Init
(defun init ()
  "Configure `helm-projectile'."
  (serika-g/helm/projectile//require)
  (serika-g/helm/projectile//add-projectile-bindings))
