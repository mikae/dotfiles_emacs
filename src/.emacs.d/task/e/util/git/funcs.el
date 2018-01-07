(defun func/git/repo-path-p (repo-path)
  "Return t if REPO-PATH is git directory."
  (let ((--result))
    (shell-command (format "cd %s;"
                           repo-path))
    (setq --result
          (shell-command "git rev-parse --git-dir 2> /dev/null;"))
    (shell-command "cd -;")
    (eq --result
        0)))
