;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'core-package)
(require 'core--execution)
(require 'core-funcs)

(defvar --serika-execution-graph nil
  "Execution graph for all tasks.")

(defun serika-c/eg/create (&rest packages)
  (setq --serika-execution-graph (eg/create))

  (dolist (task '(
                  ;; zero tasks
                  "zero require"
                  "zero configure"
                  "zero lib install"
                  "zero lib require"

                  ;; base tasks
                  "base require"
                  "base configure"
                  "base interface"
                  "base post install"
                  "base post require"

                  ;; tasks
                  "install"
                  "require"
                  "interface"
                  "settings"
                  "keymap"
                  "global-keymap"
                  "hook"
                  "post activate"))
    (eg/create-path --serika-execution-graph task)))

(cl-defun serika-c/eg/add (&key name         (last '__unnamed__)
                                &key parents (last nil)
                                &key func    (last nil)
                                &key node    (last nil))
  "Add new execution node."
  (eg/add --serika-execution-graph
          :name    name
          :parents parents
          :func    func
          :node    node))

(cl-defun serika-c/eg/add-install (&key type (last 'package)
                                        &key package-list (last '())
                                        &key name         (last '__unnamed__)
                                        &key parents      (last nil)
                                        &key src          (last nil)
                                        &key post-hook    (last nil))
  "Add new execution node which installs PACKAGE-LIST."
  (let ((--type         (or type    'package))
        (--parents      (or parents '("install")))
        (--package-list package-list)
        (--src          src)
        (--name         name)
        (--lambda       nil)
        (--post-hook    post-hook))
    (setq --lambda
          (cond
           ((eq --type 'package) (lambda ()
                                   (mapcar #'serika-f/package/make-sure-installed
                                           --package-list)))
           ((eq --type 'download) (when (stringp --src)
                                    (lambda ()
                                      (let ((--destination (func/path-join serika-plugin-directory
                                                                               (func/string-parse-url (file-name-nondirectory --src)))))
                                        (unless (file-exists-p --destination)
                                          (url-copy-file --src
                                                         --destination))))))
           ((eq --type 'git) (when (stringp --src)
                               (lambda ()
                                 (let ((--destination (func/path-join serika-plugin-directory
                                                                          (file-name-nondirectory --src))))
                                   (unless (file-exists-p --destination)
                                     (shell-command-to-string (format "git clone %s %s"
                                                                      --src
                                                                      --destination))
                                     ;; Execute post hook in cloned dir
                                     (when --post-hook
                                       (shell-command-to-string (format "cd %s && %s"
                                                                        --destination
                                                                        --post-hook)))


                                     ;; Update load path
                                     (when (file-accessible-directory-p --destination)
                                       (add-to-list 'load-path --destination)))))))
           (t nil)))

    (when --lambda
      (eg/add --serika-execution-graph
              :name    --name
              :parents --parents
              :func    --lambda))))

(defmacro serika-c/eg/add-many (name &rest args)
  "Add many execution nodes at once.
Example: ."
  `(let ((--length (length ',args))
         (--args   ',args)
         (--name   ,name))
     (when (cl-evenp --length)
       (while --args
         (eg/add --serika-execution-graph
                 :name    --name
                 :parents (car --args)
                 :func    (let* ((--elem (car (cdr --args)))
                                 (--car  (car --elem)))
                            (if (eq --car
                                    'function)
                                (car (cdr --elem))
                              --elem)))
         (setq --args
               (nthcdr 2 --args))))))

(defun serika-c/eg/execute ()
  "Execute execution graph."
  (eg/execute --serika-execution-graph))

(provide 'core-execution)
;;; core-execution.el ends here
