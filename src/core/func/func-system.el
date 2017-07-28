;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun serika-f/system/mac-p ()
  "Return t if systeme is mac."
  (eq system-type 'darwin))

(defun serika-f/system/linux-p ()
  "Return t if system is linux."
  (eq system-type 'gnu/linux))

(defun serika-f/system/windows-p ()
  "Return t if system is windows."
  (eq system-type 'windows-nt))

(defun serika-f/system/user-login ()
  "Return user login."
  (getenv
    (if (serika-f/system/windows-p) "USERNAME" "USER")))

(defun serika-f/system/user-home ()
  "Return user home path."
  (getenv
    (if (serika-f/system/windows-p) "HOMEPATH" "HOME")))

(defun serika-f/system/getenv (env-variable &optional default-value)
  "Return environment variable value of ENV-VARIABLE.
If it's nil return DEFAULT-VALUE."
  (if (stringp env-variable)
      (or (getenv env-variable)
          default-value
          (error (format "Environment variable %s was not found"
                         env-variable)))
    (error "Can't get value of non-string key from environment")))

(provide 'func-system)
;;; func-system.el ends here
