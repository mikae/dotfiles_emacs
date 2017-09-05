;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun func/system/mac-p ()
  "Return t if systeme is mac."
  (eq system-type 'darwin))

(defun func/system/linux-p ()
  "Return t if system is linux."
  (eq system-type 'gnu/linux))

(defun func/system/windows-p ()
  "Return t if system is windows."
  (eq system-type 'windows-nt))

(defun func/system/user-login ()
  "Return user login."
  (getenv
    (if (func/system/windows-p) "USERNAME" "USER")))

(defun func/system/user-home ()
  "Return user home path."
  (getenv
    (if (func/system/windows-p) "HOMEPATH" "HOME")))

(defun func/system/getenv (env-variable &optional default-value)
  "Return environment variable value of ENV-VARIABLE.
If it's nil return DEFAULT-VALUE."
  (if (stringp env-variable)
      (or (getenv env-variable)
          default-value
          (error (format "Environment variable %s was not found"
                         env-variable)))
    (error "Can't get value of non-string key from environment")))
