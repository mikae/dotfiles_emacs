;;; package --- Summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun serika/system/mac-p ()
  "Return t if systeme is mac."
  (eq system-type 'darwin))

(defun serika/system/linux-p ()
  "Return t if system is linux."
  (eq system-type 'gnu/linux))

(defun serika/system/windows-p ()
  "Return t if system is windows."
  (eq system-type 'windows-nt))

(defun serika/system/user-login ()
  "Return user login."
  (getenv
    (if (serika/system/windows-p) "USERNAME" "USER")))

(defun serika/system/user-home ()
  "Return user home path."
  (getenv
    (if (serika/system/windows-p) "HOMEPATH" "HOME")))

(provide 'func-system)
;;; func-system.el ends here
