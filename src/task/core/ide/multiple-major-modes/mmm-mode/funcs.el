;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'mmm-mode)

;; TODO: Fix bug
;; (defun serika/mmm-mode/add (mode submode-param begin-pattern-param end-pattern-param)
;;   "Create subclasses."
;;   (cl-flet ((--convert-to-list (param)
;;                                (cond ((listp param)   param)
;;                                      ((symbolp param) (list param))
;;                                      ((stringp param) (list param))
;;                                      (t               (error "Incorrect parameter in `serika/mmm-mode/add'.")))))
;;     (let* ((submode-list              (--convert-to-list submode-param))
;;            (begin-pattern-list        (--convert-to-list begin-pattern-param))
;;            (end-pattern-list          (--convert-to-list end-pattern-param))
;;            (submode-list-length       (length submode-list))
;;            (begin-pattern-list-length (length begin-pattern-list))
;;            (end-pattern-list-length   (length end-pattern-list)))
;;       (if (not (= submode-list-length
;;                   begin-pattern-list-length
;;                   end-pattern-list-length))
;;           (error "Input lists have not equal lengths.")
;;         (dotimes (ind submode-list-length)
;;           (lexical-let ((submode       (nth ind submode-list))
;;                         (begin-pattern (nth ind begin-pattern-list))
;;                         (end-pattern   (nth ind end-pattern-list))
;;                         (class-symbol  (make-symbol (format "%s-in-%s-%d"
;;                                                             (symbol-name submode)
;;                                                             mode
;;                                                             ind)))))
;;           (mmm-add-classes `((,class-symbol
;;                               :submode ,submode
;;                               :front   ,begin-pattern
;;                               :back    ,end-pattern
;;           (mmm-add-mode-ext-class mode nil class-symbol))))))

;; Local
(defun serika/mmm-mode//clear ()
  "Clear `mmm-mode' classes."
  )

(defun serika/mmm-mode//activate ()
  "Activate `mmm-mode'."
  (mmm-mode +1))

(defun init ()
  "Configure `mmm-mode'."
  (serika/mmm-mode//clear))
