(require 'ert)

(defmacro test (name &rest body)
  `(ert-deftest ,(intern (concat "serika-f/emojify/activate|"
                                 (symbol-name name)))
       ()
     ,@body))

(test check-function-defined
      (should (fboundp 'serika-f/emojify/activate)))

(test check-error-if-invalid-arg-style
      (should-error (serika-f/emojify/activate :emoji-styles 123)))
