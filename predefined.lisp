;;;; Define some symbols that shouldn't be looked up in the data plist for templates.
;;;; Largely loop keywords, so that the template author can write
;;;;    loop for i below 10 do
;;;; instead of
;;;;    loop :for i :below 10 :do

(in-package #:cl-template-template-symbols)

;; Loop keywords. Hopefully this is an exhaustive list, but I might be missing some.
;; Yes, some of these are already interned by inheriting from the cl package.
#|(mapcar #'intern
        (list "across" "always" "and" "append" "appending" "being" "by" "collect" "collecting" "count" "counting"
        "do" "downto" "else" "end" "finally" "for" "from" "hash-key" "hash-keys" "hash-value" "hash-values" "if"
        "in" "initially" "into" "maximize" "maximizing" "minimize" "minimizing" "nconc" "nconcing" "never" "on"
        "repeat" "sum" "summing" "the" "then" "thereis" "to" "unless" "upto" "using" "when" "with"))|#

(defun @ (variable)
    (getf __data (intern (symbol-name variable) :keyword)))
