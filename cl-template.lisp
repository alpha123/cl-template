(in-package #:cl-template)

(defun compile-expression (code stream expressions stack)
  (declare (ignore stream))
  (if (or (string= code "end") (string= code ")"))
      (let* ((last-expression (pop (car stack)))
             (contained
              (loop for expression in (car expressions) until (equal expression last-expression) collect expression do (pop (car expressions)))))
        (nconc last-expression (reverse contained))
        (return-from compile-expression nil)))
  (let* ((pairs (match-pairs-ignoring code '(#\( . #\)) :ignore-list '((#\" . #\"))))
         (needs-pushing
          (cond ((char/= (char code 0) #\()
                 (setf code (concatenate 'string "(" code ")"))
                 t)
                ((> pairs 0)
                 (setf code (concatenate 'string code (make-string pairs :initial-element #\))))
                 t)
                (t nil))))
    (let ((expression (read-from-string code)))
      (if needs-pushing
          (push expression (car stack)))
      (push expression (car expressions))
      expression)))

(defun compile-echo-expression (code stream expressions stack)
  (declare (ignore stack))
  (let ((expression
          (cond
            ;; A function call, (format nil "~r" 123)
            ((char= (char code 0) #\()
             `(write-string ,(read-from-string code)  ,stream))
            ;; A variable, stuff
            ((= (length code) (length (scan-string-until-ignoring code " " :ignore-list '((#\" . #\")))))
             `(write-string ,(read-from-string code)  ,stream))
            ;; A function call without outer parens, format nil "~r" 123
            (t
             `(write-string ,(read-from-string (concatenate 'string "(" code ")"))  ,stream)))))
    (push expression (car expressions))
    expression))

(defun compile-string (string stream expressions stack)
  (declare (ignore stack))
  (let ((expression `(write-string ,string ,stream)))
    (push expression (car expressions))
    expression))

(defun compile-template-part (string start start-delimiter start-echo-delimiter end-delimiter stream expressions stack)
  (let* ((start-index (search start-delimiter string :start2 start))
         (echo-index (search start-echo-delimiter string :start2 start))
         (delimiter-index (if (eql start-index echo-index) echo-index start-index)))
    (if (and delimiter-index (= (- delimiter-index start) 0))
        (let ((is-echo (string= (subseq string delimiter-index (+ delimiter-index (length start-echo-delimiter))) start-echo-delimiter)))
          (multiple-value-bind (inner end)
              (scan-between-delimiters string (if is-echo start-echo-delimiter start-delimiter)
                                       end-delimiter :start start :ignore-list '((#\" . #\")))
            (list (funcall
                   (if is-echo #'compile-echo-expression #'compile-expression)
                   (string-trim '(#\Space #\Tab #\Newline) inner) stream expressions stack)
                  (+ start end))))
        (list (compile-string (subseq string start delimiter-index) stream expressions stack) (or delimiter-index (length string))))))

(defun internal-compile-template (string start-delimiter start-echo-delimiter end-delimiter stream-name)
  (let ((start 0) (expressions (list nil)) (stack (list nil)))
    `(with-output-to-string (,stream-name)
       ,@(progn
          (loop
             for (form end) = (compile-template-part string start start-delimiter start-echo-delimiter end-delimiter stream-name expressions stack)
             until (>= end (length string)) do
               (setf start end))
          (reverse (car expressions))))))

(defun compile-template (string &key (start-delimiter "<%") (start-echo-delimiter "<%=") (end-delimiter "%>"))
  (let ((stream (gensym)))
    (eval
     `(lambda (__data)
        (declare (dynamic-extent __data))
        ,(internal-compile-template string start-delimiter start-echo-delimiter end-delimiter stream)))))
