(in-package #:cl-template)

(defun compile-expression (code stream data expressions stack)
  (declare (ignore stream))
  (if (or (string= code "end") (string= code ")"))
      (let* ((last-expression (pop (car stack)))
             (contained
              (loop for expression in (car expressions) until (equal expression last-expression) collect expression do (pop (car expressions)))))
        (nconc last-expression (reverse contained))
        nil)
      (let ((needs-pushing nil))
        (unless (char= (char code 0) #\()
          (setf needs-pushing t)
          (setf code (concatenate 'string "(" code ")")))
        (if (and (char= (char code 0) #\() (char/= (char code (1- (length code))) #\)))
            (setf needs-pushing t))
        (let ((expression (rewrite-variable-accesses (read-from-string code) data)))
          (if needs-pushing
              (push expression (car stack)))
          (push expression (car expressions))
          expression))))

(defun compile-echo-expression (code stream data expressions stack)
  (declare (ignore stack))
  (let ((expression
          (cond
            ;; A function call, (format nil "~r" 123)
            ((char= (char code 0) #\()
             `(write-string ,(rewrite-variable-accesses (read-from-string code) data) ,stream))
            ;; A variable, stuff
            ((= (length code) (length (scan-string-until-ignoring code " " :ignore-list '((#\" . #\")))))
             `(write-string ,(rewrite-variable-accesses (read-from-string code) data) ,stream))
            ;; A function call without outer parens, format nil "~r" 123
            (t
             `(write-string ,(rewrite-variable-accesses (read-from-string (concatenate 'string "(" code ")")) data) ,stream)))))
    (push expression (car expressions))
    expression))

(defun compile-string (string stream data expressions stack)
  (declare (ignore data stack))
  (let ((expression `(write-string ,string ,stream)))
    (push expression (car expressions))
    expression))

(defun compile-template-part (string start start-delimiter start-echo-delimiter end-delimiter stream data expressions stack)
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
                   (string-trim '(#\Space #\Tab #\Newline) inner) stream data expressions stack)
                  (+ start end))))
        (list (compile-string (subseq string start delimiter-index) stream data expressions stack) (or delimiter-index (length string))))))

(defun internal-compile-template (string start-delimiter start-echo-delimiter end-delimiter stream-name data-name)
  (let ((start 0) (expressions (list nil)) (stack (list nil)))
    `(with-output-to-string (,stream-name)
       ,@(progn
          (loop
             for (form end) = (compile-template-part string start start-delimiter start-echo-delimiter end-delimiter stream-name data-name expressions stack)
             until (>= end (length string)) do
               (setf start end))
          (reverse (car expressions))))))

(defun compile-template (string &key (start-delimiter "<%") (start-echo-delimiter "<%=") (end-delimiter "%>"))
  (let ((stream (gensym)) (data (gensym)))
    (eval
     `(lambda (,data)
        ,(internal-compile-template string start-delimiter start-echo-delimiter end-delimiter stream data)))))
