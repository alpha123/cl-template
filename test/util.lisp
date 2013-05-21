(in-package #:cl-template-tests)

(def-suite util-tests)

(in-suite util-tests)

(test scan-string-until-ignoring
  "Test the function to scan a string until a certain delimiter."
  (labels ((scan-string-until-ignoring (&rest args) (apply #'cl-template::scan-string-until-ignoring args)))
    (is (string= "yeah" (scan-string-until-ignoring "yeah" "%}")))
    (is (string= "abc" (scan-string-until-ignoring "abc|def" "|")))
    (is (string= "" (scan-string-until-ignoring "&stuff" "&")))
    (is (string= "hello" (scan-string-until-ignoring "hello##world" "##")))
    (is (string= "#hello" (scan-string-until-ignoring "#hello###world" "###")))
    (is (string= "abc <ignor|ing> def" (scan-string-until-ignoring "abc <ignor|ing> def" "|" :ignore-list '((#\< . #\>)))))
    (is (string= "\"{{stuff}}\"}now" (scan-string-until-ignoring "\"{{stuff}}\"}now}}" "}}" :ignore-list '((#\" . #\")))))
    (is (string= " stuff " (scan-string-until-ignoring "some <% stuff %>" "%>" :start 7)))
    (is (string= "yeah " (scan-string-until-ignoring "yeah garbage" "garbage")))
    (is (string= "ye" (scan-string-until-ignoring "garbage ye:ah garbage" ":" :start 8 :end 13)))))

(test scan-between-delimiters
  "Test scanning a string for a substring between two delimiters."
  (labels ((scan-between-delimiters (&rest args) (apply #'cl-template::scan-between-delimiters args)))
    (is (string= "" (scan-between-delimiters "thingy" "{%" "%}")))
    (is (string= " stuff " (scan-between-delimiters "<% stuff %>" "<%" "%>")))
    (is (string= "stuff" (scan-between-delimiters "moar <%=stuff%>" "<%=" "%>")))
    (is (string= " whale " (scan-between-delimiters "<html><%= whale %></html>" "<%=" "%>")))
    (is (string= " other \"<%stuff%>\" " (scan-between-delimiters "<% other \"<%stuff%>\" %>" "<%" "%>" :ignore-list '((#\" . #\")))))))

(test rewrite-variable-accesses
  "Test rewriting variable accesses to lookups in a plist."
  (labels ((rewrite-variable-accesses (&rest args) (apply #'cl-template::rewrite-variable-accesses args)))
    (let ((data (gensym)))
      (is (= 10 (rewrite-variable-accesses 10 data)))
      (is (char= #\c (rewrite-variable-accesses #\c data)))
      (is (equal `(getf ,data :stuff) (rewrite-variable-accesses 'stuff data)))
      (is (equal `(format nil "~r" (getf ,data :n)) (rewrite-variable-accesses '(format nil "~r" n) data)))
      (is (equal `(format t "~@r" (add-stuff (getf ,data :x) (get-real-y (getf ,data :y)))) (rewrite-variable-accesses '(format t "~@r" (add-stuff x (get-real-y y))) data))))))
