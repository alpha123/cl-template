(in-package #:cl-template-tests)

(def-suite templating-tests)

(in-suite templating-tests)

(test compile-template
  "Test that templates compile correctly."
  (labels ((compile-template (string)
             (cl-template::internal-compile-template string "<%" "<%=" "%>" '__stream '__data)))
    (is (equal
         '(with-output-to-string (__stream)
           (write-string "I need more creative dummy data." __stream))
         (compile-template "I need more creative dummy data.")))
    (is (equal
         '(with-output-to-string (__stream)
           (write-string (getf __data :fish) __stream))
         (compile-template "<%= fish %>")))
    (is (equal
         '(with-output-to-string (__stream)
           (write-string "fish: " __stream)
           (write-string (getf __data :fish) __stream)
           (write-string "..." __stream))
         (compile-template "fish: <%= fish %>...")))
    (is (equal
         '(with-output-to-string (__stream)
           (write-string "yeah! " __stream)
           (write-string (extra-fish) __stream))
         (compile-template "yeah! <%= (extra-fish) %>")))
    (is (equal
         '(with-output-to-string (__stream)
           (write-string "The number is " __stream)
           (write-string (format nil "~r" (getf __data :n)) __stream))
         (compile-template "The number is <%= format nil \"~r\" n %>")))
    (is (equal
         '(with-output-to-string (__stream)
           (if (getf __data :thing)
               (write-string (getf __data :thing) __stream)))
         (compile-template "<% if thing %><%= thing %><% end %>")))
    (is (equal
         '(with-output-to-string (__stream)
           (loop for i below 10 do
                (write-string "i: " __stream)
                (write-string (write-to-string i) __stream)))
         (compile-template "<% loop for i below 10 do %>i: <%= write-to-string i %><% end %>")))
    (is (equal
         '(with-output-to-string (__stream)
           (loop for person in (getf __data :people) do
                (write-string "Name: " __stream)
                (write-string (name person) __stream)))
         (compile-template "<% (loop for person in people do %>Name: <%= (name person) %><% ) %>")))))
