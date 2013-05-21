(in-package #:cl-template-tests)

(def-suite templating-tests)

(in-suite templating-tests)

(test compile-template
  "Test that templates compile correctly."
  (labels ((compile-template (string)
             (cl-template::internal-compile-template string "<%" "<%=" "%>" '__stream)))
    (is (equal
         '(with-output-to-string (__stream)
           (write-string "I need more creative dummy data." __stream))
         (compile-template "I need more creative dummy data.")))
    (is (equal
         '(with-output-to-string (__stream)
           (write-string fish __stream))
         (compile-template "<%= fish %>")))
    (is (equal
         '(with-output-to-string (__stream)
           (write-string "fish: " __stream)
           (write-string (@ fish) __stream)
           (write-string "..." __stream))
         (compile-template "fish: <%= @ fish %>...")))
    (is (equal
         '(with-output-to-string (__stream)
           (write-string "yeah! " __stream)
           (write-string (extra-fish) __stream))
         (compile-template "yeah! <%= (extra-fish) %>")))
    (is (equal
         '(with-output-to-string (__stream)
           (write-string "The number is " __stream)
           (write-string (format nil "~r" n) __stream))
         (compile-template "The number is <%= format nil \"~r\" n %>")))
    (is (equal
         '(with-output-to-string (__stream)
           (if (@ thing)
               (write-string (@ thing) __stream)))
         (compile-template "<% if (@ thing) %><%= @ thing %><% end %>")))
    (is (equal
         '(with-output-to-string (__stream)
           (if (@ thing)
               (write-string (@ thing) __stream)))
         (compile-template "<% (if (@ thing) %><%= @ thing %><% ) %>")))
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
         (compile-template "<% (loop for person in (getf __data :people) do %>Name: <%= (name person) %><% ) %>")))
    (is (equal
         '(with-output-to-string (__stream)
           (let ((x "42"))
             (write-string "x: " __stream)
             (write-string x __stream)))
         (compile-template "<% let ((x \"42\")) %>x: <%= x %><% end %>")))))

(test run-template
  "Test that templates run correctly."
  (labels ((run-template (string &optional data)
             (funcall (compile-template string) data)))
    (is (string= "Hello world!" (run-template "Hello world!")))
    (is (string= "red fish" (run-template "<%= getf cl-template::__data :color %> fish" '(:color "red"))))
    (is (string= "CXXIII" (run-template "<%= format nil \"~@r\" 123 %>")))
    (is (string= "thing 1 thing 2 thing 3 "
                 (run-template "<% loop for i from 1 to 3 do %>thing <%= write-to-string i %> <% end %>")))
    (is (string= "one" (run-template "<% if (= (getf cl-template::__data :x) 1) %>one<% end %>" '(:x 1))))
    (is (string= "" (run-template "<% if (= (getf cl-template::__data :x) 1) %>one<% end %>" '(:x 0))))))
