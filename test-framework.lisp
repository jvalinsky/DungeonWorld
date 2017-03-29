; Test Library

(defparameter *RED*   (concatenate 'string (list (code-char 27) #\[ #\3 #\1 #\m))) 
(defparameter *GREEN* (concatenate 'string (list (code-char 27) #\[ #\3 #\2 #\m))) 
(defparameter *LIGHT-PURPLE* (concatenate 'string (list (code-char 27) #\[ #\3 #\5 #\m))) 
(defparameter *BLUE* (concatenate 'string (list (code-char 27) #\[ #\3 #\4 #\m))) 
(defparameter *WHITE* (concatenate 'string (list (code-char 27) #\[ #\3 #\7 #\m))) 
(defparameter *RESET* (concatenate 'string (list (code-char 27) #\[ #\0 #\m))) 

(defmacro deftest (name parameters &body body)
  (defvar *test-name* nil)
  `(defun ,name ,parameters
    (setf *test-name* ',name)
       ,@body))


(defun print-result (name result form)
  "Prints and returns result of running test"
  (format t (concatenate 'string "~:[" *RED* "FAIL~;" *GREEN* "pass~]" *WHITE* " ... ~a:" *BLUE* " ~a~%") result name form)
  result)

(defmacro check (&body tests)
  `(combine-results
     ,@(mapcar #'(lambda (test) `(print-result *test-name* ,test ',test)) tests)))

(defmacro combine-results (&body results)
  (reduce #'(lambda(r1 r2) `(and ,r1 ,r2 )) results))


(defmacro with-env (&key (setup nil) (cleanup nil) (body nil))
  `(mapcar #'(lambda (work) work) ,@(append setup body cleanup)))

