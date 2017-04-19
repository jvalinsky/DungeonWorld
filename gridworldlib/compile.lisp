(load "dungeonworld_test.lisp")

; Creates a binary executable named "dungeonworld-tests that executes the main 
; function defined in dungeonworld_test.lisp when run.
; Source: http://stackoverflow.com/questions/14171849/compiling-common-lisp-to-an-executable 
(sb-ext:save-lisp-and-die "dungeonworld-tests" :toplevel #'main :executable t)
