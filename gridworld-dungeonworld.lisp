; returns a list of points for a room
; symbol format ?room-?x-?y
(defun def-room (?room ?x ?y)
  (loop for x from 0 to ?x
        append
            (loop for y from 0 to ?y
                collect (intern (format nil "~{~a~^-~}" (list ?room x y))))))
