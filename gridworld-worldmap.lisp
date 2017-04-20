; returns a list of points for a room
; HELPER FUNCTIONS FOR BUILDING MAP
; symbol format ?room-?x-?y
(require :regexp2)

(defun room-pts-rect (?room ?x1 ?x2 ?y1 ?y2)
  (loop for x from ?x1 to ?x2
                     append
                     (loop for y from ?y1 to ?y2
                           collect (intern (format nil "~{~a~^&~}" (list ?room x y))))))

; for loop in for loop that checks if there should be a path between points
; then makes path
(defun room-edges-rect-helper (?room ?points ?lst)
  (if (or (null ?points) (eq 1 (length ?points)))
    ?lst
    (let ((hd (car ?points)) (tl (cdr ?points)))
      (loop for e in tl do
        (if (or 
              (and 
                (equal (cadr (split-re "\\-" (string hd))) (cadr (split-re "\\-" (string e)))) 
                (or 
                  (eq 
                    (+ 1 (parse-integer (cadr (cdr (split-re "\\-" (string hd)))))) 
                    (parse-integer (cadr (cdr (split-re "\\-" (string e)))))) 
                  (eq 
                    (+ 1 (parse-integer (cadr (cdr (split-re "\\-" (string e)))))) 
                    (parse-integer (cadr (cdr (split-re "\\-" (string hd))))))))
              (and 
                (equal (cadr (cdr (split-re "\\-" (string hd)))) (cadr (cdr (split-re "\\-" (string e)))))
                (or 
                  (eq 
                    (+ 1 (parse-integer (cadr (split-re "\\-" (string hd))))) 
                    (parse-integer (cadr (split-re "\\-" (string e)))))
                  (eq 
                    (+ 1 (parse-integer (cadr (split-re "\\-" (string e)))))
                    (parse-integer (cadr (split-re "\\-" (string hd))))))))
          (setf ?lst (append (list (list (intern (concatenate 'string (string hd) "_" (string e))) hd 1 e)) ?lst)))) 
      (room-edges-rect-helper ?room (cdr ?points) ?lst))))

(defun room-edges-rect (?room ?points)
 (room-edges-rect-helper ?room ?points '()))

(defun def-room (?room ?x1 ?x2 ?y1 ?y2)
  (let* ((points (room-pts-rect ?room ?x1 ?x2 ?y1 ?y2))
        (edges (room-edges-rect ?room points)))
    (list points edges)))