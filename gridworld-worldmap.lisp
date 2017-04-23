; returns a list of points for a room
; HELPER FUNCTIONS FOR BUILDING MAP
; symbol format ?room-?x-?y
;(require :regexp2)

(defparameter *room-facts* (make-hash-table :test #'equal))


(defun width (?room)
     (caddr (cadr (gethash (list 'width ?room nil) *room-facts*))))

(defun depth (?room)
     (caddr (cadr (gethash (list 'depth ?room nil) *room-facts*))))

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
                (equal (cadr (split-regexp "&" (symbol-name hd))) (cadr (split-regexp "&" (symbol-name e)))) 
                (or 
                  (eq 
                    (+ 1 (parse-integer (cadr (cdr (split-regexp "&" (symbol-name hd)))))) 
                    (parse-integer (cadr (cdr (split-regexp "&" (symbol-name e)))))) 
                  (eq 
                    (+ 1 (parse-integer (cadr (cdr (split-regexp "&" (symbol-name e)))))) 
                    (parse-integer (cadr (cdr (split-regexp "&" (symbol-name hd))))))))
              (and 
                (equal (cadr (cdr (split-regexp "&" (symbol-name hd)))) (cadr (cdr (split-regexp "&" (symbol-name e)))))
                (or 
                  (eq 
                    (+ 1 (parse-integer (cadr (split-regexp "&" (symbol-name hd))))) 
                    (parse-integer (cadr (split-regexp "&" (symbol-name e)))))
                  (eq 
                    (+ 1 (parse-integer (cadr (split-regexp "&" (symbol-name e)))))
                    (parse-integer (cadr (split-regexp "&" (symbol-name hd))))))))
          (setf ?lst (append (list (list (intern (concatenate 'string (symbol-name hd) "_" (symbol-name e))) hd 1 e)) ?lst)))) 
      (room-edges-rect-helper ?room (cdr ?points) ?lst))))

(defun room-edges-rect (?room ?points)
 (room-edges-rect-helper ?room ?points '()))

(defun def-room (?room ?x1 ?x2 ?y1 ?y2)
  (let* ((w (- ?x2 ?x1))
         (d (- ?y2 ?y1))
         (points (room-pts-rect ?room ?x1 ?x2 ?y1 ?y2))
         (edges (room-edges-rect ?room points)))
    (add_tuple_to_hashtable (list 'width ?room w) *room-facts* nil)
    (add_tuple_to_hashtable (list 'depth ?room d) *room-facts* nil)
    (list points edges)))
