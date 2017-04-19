; returns a list of points for a room
; symbol format ?room-?x-?y

(defun room-pts-rect (?room ?x1 ?x2 ?y1 ?y2)
  (loop for x from ?x1 to ?x2
                     append
                     (loop for y from ?y1 to ?y2
                           collect (intern (format nil "~{~a~^-~}" (list ?room x y))))))

; for loop in for loop that checks if there should be a path between points
; then makes path
(defun room-edges-rect (?room ?points)

    )

(defun def-room (?room ?x1 ?x2 ?y1 ?y2)
  (let* ((points (room-pts-rect ?room ?x1 ?x2 ?y1 ?y2))
        (edges (room-edges-rect ?room points)))
    (list points edges)))
