; returns a list of points for a room
; HELPER FUNCTIONS FOR BUILDING MAP
; ====================================================================================================

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

; ===================================================================================================

(defvar *dungeonworld-points* '())
(defvar *dungeonworld-paths* '())

(let* ((main (def-room 'main 0 10 0 10))
       (main-points (car main))
       (main-paths (car (car main))))
  (append dungeonworld-points main-points)
  (append dungeonworld-paths main-paths))


(def-roadmap *dungeonworld-points* *dungeonworld-paths*)

(def-object 'apple '(is_inanimate is_edible (has_cost 3.0)))

(place-object 'apple1-main 'apple 'main-5-5 0 
	nil 
	'((is_edible apple1-main) 
	 )
    nil 
)
