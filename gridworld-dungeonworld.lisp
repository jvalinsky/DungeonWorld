; returns a list of points for a room
; HELPER FUNCTIONS FOR BUILDING MAP
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
; Building world

(defvar *dungeonworld-points* '())
(defvar *dungeonworld-paths* '())

(let* ((main (def-room 'main 0 10 0 10))
       (main-points (car main))
       (main-paths (car (car main))))
  (append dungeonworld-points main-points)
  (append dungeonworld-paths main-paths))

(def-roadmap *dungeonworld-points* *dungeonworld-paths*)

;(def-roadmap '(main gold) '( (path1 main 3 gold) ))


(def-object 'robot '(is_animate can_talk))
(def-object 'apple '(is_inanimate is_edible (has_cost 3.0)))

(place-object 'apple1-main 'apple 'main-0-0 0 
	nil 
	'((is_edible apple1-main) 
	 )
    nil 
)

(place-object 'AG 'robot 'main-5-5 0
 nil
 '( (is_facing NORTH)
    (not (is_facing SOUTH))
    (not (is_facing EAST))
    (not (is_facing WEST))
   )
 nil
)


(setq *operators* '(turn-north turn-south turn-west turn-east))
(setq *search-beam*
	(list (cons 5 *operators*) (cons 4 *operators*) (cons 3 *operators*) ))
; ======================================================================================================
; defining Actions

; Perception

(setq turn-north
      (make-op :name 'turn-north :pars '(?dir)
      :preconds '( (not (is_facing NORTH)) (is_facing ?dir) ) 
      :effects '( (is_facing NORTH) )
      :time-required 0
      :value 3
      )
)

(setq turn-north.actual 
	(make-op.actual :name 'turn-north.actual :pars '(?dir)
	:startconds '( (not (is_facing NORTH)) (is_facing ?dir) )
    :stopconds '( (is_facing NORTH) )
	:deletes '( (is_facing ?dir) (not (is_facing NORTH)) )
    :adds '( (is_facing NORTH) (not (is_facing ?dir)) )
	)
)

(setq turn-south
      (make-op :name 'turn-south :pars '(?dir)
      :preconds '( (not (is_facing SOUTH)) (is_facing ?dir) )
      :effects '( (is_facing SOUTH) )
      :time-required 0
      :value 3
      )
)

(setq turn-south.actual 
	(make-op.actual :name 'turn-south.actual :pars '(?dir)
	:startconds '( (not (is_facing SOUTH)) (is_facing ?dir) )
    :stopconds '( (is_facing SOUTH) )
	:deletes '( (is_facing ?dir) (not (is_facing SOUTH)) )
    :adds '( (is_facing SOUTH) (not (is_facing ?dir)) )
	)
)

(setq turn-west
      (make-op :name 'turn-west :pars '(?dir)
      :preconds '( (not (is_facing WEST)) (is_facing ?dir) )
      :effects '( (is_facing WEST) )
      :time-required 0
      :value 3
      )
)

(setq turn-west.actual 
	(make-op.actual :name 'turn-west.actual :pars '(?dir)
	:startconds '( (not (is_facing WEST)) (is_facing ?dir) )
    :stopconds '( (is_facing WEST) )
	:deletes '( (is_facing ?dir) (not (is_facing WEST)) )
    :adds '( (is_facing WEST) (not (is_facing ?dir)) )
	)
)

(setq turn-east
      (make-op :name 'turn-east :pars '(?dir)
      :preconds '( (not (is_facing EAST)) (is_facing ?dir) )
      :effects '( (is_facing EAST) )
      :time-required 0
      :value 3
      )
)

(setq turn-east.actual 
	(make-op.actual :name 'turn-east.actual :pars '(?dir)
	:startconds '( (not (is_facing EAST)) (is_facing ?dir) )
    :stopconds '( (is_facing EAST) )
	:deletes '( (is_facing ?dir) (not (is_facing EAST)) )
    :adds '( (is_facing EAST) (not (is_facing ?dir)) )
	)
)

