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

;(def-roadmap '(main-0-5 main-1-5 main-2-5 main-3-5 main-4-5 main-5-5 main-6-5 main-7-5 main-8-5 main-9-5) '( (path1 main-0-5 1 main-1-5) ) )


(def-object 'robot '(is_animate can_talk))
(def-object 'apple '(is_inanimate is_edible (has_cost 3.0)))

(place-object 'apple1-main 'apple 'main-0-5 0 
	nil 
	'((is_edible apple1-main) 
	 )
    nil 
)

(place-object 'apple2-main 'apple 'main-0-5 0 
	nil 
	'((is_edible apple2-main) 
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

(defparameter *room-facts*
      (make-htable '( (width 'main 10)
                      (depth 'main 10) )))

; (find-location 'AG *world-facts*)
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
      :time-required 1
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
      :time-required 1
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
      :time-required 1
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
      :time-required 1
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

(defun width (?room)
     (caddr (cadr (gethash (list 'width 'that ?room nil) *room-facts*))))

(defun depth (?room)
     (caddr (cadr (gethash (list 'depth 'that ?room nil) *room-facts*))))

(defun saw (pos dir)
 (let* ((sym-lst (split-regexp "-" (symbol-name pos)))
       (room (intern (car sym-lst)))
       (x (parse-integer (cadr sym-lst)))
       (y (parse-integer (caddr sym-lst)))
       (dx (width room))
       (dy (depth room))
       (begin (cond 
                ((equal dir 'NORTH) (+ x 1))
                ((equal dir 'SOUTH) 0)
                ((equal dir 'WEST)  0)
                ((equal dir 'EAST)  (+ y 1))))
       (end (cond 
                ((equal dir 'NORTH) dx)
                ((equal dir 'SOUTH) (- x 1))
                ((equal dir 'WEST) (- y 1))
                ((equal dir 'EAST) dy))))
   (append '(can_see) (list
   (loop for n from begin to end
         append
            (let* ((xp (if (or (equal dir 'NORTH) (equal dir 'SOUTH)) n x))
                   (yp (if (or (equal dir 'EAST) (equal dir 'WEST)) n y))
                   (plst (list room xp yp))
                   (point (intern (format nil "~{~a~^-~}" plst)))
                   (hval  (gethash (list 'is_at nil point) *world-facts*))
                   (nobjs (car hval))
                   (pred (cdr hval))
                   (objs (mapcar 'cadr pred)))
              objs))))))

(setq see.actual 
	(make-op.actual :name 'see.actual :pars '(?pos ?dir ?objects)
	:startconds '( (is_facing ?dir) (is_at ?pos) (can_see ?objects) )
    :stopconds '(T)
	:deletes '( (can_see ?objects) )
    :adds '( (saw ?pos ?dir) )
	)
)

