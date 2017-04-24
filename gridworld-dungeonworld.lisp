(load "gridworld-worldmap.lisp")
; ===================================================================================================
; Building world

(defvar *dungeonworld-points* '())
(defvar *dungeonworld-paths* '())

(let* ((main (def-room 'main 0 5 0 5))
       (main-points (car main))
       (main-paths (cadr main)))
  (setq *dungeonworld-points* main-points)
  (setq *dungeonworld-paths* main-paths))

(def-roadmap *dungeonworld-points* *dungeonworld-paths*)


(def-object 'robot '(is_animate can_talk))
(def-object 'apple '(is_inanimate is_edible is_item (has_cost 3.0)))
(def-object 'bomb '(is_explodable is_item (has_damage 10.0)))
(def-object 'box '(is_inanimate is_openable))

(place-object 'box1@main 'box 'main&0&0 0
  '((apple apple0@main))
  '(
    (is_openable box1@main)
    ) 
  nil)

(place-object 'box2@main 'box 'main&3&3 0
  '((bomb bomb0@main))
  '(
    (is_openable box2@main)
    ) 
  nil)

(place-object 'apple1@main 'apple 'main&1&5 0 
	nil 
	'(
      (is_edible apple1@main)
	 )
    nil 
)

(place-object 'apple2@main 'apple 'main&3&5 0 
	nil 
	'(
      (is_edible apple2@main)
	 )
    nil 
)

(place-object 'apple3@main 'apple 'main&5&1 0 
	nil 
	'((is_edible apple3@main) 
	 )
    nil 
)

(place-object 'apple4@main 'apple 'main&5&4 0 
	nil 
	'((is_edible apple4@main) 
	 )
    nil 
)

(place-object 'apple5@main 'apple 'main&5&4 0 
	nil 
	'((is_edible apple5@main) 
	 )
    nil 
)


(place-object 'AG 'robot 'main&3&3 0
 nil
 '(
   (is_facing AG SOUTH)
   ;(is_at AG main&3&3)
   (eyes_open AG)
   (can_see AG (apple3@main apple1@main))

   (is_happy AG)
   (not (is_scared AG))
   (not (is_surprised AG))

   (is_hungry_to_degree AG 4.0)
   (is_thirsty_to_degree AG 2.0)
   (is_tired_to_degree AG 0.0)
   ;(has AG nil)
  
  )
 nil
)


(setq *operators* '(takeItem turn+north turn+south turn+west turn+east answer_user_whq walk))

(setq *search-beam*
	(list (cons 5 *operators*) (cons 4 *operators*) (cons 3 *operators*) ))
; ======================================================================================================
; defining Actions

; Perception


(defun saw? (pos dir)
 (let* ((sym-lst (split-regexp "&" (symbol-name pos)))
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
   (loop for n from begin to end
         append
            (let* ((xp (if (or (equal dir 'NORTH) (equal dir 'SOUTH)) n x))
                   (yp (if (or (equal dir 'EAST) (equal dir 'WEST)) n y))
                   (plst (list room xp yp))
                   (point (intern (format nil "~{~a~^&~}" plst)))
                   (hval  (gethash (list 'is_at nil point) *world-facts*))
                   (nobjs (car hval))
                   (pred (cdr hval))
                   (objs (mapcar 'cadr pred)))
              objs))))



(setq turn+north
      (make-op :name 'turn+north :pars '(?dir)
      :preconds '( (not (is_facing AG NORTH)) (is_facing AG ?dir) ) 
      :effects '( (is_facing AG NORTH) )
      :time-required 1
      :value 3
      )
)

(setq turn+north.actual 
	(make-op.actual :name 'turn+north.actual :pars '(?dir)
	:startconds '( (not (is_facing AG NORTH)) (is_facing AG ?dir) )
    :stopconds '( (is_facing AG NORTH) )
	:deletes '( (is_facing AG ?dir) (not (is_facing AG NORTH))  )
    :adds '( (is_facing AG NORTH) (not (is_facing AG ?dir))  )
	)
)

(setq turn+south
      (make-op :name 'turn+south :pars '(?dir)
      :preconds '( (not (is_facing AG SOUTH)) (is_facing AG ?dir) )
      :effects '( (is_facing AG SOUTH) )
      :time-required 1
      :value 3
      )
)

(setq turn+south.actual 
	(make-op.actual :name 'turn+south.actual :pars '(?dir)
	:startconds '( (not (is_facing AG SOUTH)) (is_facing AG ?dir) )
    :stopconds '( (is_facing AG SOUTH) )
	:deletes '( (is_facing AG ?dir) (not (is_facing AG SOUTH))  )
    :adds '( (is_facing AG SOUTH) (not (is_facing AG ?dir))  )
	)
)

(setq turn+west
      (make-op :name 'turn+west :pars '(?dir)
      :preconds '( (not (is_facing AG WEST)) (is_facing AG ?dir) )
      :effects '( (is_facing AG WEST)  )
      :time-required 1
      :value 3
      )
)

(setq turn+west.actual 
	(make-op.actual :name 'turn+west.actual :pars '(?dir)
	:startconds '( (not (is_facing AG WEST)) (is_facing AG ?dir)  )
    :stopconds '( (is_facing AG WEST) )
	:deletes '( (is_facing AG ?dir) (not (is_facing AG WEST)) )
    :adds '( (is_facing AG WEST) (not (is_facing AG ?dir)) )
	)
)

(setq turn+east
      (make-op :name 'turn+east :pars '(?dir)
      :preconds '( (not (is_facing AG EAST)) (is_facing AG ?dir) )
      :effects '( (is_facing AG EAST) )
      :time-required 1
      :value 3
      )
)

(setq turn+east.actual 
	(make-op.actual :name 'turn+east.actual :pars '(?dir)
	:startconds '( (not (is_facing AG EAST)) (is_facing AG ?dir) )
    :stopconds '( (is_facing AG EAST) )
	:deletes '( (is_facing AG ?dir) (not (is_facing AG EAST))  )
    :adds '( (is_facing AG EAST) (not (is_facing AG ?dir)) )
	)
)

(setq see.actual 
	(make-op.actual :name 'see.actual :pars '(?dir ?pos ?objects)
	:startconds '( (eyes_open AG) (is_facing AG ?dir) (is_at AG ?pos) (can_see AG ?objects) ) 
    :starredStopConds  '( (not (eyes_open AG)) )
	:deletes '( (can_see AG ?objects) ) 
    :adds '( (can_see AG (saw? ?pos ?dir))  )
	)
)

;Helper to see if two points are adjacent
(defun is_adjacent? (?x ?y)
  (format t "~S is_adjacent? ~S~%" ?x ?y)
  (not (null (find
    (+
      (abs
        (-
          (parse-integer (cadr (split-regexp "&" (symbol-name ?x))))
          (parse-integer (cadr (split-regexp "&" (symbol-name ?y))))))
      (abs
        (-
          (parse-integer (cadr (cdr (split-regexp "&" (symbol-name ?x)))))
          (parse-integer (cadr (cdr (split-regexp "&" (symbol-name ?y))))))))
    '(0 1))))
)

;Helper to test that ?x is ?dir of ?y (e.g. x is NORTH of y or similar)
(defun is_direction? (?dir ?x ?y)
  (if (equal ?x ?y)
    t
    (cond 
      ((equal ?dir 'EAST)
        (and
          (equal 0
            (-
                (parse-integer (cadr (split-regexp "&" (symbol-name ?x))))
                (parse-integer (cadr (split-regexp "&" (symbol-name ?y))))))
          (< 0
            (-
                (parse-integer (cadr (cdr (split-regexp "&" (symbol-name ?x)))))
                (parse-integer (cadr (cdr (split-regexp "&" (symbol-name ?y)))))))))
      ((equal ?dir 'WEST)
        (and
          (equal 0
            (-
                (parse-integer (cadr (split-regexp "&" (symbol-name ?x))))
                (parse-integer (cadr (split-regexp "&" (symbol-name ?y))))))
          (> 0
            (-
                (parse-integer (cadr (cdr (split-regexp "&" (symbol-name ?x)))))
                (parse-integer (cadr (cdr (split-regexp "&" (symbol-name ?y)))))))))
      ((equal ?dir 'NORTH)
        (and
          (< 0
            (-
                (parse-integer (cadr (split-regexp "&" (symbol-name ?x))))
                (parse-integer (cadr (split-regexp "&" (symbol-name ?y))))))
          (equal 0
            (-
                (parse-integer (cadr (cdr (split-regexp "&" (symbol-name ?x)))))
                (parse-integer (cadr (cdr (split-regexp "&" (symbol-name ?y)))))))))
      ((equal ?dir 'SOUTH)
        (and
          (> 0
            (-
                (parse-integer (cadr (split-regexp "&" (symbol-name ?x))))
                (parse-integer (cadr (split-regexp "&" (symbol-name ?y))))))
          (equal 0
            (-
                (parse-integer (cadr (cdr (split-regexp "&" (symbol-name ?x)))))
                (parse-integer (cadr (cdr (split-regexp "&" (symbol-name ?y))))))))))
  ))

(defun equal? (x y)
  (equal x y)
  )

(setq takeItem
      (make-op :name 'takeItem :pars '(?pos ?itemPos ?dir ?item)
      :preconds '(
        (is_at AG ?pos) 
        (is_adjacent? ?pos ?itemPos)
        ;(is_direction ?dir ?itemPos ?pos)
        (is_at ?item ?itemPos) 
        (not (has AG ?item))
        (is_item ?item)
        (not (equal? AG ?item))
                   )
      :effects '( (has AG ?item) )
      :time-required 1
      :value 4
      )
)

(setq takeItem.actual 
  (make-op.actual :name 'takeItem.actual :pars '(?pos ?itemPos ?dir ?item)
  :startconds '( 
        (is_at AG ?pos) 
        (is_adjacent? ?pos ?itemPos)
        ;(is_direction ?dir ?itemPos ?pos)
        (is_item ?item)
        (is_at ?item ?itemPos) 
        (not (has AG ?item))
        (not (equal? ?item AG))
                   )
  :stopconds '( (has AG ?item) )
  :adds '( (has AG ?item) )
  )
)


; Modified from orginal file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator walk, AG walks from point ?x to point ?y, with 
;; initial fatigue level ?f, assuming speed of one unit per time step.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nextPos? (?curPos ?dir)
  "return the next position after action"
  (format t "pos: ~a dir: ~a ~%" ?curPos ?dir)
  (cond 
    ((equal ?dir 'EAST)
      (intern (format nil "~{~a~^&~}" (list (car (split-regexp "&" (symbol-name ?curPos))) 
                                          (+ 1 (parse-integer (cadr (split-regexp "&" (symbol-name ?curPos))))) 
                                          (cadr (cdr (split-regexp "&" (symbol-name ?curPos))))))))
    ((equal ?dir 'WEST)
      (intern (format nil "~{~a~^&~}" (list (car (split-regexp "&" (symbol-name ?curPos))) 
                                          (- (parse-integer (cadr (split-regexp "&" (symbol-name ?curPos)))) 1) 
                                          (cadr (cdr (split-regexp "&" (symbol-name ?curPos))))))))
    ((equal ?dir 'NORTH)
      (intern (format nil "~{~a~^&~}" (list (car (split-regexp "&" (symbol-name ?curPos))) 
                                          (cadr (split-regexp "&" (symbol-name ?curPos)))
                                          (+ 1 (parse-integer (cadr (cdr (split-regexp "&" (symbol-name ?curPos))))))))))
    ((equal ?dir 'SOUTH)
      (intern (format nil "~{~a~^&~}" (list (car (split-regexp "&" (symbol-name ?curPos))) 
                                          (cadr (split-regexp "&" (symbol-name ?curPos)))
                                          (- (parse-integer (cadr (cdr (split-regexp "&" (symbol-name ?curPos))))) 1)))))
  ))

(setq walk 
  (make-op :name 'walk :pars '(?x ?dir ?f)
  :preconds '((is_facing AG ?dir)
              (is_at AG ?x)  
              (is_tired_to_degree AG ?f))
    :effects '( 
                (is_at AG (nextPos? ?x ?dir) )
              (not (is_at AG ?x))
               (is_tired_to_degree AG (+ ?f 0.5))
               (not (is_tired_to_degree AG ?f)) 
               )
    :time-required 1
    :value 3
    )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator walk.actual, AG walks from point ?x to point ?y on road ?z,  
;; with initial fatigue level ?f, assuming speed of one unit per time step.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq walk.actual 
	(make-op.actual :name 'walk.actual :pars '(?x ?dir ?f)
	:startconds '(
                  (is_facing AG ?dir)
                  (is_at AG ?x)  
              (is_tired_to_degree AG ?f)
              )
    :stopconds '(
    			 (is_at AG (nextPos? ?x ?dir)) 
                 )
    :deletes '((is_at AG ?x)
    		   (is_tired_to_degree AG ?f))
    :adds '((is_at AG (nextPos? ?x ?dir))
              (not (is_at AG ?x))
               (is_tired_to_degree AG (+ ?f 0.5))
               (not (is_tired_to_degree AG ?f)) 
               )
    )
)



; The following is from the gridworld-world.lisp file (for testing purposes right now):

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function answer_to_whq? returns a collection of well-formed formula(s) 
;; as the answer to the arg wff reflecting what are currently in AG's KB, 
;; under the closed world assumption. Arg wff is a wh-question that has 
;; variables prefixed with ? appearing in slots filled by wh-words.  
;; For example, if AG likes only APPLE1 and BANANA2 according to AG's KB,
;; then ((likes AG APPLE1) (likes AG BANANA2)) is returned as response to 
;; (answer_to_whq? '(likes AG ?wh)). If no answer is found, 
;; then '(not (knows (AG the-answer))) is returned.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun answer_to_whq? (wff)
	(check-whq-answer-in-kb 'NIL wff (state-node-wff-htable *curr-state-node*))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function answer_to_whq.actual? returns a collection of well-formed 
;; formula(s) as the answer to the arg wff reflecting what are currently in 
;; AG's KB, under the closed world assumption. Arg wff is a wh-question 
;; with variables prefixed with ? appearing in slots filled by wh-words.  
;; For example, if AG likes only APPLE1 and BANANA2 according to AG's KB,
;; ((likes AG APPLE1) (likes AG BANANA2)) is returned as the response to 
;; (answer_to_whq.actual? '(likes AG ?wh)), and ``AG likes APPLE1'' and ``AG likes 
;; BANANA2'' without double quotes are printed on two lines.  If no answer 
;; is found, '(not (knows (AG the-answer))) is returned and ``it is not the 
;; case that AG knows the answer'' without the double quotes is printed .
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun answer_to_whq.actual? (wff)
	(check-whq-answer-in-kb 'T wff (state-node-wff-htable *curr-state-node*))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator answer_user_whq, AG answers the wh-question ?q asked by 
;; USER.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq answer_user_whq 
	(make-op :name 'answer_user_whq :pars '(?q)
	:preconds '( (wants USER (that (tells AG USER (answer_to_whq ?q)))) )
	:effects '( (not (wants USER (that (tells AG USER (answer_to_whq ?q)))))
				(knows USER (that (answer_to_whq? ?q)))
			  )
	:time-required 1
	:value 10
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator answer_user_whq.actual, AG answers the wh-question ?q 
;; asked by USER.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq answer_user_whq.actual 
	(make-op.actual :name 'answer_user_whq.actual :pars '(?q)
	:startconds '( (wants USER (that (tells AG USER (answer_to_whq ?q)))) )
	:stopconds '( (not (wants USER (that (tells AG USER (answer_to_whq ?q))))) )
	:deletes '( (wants USER (that (tells AG USER (answer_to_whq ?q)))) )
	:adds	'( ;(knows USER (that (answer_to_whq.actual? ?q)))				
			   (says+to+at_time AG (that (answer_to_whq.actual? ?q)) USER (current_time?))
			   (not (wants USER (that (tells AG USER (answer_to_whq ?q)))))
			 )
	)
)
