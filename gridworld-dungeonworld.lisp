(load "gridworld-worldmap.lisp")
; ===================================================================================================
; Building world

(defvar *dungeonworld-points* '())
(defvar *dungeonworld-paths* '())

(let* ((main (def-room 'main 0 10 0 10))
       (main-points (car main))
       (main-paths (car (car main))))
  (append *dungeonworld-points* main-points)
  (append *dungeonworld-paths* main-paths))

(def-roadmap *dungeonworld-points* *dungeonworld-paths*)


(def-object 'robot '(is_animate can_talk))
(def-object 'apple '(is_inanimate is_edible (has_cost 3.0)))

(place-object 'apple1@main 'apple 'main&1&5 0 
	nil 
	'((is_edible apple1@main) 
	 )
    nil 
)

(place-object 'apple2@main 'apple 'main&7&5 0 
	nil 
	'((is_edible apple2@main) 
	 )
    nil 
)

(place-object 'apple3@main 'apple 'main&5&1 0 
	nil 
	'((is_edible apple3@main) 
	 )
    nil 
)

(place-object 'apple4@main 'apple 'main&5&6 0 
	nil 
	'((is_edible apple4@main) 
	 )
    nil 
)

(place-object 'apple5@main 'apple 'main&5&6 0 
	nil 
	'((is_edible apple5@main) 
	 )
    nil 
)

(place-object 'AG 'robot 'main&5&5 0
 nil
 '(
   (is_facing AG SOUTH)
   (is_at AG main&5&5)
   (eyes_open AG)
   (can_see AG (apple3@main apple1@main))
  )
 nil
)



; (find-location 'AG *world-facts*)
(setq *operators* '(turn+north turn+south turn+west turn+east answer_user_whq ))
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
