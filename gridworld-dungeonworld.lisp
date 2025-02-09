(load "gridworld-worldmap.lisp")


; ===================================================================================================
;=================
; Building World =
;=================

(defvar *dungeonworld-points* '())
(defvar *dungeonworld-paths* '())

(let* ((main (def-room 'main 0 5 0 5))
       (main-points (car main))
       (main-paths (cadr main)))
  (setq *dungeonworld-points* main-points)
  (setq *dungeonworld-paths* main-paths))

(def-roadmap *dungeonworld-points* *dungeonworld-paths*)

; used to check if agent escaped
(defparameter *sim-over* nil)

;*******************
; Initialize Types *
;*******************
(def-object 'robot '(is_animate can_talk))
(def-object 'apple '(is_inanimate is_edible is_item))
(def-object 'box '(is_inanimate is_openable))
(def-object 'door '(is_inanimate is_accessible))
(def-object 'key '(is_inanimate is_item))


; Get list of all object types
(defparameter *obj-types* (remove-if #'(lambda (x) (member x '(knows tells))) (mapcar 'car (remove-duplicates (mapcar 'car *general-knowledge*)))))


;****************
; Place Objects *
;****************

(place-object 'door1@main 'door 'main&3&2 0 ;; like a door to go through a portal in the middle of a room
    nil
    '(
      )
    nil)

(place-object 'box1@main 'box 'main&0&0 0
  '((apple apple0@main))
  '(
    ) 
  nil)

(place-object 'box2@main 'box 'main&4&4 0
  nil
  '(
    ) 
  nil)

(place-object 'chest@main 'box 'main&3&2 0
    '((key key1@main))
    '(
      )
    nil)

(place-object 'apple1@main 'apple 'main&3&3 0 
	nil 
	'(
      (is_seeable apple1@main)
	 )
    nil 
)

(place-object 'apple2@main 'apple 'main&5&1 0 
	nil 
	'(
      (is_seeable apple2@main)
	 )
    nil 
)

(place-object 'apple3@main 'apple 'main&5&4 0 
	nil 
	'(
      (is_seeable apple3@main)
	 )
    nil 
)

(place-object 'apple4@main 'apple 'main&3&1 0 
	nil 
	'(
      (is_seeable apple4@main)
	 )
    nil 
)

#|
(place-object 'apple5@main 'apple 'main&3&2 0 
	nil 
	'(
      (is_seeable apple5@main)
	 )
    nil 
)
|#



;********
; Agent *
;********
(place-object 'AG 'robot 'main&3&2 0
 nil
 '(
   (is_facing AG SOUTH)
   (eyes_open AG)
   (can_see AG see@placeholder)

   (is_hungry_to_degree AG 2.0)
   (is_tired_to_degree AG 0.0)
  
   (not (has_won AG))
   (is_in key1@main chest@main)
   (is_hidden key1@main)

  )
 nil
)


;********************
; Include Operators *
;********************
(setq *operators* '(openDoor openContainer takeItem walk eat turn+north turn+south turn+west turn+east sleep answer_user_whq))

;*********************
; Define Search Beam *
;*********************
(setq *search-beam*
	(list (cons 5 *operators*) (cons 4 *operators*) (cons 3 *operators*) ))




; ======================================================================================================
;===================
; Defining Actions =
;===================




;*************
; Perception *
;*************

(defun saw? (pos dir)
 (let* ((sym-lst (split-regexp "&" (symbol-name pos)))
       (?room (intern (car sym-lst)))
       (x (parse-integer (cadr sym-lst)))
       (y (parse-integer (caddr sym-lst)))
       (b (bounds ?room))
       (ex (cadr b))
       (ey (cadddr b))
       (begin (cond 
                ((equal dir 'NORTH) y)
                ((equal dir 'SOUTH) 0)
                ((equal dir 'WEST)  0)
                ((equal dir 'EAST)  x)))
       (end (cond 
                ((equal dir 'NORTH) ey)
                ((equal dir 'SOUTH) y)
                ((equal dir 'WEST) x)
                ((equal dir 'EAST) ex)))
       objects
       )
   ;(format t "bounds: ~a~%" b)
   (setq objects (remove 'AG (loop for n from begin to end
         append
            (let* ((yp (if (or (equal dir 'NORTH) (equal dir 'SOUTH)) n y))
                   (xp (if (or (equal dir 'EAST) (equal dir 'WEST)) n x))
                   (plst (list ?room xp yp))
                   (point (intern (format nil "~{~a~^&~}" plst)))
                   (hval  (gethash (list 'is_at nil point) *world-facts*))
                   (nobjs (car hval))
                   (pred (cdr hval))
                   (objs (mapcar 'cadr pred)))
              objs))))
   (mapcar #'(lambda (o) (push o *visited-objects*)) objects)
   (setq *visited-objects* (remove-duplicates *visited-objects*))
   objects))

;******************
; Emotive Outputs *
;******************
(defun surprised? ()
  (format t "~%~%******************************~%AGENT SAYS:~%Whoa what's in here?~%******************************~%"))

(defun satisfied? ()
  (format t "~%~%******************************~%AGENT SAYS:~%Yum, I needed that!~%******************************~%"))

(defun rested? ()
   (format t "~%~%******************************~%AGENT SAYS:~%Ah that was a good nap!~%******************************~%"))

(defun happy? ()
   (format t "~%~%******************************~%AGENT SAYS:~%Wooohoo I'm free!~%******************************~%"))



;**************************
; Action Helper Functions *
;**************************

;Helper to see if two points are adjacent
(defun is_adjacent? (?x ?y)
  (equal 1
    (+
      (abs
        (-
          (parse-integer (cadr (split-regexp "&" (symbol-name ?x))))
          (parse-integer (cadr (split-regexp "&" (symbol-name ?y))))))
      (abs
        (-
          (parse-integer (cadr (cdr (split-regexp "&" (symbol-name ?x)))))
          (parse-integer (cadr (cdr (split-regexp "&" (symbol-name ?y))))))))
))

;Helper to test that ?x is ?dir of ?y (e.g. x is NORTH of y or similar)
(defun is_direction? (?dir ?x ?y)
  (cond
    ;((equal ?x ?y) NIL)
    ((equal ?dir 'NORTH)
      (and
        (equal 0
          (-
              (parse-integer (cadr (split-regexp "&" (symbol-name ?x))))
              (parse-integer (cadr (split-regexp "&" (symbol-name ?y))))))
        (< 0
          (-
              (parse-integer (cadr (cdr (split-regexp "&" (symbol-name ?x)))))
              (parse-integer (cadr (cdr (split-regexp "&" (symbol-name ?y)))))))))
    ((equal ?dir 'SOUTH)
      (and
        (equal 0
          (-
              (parse-integer (cadr (split-regexp "&" (symbol-name ?x))))
              (parse-integer (cadr (split-regexp "&" (symbol-name ?y))))))
        (> 0
          (-
              (parse-integer (cadr (cdr (split-regexp "&" (symbol-name ?x)))))
              (parse-integer (cadr (cdr (split-regexp "&" (symbol-name ?y)))))))))
    ((equal ?dir 'EAST)
      (and
        (< 0
          (-
              (parse-integer (cadr (split-regexp "&" (symbol-name ?x))))
              (parse-integer (cadr (split-regexp "&" (symbol-name ?y))))))
        (equal 0
          (-
              (parse-integer (cadr (cdr (split-regexp "&" (symbol-name ?x)))))
              (parse-integer (cadr (cdr (split-regexp "&" (symbol-name ?y)))))))))
    ((equal ?dir 'WEST)
      (and
        (> 0
          (-
              (parse-integer (cadr (split-regexp "&" (symbol-name ?x))))
              (parse-integer (cadr (split-regexp "&" (symbol-name ?y))))))
        (equal 0
          (-
              (parse-integer (cadr (cdr (split-regexp "&" (symbol-name ?x)))))
              (parse-integer (cadr (cdr (split-regexp "&" (symbol-name ?y))))))))))
  )

(defun checkKey? (?item ?h)
  (if (equal (subseq  (symbol-name ?item) 0 3) "KEY")
    20 ;; if the item is a key, give it 20 as value
    (if (equal (subseq  (symbol-name ?item) 0 5) "APPLE")
      (+ 0 ?h)
      4
)))

; if flag is T, then program exits
; otherwise just set *sim-over* to T
(defun terminate? (flag)
  (format t "~%The agent escaped!~%~%")
  (if flag
  (exit)
  (progn
    (setf *sim-over* T)
    T)))

(defun nextPos? (?curPos ?dir)
  ;;"return the next position after action"
  ;;(format t "pos: ~a dir: ~a ~%" ?curPos ?dir)
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

;**********
; Actions *
;**********

(setq turn+north
      (make-op :name 'turn+north :pars '(?dir ?f)
      :preconds '( (not (is_facing AG NORTH)) (is_facing AG ?dir) (is_tired_to_degree AG ?f) ) 
      :effects '( (is_facing AG NORTH)
                  (is_tired_to_degree AG (+ ?f 0.1))
                  (not (is_tired_to_degree AG ?f)) )
      :time-required 1
      :value '(- 2 ?f)
      )
)

(setq turn+north.actual 
	(make-op.actual :name 'turn+north.actual :pars '(?dir ?f)
	:startconds '( (not (is_facing AG NORTH)) (is_facing AG ?dir) (is_tired_to_degree AG ?f) )
    :stopconds '( (is_facing AG NORTH) )
	:deletes '( (is_facing AG ?dir) (not (is_facing AG NORTH)) 
              (is_tired_to_degree AG ?f) )
    :adds '( (is_facing AG NORTH) (not (is_facing AG ?dir))
           (is_tired_to_degree AG (+ ?f 0.1)) (not (is_tired_to_degree AG ?f)) )
	)
)

(setq turn+south
      (make-op :name 'turn+south :pars '(?dir ?f)
      :preconds '( (not (is_facing AG SOUTH)) (is_facing AG ?dir (is_tired_to_degree AG ?f)) )
      :effects '( (is_facing AG SOUTH)
                    (is_tired_to_degree AG (+ ?f 0.1))
                    (not (is_tired_to_degree AG ?f)) )
      :time-required 1
      :value '(- 2 ?f)
      )
)

(setq turn+south.actual 
	(make-op.actual :name 'turn+south.actual :pars '(?dir ?f)
	:startconds '( (not (is_facing AG SOUTH)) (is_facing AG ?dir) (is_tired_to_degree AG ?f) )
    :stopconds '( (is_facing AG SOUTH) )
	:deletes '( (is_facing AG ?dir) (not (is_facing AG SOUTH)) 
              (is_tired_to_degree AG ?f) )
    :adds '( (is_facing AG SOUTH) (not (is_facing AG ?dir))
           (is_tired_to_degree AG (+ ?f 0.1)) (not (is_tired_to_degree AG ?f)) )
	)
)

(setq turn+west
      (make-op :name 'turn+west :pars '(?dir ?f)
      :preconds '( (not (is_facing AG WEST)) (is_facing AG ?dir) (is_tired_to_degree AG ?f) )
      :effects '( (is_facing AG WEST)
                  (is_tired_to_degree AG (+ ?f 0.1))
                  (not (is_tired_to_degree AG ?f)) )
      :time-required 1
      :value '(- 2 ?f)
      )
)

(setq turn+west.actual 
	(make-op.actual :name 'turn+west.actual :pars '(?dir ?f)
	:startconds '( (not (is_facing AG WEST)) (is_facing AG ?dir)  (is_tired_to_degree AG ?f) )
    :stopconds '( (is_facing AG WEST) )
	:deletes '( (is_facing AG ?dir) (not (is_facing AG WEST))
              (is_tired_to_degree AG ?f) )
    :adds '( (is_facing AG WEST) (not (is_facing AG ?dir))
             (is_tired_to_degree AG (+ ?f 0.1)) (not (is_tired_to_degree AG ?f)) )
	)
)

(setq turn+east
      (make-op :name 'turn+east :pars '(?dir ?f)
      :preconds '( (not (is_facing AG EAST)) (is_facing AG ?dir) (is_tired_to_degree AG ?f) )
      :effects '( (is_facing AG EAST)
                  (is_tired_to_degree AG (+ ?f 0.1))
                    (not (is_tired_to_degree AG ?f)) )
      :time-required 1
      :value '(- 2 ?f)
      )
)

(setq turn+east.actual 
	(make-op.actual :name 'turn+east.actual :pars '(?dir ?f)
	:startconds '( (not (is_facing AG EAST)) (is_facing AG ?dir) (is_tired_to_degree AG ?f) )
    :stopconds '( (is_facing AG EAST) )
	:deletes '( (is_facing AG ?dir) (not (is_facing AG EAST)) 
              (is_tired_to_degree AG ?f) )
    :adds '( (is_facing AG EAST) (not (is_facing AG ?dir))
             (is_tired_to_degree AG (+ ?f 0.1)) (not (is_tired_to_degree AG ?f)) )
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



(setq takeItem
      (make-op :name 'takeItem :pars '(?pos ?dir ?item ?itemPos ?h)
      :preconds '(
        (is_item ?item)
        (is_at AG ?pos)
        (is_adjacent? ?pos ?itemPos)
        (is_at ?item ?itemPos)
        (is_facing AG ?dir)
        (is_direction? ?dir ?itemPos ?pos)
        (not (has AG ?item))
        (not (is_hidden ?item))
        (is_hungry_to_degree AG ?h)
                   )
      :effects '( (has AG ?item) )
      :time-required 1
      :value '(checkKey? ?item ?h)
      )
)

(setq takeItem.actual 
  (make-op.actual :name 'takeItem.actual :pars '(?pos ?dir ?item ?itemPos)
  :startconds '(
        (is_item ?item)
        (is_at AG ?pos) 
        (is_adjacent? ?pos ?itemPos)
        (is_at ?item ?itemPos) 
        (is_facing AG ?dir)
        (is_direction? ?dir ?itemPos ?pos)
        (not (has AG ?item))
        (not (is_hidden ?item))
                   )
  :stopconds '( (has AG ?item) )
  :adds '( (has AG ?item) )
  )
)



(setq openContainer
      (make-op :name 'openContainer :pars '(?pos ?dir ?obj ?objPos ?item)
      :preconds '(
        (is_openable ?obj)
        (not (is_open ?obj))
        (is_at AG ?pos)
        (is_facing AG ?dir)
        (is_adjacent? ?pos ?objPos)
        (is_at ?obj ?objPos)
        (is_direction? ?dir ?objPos ?pos)
        (is_in ?item ?obj)
      )
      :effects '( (is_open ?obj) (not (is_hidden ?item)) )
      :time-required 1
      :value 5
      )
)

(setq openContainer.actual 
  (make-op.actual :name 'openContainer.actual :pars '(?pos ?dir ?obj ?objPos ?item)
  :startconds '( 
        (is_openable ?obj)
        (not (is_open ?obj))
        (is_at AG ?pos)
        (is_facing AG ?dir)
        (is_adjacent? ?pos ?objPos)
        (is_at ?obj ?objPos)
        (is_direction? ?dir ?objPos ?pos)
        (is_in ?item ?obj)
                   )
  :stopconds '( (is_open ?obj) )
  :deletes '( (is_hidden ?item) )
  :adds '( (is_open ?obj) (not (is_hidden ?item)) (surprised?) )
  )
)



(setq openDoor
      (make-op :name 'openDoor
               :pars '(?pos ?key ?door)
               :preconds '(
                             (is_at AG ?pos)
                             (is_accessible ?door)
                             (is_at ?door ?pos)
                             (has AG ?key)
                             (key ?key)
                        )
               :effects '(
                          (has_won AG)
                          )
               :time-required 1
               :value 10
    )
)

(setq openDoor.actual 
      (make-op.actual 
        :name 'openDoor.actual
        :pars '(?pos ?key ?door)
        :startconds '(
                        (is_at AG ?pos)
                        (is_at ?door ?pos)
                        (is_accessible ?door)
                        (key ?key)
                        (has AG ?key)
                    )
        :stopconds '(
                        (has_won AG)
                    )
        :deletes '(
                    (has AG ?key)
                )
        :adds '(
                (has_won AG)
                (happy?)
                (terminate (terminate? nil))
                )
    )
)

; Helper function for server

(defun agent_direction ()
  (symbol-name (caddr (cadr (gethash '(is_facing AG nil) *world-facts*)))))

;*************************************
; Actions Modified From Example File *
;*************************************

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator walk, AG walks from point ?x to point ?y, with 
;; initial fatigue level ?f, assuming speed of one unit per time step.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    :value '(- 2 ?f)
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If hungry, at the same location ?y as is an is_edible food item ?x, and 
;; aware of the item being is_edible, then AG can eat the item to assuage his 
;; hunger ?h provided there is no fire or flood. Currently, food items are 
;; inexhaustible.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq eat 
  (make-op :name 'eat :pars '(?h ?x) ; level of hunger ?h
  :preconds '( (is_hungry_to_degree AG ?h) 
               (>= ?h 2.0)
               (is_edible ?x)
               (has AG ?x)
              )
  :effects '( (is_hungry_to_degree AG 0.0) (not(has AG ?x))
              (not (is_hungry_to_degree AG ?h)) )
  :time-required 1
  :value '(* 2 ?h)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If at the same location ?y as is an is_edible food item ?x and aware of 
;; the item being is_edible, and as long as he is hungry, then AG can eat the 
;; item to assuage his hunger ?h provided there is no fire or flood.
;; Currently, food items are inexhaustible.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq eat.actual 
  (make-op.actual :name 'eat.actual :pars '(?h ?x)
  :startconds '( (is_hungry_to_degree AG ?h) 
                 (>= ?h 2.0)
                 (is_edible ?x)
                 (has AG ?x)
                )
  :stopconds '( (is_hungry_to_degree AG 0.0) )
  :deletes '( (is_hungry_to_degree AG ?#1) (has AG ?x) )
  :adds '( (is_hungry_to_degree AG 0.0) (satisfied?) )
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator sleep, AG sleeps to relieve his fatigue ?f, but experiences 
;; an increase in his hunger ?h.
;; This is the `model' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq sleep 
  (make-op :name 'sleep :pars '(?f ?h) ; level of fatigue ?f 
                                         ; {0, 0.5, 1.0, 1.5, ...}
                                         ; similarly for hunger ?h
    :preconds '((is_tired_to_degree AG ?f)
                (>= ?f 2.5);(>= ?f 0.5)
                (is_hungry_to_degree AG ?h)
                (> ?f ?h) ; more tired than hungry
                )
    :effects '( (is_tired_to_degree AG 0.0)
                (not (is_tired_to_degree AG ?f))
                (is_hungry_to_degree AG (+ ?h (* 0.5 ?f))) )
    :time-required '(* 4 ?f)
    :value '(* 1 ?f)
    )
)
                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With operator sleep.actual, AG sleeps to relieve his fatigue ?f, but 
;; experiences an increase in his hunger ?h.
;; This is the `actual' version.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq sleep.actual 
  (make-op.actual :name 'sleep.actual :pars '(?f ?h) ; level of fatigue ?f 
                                                     ; level of hunger ?h
    :startconds '((is_tired_to_degree AG ?f)
                  (>= ?f 2.5)
                  (is_hungry_to_degree AG ?h)
                  (> ?f ?h) ); more tired than hungry
    :stopconds '( (is_tired_to_degree AG 0.0) )
    :deletes '((is_tired_to_degree AG ?#1) 
               (is_hungry_to_degree AG ?#2) )
    :adds '((is_tired_to_degree AG (- ?f (* 0.5 (elapsed_time?))))
            (is_hungry_to_degree AG (+ ?h (* 0.25 (elapsed_time?)))) (rested?) ) 
    )
)


;*******************************************
; Actions Taken Directly From Example File *
;*******************************************

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
