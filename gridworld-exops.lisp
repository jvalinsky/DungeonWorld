;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This procedure handles the external spontaneous (rain or fire) operator 
;; named by the given op-name.  If the given arg conds-checked is true, 
;; then the startconds of the given operator already are true of the world 
;; and need not be checked again.  Otherwise, the startconds need be 
;; checked.  The startconds must be true in order to execute the operator.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun handleSee (op-name conds-checked)
	(let*	(
             (op (eval op-name))
			 (startconds (op.actual-startconds op))
			 (adds (op.actual-adds op))
			 (deletes (op.actual-deletes op))
			 (stopconds (op.actual-stopconds op))
			 (bindings 'NIL) instances state-nodes implied-facts record
			 children ; as a list of (action . state) pairs 
			 event
			 evaledStartConds
             action
             ;(action (eval action-name))
             (pars (op.actual-pars op))
             par-values
             (par 'NIL)
             (par-value 'NIL)
             state
             (unifiers (all-bindings-of-goals-to-fact-htable startconds *world-facts* (state-node-terms *curr-state-node*)))
             inst-op
			)
            (setq inst-op (instantiate-op op (car unifiers)))
            (setq action (eval inst-op))
            ;(format t "what uni: ~a ~%" unifiers)
            ;(format t "what op: ~a ~%" action)
            (setq par-values (op.actual-pars action))
            ;(format t "what op-pars: ~a ~%" pars)
            ;(format t "what op-pars: ~a ~%" par-values)
            
        (while pars
           ;(block continue
           (setq par (pop pars))
           (setq par-value (pop par-values))
           ;(when (not par-value)
            ; return-from continue)
           (setq adds (subst par-value par adds))
           (setq deletes (subst par-value par deletes));)
           ;(setq stopconds.actual (subst par-value par stopconds)))
        )

(format t "~% handleSee ~a ~a" (op.actual-name (eval op-name)) conds-checked)		
		
    (if	(eq 'T conds-checked)
			(setq bindings '(T))
			(progn ;prog2
				(setq evaledStartConds (mapcar #'(lambda (x) (evalFunctionPredicateExt x)) startconds))
(format t "~%evaledStartConds = ~a~%" evaledStartConds)
				(when (eq 'NIL
				 		(or 
				 			(eq 'T (eval (cons 'memb (list (quote 'UNKNOWN) (list 'quote evaledStartConds)))))
							(eq 'T (eval (cons 'memb (list (quote 'NIL) (list 'quote evaledStartConds)))))
						)
					  )
					(setq bindings '(T))
				)
			)
		)
		
        ; Execute the operator only if its startconds are true of the world.
		(when (equal bindings '(T))
(format t " on~%")
			(setq instances 
					(mapcar #'(lambda (u) (instantiate-op.actual op u)) bindings))
			(setq state-nodes (mapcar #'(lambda (i) 
					(generate-ext-op-state-node i *curr-state-node*)) instances))
			(setq children (mapcar #'cons instances state-nodes))
			(setf (state-node-children *curr-state-node*)  children)
			(setf (state-node-operators *curr-state-node*) (list op-name))
			(setf (state-node-forward-value *curr-state-node*) 
					(inclusive-value (car children)))
			
			(setq *curr-state-node* (eval (cdar children)))

			
;(format t "~a~%" adds)
            (setq adds (mapcar #'simplify-value adds))

			(setq deletes (mapcar #'simplify-value deletes))
;(format t "~a~%" adds)
;(format t "~a~%" deletes)
			(setq deletes (set-differencef deletes adds))
			(remove_list_of_tuples_from_hashtable deletes *protected-facts* 'NIL)
			(add_list_of_tuples_to_hashtable adds *protected-facts* 'NIL)
			
;(format t "~% adds: ~a~%." adds)			
			(setq *world-facts* (all-inferences *protected-facts* 
					*general-knowledge* *inference-limit*))
			(add_htable_to_hashtable *protected-facts* *world-facts* 'NIL)
			
			; Modify *curr-state-node* to update the agent's beliefs in light of the
			; locally evident facts (and beliefs that have evidently become false).
			(setq new-state (copy_construct_hashtable (notice-new-local-facts *curr-state-node*)))
			(setq event (cons (caar children) new-state))
			
			; Place this external event logged with the current real time of the 
			; simualted world in the queue of active events in the simulated world.
			(setq *event-queue* (append *event-queue* 
						(list (cons event *real-clock*))))
			
			; Log this external event with the current real time of the simulated 
			; world in *real-history* and *AG-history*.
			(push (list event *real-clock*) *real-history*)
			(push (list event *AG-clock*) *AG-history*)
			(setq *states* (cons new-state (cdr *states*)))
		)
	)
); end of handleSee

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This procedure handles the external spontaneous (rain and fire) 
;; operators.  If an active external event is already in *event-queue*, 
;; its termination conditions (both normal and abnormal) are checked to 
;; see if it can be extended for another time step, and it is extended for 
;; another time step only if its none of its termination conditions hold 
;; true in the world KB.  On the other hand, if an external operator is 
;; not currently an active event in *event-queue*, its startconds are 
;; checked to see if it can be instantiated as an active event, and it is  
;; instantiated as an active event in *event-queue* only if its startconds 
;; hold true in the world KB.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Daphne: Revised 10/18/2012 to handle new representation of terms
; Daphne: Revised Dec. 2009 to handle new representation of wff-htable
(defun handleExtOps-Dungeon ()
	(let	(
             (is-see 'NIL) 
             ;(is-fire 'NIL) 
			 ;(is-rain 'NIL) 
			 ;(is-fire-handled 'NIL)
			 ;(is-rain-handled 'NIL)
			 (is-see-handled 'NIL)
			 (is-abn 'NIL) 
			 (is-terminated 'NIL) op name new-terms 
			 (new-wff-htable (state-node-wff-htable *curr-state-node*))
			 ststopconds stopconds adds deletes stadds stdeletes queue-length
			)

		(setq queue-length (length  *event-queue*))

		; Process the currently active external events in *event-queue* one at 
		; at time, extending an event for another time step only if none of 
		; its termination conditions are true in the world KB.
		(dotimes (i queue-length)
			(setq op (caar (pop *event-queue*)))
			(setq name (op.actual-name (eval op)))
(format t "~%HEOEventName ~a ~a" name (length *event-queue*))
			(setq adds (op.actual-adds (eval op)))
			(setq deletes (op.actual-deletes (eval op)))
			(setq stadds (op.actual-starredAdds (eval op)))
			(setq stdeletes (op.actual-starredDeletes (eval op)))
			(setq ststopconds (op.actual-starredStopConds (eval op)))
			(setq stopconds (op.actual-stopconds (eval op)))
			(setq is-abn 'NIL)
			(setq is-terminated 'NIL)

            ;(format t "adds: ~a ~%" adds)
            (if (equal name 'see.actual)
				(progn 
					(setq is-see 'T)
					(setq is-see-handled 'T)
				)
            )

			; Evaluate whether the termination conditions of the current 
			; active event are true in the world KB.
			(if (eq 'T 
							(eval (cons 'memb (list (quote 'T) 
							(list 'quote (mapcar #'(lambda (x) (evalFunctionPredicateExt x)) ststopconds))
							)))
					)
					(setq is-abn 'T)
					(when (eq 'T 
								(eval (cons 'memb (list (quote 'T) 
								(list 'quote (mapcar #'(lambda (x) (evalFunctionPredicateExt x)) stopconds))
								)))
								)
								(setq is-terminated 'T)
					)
			)
			
			; Handle starred adds and starred deletes for the case where 
			; some of the termination conditions are true in the world KB.
			(if (or (eq 'T is-terminated) (eq 'T is-abn))
				(progn; start of then clause of applying starredAdds and starredDeletes
					; Insert into history as completed. (maybe to-do?)
					; Update the world KB and the agent's KB with starred 
					; adds and starred deletes.
(format t "~% HEOExt ~a Is Terminated." name)			
					(setq stadds (mapcar #'simplify-value stadds))
					(setq stdeletes (mapcar #'simplify-value stdeletes))
					(setq stdeletes (set-differencef stdeletes stadds))
					
					(remove_list_of_tuples_from_hashtable stdeletes *protected-facts* 'NIL)
					(add_list_of_tuples_to_hashtable stadds *protected-facts* 'NIL)
					(setq *world-facts* (all-inferences *protected-facts* 
								*general-knowledge* *inference-limit*))
					(add_htable_to_hashtable *protected-facts* *world-facts* 'NIL)
					
					(setq new-terms (state-node-terms *curr-state-node*))
					(setq new-wff-htable (state-node-wff-htable *curr-state-node*))
					(setq new-terms	(remove_term_list_from_term_list
						(remove_list_of_tuples_from_hashtable stdeletes new-wff-htable 'T)
						new-terms)
					)
					(setq new-terms (merge_term_list_with_term_list
						(add_list_of_tuples_to_hashtable stadds new-wff-htable 'T)
						new-terms)
					)
					
					(setf (state-node-terms *curr-state-node*) new-terms)	
					; Modify *curr-state-node* to update the agent's beliefs 
					; in light of the locally evident facts (and beliefs that 
					; have evidently become false).
					(setq new-state (copy_construct_hashtable (notice-new-local-facts *curr-state-node*)))
					(setq *states* (cons new-state (cdr *states*)))		
				); end of then clause of applying starredAdds and starredDeletes
				
				; Extend the current active event for another time step since 
				; none of its termination conditions are true in the world KB.

                (if	(eq 'T is-see)
					(handleSee see.actual 'T)
				)
				
			); end of if-then-else clause for 
			
		); end of processing active external events in *event-queue*

		(when (eq 'NIL is-see-handled)
			(handleSee see.actual 'NIL)
		)

	)
); end of handleExtOps-Dungeon
