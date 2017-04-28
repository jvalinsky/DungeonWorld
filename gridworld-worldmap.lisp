; returns a list of points for a room
; HELPER FUNCTIONS FOR BUILDING MAP
; symbol format ?room&?x&?y
;(require :regexp2)

(defparameter *room-facts* (make-hash-table :test #'equal))


(defun width (?room)
     (caddr (cadr (gethash (list 'width ?room nil) *room-facts*))))

(defun depth (?room)
     (caddr (cadr (gethash (list 'depth ?room nil) *room-facts*))))

(defun bounds (?room)
     (caddr (cadr (gethash (list 'bounds ?room nil nil nil nil) *room-facts*))))

(defun get-points ()
  (gethash (list 'point nil) *roadmap-knowledge*))

(defun room-pts-rect (?room ?x1 ?x2 ?y1 ?y2)
  (loop for x from ?x1 to ?x2
                     append
                     (loop for y from ?y1 to ?y2
                           collect (intern (format nil "~{~a~^&~}" (list ?room x y))))))

; Helper functions for server, they return either strings or hashtables that are converted
; to json by the server
(defun get-obj-type (object)
   (symbol-name (gethash object *object-name-types*)))

; list (x,y) of position of object
(defun get-object-location (object)
  (let* ((pt (caddr (cadr (gethash (list 'is_at object nil) *world-facts*))))
        (ptlst (split-regexp "&" (symbol-name pt)))
        (x (parse-integer (cadr ptlst)))
        (y (parse-integer (caddr ptlst))))
    (list x y)))

; get info on object hashtable of name, type, location, and if AG the direction it is facing
(defun get-object (name)
  (let* ((obj (make-hash-table :test #'equal))
        (obj-type (get-obj-type name))
        (loc (get-object-location name))
        (x (car loc))
        (y (cadr loc)))
    (setf (gethash "name" obj) (symbol-name name))
    (setf (gethash "x" obj) x)
    (setf (gethash "y" obj) y)
    (setf (gethash "type" obj) obj-type)
    obj))

(defun get-objs ()
  (let ((objs (make-hash-table :test #'equal))
        (objlst (mapcar #'get-object *object-names*)))
    (setf (gethash "data" objs) objlst)
    objs))

; convert point symbol to hashtable with room, x, and y keys
(defun point-to-htable (point)
  (let* ((plist (split-regexp "&" (symbol-name point)))
        (room (car plist))
        (x (parse-integer (cadr plist)))
        (y (parse-integer (caddr plist)))
        (data (make-hash-table :test 'equal))
        (pobj (make-hash-table :test 'equal)))
    (setf (gethash "room" pobj) room)
    (setf (gethash "x" pobj) x)
    (setf (gethash "y" pobj) y)
    pobj))

; modifies hashtable to add point under key for its room
; a point is a hashtable with x and y keys
(defun points-to-htable-helper (point htable)
  (let* ((plist (split-regexp "&" (symbol-name point)))
        (room (car plist))
        (x (parse-integer (cadr plist)))
        (y (parse-integer (caddr plist)))
        (pobj htable)
        (data (make-hash-table :test #'equal)))
    (setf (gethash "x" data) x)
    (setf (gethash "y" data) y)
    (setf (gethash room pobj) (append (gethash room pobj) (list data)))))

; get big hash table of hash table of points where keys are the different rooms
(defun points-to-htable (points)
  (let ((psobj (make-hash-table :test #'equal)))
    (mapcar #'(lambda (p) (points-to-htable-helper p psobj)) points)
    psobj))

; get all points in world
(defun world-pts ()
  (mapcar #'cadr (cdr (get-points))))

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
          (setf ?lst (append (list (list (intern (concatenate 'string (symbol-name hd) "$" (symbol-name e))) hd 1 e)) ?lst)))) 
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
    (add_tuple_to_hashtable (list 'bounds ?room (list ?x1 ?x2 ?y1 ?y2) ) *room-facts* nil)
    (list points edges)))
