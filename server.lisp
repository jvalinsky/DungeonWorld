; we load our gridworld code so it is in the cl-user
; package namespace, otherwise our code/gridworld in general
; doesn't make use of the common lisp packaging system
(in-package cl-user)
; We use quicklisp to handle external library depenencies
; This is how we load those depenencies
; ningle is a web framework with support for routing
; clack is the underlying http server library it depends on
; clack has a middleware library it imports called lack
; that we use for static file serving
; yason is for json encoding/decoding
(ql:quickload '(:ningle :clack :yason))
(load "init.lisp")

; External libraries use common lisp package system
; so the following create a package called server and
; exposes all the relevant functions to the package from
; our gridworld code and external libraries
(defpackage server
  (:use :cl
        :ningle
        :yason)
  ; This is so appropriate gridworld code is exposed to this package
  (:import-from cl-user 
                go! 
                listen!
                get-points 
                split-regexp 
                parse-integer 
                read-from-string 
                eval 
                get-obj-types 
                get-obj-type 
                points-to-htable 
                world-pts get-objs
                *sim-over*
                agent_direction)
  (:import-from :lack.response
                :response-headers
                :response-body)
  (:import-from :yason
                :parse))

(in-package :server)

(defvar *app* (make-instance 'ningle:<app>))


; Helper function to render json for the client
; takes a hashtable, string, or list and returns
; a json string while also setting the headers for the http response
(defun render-json (object)
  (let ((strstream (make-string-output-stream)))
    ; Set headers for HTTP response for json and proper text encoding
    (setf (getf (response-headers *response*) :content-type) "application/json")
    (setf (getf (response-headers *response*) :charset) "utf-8")
    ; takes a hashtable, list, bool, or string and converts to equivalent
    ; JSON data type (object, array, string, etc)
    ; It takes a stream so we give it a string stream and then 
    ; get the string from the stream
    (yason:encode object strstream)
    (get-output-stream-string strstream)))

; Route Handlers (the following return json)
; ===============================================================================

; This route returns json for the (x,y) coordinates of all points in our world
; and what room(s) they are in
(setf (ningle:route *app* "/gridworld-points" :method :GET)
      #'(lambda (params)
          (declare (ignore params))
          (render-json (points-to-htable (world-pts)))))

(setf (ningle:route *app* "/agent_direction" :method :GET)
      #'(lambda (params)
          (declare (ignore params))
          (render-json (agent_direction))))

; Not the safest method, but this basically takes a string from the server
; and interprets it as a common lisp command then runs it
; probably very easy to crash server if one wanted to
; this was implemented for testing new code
(setf (ningle:route *app* "/eval" :method :POST)
      #'(lambda (params)
          ; get command string from "command" parameter of POST request
          (let* ((command (cdr (assoc "command" params :test #'equal)))
                ; convert string to lisp code and eval
                (output (eval (read-from-string command))))
          (format t "~a~%" command)
          output)))

; Calls the go! macro and returns the as a string what go! prints to stdout
; safer to use instead of eval
(setf (ningle:route *app* "/go" :method :GET)
      #'(lambda (params)
          (declare (ignore params))
           (go! T)))

; Calls modified listen! macro with parameter from POST request
; safer to use instead of eval
(setf (ningle:route *app* "/listen" :method :POST)
      #'(lambda (params)
          ; get listen parameter string from "questions" parameter of POST request
          (let* ((listen-args (cdr (assoc "questions" params :test #'equal)))
                ; convert string to lisp code and feed to modified listen! macro
                (output (listen! listen-args T)))
          output)))

; Get info on all objects in world (name, type, location) 
(setf (ningle:route *app* "/objects" :method :GET)
      #'(lambda (params)
          (declare (ignore params))
          (render-json (get-objs))))

; reload Gridworld 
(setf (ningle:route *app* "/restart" :method :GET)
      #'(lambda (params)
          (declare (ignore params))
          ; Easter Egg prints quote from Asimov short story
          (format t "~%~%And it came to pass that AC learned how to reverse the direction of entropy.~%But there was now no man to whom AC might give the answer of the last question. No matter. The answer -- by demonstration -- would take care of that, too.~%For another timeless interval, AC thought how best to do this. Carefully, AC organized the program.~%The consciousness of AC encompassed all of what had once been a Universe and brooded over what was now Chaos. Step by step, it must be done.~%And AC said, 'LET THERE BE LIGHT!'~%~%And there was light ~%-- from The Last Question by Isaac Asimov~%")
          (load "init.lisp")
          "gridworld reloaded"))

(setf (ningle:route *app* "/is_over" :method :GET)
      #'(lambda (params)
          (declare (ignore params))
          (let ((is_over *sim-over*))
          (render-json is_over))))


; Middleware for server to serve static files in public/ folder 
; Takes a ningle app and returns a modified ningle app with new routes
; ===============================================================================
(setf *app* (lack:builder
; Create routes for static files in public/
 (:static :path (lambda (path)
                  (format t "path requested: ~a~%" path)
                  (cond
                    ((equal "/" path) "/index.html")
                    ((equal "/index" path) "/index.html")
                    ; If route is of any of the special ones
                    ; we have to send data to client then
                    ; return nil and let our custom handlers above
                    ; handle the route
                    ((equal "/gridworld-points" path) nil)
                    ((equal "/eval" path) nil)
                    ((equal "/objects" path) nil)
                    ((equal "/restart" path) nil)
                    ((equal "/is_over" path) nil)
                    ((equal "/listen" path) nil)
                    ((equal "/go" path) nil)
                    ((equal "/agent_direction" path) nil)
                    (t path)))
          :root "./public/")
 :accesslog
 :session
 :backtrace
 *app*))


; Starts webserver
(clack:clackup *app*)

