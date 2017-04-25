(in-package cl-user)
(ql:quickload '(:websocket-driver :clack) :silent t)
(load "init_web.lisp")


(defpackage ltest
  (:use :cl
        :websocket-driver)
  (:import-from cl-user go_web! listen!))

(in-package :ltest)


(defvar *app* nil) 

(defparameter *handler*
  (intern (string-upcase (or (asdf::getenv "CLACK_HANDLER") "hunchentoot")) :keyword))

(defvar *echo*
  (lambda (env)
       (let ((ws (make-server env)))
         (on :connect ws
             (lambda (x)
               (send ws "hi")))
         (on :message ws
             (lambda (message)
               (if (equal message "go") (send ws (go_web!))
               (send ws message))))
         (lambda (responder)
           (declare (ignore responder))
           (start-connection ws)))))
      
(setf *app* (lack:builder
 (:static :path (lambda (path)
                  (format t "path requested: ~a~%" path)
                  (cond
                    ((equal "/" path) "/index.html")
                    ((equal "/index" path) "/index.html")
                    ((equal "/echo" path) nil)
                    (t path)))
          :root "./public/")
 :accesslog
 :session
 :backtrace
 *echo*))

(clack:clackup *app* :server *handler* :port 8080)
