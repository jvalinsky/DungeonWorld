(in-package cl-user)
(ql:quickload '(:websocket-driver :clack) :silent t)
(load "init_web.lisp")


(defpackage ltest
  (:use :cl
        :websocket-driver)
  (:import-from cl-user go_web! listen! get-points split-regexp))

(in-package :ltest)


(defvar *app* nil) 

(defparameter *handler*
  (intern (string-upcase (or (asdf::getenv "CLACK_HANDLER") "hunchentoot")) :keyword))

(defvar *echo*
  (lambda (env)
       (let* ((ws (make-server env)) (pts (get-points)) (npts (car pts)) (points (mapcar #'cadr (cdr pts))))
         (setq points (mapcar #'(lambda (p) (split-regexp "&" (symbol-name p)) ) points))
         (setq points (mapcar #'(lambda (p) (format nil "[\"~a\", ~a, ~a]" (car p) (cadr p) (caddr p)) ) points) )
         (on :connect ws
             (lambda (x)
               (send ws "hi")

               ))
         (on :message ws
             (lambda (message)
               (cond
                 ((equal message "num-points")
                  (send ws (format nil "number of points: ~a" npts)))

                ((equal message "points")
                 (send ws (format nil "\{ \"points\": [ ~{ ~a ~^,~} ] \}" points)))
                ((equal message "go") (send ws (go_web!)))
                (t (send ws message)))))
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
