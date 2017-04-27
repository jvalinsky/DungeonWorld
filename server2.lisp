(in-package cl-user)
(ql:quickload '(:ningle :clack :yason))
(load "init_web.lisp")


(defpackage server
  (:use :cl
        :ningle
        :yason)
  (:import-from cl-user go_web! listen! get-points split-regexp parse-integer read-from-string eval get-obj-types get-obj-type points-to-htable world-pts get-object-locations)
  (:import-from :lack.response
                :response-headers
                :response-body)
  (:import-from :yason
                :parse))

(in-package :server)

(defvar *app* (make-instance 'ningle:<app>))

(defun render-json (object)
  (let ((s nil))
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (setf (getf (response-headers *response*) :charset) "utf-8")
  (with-output-to-string (s)
    (yason:encode object s))))

(setf (ningle:route *app* "/gridworld-points" :method :GET)
      #'(lambda (params)
          (declare (ignore params))
          (render-json (points-to-htable (world-pts)))))

(setf (ningle:route *app* "/eval" :method :POST)
      #'(lambda (params)
          (let* ((command (cdr (assoc "command" params :test #'equal)))
                (output (eval (read-from-string command))))
          (format t "~a~%" command)
          output)))

(setf (ningle:route *app* "/object-locations" :method :GET)
      #'(lambda (params)
          (declare (ignore params))
          (render-json (object-locations))))

(setf *app* (lack:builder
 (:static :path (lambda (path)
                  (format t "path requested: ~a~%" path)
                  (cond
                    ((equal "/" path) "/index.html")
                    ((equal "/index" path) "/index.html")
                    ((equal "/gridworld-points" path) nil)
                    ((equal "/eval" path) nil)
                    (t path)))
          :root "./public/")
 :accesslog
 :session
 :backtrace
 *app*))

(clack:clackup *app*)

