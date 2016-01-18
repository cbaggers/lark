(in-package :lark)

(defvar *started* nil)

(defun start-engine ()
  (setf *started* t))

(defun stop-engine ()
  ;; gotta free stuff here
  (cepl:quit))

(defparameter *step-func-name* '%%step)
(defparameter *running-var-name* '%%running)

(defmacro defgame-loop (name (&key startup-function) &body body)
  (let ((run-symb (cepl-utils:symb :run-loop))
        (stop-symb (cepl-utils:symb :stop-loop)))
    `(progn
       (defvar ,*running-var-name* nil)

       (defun ,*step-func-name* ()
	 ,@body)

       (defun ,stop-symb () (setf ,*running-var-name* nil))

       (let ((quit-listener (evt:make-event-node
                             :subscribe-to evt:|sys|
                             :filter (lambda (e) (typep e 'evt:will-quit))
                             :body (,stop-symb))))
         (defun ,run-symb ()
           (if ,*running-var-name*
	       (print "already running")
	       (let ((main-loop-stepper (temporal-functions:make-stepper
					 (seconds (/ 1.0 60.0))))
		     (swank-stepper (temporal-functions:make-stepper
				     (seconds (/ 1.0 4.0)))))
		 (format t "-starting-")
		 (funcall ,startup-function)
		 (setf ,*running-var-name* t)
		 (loop :while ,*running-var-name* :do
		    ;; update swank
		    (when (funcall swank-stepper)
		      (live:continuable (update-swank)))
		    ;; update event system
		    (live:continuable (evt:pump-events))
		    ;; update temporal pool
		    (ttm:update)
		    ;; run step function
		    (when (funcall main-loop-stepper)
		      (live:continuable (funcall ,*step-func-name*)))
		    ;; run all entity component system code (except rendering)
		    (hasty:step-hasty)
		    ;; run render pass
		    ;; (hasty:run-pass *render-pass*)
		    )
		 (print "-shutting down-")))
           ',name)))))
