(in-package :lark)

(defvar *started* nil)

(defun start-engine ()
  (unless *started*
    (setf *started* t)
    (cepl:repl)))

(defun stop-engine ()
  ;; gotta free stuff here
  (cepl:quit))

(defparameter *step-func-name* '%%step)
(defparameter *running-var-name* '%%running)

;; {TODO} put this in main loop eventually
(defvar *render-pass* nil)

(defmacro defgame-loop (name (&key startup-function) &body body)
  (let ((run-symb (cepl-utils:symb :run-loop))
        (stop-symb (cepl-utils:symb :stop-loop)))
    `(progn
       (defvar ,*running-var-name* nil)

       (defun ,*step-func-name* ()
	 ,@body)

       (defun ,stop-symb () (setf ,*running-var-name* nil))

       (evt:def-named-event-node %%sys-listener (e evt:|sys|)
	 (when (typep e 'evt:will-quit) (,stop-symb)))

       (defun ,run-symb ()
	 (if *started*
	     (if ,*running-var-name*
		 (print "already running")
		 (let ((main-loop-stepper (temporal-functions:make-stepper
					   (seconds (/ 1.0 60.0))))
		       (swank-stepper (temporal-functions:make-stepper
				       (seconds (/ 1.0 10.0)))))
		   (format t "-starting-")
		   ,(when startup-function `(funcall ,startup-function))
		   (setf ,*running-var-name* t)
		   (unwind-protect
			(loop :while ,*running-var-name* :do
			   ;; update swank
			   (when (funcall swank-stepper)
			     (live:continuable (cepl::update-swank)))
			   ;; update event system
			   (live:continuable (evt:pump-events))
			   ;; update temporal pool
			   (ttm:update)
			   ;; run step function
			   (when (funcall main-loop-stepper)
			     (live:continuable (funcall #',*step-func-name*)))
			   ;; run all entity component system code (except rendering)
			   (hasty:step-hasty)
			   ;; run render pass
			   (hasty:run-pass *render-pass*)
			   )
		     (setf ,*running-var-name* nil))
		   (print "-shutting down-")))
	     (format t "Lark: Cannot run loop as engine has not been started."))
	 ',name))))
