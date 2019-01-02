;;;; repl.lisp
;;
;;;; Copyright (c) 2018 Lucas Vieira


(in-package #:diophantine)

(defun debrief (suite)
  (format t "Generation: ~a~&Fitness: ~a~&Equation: ~a~&Genome Range: ~a~&"
	  (generation suite)
	  (calc-fitness (equation suite) (car (fittest suite)))
	  (equation-format (equation suite))
	  (gen-range suite)))

(defparameter *debrief-threshold* 10000000)

(defun run-genetic-algorithm (equation)
  (let ((suite (make-instance 'solution-suite :equation equation)))
    (format t "Initializing population...~&")
    (init-population suite)
    (format t "Attempting a solution...~&")
    (loop named genetic-loop
       with i = 0
       with last-fitness = (best-fitness suite)
       with debrief-threshold = *debrief-threshold*
       when (zerop last-fitness)
       do (return-from genetic-loop)
       else do (progn (crossover suite)
		      (let ((best-fitness (best-fitness suite)))
			(if (= last-fitness best-fitness)
			    (incf i)
			    (setf last-fitness best-fitness
				  i 0)))
		      (when (zerop (mod (generation suite) 50000))
			(format t "~%Status report~&")
			(debrief suite))
		      (when (> i debrief-threshold)
			(format t "~%Partial debrief~&")
			(debrief suite)
			(if (y-or-n-p "Iteration limit reached. Keep trying?")
			    (setf i 0
				  debrief-threshold
				  (+ debrief-threshold (truncate
							(* debrief-threshold 0.1))))
			    (return-from genetic-loop)))))
    (format t "~%Final debrief:~&")
    (debrief suite)
    (format t "Best answer found: ~a~&"
	    (format-best-answer suite))
    (and (y-or-n-p "Show population?")
	 (let ((i 1))
	   (mapc (lambda (answer)
		   (format t "Answer #~a: ~a~&"
			   i (format-answer suite answer))
		   (incf i))
		 (population suite))))))

	 
	 
		 

(defun ask-for-equation ()
  (labels ((prompt (query) (princ query) (finish-output) (read-line)))
    (loop for input = (prompt "Please enter an equation with a valid format: ")
       until (string= input "q")
       do (handler-bind ((parse-error
			  #'(lambda (e)
			      (declare (ignore e))
			      (invoke-restart 'ignore-equation-parsing))))
	    (restart-case
		(let ((dismembered-equation (parse-equation input)))
		  (format t "You've entered the equation ~a~&"
			  (equation-format dismembered-equation))
		  (run-genetic-algorithm dismembered-equation))
	      (ignore-equation-parsing ()
		(format t "Invalid equation. Check the syntax.~&")))))))
