;;;; solver.lisp
;;
;;;; Copyright (c) 2018 Lucas Vieira


(in-package #:diophantine)

(defparameter *population-size* 100)
(defparameter *rnd-state* (make-random-state))
(defparameter *initial-range* 5)

(defun calc-fitness (equation solution)
  "Calculates fitness for a solution in a given equation. The fitness is the
absolute difference between each sides of the equation (in other words, how
distant the solution is from zero). Less fitness is better.
Yields the fitness and a pair containing the values for left-hand side and
right-hand side."
  (labels ((calc-side (eq-side sol-side)
	     (loop for var-info in eq-side
		for var in sol-side
		;; Anything to the power of 0 is 1
		when (zerop (cdr var-info))
		sum (car var-info)
		;; Anything to the power of 1 is itself
		else when (= 1 (cdr var-info))
		sum (* (car var-info) var)
		;; Else use expt
		else sum (* (expt var (cdr var-info))
			    (car var-info)))))
    (let ((lhs (calc-side (car equation) (car solution)))
	  (rhs (calc-side (cadr equation) (cadr solution))))
      (values (abs (- lhs rhs)) (cons lhs rhs)))))

(defclass solution-suite ()
  ((equation     :accessor equation
	         :initarg :equation)
   (fittest      :accessor fittest
	         :initform nil)
   (population   :accessor population
	         :initform nil)
   (genome-range :accessor gen-range)
   (generation   :accessor generation
		 :initform 0)))

(defgeneric make-genome (suite))
(defgeneric remove-exceed (suite))
(defgeneric genome-member (suite genome))
(defgeneric add-genome  (suite genome))
(defgeneric init-population (suite))
(defgeneric mutate (suite solution))
(defgeneric breed (suite))
(defgeneric crossover (suite))
(defgeneric best-fitness (suite))
(defgeneric format-answer (suite answer))
(defgeneric format-best-answer (suite))


(defmethod make-genome ((suite solution-suite))
  (let ((lhs-size (length (car (equation suite))))
	(rhs-size (length (cadr (equation suite)))))
    (list (loop for x below lhs-size
	     collect (1+ (random (gen-range suite) *rnd-state*)))
	  (loop for x below rhs-size
	     collect (1+ (random (gen-range suite) *rnd-state*))))))

(defmethod remove-exceed ((suite solution-suite))
  (let* ((population-length (length (population suite)))
	 (exceed (- population-length *population-size*)))
    (labels ((remove-least-fit (population-fitness)
	       (let ((unfit-index (loop for i below population-length
				     for elt in population-fitness
				     with max = (cons 0 nil)
				     when (> elt (car max))
				     do (setf max (cons elt i))
				     finally (return (cdr max)))))
		 (unless (null unfit-index)
		   (setf (population suite) (loop for elt in (population suite)
					       for i from 0
					       unless (= i unfit-index)
					       collect elt)
			 population-length (1- population-length))))))
      (dotimes (i exceed)
	(remove-least-fit (mapcar (lambda (x) (calc-fitness (equation suite) x))
				  (population suite)))))))

(defun genome-different (genome-1 genome-2)
  (labels ((list= (list1 list2)
	     (loop for elt1 in list1
		for elt2 in list2
		always (= elt1 elt2))))
    (or (not (list= (car genome-1)
		    (car genome-2)))
	(not (list= (cadr genome-1)
		    (cadr genome-2))))))

(defmethod genome-member ((suite solution-suite) genome)
  (loop for elt in (population suite)
     always (genome-different elt genome)))

(defmethod add-genome ((suite solution-suite) genome)
  (push genome (population suite))
  (remove-exceed suite)
  ;; Selection phase
  (let ((fitness (calc-fitness (equation suite) genome)))
    (cond ((null (fittest suite))
	   (push genome (fittest suite)))
	  ((not (genome-member suite genome))
	   (setf (fittest suite)
		 (cond ((< fitness (calc-fitness (equation suite)
						 (car (fittest suite))))
			(list genome (car (fittest suite))))
		       ((< fitness (calc-fitness (equation suite)
						 (cadr (fittest suite))))
			(list (car (fittest suite)) genome))
		       (t (fittest suite))))))))

(defmethod init-population ((suite solution-suite))
  (setf (population suite) nil
	(fittest suite) nil
	(gen-range suite) (1+ *initial-range*)
	(generation suite) 0)
    (dotimes (i *population-size*)
      (add-genome suite (make-genome suite))))

#|
When breeding, we pick up the hand side with most elements and exchange them,
creating two siblings.
|#

(defmethod mutate ((suite solution-suite) solution)
  (labels ((mutate-p ()
	     ;; d20 < 5 chance of mutation
	     (< (random 200 *rnd-state*) 5))
	   (mutate-global-p ()
	     (< (random 10000 *rnd-state*) 1))
	   (mutate-list (list)
	     (loop for x in list
		collect (if (mutate-p)
			    (progn
			      (incf (gen-range suite)
				    (if (mutate-global-p)
					(random 14 *rnd-state*)
					0))
			      (1+ (random (gen-range suite) *rnd-state*)))
			    x))))
    (mapcar #'mutate-list solution)))

(defmethod breed ((suite solution-suite))
  (let ((fst-fit (car (fittest suite)))
	(snd-fit (cadr (fittest suite))))
    (values (list (car fst-fit) (cadr snd-fit))
	    (list (car snd-fit) (cadr fst-fit)))))

(defmethod crossover ((suite solution-suite))
  (multiple-value-bind (firstborn junior)
      (breed suite)
    (add-genome suite (mutate suite firstborn))
    (add-genome suite (mutate suite junior)))
  (incf (generation suite)))

(defmethod best-fitness ((suite solution-suite))
  (calc-fitness (equation suite) (car (fittest suite))))

(defmethod format-answer ((suite solution-suite) answer)
  (let ((var-name (char-code #\a)))
    (with-output-to-string (*standard-output*)
      (labels ((print-vars (list equation-side)
		 (loop for value in list
		    for var in equation-side
		    unless (zerop (cdr var))
		    do (format t (if (= var-name (char-code #\a))
				     "~a = ~a"
				     ", ~a = ~a")
			       (code-char var-name)
			       value)
		      (incf var-name))))
	(print-vars (car answer)
		    (car (equation suite)))
	(print-vars (cadr answer)
		    (cadr (equation suite)))))))

(defmethod format-best-answer ((suite solution-suite))
  (format-answer suite (car (fittest suite))))

