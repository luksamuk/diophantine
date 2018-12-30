;;;; parsing.lisp
;;
;;;; Copyright (c) 2018 Lucas Vieira


(in-package #:diophantine)

#|

Diophantine equations are indeed EQUATIONS, therefore we need to SPLIT them into
two formulas and parse each one of them!
Example:
 
                          w^3 + 2x^3 = 3y^3 + 4z^3

                              can be stored as

                          ( ( (1 . 3) (2 . 3) )
                            ( (3 . 3) (4 . 3) ) )

- List of exactly two lists
- Each sublist contains pairs (coefficient . power)
- Position of lists imply position around the = sign

It would seem like we could actually make minor changes to the current code in
order to make it create each list at the end, and then we just create a
super-function two split the equation in two and do what is needed. In the end,
just join the two lists as elements of another list.

|#

(defun equation-format (equation)
  (let ((letter (char-code #\a)))
    (with-output-to-string (*standard-output*)
      (labels ((format-side (side)
		 (loop for variable in side
		    for i from 0
		    for coefficient = (car variable)
		    for power = (cdr variable)
		    unless (= coefficient 0)
		    do (progn
			 ;; Sum/sub signal
			 (unless (zerop i)
			   (princ (if (minusp coefficient) " - " " + ")))
			 ;; Coefficient
			 (when (or (not (= (abs coefficient) 1))
				   (zerop power))
			   (princ (abs coefficient)))
			 (unless (zerop power)
			   (princ (code-char letter))
			   (unless (= power 1)
			     (princ #\^)
			     (princ power))
			   (incf letter))))))
	(format-side (car equation))
	(princ " = ")
	(format-side (cadr equation))))))

(defun split-equation (equation)
  (mapcar (lambda (string)
	    (string-trim '(#\Space #\Newline #\Backspace #\Tab
			   #\Linefeed #\Page #\Return #\Rubout)
			 string))
	  (split-sequence #\= equation)))

;; TODO: Make sure variables are different
(defun parse-side (side-string)
  (declare (string side-string))
  (let ((split-string (split-sequence #\Space side-string))
	(results nil))
    (labels ((parse-power (potential-power)
	       (handler-bind ((error (invoke-restart 'recover)))
		 (restart-case (parse-integer potential-power)
		   (recover () 1))))
	     (check-variable (potential-coefd-variable)
	       (handler-bind ((error (invoke-restart 'recover)))
		 (restart-case
		     (progn (parse-integer potential-coefd-variable)
			    nil)
		   (recover () t)))))
      (loop for part in split-string
	 with token = 'variable
	 with flip-coef = nil
	 do (if (eq token 'variable)
		(let ((coefficient (parse-integer part :junk-allowed t))
		      (power nil)
		      (split-tokens (split-sequence #\^ part)))
		  (setf coefficient (if coefficient coefficient 1))
		  (setf power (if (check-variable (car split-tokens))
				  (parse-power (car (last split-tokens)))
				  0))
		  (unless (zerop coefficient)
		    (push (cons coefficient power) results)))
		(cond ((string= part "-") (setf flip-coef t))
		      ((not (string= part "+")) (error 'parse-error))))
	 do (setf token (if (eq token 'variable) 'operator 'variable))))
    (reverse results)))

(defun parse-equation (equation-string)
  (let ((parsed-equation (mapcar #'parse-side
				 (split-equation equation-string))))
    (if (not (= (length parsed-equation) 2))
	(error 'parse-error)
	parsed-equation)))

