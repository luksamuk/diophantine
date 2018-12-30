;;;; diophantine.asd
;;
;;;; Copyright (c) 2018 Lucas Vieira


(asdf:defsystem #:diophantine
  :description "Genetic algorithm for solving diophantine equations"
  :author "Lucas Vieira <lucasvieira@lisp.com.br>"
  :license  "BSD-2"
  :version "0.0.1"
  :serial t
  :depends-on (:split-sequence)
  :components ((:file "package")
	       (:file "parsing")
	       (:file "solver")
	       (:file "repl")))
