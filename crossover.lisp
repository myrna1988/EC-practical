;;;
;;; added 17/12/12
;;; crossover
;;; alice 17/12/12
;;;


;; selects pairs of parents and performs crossover with pc probability
;; returns a list of child bitstrings to be mutated
;; I just go through parents in order since it was randomly generated
;; type is either '2-point or 'uniform
(defun crossover (parents pc type)
  (cond ((equal type '2-point) (2-point parents pc nil))
	((equal type 'uniform) (uniform parents pc nil))
	(t "SOMETHING IS WRONG HERE")))

;; performs 2-point crossover on pairs of parents with pc probability
;; adds pairs of parents to child-strings uncrossed with 1-pc probability
(defun 2-point (parents pc child-strings)
  (cond ((null parents) child-strings)
	(t 
	 (let* ((string1 (first (first parents)))
		(string2 (first (second parents)))
		(point1 (random (length string1)))
		(point2 (random (length string1)))
		(new-strings (2-point-aux string1 
					  string2
					  (min point1 point2)
					  (max point1 point2)
					  nil
					  nil)))
	   (cond ((<= (random 1.0) pc)
		  (2-point (rest (rest parents)) 
			   pc 
			   (append child-strings new-strings)))
		 (t 
		  (2-point (rest (rest parents)) 
			   pc 
			   (append child-strings (append (list string1) (list string2))))))))))

;; performs the crossovers of individual parents
;; returns a list of two new child strings
(defun 2-point-aux (parent1 parent2 start end child1 child2)
  (cond ((null parent1) (list child1 child2))
	(t
	 (cond ((or (>= start 0) (< end 0))
		(2-point-aux (rest parent1) 
			     (rest parent2) 
			     (- start 1) 
			     (- end 1)
			     (append child1 (list (first parent1)))
			     (append child2 (list (first parent2)))))
	       ((and (< start 0) (>= end 0))
		(2-point-aux (rest parent1)
			     (rest parent2)
			     (- start 1)
			     (- end 1)
			     (append child1 (list (first parent2)))
			     (append child2 (list (first parent1)))))))))
  
;; performs uniform crossover on pairs of parents with pc probability
;; adds pairs of parents to child-strings uncrossed with 1-pc probability 
(defun uniform (parents pc child-strings)
  (cond ((null parents) child-strings)
	(t 
	 (let* ((string1 (first (first parents)))
		(string2 (first (second parents)))
		(new-strings (uniform-aux string1 
					  string2
					  nil
					  nil)))
	   (cond ((<= (random 1.0) pc)
		  (uniform (rest (rest parents)) 
			   pc 
			   (append child-strings new-strings)))
		 (t 
		  (uniform (rest (rest parents)) 
			   pc 
			   (append child-strings (append (list string1) (list string2))))))))))

;; performs uniform crossover on individual parents 
;; returns a list of two new child strings  
(defun uniform-aux (parent1 parent2 child1 child2)
  (cond ((null parent1) (list child1 child2))
	(t
	 (cond ((= (random 2) 0)
		(uniform-aux (rest parent1) 
			     (rest parent2)
			     (append child1 (list (first parent1)))
			     (append child2 (list (first parent2)))))
	       (t
		(uniform-aux (rest parent1)
			     (rest parent2)
			     (append child1 (list (first parent2)))
			     (append child2 (list (first parent1)))))))))
