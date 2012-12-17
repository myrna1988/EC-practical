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
