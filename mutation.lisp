;;;
;;; added 18/12/12
;;; mutate
;;; alice 18/12/12
;;;

;; mutates the members of child-strings with (1-pc) probability
;; the number of bits to be mutated is proportional to the negative 
;; log2 of a random number between approximately 0 and 1
(defun mutate (child-strings pc)
  (mutate-aux child-strings pc 0))

(defun mutate-aux (child-strings pc index)
  (cond ((= index (length child-strings)) child-strings)
  ((>= (random 1.0) pc)
	 (let* ((individual (nth index child-strings))
		(bits-to-mutate (abs (floor (log (+ (random (- 1 1e-10)) 1e-10) 2)))) 
					;number of bits to be mutated
		(indeces-of-mutation 
		 (sort (sublist bits-to-mutate 
				(index-list (length individual))) #'<)))
					;list of indeces of bits to be mutated
	   (mutate-individual individual indeces-of-mutation))
	 (mutate-aux child-strings  pc (+ index 1)))
	(t (mutate-aux child-strings  pc (+ index 1)))))
	   

(defun mutate-individual (string indeces-of-mutation)
  (cond ((null indeces-of-mutation) string)
	(t
	 (setf (nth (first indeces-of-mutation) string) 
	       (abs (- (nth (first indeces-of-mutation) string) 1)))
	 (mutate-individual string (rest indeces-of-mutation)))))
	 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STUFF THIS FUNCTION NEEDS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sublist (n L)
  "returns a list containing n randomly selected elements of list L"
  (sublist-helper n nil L))

(defun sublist-helper (n new-list L)
  (cond ((= n 0) new-list)
	(t
	 (let ((element (nth (random (length L)) L)))
	   (sublist-helper (- n 1) 
			   (cons element new-list)
			   (remove-if #'(lambda (x) (equal x element)) L))))))

;; generates a list of indeces
(defun index-list (length)
  (index-list-aux length nil))

(defun index-list-aux (length indeces)
  (cond ((= length 0) indeces)
	(t
	 (index-list-aux (- length 1) (cons length indeces)))))
