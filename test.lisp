;;;
;;; woo
;;;

(defun double (X)
  (* X 2))


;; returns all the elements of list L except the last
(defun my-butlast (L)
  (cond ((null L) nil)
  ((null (rest L)) nil)
	(t (my-bl-helper (rest L) (cons (first L) nil)))))
  
(defun my-bl-helper (L M)
  (if (null (rest L))
      M
      (my-bl-helper (rest L) (append M (cons (first L) nil)))))

(defun my-union (L M)
  (cond ((null L) M)
	((member (first L) M) 
	 (my-union (rest L) M))
	(t (cons (first L) (my-union (rest L) M)))))

(defun my-difference (L M)
  (cond ((null L) nil)
	((member (first L) M)
	 (my-difference (rest L) M))
	(t (cons (first L) (my-difference (rest L) M)))))


(defun triangular (N)
  (tri-helper N 1))

(defun tri-helper (N A)
  (if (= N 1)
      A

      (tri-helper (- N 1) (+ N A))))

(defun fast-power (B E)
  (fast-power-helper B B E))

(defun fast-power-helper (P B E)
  (if (= E 1)
      P
      (fast-power-helper (* P B) B (- E 1))))
      
(defun fast-list-length (L)
  (fll-helper L 0))

(defun fll-helper (L A)
  (if (null L) A
      (fll-helper (rest L) (+ A 1))))

(defun apply-func-list (L X)
  (if (null L)
      X
      (funcall (first L) (apply-func-list (rest L) X))))
  
(defun find-nonempty (L)
  (if (null L)
      nil
      (if (null (first L))
	  (find-nonempty (rest L))
	  (first L))))

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
