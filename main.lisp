;;;
;;; added 16/12/12
;;;

;;;; program is started by calling the main function.
;;;; the variables that are given as input are:
;;;; pop size, which fitness function, tournament size, Pc, Uniform/ 2-point crossover

;; main function with all the variables.
;; this function first calls genPop.
;; after the population is generated, it calls a fitness function

;;;; generate population

;; the list 'pop' starts empty but adds individuals by calling genList
(defun gen-pop (pop-size fitness-fun)
 (let* ((individual (gen-ind 100)) 
       (fitness (case fitness-fun 
             (1 (co individual)) 
             (2 (sco individual))
             (3 (d-trapfun individual))
             (4 (n-trapfun individual)) )))
  (if (> pop-size 0)
   (cons (list individual fitness) (gen-pop (- pop-size 1) fitness-fun))
    nil)))

;; this function generates a list of 100 bits
(defun gen-ind (ind-size)
 (if (> ind-size 0)
  (cons (random 2) (gen-ind (- ind-size 1)))
   nil))


;;;; fitness functions
  
(defun co (individual)
 (if individual
  (+ (first individual) (co (rest individual)))
   0))

(defun sco (individual) (sco2 individual 1))

(defun sco2 (individual bit)
 (if individual
  (+ (* (first individual) bit) (sco2 (rest individual) (+ bit 1)))
   0)) 

(defun d-trapfun (individual) (trapfun individual 4 1))

(defun n-trapfun (individual) (trapfun individual 4 2.5))

(defun trapfun (individual k d)
 (let ((sublist (sub individual nil k)))
  (+ (b (first sublist) k d) (trapFun (rest sublist) k d)) ))


;; sublist of length k that takes the first k bits of individual
(defun sub (individual sublist k)
 (if (> k 0)
  (sub (rest individual) (append sublist (list (first individual))) (- k 1))
   (list individual sublist) ))

(defun b (sublist k d)
 (let ((count (co sublist)))
  (if (= count k)
   k  
    (- (- k d) (* (/ (- k d) (- k 1)) count)) )))



;; sublist that will be called often
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

;;;; tournament

;;;; crossover

;;;; mutation
