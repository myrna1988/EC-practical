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

(defun genPop (popSize fitFun)
 (let* ((individual (genInd 100)) 
       (fitness (case fitFun 
             (1 (co individual)) 
             (2 (sco individual))
             (3 (dTrapFun individual))
             (4 (nTrapFun individual)) )))
  (if (> popSize 0)
   (cons (list individual fitness) (genPop (- popSize 1) fitFun))
    nil)))

(defun genInd (indSize)
 (if (> indSize 0)
  (cons (random 2) (genInd (- indSize 1)))
   nil))
  
;;;; fitness functions
  
;; fitness function 1 (co)
;; fitness function 2 (sco)
;; fitness function 3 (d=1)
;; fitness function 4 (d=2.5)

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
