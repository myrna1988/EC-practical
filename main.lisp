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
(let ((pop nil))
 (defun genPop (p)
  (dotimes (i p pop) (setq pop (cons (genList) pop))) ))

;; this function generates a list of 100 bits
(let ((list (make-list 0)))
 (defun genList ()
  (dotimes (i 100 list) (setq list (cons (random 2) list))) ))

  
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
