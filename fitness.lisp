;; added 16/12/12
;; by Myrna

(defun co (individual)
 (if individual
  (+ (first individual) (co (rest individual)))
   0))

(defun sco (individual) (sco2 individual 1))

(defun sco2 (individual bit)
 (if individual
  (+ (* (first individual) bit) (sco2 (rest individual) (+ bit 1)))
   0)) 

(defun dTrapFun (individual) trapFun (individual 4 1))

(defun nTrapFun (individual) trapFun (individual 4 2.5))

(defun trapFun (individual k d)
 (let ((sublist (sub individual nil k)))
  (+ (b (first sublist) k d) (trapFun (rest sublist) k d)) ))

(defun sub (individual sublist k)
 (if (> k 0)
  (sub (rest individual) (append sublist (list (first individual))) (- k 1))
   (list individual sublist) ))

(defun b (sublist k d)
 (let ((count (co sublist)))
  (if (= count k)
   k  
    (- (- k d) (* (/ (- k d) (- k 1)) count)) )))
