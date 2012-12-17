;;;
;;; added 17/12/12
;;; fitness sorter
;;; alice 17/12/12
;;;


;; assuming each individual looks like ((bitstring) fitness)
;; returns a list of individuals sorted by fitness 
(defun sort-by-fitness (L)
  (sort L #'< :key #'second))
