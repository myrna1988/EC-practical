;;;
;;; added 18/12/12
;;; generates a list of indeces
;;; alice 18/12/12

(defun index-list (length)
  (index-list-aux length nil))

(defun index-list-aux (length indeces)
  (cond ((= length 0) indeces)
  (t
	 (index-list-aux (- length 1) (cons length indeces)))))
