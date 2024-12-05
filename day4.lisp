(require 'uiop)
(require 'cl-ppcre)
(defmacro ^ (&rest forms) `(lambda ,@forms))

(defun ingest (rows)
  (loop with table = (make-hash-table :test 'equal)
	for row in rows
	for y from 0
	do (loop for char across row
		 for x from 0
		 unless (equal char #\ )
		   do (setf (gethash (list x y) table) char))
	finally (return table)))

(defun range (a b &optional invert)
  (if invert
      (loop for i from b downto a collect i)
      (loop for i from a to b collect i)))
(defun repeat (n i) (make-list i :initial-element n))

(defun cells (table xs ys)
  (format nil "~{~A~}"
	  (loop for x in xs
		for y in ys
		for coord = (list x y)
		for neigh = (gethash coord table)
		when neigh collect neigh)))

(defun neighbors (table x y)
  (list
   ;; Horizontal neighbors
   (cells table (range (- x 3) x t) (repeat y 4))
   (cells table (range x (+ x 3)) (repeat y 4))
   ;; Vertical neighbors
   (cells table (repeat x 4) (range (- y 3) y t))
   (cells table (repeat x 4) (range y (+ y 3)))
   ;; Diagonal neighbors
   (cells table (range (- x 3) x t) (range (- y 3) y t))
   (cells table (range (- x 3) x t) (range y (+ y 3)))
   (cells table (range x (+ x 3)) (range (- y 3) y t))
   (cells table (range x (+ x 3)) (range y (+ y 3)))))

;; Part 1
(defun wordsearch (rows)
  (loop with table = (ingest rows)
	for y from 0 to (length rows)
	sum (loop for x from 0 to (length (first rows))
		  sum (count "XMAS" (neighbors table x y) :test 'equal))))
(wordsearch (ppcre:split "\\n" example)) ;; 18
(wordsearch (uiop:read-file-lines "day4.input")) ;; 2571

;; part2
(defun xmas-at-p (table x y)
  (= 2 (count "MAS"
	  (list
	   ;; Diagonal neighbors
	   (cells table (range (1- x) (1+ x) t) (range (1- y) (1+ y) t))
	   (cells table (range (1- x) (1+ x) t) (range (1- y) (1+ y)))
	   (cells table (range (1- x) (1+ x)) (range (1- y) (1+ y) t))
	   (cells table (range (1- x) (1+ x)) (range (1- y) (1+ y))))
	  :test 'equal)))
(defun wordsearch2 (rows)
  (loop with table = (ingest rows)
	for y from 0 to (length rows)
	sum (loop for x from 0 to (length (first rows))
		  for coord = (list x y)
		  sum (if (xmas-at-p table x y) 1 0))))
(wordsearch2 (ppcre:split "\\n" example)) ;; 9
(wordsearch2 (uiop:read-file-lines "day4.input")) ;; 1992
