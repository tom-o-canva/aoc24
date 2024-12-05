(require 'uiop)
(require 'cl-ppcre)
(defmacro ^ (&rest forms) `(lambda ,@forms))

;; uiop:read-file-lines fname
;; ppcre:split regex line

(defvar example
  "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS 
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

;; horz - both dirs (xmas samx)
;; vert - both dirs (xmas samx)
;; diag - all four dirs (xmas samx)

(defun ingest (rows)
  (let ((table (make-hash-table :test 'equal)))
    (loop
      for row in rows
      for y from 0
      do (loop for char across row
	       for x from 0
	       unless (equal char #\ )
	       do (setf (gethash (list x y) table) char)))
    table))

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
  (count "XMAS"
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
	   (cells table (range x (+ x 3)) (range y (+ y 3))))
  :test 'equal))

(neighbors (ingest (ppcre:split "\\n" example)) 5 0)

;; Part 1
(defun wordsearch (rows)
  (let ((table (ingest rows))
	(sum 0)
	(terms '()))
    (loop
      for y from 0 to (length rows)
      do (loop for x from 0 to (length (first rows))
	       for coord = (list x y)
	       do (incf sum  (neighbors table x y))))
    sum))

(wordsearch (ppcre:split "\\n" example)) ;; 18
(wordsearch (uiop:read-file-lines "day4.input")) ;; 2571

;; part2
(defun count-xmas (table x y)
  (count "MAS"
	  (list
	   ;; Diagonal neighbors
	   (cells table (range (1- x) (1+ x) t) (range (1- y) (1+ y) t))
	   (cells table (range (1- x) (1+ x) t) (range (1- y) (1+ y)))
	   (cells table (range (1- x) (1+ x)) (range (1- y) (1+ y) t))
	   (cells table (range (1- x) (1+ x)) (range (1- y) (1+ y))))
	  :test 'equal))
(defun wordsearch2 (rows)
  (let ((table (ingest rows))
	(sum 0)
	(terms '()))
    (loop
      for y from 0 to (length rows)
      do (loop for x from 0 to (length (first rows))
	       for coord = (list x y)
	       for xmas = (count-xmas table x y)
	       when (= 2 xmas)
		 do (incf sum)))
    sum))
(wordsearch2 (ppcre:split "\\n" example))
(wordsearch2 (uiop:read-file-lines "day4.input"))
