(require 'uiop)
(require 'cl-ppcre)
(defmacro ^ (&rest forms) `(lambda ,@forms))

(defun parse (input)
  (loop
    with table = (make-hash-table :test 'equal)
    with height = (length input)
    for row in input
    for y from 0
    do (loop for c across row
	     for x from 0
	     do (setf (gethash `(,x ,y) table) c))
    finally (return table)))

(defun collect-freqs (table)
  (loop with freqs = (make-hash-table :test 'equal)
	for pos being the hash-key using (hash-value cell) of table
	when (ppcre:scan "\\w" (format nil "~A" cell))
          do (setf (gethash cell freqs) (append `(,pos) (gethash cell freqs)))
	finally (return freqs)))

(defun pairs (xs)
  (loop for (x . rest) on xs nconc (loop for y in rest collect `(,x ,y))))

(defun antinode (ps)
  (destructuring-bind ((x1 y1) (x2 y2)) ps
    (let ((dx (- x2 x1))
	  (dy (- y2 y1)))
      (list (list (- x1 dx) (- y1 dy))
	    (list (+ x2 dx) (+ y2 dy))))))

(defun inside-p (p x1 y1 x2 y2)
  (destructuring-bind (x y) p
    (and (<= x1 x) (<= x x2)
	 (<= y1 y) (<= y y2))))

;; part 1
(length
 (remove-duplicates
  (remove-if
   (^ (p) (not (inside-p p 0 0 49 49)))
   (loop
     with table = (parse (uiop:read-file-lines "day8.input"))
     with freqs = (collect-freqs table)
     for freq being the hash-key using (hash-value coords) of freqs
     nconc (mapcan 'antinode (pairs coords))))
  :test 'equal))

;; part 2
(defun antinode-2 (ps w h)
  (destructuring-bind (p1 p2) ps
     (nconc
      (list p1 p2)
      (loop with dp = (mapcar '- p1 p2)
	    repeat 100
	    for p = (mapcar '+ p1 dp) then (mapcar '+ p dp)
	    when (inside-p p 0 0 w h) collect p)
      (loop with dp = (mapcar '- p2 p1)
	    repeat 100
	    for p = (mapcar '+ p1 dp) then (mapcar '+ p dp)
	    when (inside-p p 0 0 w h) collect p))))
(length
 (remove-duplicates
   (loop
     with table = (parse (uiop:read-file-lines "day8.input"))
     with freqs = (collect-freqs table)
     for freq being the hash-key using (hash-value coords) of freqs
     nconc (mapcan (^ (ps) (antinode-2 ps 49 49)) (pairs coords)))
  :test 'equal))
