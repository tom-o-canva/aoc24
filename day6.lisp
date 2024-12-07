(require 'uiop)
(require 'cl-ppcre)
(defmacro ^ (&rest forms) `(lambda ,@forms))

;; uiop:read-file-lines fname
;; ppcre:split regex line

(defun obstacle-p (tile) (equal tile #\#))

(defun parse (input)
  (loop with table = (make-hash-table :test 'equal)
	for line in input
	for y from 0
	do (loop for char across line
		 for x from 0
		 for coord = (list x y)
		 do (setf (gethash coord table) char))
	finally (return table)))

(defun locate (chr table)
  (loop for k being the hash-key using (hash-value v) of table
	when (equal v chr) do (return k)))

(defun up (p) (mapcar '+ p (list 0 -1)))
(defun left (p) (mapcar '+ p (list -1 0)))
(defun right (p) (mapcar '+ p (list 1 0)))
(defun down (p) (mapcar '+ p (list 0 1)))

(defun proceed (p dir)
  (case dir
    (:up (up p))
    (:left (left p))
    (:right (right p))
    (:down (down p))))

(defun rotate (dir)
  (case dir
    (:up :right)
    (:right :down)
    (:down  :left)
    (:left :up)))

;; part1
(loop with table = (parse (uiop:read-file-lines "day6.input"))
      with dir = :up
      for p = (locate #\^ table) then (proceed p next-dir)
      for blocked-p = (obstacle-p (gethash (proceed p dir) table))
      for next-dir = (if blocked-p (rotate dir) dir)
      do (setf dir next-dir)
      while (gethash p table)
      collect p into seen
      finally (return (length (remove-duplicates seen :test 'equal))))

;; part2
(defun free-row-p (table x1 x2 y)
  (loop for x from x1 to x2
	for c = (gethash (list x y) table)
	always (not (obstacle-p c))))
(defun free-col-p (table y1 y2 x)
  (loop for y from y1 to y2
	for c = (gethash (list x y) table)
	always (not (obstacle-p c))))
(defun has-cycle-p (pos dir steps table)
  (loop
    with x1 = (first pos) and y1 = (second pos)
    for step in steps
    for p = (first step)
    for x2 = (first p)
    for y2 = (second p)
      thereis (or (and (find dir '(:up :down))
		       (= x1 x2)
		       (free-col-p table (min y1 y2) (max y1 y2) x1))
		  (and (find dir '(:left :right))
		       (= y1 y2)
		       (free-row-p table (min x1 x2) (max x1 x2) y1)))))

;; Doesnt work
(let ((table (parse (uiop:read-file-lines "day6.example")))
      (seen '()))
  (loop with dir = :up
	for p = (locate #\^ table) then (proceed p next-dir)
	for blocked-p = (obstacle-p (gethash (proceed p dir) table))
	for next-dir = (if blocked-p (rotate dir) dir)
	do (setf dir next-dir)
	while (gethash p table) do (push (list p dir) seen))
  (setf seen (remove-duplicates seen :test 'equal))
  (loop for step in seen
	for p = (first step)
	for dir = (second step)
	when (has-cycle-p p (rotate dir) seen table) collect p))
