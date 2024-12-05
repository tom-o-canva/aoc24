(require 'uiop)
(require 'cl-ppcre)

(defun read-pairs (fname)
  (loop for line in (uiop:read-file-lines fname)
	for raw-nums = (ppcre:split "\\s+" line)
	collect (mapcar #'parse-integer raw-nums)))
(defun transpose (xs)
  (loop with as = '()
	with bs = '()
	for (x y) in xs
	do (push x as) (push y bs)
	finally (return (list as bs))))
(defun absdiff (a b) (abs (- a b)))
(defun diffs (ls) (mapcar 'absdiff (first ls) (second ls)))
(defun sorted (xs) (sort xs #'<))
(defun sum (xs) (reduce #'+ xs))

;; Part 1
(sum (diffs (mapcar 'sorted (transpose (read-pairs "day1.input")))))

;; Part 2
(loop with nums = (transpose (read-pairs "day1.input"))
      with xs = (first nums)
      with ys = (second nums)
      with similarity = 0
      for x in xs
      for freq = (count x ys)
      for ds = (* x freq)
      do (incf similarity ds)
      finally (return similarity))
