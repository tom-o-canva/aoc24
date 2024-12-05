(require 'uiop)
(require 'cl-ppcre)

(defun read-pairs (fname)
  (loop for line in (uiop:read-file-lines fname)
	for raw-nums = (ppcre:split "\\s+" line)
	collect (mapcar #'parse-integer raw-nums)))

(defun unzip (pairs)
  (loop with xs = '()
	with ys = '()
	for (x y) in pairs
	do (push x xs) (push y ys)
	finally (return `(,xs ,ys))))

;; Part 1
(loop with nums = (unzip (read-pairs "day1.input"))
      for x in (sort (first nums) #'<)
      for y in (sort (second nums) #'<)
      sum (abs (- x y)))
;; Part 2
(loop with nums = (unzip (read-pairs "day1.input"))
      for x in (first nums)
      for freq = (count x (second nums))
      sum (* x freq))
