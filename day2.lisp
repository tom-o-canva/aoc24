(require 'uiop)
(require 'cl-ppcre)
(defmacro ^ (&rest forms) `(lambda ,@forms))

;; part 1
(defun diffs (xs) (loop for (a b) on xs while b collect (- a b)))
(defun between-p (x lo hi) (and (<= lo x) (<= x hi)))
(defun safe-p (xs)
  (and (or (apply #'< xs) (apply #'> xs))
       (every (^ (x) (between-p (abs x) 1 3)) (diffs xs))))
(count t (mapcar 'safe-p (read-reports "day2.input")))

(defun read-reports (fname)
  (loop for line in (uiop:read-file-lines fname)
	for nums = (ppcre:split "\\s+" line)
	collect (mapcar 'parse-integer nums)))

;; part 2
(defun safe-ish-p (xs)
  (or (safe-p xs)
      (loop for i from 0 for x in xs
	    for dxs = (remove x xs :start i :count 1)
	    when (safe-p dxs) return t)))
(count t (mapcar 'safe-ish-p (read-reports "day2.input")))
