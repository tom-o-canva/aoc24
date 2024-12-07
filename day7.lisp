(require 'uiop)
(require 'cl-ppcre)
(defmacro ^ (&rest forms) `(lambda ,@forms))

(defun parse (lines)
  (loop for line in lines
	for cs = (ppcre:all-matches-as-strings "\\d+" line)
	for xs = (mapcar 'parse-integer cs)
	collect (list :result (first xs)
		      :operands (rest xs))))

;; part 1
(defun computable-p (result xs)
  (if (= 1 (length xs))
    (= result (car xs))
    (destructuring-bind (a b &rest rest) xs
      (or (computable-p result (append `(,(+ a b)) rest))
	  (computable-p result (append `(,(* a b)) rest))))))

(defun join (a b) (parse-integer (format nil "~A~A" a b)))

;; part 2
(defun computable2-p (result xs)
  (if (= 1 (length xs))
    (= result (car xs))
    (destructuring-bind (a b &rest rest) xs
      (or (computable2-p result (append `(,(+ a b)) rest))
	  (computable2-p result (append `(,(* a b)) rest))
	  (computable2-p result (append `(,(join a b)) rest))))))

(loop with eqs = (parse (uiop:read-file-lines "day7.input"))
      for eq in eqs
      for result = (getf eq :result)
      for xs = (getf eq :operands)
      when (computable2-p result xs)
      sum result)
