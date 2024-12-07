(require 'uiop)
(require 'cl-ppcre)
(defmacro ^ (&rest forms) `(lambda ,@forms))

(defun parse (lines)
  (loop for line in lines
	for cs = (ppcre:all-matches-as-strings "\\d+" line)
	for xs = (mapcar 'parse-integer cs)
	collect (list :result (first xs)
		      :operands (rest xs))))

(defun computable-p (result xs ops)
  (if (= 1 (length xs))
      (= result (car xs))
      (destructuring-bind (a b &rest rest) xs
	(loop for op in ops
	      for dx = (append (list (funcall op a b)) rest)
	      for rv = (computable-p result dx ops)
	      when rv do (return t)))))

(defun join (a b) (parse-integer (format nil "~A~A" a b)))

(loop with eqs = (parse (uiop:read-file-lines "day7.input"))
      for eq in eqs
      for result = (getf eq :result)
      for xs = (getf eq :operands)
      when (computable-p result xs (list '+ '* 'join))
      sum result)
