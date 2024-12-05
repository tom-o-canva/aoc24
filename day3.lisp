(require 'uiop)
(require 'cl-ppcre)
(require 'str)
(defmacro ^ (&rest forms) `(lambda ,@forms))

;; uiop:read-file-lines fname
;; ppcre:split regex line

(defvar example "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

;; part 1
(defun extract-muls (input)
  (loop for expr in (ppcre:all-matches-as-strings "mul\\(\\d+,\\d+\\)" input)
	for operands = (mapcar 'parse-integer (ppcre:all-matches-as-strings "\\d+" expr))
	collect operands))
(reduce '+ (mapcar (^ (xs) (apply #'* xs))
		   (extract-muls (uiop:read-file-string "day3.input"))))

;; part 2
(defun extract-statements (input)
  (ppcre:all-matches-as-strings "(do\\(\\)|don\\'t\\(\\)|mul\\(\\d+,\\d+\\))" input))
(defun parse-expr (expr)
  (cond ((str:starts-with-p "do(" expr) :enable)
	((str:starts-with-p "don" expr) :disable)
	((str:starts-with-p "mul(" expr)
	 (mapcar 'parse-integer (ppcre:all-matches-as-strings "\\d+" expr)))))
(apply '+
       (loop with statements =  (extract-statements (uiop:read-file-string "day3.input"))
	     with ops = (mapcar 'parse-expr statements)
	     with enabled = t
	     for op in ops
	     when (equal op :enable) do (setf enabled t)
	     when (equal op :disable) do (setf enabled nil)
	     when (and enabled (listp op)) collect (apply #'* op)))
	    
