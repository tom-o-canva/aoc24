(require 'uiop)
(require 'cl-ppcre)

;; part 1
(defun match-nums (text) (ppcre:all-matches-as-strings "\\d+" text))

(loop with text = (uiop:read-file-string "day3.input")
      with exprs = (ppcre:all-matches-as-strings "mul\\(\\d+,\\d+\\)" text)
      for expr in exprs
      for xs = (mapcar 'parse-integer (match-nums expr))
      sum (apply #'* xs))

;; part 2
(defun match-exprs (input)
  (ppcre:all-matches-as-strings "(do\\(\\)|don\\'t\\(\\)|mul\\(\\d+,\\d+\\))" input))

(defun parse-expr (expr)
  (cond ((ppcre:scan "^do\\(" expr) :enable)
	((ppcre:scan "^don" expr) :disable)
	((ppcre:scan "^mul\\(" expr) (mapcar 'parse-integer (match-nums expr)))))

(loop with text = (uiop:read-file-string "day3.input")
      with exprs = (match-exprs text)
      for expr in exprs
      for op = (parse-expr expr)
      for enabled = t then (cond ((equal op :enable) t)
				 ((equal op :disable) nil)
				 (:else enabled))
      when (and enabled (listp op)) sum (apply #'* op))
