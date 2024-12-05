(require 'uiop)
(require 'cl-ppcre)
(defmacro ^ (&rest forms) `(lambda ,@forms))

(defun parse-rules (rules-raw)
  (loop
    with table = (make-hash-table :test 'equal)
    for line in (ppcre:split "\\n" rules-raw)
    do (loop
	 for (before after) on (mapcar 'parse-integer (ppcre:split "\\|" line))
	 for afters = (append (list after) (gethash before table))
	 do (setf (gethash before table) afters))
    finally (return table)))

(defun parse-prints (prints-raw)
  (loop for line in (ppcre:split "\\n" prints-raw)
	collect (mapcar 'parse-integer (ppcre:split "," line))))

(defun parse (input)
  (let ((sections (ppcre:split "\\n\\n" input)))
    (list :rules (parse-rules (first sections))
	  :prints (parse-prints (second sections)))))

(defun valid-p (print rules)
  (loop
    with seen = '()
    for n in print
    for afters = (gethash n rules)
    when (intersection seen afters) return nil
    do (push n seen)
    finally (return t)))

(defun middle-num (print)
  (nth (floor (/ (length print) 2)) print))

;; part 1
(loop
  with data = (parse (uiop:read-file-string "day5.input"))
  with rules = (getf data :rules)
  for print in (getf data :prints)
  when (valid-p print rules) sum (middle-num print))

;; part 2
(loop
  with data = (parse (uiop:read-file-string "day5.input"))
  with rules = (getf data :rules)
  for print in (getf data :prints)
  when (not (valid-p print rules))
  sum (middle-num (sort print (^ (a b) (member b (gethash a rules) :test 'equal)))))
