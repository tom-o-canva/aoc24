(require 'uiop)
(require 'cl-ppcre)
(defmacro ^ (&rest forms) `(lambda ,@forms))

(defvar example
"47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(defvar input (uiop:read-file-string "day5.input"))

(defun parse-rules (rules-raw)
  (let ((table (make-hash-table :test 'equal)))
    (loop
      for line in (ppcre:split "\\n" rules-raw)
      do (loop
	   for (a b) on (mapcar 'parse-integer (ppcre:split "\\|" line))
	   for val = (gethash a table)
	   do (setf (gethash a table) (append (list b) val))))
    table))

(defun parse-prints (prints-raw)
  (loop
    for line in (ppcre:split "\\n" prints-raw)
    collect (loop for x in (ppcre:split "," line)
		  collect (parse-integer x))))

(defun ingest (input)
  (let* ((sections (ppcre:split "\\n\\n" input))
	 (rules-raw (first sections))
	 (prints-raw (second sections)))
    (list :rules (parse-rules rules-raw)
	  :prints (parse-prints prints-raw))))

(defun valid-p (print rules)
  (loop
    with seen = '()
    for n in print
    for afters = (gethash n rules)
    when (intersection seen afters) return nil
    do (push n seen)
    finally (return t)))
(defun middle-num (print) (nth (floor (/ (length print) 2)) print))

;; part 1
(loop
  with data = (ingest (uiop:read-file-string "day5.input"))
  with rules = (getf data :rules)
  with prints = (getf data :prints)
  for print in prints
  when (valid-p print rules) sum (middle-num print))

;; part 2
(defun sorted (print rules)
  (let ((result (make-array (list (length print)))))
    (loop
      for n in print
      for afters = (intersection print (gethash n rules))
      for idx = (1- (- (length print) (length afters)))
      do (setf (aref result idx) n)
      finally (return (coerce result 'list)))))
	 
(loop
  with data = (ingest (uiop:read-file-string "day5.input"))
  with rules = (getf data :rules)
  with prints = (getf data :prints)
  for print in prints
  when (not (valid-p print rules))
    sum (middle-num (sorted print rules)))
