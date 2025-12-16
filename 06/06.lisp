(defparameter *test-input* "06/test")
(defparameter *input* "06/input")

(defun parse-item (str)
  "Parse a string as either an integer or an operator symbol."
  (handler-case
      (parse-integer str)
    (error () (read-from-string str))))

(defun parse-line (line)
  (mapcar #'parse-item
    (split-sequence:split-sequence #\Space line :remove-empty-subseqs t)))

(defun read-input (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
          while line
          collect (parse-line line))))

(defun transpose (list-of-lists)
  (apply #'mapcar #'list list-of-lists))

(defun evaluate-list (lst)
  (mapcar (lambda (seq) (eval (reverse seq))) lst))

(defun sum (lst)
  (reduce #'+ lst))

(defun p1 (input-file)
  (let* ((data (read-input input-file))
         (transposed (transpose data))
         (evaluated (evaluate-list transposed)))
    (sum evaluated)))

;; PART 2

(defun transpose-strings (lines)
  "Transpose uniform-length lines character-by-character."
  (let* ((num-cols (length (first lines)))
         (blank-line (make-string num-cols :initial-element #\Space))
         (operator-line (car (last lines)))
         (num-rows (length lines))
         (numeric-lines (subseq lines 0 (1- num-rows)))
         ;; Build the final list: operator, blank, then numeric lines
         (reordered-lines (cons operator-line
                                (cons blank-line numeric-lines))))
    (loop for col from 0 below num-cols
          collect (coerce (mapcar (lambda (line) (char line col))
                              reordered-lines)
                          'string))))

(defun read-input-as-strings (path)
  "Read all lines from a file as strings without parsing."
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
          while line
          collect line)))


(defun column-all-spaces-p (col)
  (every (lambda (ch) (char= ch #\Space)) col))

(defun column->form-string (col)
  "Turn one column into a piece of a Lisp expression.
If the column is all spaces, it ends a form; otherwise it contributes
operator and digits for one (operator number number number ...)."
  (if (column-all-spaces-p col)
      " ) " ; close current form
      (let* ((len (length col))
             (op-char (char col 0))
             (op-str (cond ((char= op-char #\+) "(+ ") ; note opening paren
                           ((char= op-char #\*) "(* ")
                           (t "")))
             (digit-chars (loop for i from 1 below len
                                collect (char col i)))
             (digit-string (coerce digit-chars 'string)))
        (concatenate 'string op-str digit-string " "))))

(defun column->items (col)
  "Parse a column string into operator/number items, using layout:
   row 0 = operator (+ or * or space),
   rows 1.. = digits/spaces forming one integer (or nothing)."
  (let* ((len (length col))
         (op-char (char col 0))
         (op (cond ((char= op-char #\+) '+)
                   ((char= op-char #\*) '*)
                   (t nil)))
         (digit-chars (loop for i from 1 below len
                            collect (char col i)))
         (digit-string (coerce digit-chars 'string))
         (num-strings (split-sequence:split-sequence
                        #\Space digit-string
                        :remove-empty-subseqs t))
         (nums (mapcar #'parse-integer num-strings)))
    (append (when op (list op)) nums)))

(defun columns->forms (columns)
  "Build a list of forms from columns, using all-space columns as form separators."
  (let ((forms '())
        (current '()))
    (dolist (col columns)
      (if (column-all-spaces-p col)
          (when current
                (push (nreverse current) forms)
                (setf current '()))
          (dolist (item (column->items col))
            (push item current))))
    (when current
          (push (nreverse current) forms))
    (nreverse forms)))

(defun p2 (input-file)
  (let* ((lines (read-input-as-strings input-file))
         (columns (transpose-strings lines))
         (forms (columns->forms columns))
         (values (mapcar #'eval forms)))
    (sum values)))

;; Debug only; safe version without eval:
(let* ((lines (read-input-as-strings *test-input*))
       (columns (transpose-strings lines))
       (body (apply #'concatenate 'string
               (mapcar #'column->form-string columns)))
       (expr-string (concatenate 'string "(list " body ")")))
  (format t "~&Expr: ~A~%" expr-string))
;; (eval (read-from-string expr-string))  ; <-- comment this out