(defparameter *start-position* 50)
(defparameter *modulo* 100)
(defparameter *input* "01/input")

(defparameter *test-data* '(L68
                            L30
                            R48
                            L5
                            R60
                            L55
                            L1
                            L99
                            R14
                            L82))


(defun parse-instruction-symbols (instruction-symbols)
  "Parse a list of instruction symbols into list of signed integers."
  (mapcar (lambda (sym)
            (let* ((name (symbol-name sym))
                   (direction (if (char= (char name 0) #\L) -1 1))
                   (steps (parse-integer (subseq name 1))))
              (* direction steps)))
      instruction-symbols))


(defun acc (movements initial modulo)
  "Produce list of positions."
  (let ((result '())
        (current initial))
    (dolist (move movements)
      (setf current (mod (+ current move) modulo))
      (push current result))
    (nreverse result)))


(defun count-zeros (positions)
  "Count number of zeros in positions."
  (count 0 positions))

;; PART 1

(defun part1 ()
  (with-open-file (stream "01/input")
    (loop for symbol = (read stream nil)
          while symbol
          collect symbol into instruction-symbols
          finally
            (let* ((movements (parse-instruction-symbols instruction-symbols))
                   (positions (acc movements *start-position* *modulo*))
                   (zero-count (count-zeros positions)))
              (format t "Number of times at position 0: ~A~%" zero-count)))))

;; PART 2

(defun parse-single-symbol (sym)
  (let* ((name (symbol-name sym))
         (direction (if (char= (char name 0) #\L) -1 1))
         (steps (parse-integer (subseq name 1))))
    (* direction steps)))


(defun fetch-instructions-2 ()
  "Fetch and parse in one pass."
  (with-open-file (stream *input*)
    (loop for symbol = (read stream nil :eof)
          until (eq symbol :eof)
          collect (parse-single-symbol symbol))))


(defun acc-2 (movements initial modulo)
  "Count zero landings using floor division math."
  (let ((zeroes 0)
        (current initial))
    (dolist (delta movements)
      (cond
       ;; Moving Right (Positive)
       ;; Count multiples of modulo in range (current, current + delta]
       ((> delta 0)
         (incf zeroes (floor (+ current delta) modulo)))

       ;; Moving Left (Negative)
       ;; Count multiples of modulo in range [current + delta, current)
       ;; We use offset -1 to handle the exclusive upper bound of the range
       ((< delta 0)
         (incf zeroes (- (floor (1- current) modulo)
                         (floor (+ current delta -1) modulo)))))

      (setf current (mod (+ current delta) modulo)))
    zeroes))


(defun part2 ()
  (let ((instructions (fetch-instructions-2)))
    (acc-2 instructions *start-position* *modulo*)))
