(defparameter *test-input* "05/test-input")
(defparameter *input* "05/input")

(defun parse-input (file-name)
  (with-open-file (stream file-name)
    (let ((ranges '())
          (numbers '()))
      (loop for line = (read-line stream nil :eof)
            while (not (eq line :eof))
            do (cond
                ;; Line contains "-", so it's a range
                ((find #\- line)
                  (let ((parts (split-sequence:split-sequence #\- line)))
                    (push (cons (parse-integer (first parts))
                                (parse-integer (second parts)))
                          ranges)))
                ;; Line is blank (separator) - skip it
                ((string= (string-trim " " line) "")
                  nil)
                ;; Otherwise it's a number
                (t
                  (push (parse-integer line) numbers))))
      (values ranges numbers))))

(defun in-range-p (num range)
  (and (>= num (car range))
       (<= num (cdr range))))

(defun p1 (input-file)
  (multiple-value-bind (ranges numbers) (parse-input input-file)
    (let ((count 0))
      (dolist (num numbers)
        (when (some (lambda (range) (in-range-p num range)) ranges)
              (incf count)))
      (format t "Part 1: Count of numbers in ranges: ~A~%" count))))

;; PART 2

(defun ranges-connect-p (r1 r2)
  "Check if two ranges overlap or are adjacent."
  (not (or (< (cdr r1) (1- (car r2))) ; r1 ends before r2 starts
           (< (cdr r2) (1- (car r1)))))) ; r2 ends before r1 starts

(defun merge-intersecting-ranges (r1 r2)
  (cons (min (car r1) (car r2))
        (max (cdr r1) (cdr r2))))

(defun apply-range (list-of-ranges new-range)
  "Merge new-range into list-of-ranges, combining any overlapping/adjacent ranges."
  (let ((results '())
        (merged new-range))
    (dolist (r list-of-ranges)
      (if (ranges-connect-p merged r)
          (setf merged (merge-intersecting-ranges merged r))
          (push r results)))
    (push merged results)
    results))

(defun consolidate-ranges (list-of-ranges)
  "Consolidate a list of ranges by merging overlapping/adjacent ones."
  (let ((consolidated '()))
    (dolist (r list-of-ranges)
      (setf consolidated (apply-range consolidated r)))
    consolidated))

(defun sum-range-lengths (list-of-ranges)
  (reduce #'+ (mapcar (lambda (r) (+ 1 (- (cdr r) (car r)))) list-of-ranges)))

(defun p2 (input-file)
  (let* ((ranges (parse-input input-file))
         (consolidated-ranges (consolidate-ranges ranges))
         (total-length (sum-range-lengths consolidated-ranges)))
    (format t "Part 2: Total length of consolidated ranges: ~A~%" total-length)))


; cl-user>(time (p2 *input*))
; Evaluation took:
;   0.001 seconds of real time
;   0.000793 seconds of total run time (0.000793 user, 0.000000 system)
;   100.00% CPU
;   2,704,712 processor cycles
;   358,704 bytes consed
