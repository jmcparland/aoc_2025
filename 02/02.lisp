(defparameter *test-data*
              "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(defparameter *input* "02/input")

(defun input-to-ranges (input)
  "split string on comma, then split each part on hyphen, convert to integers cons."
  (let* ((range-strings (split-sequence:split-sequence #\, input))
         (ranges (mapcar (lambda (range-str)
                           (let* ((bounds (split-sequence:split-sequence #\- range-str))
                                  (lower (parse-integer (first bounds)))
                                  (upper (parse-integer (second bounds))))
                             (cons lower upper)))
                     range-strings)))
    ranges))

;; PART 1

(defun digit-count (n)
  "Count number of digits in a non-negative integer N."
  (if (zerop n)
      1
      (floor (1+ (log n 10)) 1)))

(defun invalid-p (n)
  (and (evenp (digit-count n))
       (let* ((n-str (format nil "~A" n))
              (len-str (length n-str))
              (first-half (subseq n-str 0 (floor len-str 2)))
              (second-half (subseq n-str (floor len-str 2))))
         (string= first-half second-half))))

(defun brute-force-range (range)
  (let ((start (car range))
        (end (cdr range)))
    (loop for n from start to end
            when (invalid-p n)
          collect n)))

(defun part1-check ()
  (let* ((ranges (input-to-ranges *test-data*))
         (invalid-numbers (mapcan #'brute-force-range ranges))
         (sum-of-invalids (reduce #'+ invalid-numbers)))
    (format t "Sum of invalid numbers: ~A~%" sum-of-invalids)
    (= sum-of-invalids 1227775554)))

(defun part1 ()
  (with-open-file (stream *input*)
    (let* ((input (read-line stream nil))
           (ranges (input-to-ranges input))
           (invalid-numbers (mapcan #'brute-force-range ranges))
           (sum-of-invalids (reduce #'+ invalid-numbers)))
      (format t "Sum of invalid numbers: ~A~%" sum-of-invalids))))

;; PART 2

;; Repeat a string s exactly k times.
(defun repeat-string (s k)
  (apply #'concatenate 'string (make-list k :initial-element s)))

;; Generate all “invalid” numbers with total digits between 2 and 10,
;; formed by repeating a base string n-str (length L) k times, k>=2.
(defun generate-invalids ()
  "Generate all invalid numbers with between 2 and 10 digits."
  (let ((invalids '()))
    ;; n-str length L must satisfy 1 <= L <= 5 because k>=2 and k*L <= 10
    (loop for n from 1 to 99999
          do (let* ((n-str (format nil "~A" n))
                    (len (length n-str)))
               (when (<= 1 len 5)
                     ;; k from 2 up to floor(10/len), inclusive
                     (loop for k from 2 to (floor 10 len)
                           for s = (repeat-string n-str k)
                           do (push (parse-integer s) invalids)))))
    (remove-duplicates invalids)))

;; Sum x for each range that contains x
(defun part2 ()
  (with-open-file (stream *input*)
    (let* ((input (read-line stream nil))
           (ranges (input-to-ranges input))
           (invalid-numbers (generate-invalids))
           (sum (reduce (lambda (acc x)
                          (+ acc
                             (* x (loop for r in ranges
                                          count (<= (car r) x (cdr r))))))
                    invalid-numbers
                  :initial-value 0)))
      (format t "Sum part2: ~A~%" sum)
      sum)))