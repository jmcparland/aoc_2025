(defparameter *test-data* '(987654321111111
                            811111111111119
                            234234234234278
                            818181911112111))

(defparameter *test-data-str* (mapcar #'princ-to-string *test-data*))

(defparameter *input* "03/input")

(defparameter *debug* nil)

(defun fetch-input ()
  (with-open-file (stream *input*)
    (loop for line = (read-line stream nil :eof)
          while (not (eq line :eof))
          collect line into lines
          finally (return lines))))


(defun line-to-int-list (line)
  (map 'list #'digit-char-p line))

(defun max-scan (int-list)
  (loop with current-max = -1
        for digit in int-list
        for new-max = (max current-max digit)
        do (setf current-max new-max)
        collect current-max))

(defun max-joltage (int-list)
  (loop with fwd = (max-scan int-list)
        with rev = (reverse (max-scan (reverse (rest int-list))))
        for f in fwd
        for r in rev
          maximize (+ (* 10 f) r)))

(defun p1-test ()
  (let* ((vlists (mapcar #'line-to-int-list *test-data-str*))
         (result (reduce #'+ (mapcar #'max-joltage vlists))))
    (format t "Part 1 Test Result: ~A~%" result)
    (assert (= result 357))))

(defun p1 ()
  (let* ((lines (fetch-input))
         (vlists (mapcar #'line-to-int-list lines))
         (result (reduce #'+ (mapcar #'max-joltage vlists))))
    (format t "Part 1 Result: ~A~%" result)))

;; PART 2

(defun digits-observed (int-list)
  "Find first occurance (location) of each digit in the list"
  (let ((locs (make-array 10 :initial-element nil)))
    (labels ((helper (idx lst)
                     (if (null lst)
                         locs
                         (let ((digit (first lst)))
                           (unless (aref locs digit)
                             (setf (aref locs digit) idx))
                           (helper (+ 1 idx) (rest lst))))))
      (helper 0 int-list))))

(defun pick-n-from-list (need int-list)
  "Pick NEED digits greedily from int-list: at each step, look at the
   prefix that leaves at least (n-1) elements for the tail, choose the
   largest digit observed earliest, append it to result, and continue."
  (labels ((helper (n lst acc)
                   (if (<= n 0)
                       (nreverse acc)
                       (let* ((prefix-len (max 0 (- (length lst) (1- n))))
                              (ssq (subseq lst 0 prefix-len))
                              (locs (digits-observed ssq))
                              ;; find first large observed digit (9..0)
                              (first-large (loop for d from 9 downto 0
                                                 for loc = (aref locs d)
                                                   when loc
                                                   return (cons d loc))))

                         (when *debug* (format t "n: ~A, lst: ~A, prefix-len: ~A, ssq: ~A, locs: ~A, first-large: ~A~%"
                                         n lst prefix-len ssq locs first-large))

                         (if (null first-large)
                             ;; Nothing selectable in the prefix; stop early.
                             (nreverse acc)
                             (let* ((selected-digit (car first-large))
                                    (selected-idx (cdr first-large))
                                    ;; Drop through the selected digit in the original list.
                                    (remaining-seq (subseq lst (min (length lst)
                                                                 (+ selected-idx 1)))))
                               (helper (1- n) remaining-seq (cons selected-digit acc))))))))
    (helper need int-list '())))

(defun int-from-digits (digit-list)
  (reduce (lambda (acc d) (+ (* acc 10) d)) digit-list))

(defun p2 ()

  (let* ((lines (fetch-input))
         (vlists (mapcar #'line-to-int-list lines))
         (picked-lists (mapcar (lambda (lst) (pick-n-from-list 12 lst)) vlists))
         (numbers (mapcar #'int-from-digits picked-lists))
         (result (reduce #'+ numbers)))
    (format t "Part 2 Result: ~A~%" result)))