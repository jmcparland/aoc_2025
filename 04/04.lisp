(defparameter *test-data* '("..@@.@@@@."
                            "@@@.@.@.@@"
                            "@@@@@.@.@@"
                            "@.@@@@..@."
                            "@@.@@@@.@@"
                            ".@@@@@@@.@"
                            ".@.@.@.@@@"
                            "@.@@@.@@@@"
                            ".@@@@@@@@."
                            "@.@.@@@.@."))

(defparameter *input* "04/input")

(defun char->bool (ch)
  (cond
   ((char= ch #\@) t)
   ((char= ch #\.) nil)
   (t (error "Unexpected character: ~A" ch))))

(defun strings->bool-grid (lines)
  (let* ((rows (length lines))
         (cols (length (first lines)))
         (grid (make-array (list rows cols) :element-type 'boolean :initial-element nil)))

    (loop for i from 0 below rows
          for line in lines do
            (loop for j from 0 below cols
                  do (setf (aref grid i j) (char->bool (char line j)))))
    grid))

(defun neighbors (grid row col)
  (let* ((deltas '((-1 -1) (-1 0) (-1 1)
                           (0 -1) (0 1)
                           (1 -1) (1 0) (1 1)))
         (rows (array-dimension grid 0))
         (cols (array-dimension grid 1))
         (result '()))
    (dolist (delta deltas)
      (let* ((new-row (+ row (first delta)))
             (new-col (+ col (second delta))))
        (when (and (>= new-row 0) (< new-row rows)
                   (>= new-col 0) (< new-col cols)
                   (aref grid new-row new-col))
              (push (cons new-row new-col) result))))
    result))

(defun neighbor-grid (grid)
  (let* ((rows (array-dimension grid 0))
         (cols (array-dimension grid 1))
         (result (make-array (list rows cols) :initial-element nil)))
    (loop for i from 0 below rows do
            (loop for j from 0 below cols
                    when (aref grid i j)
                  do
                    (setf (aref result i j) (< (length (neighbors grid i j)) 4))))
    result))

(defun p1 (lines)
  (let* ((grid (strings->bool-grid lines))
         (rows (array-dimension grid 0))
         (cols (array-dimension grid 1)))
    ;; Count cells that are '@' and have fewer than 4 neighbors
    (loop for i from 0 below rows
            sum (loop for j from 0 below cols
                        count (and (aref grid i j)
                                   (< (length (neighbors grid i j)) 4))))))

;; PART 2 -- DIFFERENT APPROACH

(defun fetch-input ()
  (with-open-file (stream *input*)
    (loop for line = (read-line stream nil :eof)
          while (not (eq line :eof))
          collect line into lines
          finally (return lines))))

(defun lines->cons (lines)
  (loop for line in lines
        for i from 0
          nconc (loop for ch across line
                      for j from 0
                        when (char= ch #\@)
                      collect (cons i j))))

(defun lines->hashmap (lines)
  (let ((cell-map (make-hash-table :test 'equal)))
    (loop for line in lines
          for i from 0 do
            (loop for ch across line
                  for j from 0 do
                    (when (char= ch #\@)
                          (setf (gethash (cons i j) cell-map) t))))
    cell-map))

(defun neighbor-count (cells pos)
  (let* ((deltas '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)))
         (count 0))
    (dolist (delta deltas)
      (let* ((neighbor-pos (cons (+ (car pos) (first delta))
                                 (+ (cdr pos) (second delta)))))
        (when (member neighbor-pos cells :test #'equal)
              (incf count))))
    count))

(defun neighbor-count-hashmap (cells pos)
  (let* ((deltas '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)))
         (count 0))
    (dolist (delta deltas)
      (let* ((neighbor-pos (cons (+ (car pos) (first delta))
                                 (+ (cdr pos) (second delta)))))
        (when (gethash neighbor-pos cells)
              (incf count))))
    count))

(defun p2-alg (lines)
  ;   (let* ((original-cells (lines->cons lines))
  (let* ((original-cells (lines->cons lines))
         (initial-size (length original-cells)))
    (labels ((thrash (last-size cells)
                     (let* ((new-cells (remove-if (lambda (pos)
                                                    (< (neighbor-count cells pos) 4))
                                           cells))
                            (new-size (length new-cells)))

                       (if (= new-size last-size)
                           (cons initial-size new-size)
                           (thrash new-size new-cells)))))
      (thrash initial-size original-cells))))

(defun p2 (lines)
  (let* ((result (p2-alg lines))
         (initial-size (car result))
         (final-size (cdr result)))
    (format t "Initial size: ~A~%Final size: ~A~%Difference ~A~%" initial-size final-size (- initial-size final-size))))


(defun p2-alg-hashmap (lines)
  (let* ((original-cells (lines->hashmap lines))
         (initial-size (hash-table-count original-cells)))
    (labels ((thrash (last-size cells)
                     (let ((new-cells (make-hash-table :test 'equal)))
                       ;; Copy cells with >= 4 neighbors to new hash table
                       (maphash (lambda (pos val)
                                  (declare (ignore val))
                                  (when (>= (neighbor-count-hashmap cells pos) 4)
                                        (setf (gethash pos new-cells) t)))
                                cells)
                       (let ((new-size (hash-table-count new-cells)))
                         (if (= new-size last-size)
                             (cons initial-size new-size)
                             (thrash new-size new-cells))))))
      (thrash initial-size original-cells))))


(defun p2-hashmap (lines)
  (let* ((result (p2-alg-hashmap lines))
         (initial-size (car result))
         (final-size (cdr result)))
    (format t "Initial size: ~A~%Final size: ~A~%Difference ~A~%" initial-size final-size (- initial-size final-size))))


(time (p2 (fetch-input)))

; cl-user>(time (p2 (fetch-input)))
; Evaluation took:
;   21.663 seconds of real time
;   21.691664 seconds of total run time (21.656951 user, 0.034713 system)
;   [ Real times consist of 0.005 seconds GC time, and 21.658 seconds non-GC time. ]
;   [ Run times consist of 0.005 seconds GC time, and 21.687 seconds non-GC time. ]
;   100.13% CPU
;   65,784,976 bytes consed

; cl-user>(time (p2-hashmap (fetch-input)))

; Evaluation took:
;   0.089 seconds of real time
;   0.132605 seconds of total run time (0.130605 user, 0.002000 system)
;   [ Run times consist of 0.003 seconds GC time, and 0.130 seconds non-GC time. ]
;   149.44% CPU
;   319,176,834 processor cycles
;   82,286,448 bytes consed

; NIL
; cl-user >
