(defun line-to-pairs (line)
  (let ((elements (split-sequence:split-sequence #\, line)))
    (list (parse-integer (first elements)) (parse-integer (second elements)))))

(defun list< (list1 list2)
  (cond
   ((null list1) (not (null list2)))
   ((null list2) nil)
   ((< (first list1) (first list2)) t)
   ((> (first list1) (first list2)) nil)
   (t (list< (rest list1) (rest list2)))))

(defun read-pairs-from-file (filename &key (sorted nil))
  (with-open-file (stream filename)
    (let* ((lines (loop for line = (read-line stream nil :eof)
                        while (not (eq line :eof))
                        collect line))
           (points (mapcar #'line-to-pairs lines)))
      (if sorted (sort points #'list<) points))))


(defun area (p1 p2)
  (let ((x1 (first p1))
        (y1 (second p1))
        (x2 (first p2))
        (y2 (second p2)))
    (* (1+ (abs (- x2 x1))) (1+ (abs (- y2 y1))))))

(defun max-area-scan (pairs)
  (reduce #'max (mapcar (lambda (pair) (area (first pair) (second pair))) pairs)))

(defun generate-pairs (points)
  (let ((vector-points (coerce points 'vector)))
    (loop for i from 0 below (length vector-points)
            append
            (loop for j from (1+ i) below (length vector-points)
                  collect (list (aref vector-points i) (aref vector-points j))))))

(defun p1 ()
  (max-area-scan
    (generate-pairs
      (read-pairs-from-file "09/input" :sorted t))))

(defun set-with-collision (cmap key value)
  (let ((existing (gethash key cmap)))
    (cond
     ((null existing) (setf (gethash key cmap) value))
     ((eq existing value) nil)
     (t (setf (gethash key cmap) 'collision)))))

(defun color-along-path (cmap p0 p1)
  (let* ((x0 (first p0))
         (y0 (second p0))
         (x1 (first p1))
         (y1 (second p1))
         (dx (if (> x1 x0) 1 (if (< x1 x0) -1 0)))
         (dy (if (> y1 y0) 1 (if (< y1 y0) -1 0)))
         (length (max (abs (- x1 x0)) (abs (- y1 y0))))
         (left (cond
                ((= dx 1) '(0 -1))
                ((= dx -1) '(0 1))
                ((= dy 1) '(1 0))
                ((= dy -1) '(-1 0)))))
    ; (format t "dx: ~A dy: ~A left: ~A~%" dx dy left)
    (set-with-collision cmap p0 'path)
    (set-with-collision cmap p1 'path)
    (loop for step from 1 below length
          do (let* ((x (+ x0 (* step dx)))
                    (y (+ y0 (* step dy)))
                    (lx (+ x (first left)))
                    (ly (+ y (second left))))
               ;  (format t "(~A ~A) (dx ~A dy ~A) (x ~A y ~A)~%" x0 y0 dx dy x y)
               (set-with-collision cmap (list x y) 'path)
               (set-with-collision cmap (list lx ly) 'left)))))

(defun show-hash (cmap &key (only-collisions nil))
  (maphash (lambda (key value)
             (when (or (not only-collisions) (eq value 'collision))
                   (format t "~a: ~a~%" key value)))
           cmap))


(defun process-circuit (points)
  (let* ((result (make-hash-table :test 'equal))
         (circuit (append points (list (first points)))))
    (labels ((process-segments (pts)
                               (when (>= (length pts) 2)
                                     (color-along-path result (first pts) (second pts))
                                     (process-segments (rest pts)))))
      (process-segments circuit)
      result)))

(defun just-lefts (cmap)
  (let ((lefts (make-hash-table :test 'equal)))
    (maphash (lambda (key value)
               (when (eq value 'left)
                     (setf (gethash key lefts) value)))
             cmap)
    lefts))

(defun rectangle-contains-left-p (lefts-map p1 p2)
  (let* ((x1 (first p1))
         (y1 (second p1))
         (x2 (first p2))
         (y2 (second p2))
         (xmin (min x1 x2))
         (xmax (max x1 x2))
         (ymin (min y1 y2))
         (ymax (max y1 y2)))
    (maphash (lambda (key value)
               (declare (ignore value))
               (destructuring-bind (x y) key
                 (when (and (<= xmin x xmax)
                            (<= ymin y ymax))
                       (return-from rectangle-contains-left-p t))))
             lefts-map)
    nil))

(defun p2 ()
  (let* ((points (read-pairs-from-file "09/input"))
         (cmap (process-circuit points))
         (lefts (just-lefts cmap))
         (pairs (generate-pairs points))
         (valid-pairs
          (remove-if
              (lambda (pair)
                (rectangle-contains-left-p lefts (first pair) (second pair)))
              pairs)))
    (max-area-scan valid-pairs)))


; cl-user>(time (p2))
; Evaluation took:
;   184.606 seconds of real time
;   184.164428 seconds of total run time (183.854761 user, 0.309667 system)
;   [ Real times consist of 0.038 seconds GC time, and 184.568 seconds non-GC time. ]
;   [ Run times consist of 0.038 seconds GC time, and 184.127 seconds non-GC time. ]
;   99.76% CPU
;   260,675,696 bytes consed
