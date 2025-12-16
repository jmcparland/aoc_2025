(defparameter *input* "08/input")
(defparameter *test-input* "08/test")

(defparameter *debug* nil)

(defun euclidean-distance (p1 p2)
  (sqrt (reduce #'+ (mapcar (lambda (a b) (expt (- a b) 2)) p1 p2))))

(defun list< (list1 list2)
  (cond
   ((null list1) (not (null list2)))
   ((null list2) nil)
   ((< (first list1) (first list2)) t)
   ((> (first list1) (first list2)) nil)
   (t (list< (rest list1) (rest list2)))))

(defun parse-line (line)
  (mapcar #'parse-integer (split-sequence:split-sequence #\, line)))

(defun parse-input (path)
  (with-open-file (stream path)
    (sort (loop for line = (read-line stream nil)
                while line
                collect (parse-line line))
        #'list<)))

(defun distance-ranked-pairs (points)

  (labels ((f (x ys acc)
              (cond
               ((null ys) acc)
               (t (append
                    (mapcar (lambda (y) (list (euclidean-distance x y) x y)) ys)
                    (f (first ys) (rest ys) acc))))))

    (sort (f (first points) (rest points) '())
        #'< :key #'first)))

(defun build-clusters (ranked-pairs)

  (let ((point-to-cluster (make-hash-table :test 'equal)))

    (dolist (pair ranked-pairs)
      (let* (; first position is distance; we don't need it here
             (point1 (second pair))
             (point2 (third pair))
             (cluster1 (gethash point1 point-to-cluster))
             (cluster2 (gethash point2 point-to-cluster)))

        (cond
         ; both points are in different clusters; join them
         ((and cluster1 cluster2 (not (eq cluster1 cluster2)))

           (when *debug*
                 (format t "Merging clusters for points ~a and ~a~%" point1 point2))

           (loop for key being the hash-keys of cluster2
                 do (setf (gethash key cluster1) t)
                   (setf (gethash key point-to-cluster) cluster1))
           (clrhash cluster2))

         ; only point1 is in a cluster; add point2 to it
         ((and cluster1 (null cluster2))

           (when *debug*
                 (format t "Adding point ~a to cluster of point ~a~%" point2 point1))

           (setf (gethash point2 cluster1) t)
           (setf (gethash point2 point-to-cluster) cluster1))

         ; only point2 is in a cluster; add point1 to it
         ((and cluster2 (null cluster1))

           (when *debug*
                 (format t "Adding point ~a to cluster of point ~a~%" point1 point2))

           (setf (gethash point1 cluster2) t)
           (setf (gethash point1 point-to-cluster) cluster2))

         ; neither point is in a cluster; create a new cluster
         ((not (or cluster1 cluster2))
           (let ((new-cluster (make-hash-table :test 'equal)))

             (when *debug*
                   (format t "Creating new cluster for points ~a and ~a~%" point1 point2))

             (setf (gethash point1 new-cluster) t)
             (setf (gethash point1 point-to-cluster) new-cluster)
             (setf (gethash point2 new-cluster) t)
             (setf (gethash point2 point-to-cluster) new-cluster)))

         ; both points are already in the same cluster; do nothing
         (t (when *debug*
                  (format t "Points ~a and ~a are already in the same cluster~%" point1 point2))))))

    (remove-duplicates
        (loop for value being the hash-values of point-to-cluster
              collect value))))

(defun part1 (path edge-count cluster-count)
  (let* ((points (parse-input path))
         (ranked-pairs (distance-ranked-pairs points))
         (clusters (build-clusters (subseq ranked-pairs 0 edge-count)))
         (cluster-sizes (mapcar #'hash-table-count clusters))
         (sorted-sizes (sort cluster-sizes #'>)))
    (reduce #'* (subseq sorted-sizes 0 cluster-count) :initial-value 1)))

(defun part2 (path)
  (let* ((points (parse-input path))
         (number-of-points (length points))
         (ranked-pairs (distance-ranked-pairs points))
         (consumed (make-hash-table :test 'equal))
         (rows-consumed '()))
    ;; loop through ranked pairs until all points are observed
    ;; then continue, testing until all the collection forms a single cluster
    (dolist (row ranked-pairs)
      (let* ((point1 (second row))
             (point2 (third row)))
        (setf (gethash point1 consumed) t)
        (setf (gethash point2 consumed) t))
      (push row rows-consumed)
      (when (= (hash-table-count consumed) number-of-points)
            (when *debug*
                  (format t "All points observed after ~a edges~%" (length rows-consumed)))

            (let* ((clusters (build-clusters (reverse rows-consumed)))
                   (number-of-clusters (length clusters)))
              (format t "Number of clusters: ~a~%" (length clusters))
              (when (= number-of-clusters 1)
                    (format t "All points connected after ~a edges~%" (length rows-consumed))
                    (let ((last (first rows-consumed)))

                      (format t "Last edge: ~a to ~a at distance ~a~%"
                        (second last) (third last) (first last))

                      (return (* (first (second last)) (first (third last)))))))))))


; cl-user>(time (part2 "08/input"))
; Number of clusters: 1
; All points connected after 3803 edges
; Last edge: (30764 98693 98489) to (33370 90838 88612) at distance 12885.9375
; Evaluation took:
;   0.198 seconds of real time
;   0.235435 seconds of total run time (0.223786 user, 0.011649 system)
;   [ Real times consist of 0.025 seconds GC time, and 0.173 seconds non-GC time. ]
;   [ Run times consist of 0.025 seconds GC time, and 0.211 seconds non-GC time. ]
;   118.69% CPU
;   93,408,336 bytes consed
