(defstruct puzzle
  target
  options
  joltages)

(defun parse-line (line)
  "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
  (let* ((parts (split-sequence:split-sequence #\Space line))
         (target (first parts))
         (options (mapcar (lambda (s)
                            (mapcar #'read-from-string
                              (split-sequence:split-sequence #\,
                                                             (subseq s 1 (1- (length s))))))
                      (subseq parts 1 (- (length parts) 1))))
         (joltages (mapcar #'read-from-string
                     (split-sequence:split-sequence #\,
                                                    (subseq (car (last parts)) 1 (1- (length (car (last parts)))))))))
    (make-puzzle :target target
                 :options options
                 :joltages joltages)))

(defun file->puzzles (filename)
  (with-open-file (stream filename)
    (mapcar #'parse-line
      (loop for line = (read-line stream nil)
            while line
            collect line))))


(defun target->option (target)
  "Convert target to binary representation."
  (let ((position 0)
        (result '()))
    (loop for ch across target
          do (when (member ch '(#\. #\#))
                   (when (char= ch #\#) (push position result))
                   (incf position)))
    (nreverse result)))


(defun option->binary (option)
  "Convert option (list of indices) to binary representation."
  (reduce (lambda (acc idx)
            (logior acc (ash 1 idx)))
      option
    :initial-value 0))


(defun puzzle->binary (p)
  "Convert both target and options to binary representation."
  (let* ((target-binary (option->binary (target->option (puzzle-target p))))
         (options-binary (mapcar #'option->binary (puzzle-options p))))

    (setf (puzzle-target p) target-binary)
    (setf (puzzle-options p) options-binary))
  p)

(defun next-reachable (current options)
  "With current as a map (reachable state -> steps) and options, compute next reachable states."
  (let ((next (make-hash-table :test 'equal)))
    (maphash (lambda (state steps)
               (dolist (opt options)
                 (let ((new-state (logxor state opt))
                       (new-steps (1+ steps)))
                   (when (or (not (gethash new-state next))
                             (< new-steps (gethash new-state next)))
                         (setf (gethash new-state next) new-steps)))))
             current)
    next))

(defun p1-solve (puzzle)
  "Solve puzzle for part 1."
  (let* ((current (make-hash-table :test 'equal))
         (target (puzzle-target puzzle))
         (options (puzzle-options puzzle)))
    (setf (gethash 0 current) 0) ; start from state 0 with 0 steps
    (loop for step from 1
          do (setf current (next-reachable current options))
          until (gethash target current))
    (gethash target current)))

(defun show-hash (cmap)
  (maphash (lambda (key value)
             (format t "~a: ~a~%" key value))
           cmap))

(defun p1 (path)
  (let* ((puzzles (file->puzzles path))
         (binary-puzzles (mapcar #'puzzle->binary puzzles)))
    (loop for p in binary-puzzles
            sum (p1-solve p))))

(defun solve-one (target options)
  (let* ((q (make-queue))
         (observed (make-hash-table :test 'equal)))
    (labels ((f (state options depth)
                (cond
                 ((= target state) depth)
                 ((null options) nil)
                 (t
                   (dolist (opt options)
                     (let* ((new-state (logxor state opt))
                            (new-options (remove opt options)))
                       (unless (gethash new-state observed)
                         (setf (gethash new-state observed) t)
                         (enqueue q (list new-state new-options (1+ depth))))))))))

      (enqueue q (list 0 options 0)) ; initial state
      (loop while (not (queue-empty-p q))
            do (let* ((item (dequeue q))
                      (state (first item))
                      (opts (second item))
                      (depth (third item))
                      (result (f state opts depth)))
                 (when result
                       (return result)))))))


(defun p1b-solve (puzzle)
  (let* ((puzzle-copy (copy-structure puzzle))
         (bp (puzzle->binary puzzle-copy))
         (target (puzzle-target bp))
         (options (puzzle-options bp)))
    (solve-one target options)))


(defun p1b (puzzles)
  (loop for p in puzzles
          sum (p1b-solve p)))


;; PART 2

; Find smallest convex combination of derived option vectors that add to joltage vector.
;  #S(PUZZLE
;     :TARGET [#.#.#.#]
;     :OPTIONS ((0 2 4 6) (1 2 4 6) (1 2 3 4 5) (2 3 4) (1 4) (0 3) (4 5 6)
;               (3 4 5 6))
;     :JOLTAGES (34 36 40 40 58 22 19))

(defstruct item
  count
  option)


(defun vector-of-options (puzzle)
  "Sort options by #items (descending) into vector of lists."
  (coerce
    (mapcar #'second
      (sort (mapcar (lambda (x) (list (length x) x))
                (puzzle-options puzzle))
          (lambda (x y) (> (first x) (first y)))))
    'vector))

(defun voptions->vitems (voptions)
  "Convert vector of options to vector of items with count initialized to 0."
  (let ((len (length voptions))
        (result (make-array (length voptions))))
    (dotimes (i len)
      (setf (aref result i) (make-item :count 0 :option (aref voptions i))))
    result))


(defun vitem-initialize-counts (p)
  (let* ((voptions (vector-of-options p))
         (vitems (voptions->vitems voptions))
         (state (coerce (puzzle-joltages p) 'vector)))

    (dotimes (i (length vitems))
      (let* ((item (aref vitems i))
             (option (item-option item))
             (step-result (max-step state option)))
        (when step-result
              (setf (item-count item) (first step-result))
              (setf state (second step-result)))))
    vitems))

; cl-user>(vitem-initialize-counts p)
; #(#S(ITEM :COUNT 27 :OPTION (1 2 3 4 5 6))
;   #S(ITEM :COUNT 10 :OPTION (0 2 3 4 5 6))
;   #S(ITEM :COUNT 0 :OPTION (0 1 2 3 6)) #S(ITEM :COUNT 0 :OPTION (1 2 3 4 5))
;   #S(ITEM :COUNT 13 :OPTION (0 2 5 6)))

(defun apply-vitems-to-state (state vitems)
  "Apply all vitems to get current state vector."
  (let ((new-state (copy-seq state)))
    (dotimes (i (length vitems))
      (let* ((item (aref vitems i))
             (count (item-count item))
             (option (item-option item)))
        (when count
              (dolist (pos option)
                (decf (aref new-state pos) count)))))
    new-state))

(defun decrement-state (state-vector position-list)
  "decrement current state by option (list of indices)"
  (let ((new-state (copy-seq state-vector)))
    (dolist (pos position-list)
      (if (aref new-state pos)
          (decf (aref new-state pos))
          (return-from decrement-state nil)))
    new-state))


(defun max-step (state-vector option)
  "Greedily decrement state vector with as many option applications as possible."
  ; Can't consume more than the min count in state_vector at option indices
  (let* ((min-count (reduce #'min
                      (mapcar (lambda (pos) (aref state-vector pos))
                          option)))
         (new-state (copy-seq state-vector)))
    (when min-count
          (dolist (pos option)
            (decf (aref new-state pos) min-count))
          (list min-count new-state))))


(defun step-n (state-vector option n)
  (let ((new-state (copy-seq state-vector)))
    (dolist (pos option)
      (decf (aref new-state pos) n))
    new-state))


;; Thursday 11 Dec 2025

;; set each vector to max individual account,
;; if fails, generate list of each coordinate backed off by 1

(defun initial-states (vjoltages voptions)
  (loop for opt across voptions
        collect (max-step vjoltages opt)))


(defun thursday (puzzle)
  (let* ((queue (make-queue))
         (vjoltages (coerce (puzzle-joltages puzzle) 'vector))
         (voptions (vector-of-options puzzle))
         (initial-coefficients (loop for opt across voptions
                                     collect (first (max-step vjoltages opt)))))

    (enqueue queue initial-coefficients)

    (labels ((apply-coefficients (coefficients)
                                 (let ((vstate (copy-seq vjoltages)))
                                   (dotimes (i (length coefficients))
                                     (let ((count (nth i coefficients))
                                           (option (aref voptions i)))
                                       (when count
                                             (dolist (pos option)
                                               (decf (aref vstate pos) count)))))
                                   vstate)))

      (loop while (not (queue-empty-p queue))
            do (let* ((entry (dequeue queue))
                      (state (apply-coefficients entry)))
                 (format t "Checking: ~A -> State: ~A (Queue size: ~A)~%"
                   entry state (+ (length (queue-front queue))
                                  (length (queue-rear queue))))
                 (if (every (lambda (x) (zerop x)) state)

                     ;; found a solution
                     (progn
                      (format t "~A : ~A : ~A~%" (reduce #'+ entry) entry voptions)
                      (return-from thursday t))

                     ;; else: generate next states and enqueue them
                     (dotimes (i (length entry))
                       (let ((count (nth i entry)))
                         (when (and count (> count 0))
                               (let ((new-entry (copy-list entry)))
                                 (setf (nth i new-entry) (1- count))
                                 (enqueue queue new-entry)))))))))))
