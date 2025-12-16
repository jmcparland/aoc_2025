(defun puzzle-to-matrix (p)
  "Convert puzzle to augmented matrix for Gauss-Jordan elimination."
  (let* ((options (puzzle-options p))
         (joltages (puzzle-joltages p))
         (num-opts (length options))
         (num-jolts (length joltages))
         (matrix (make-array (list num-jolts num-opts)
                   :initial-element 0)))
    ;; Fill in the matrix
    (dotimes (j num-jolts)
      (dotimes (i num-opts)
        (when (member j (nth i options))
              (setf (aref matrix j i) 1))))
    ;; Augment with joltages
    (let ((augmented-matrix (make-array (list num-jolts (1+ num-opts))
                              :initial-element 0)))
      (dotimes (j num-jolts)
        (dotimes (i num-opts)
          (setf (aref augmented-matrix j i) (aref matrix j i)))
        (setf (aref augmented-matrix j num-opts) (nth j joltages)))
      augmented-matrix)))


(defun max-button-pushes (p)
  "Calculate the max times an individual button (option) can be pushed."
  (let* ((options (puzzle-options p))
         (jvector (coerce (puzzle-joltages p) 'vector)))

    (labels ((f (opt)
                (let ((affected (mapcar (lambda (pos) (aref jvector pos)) opt)))
                  (reduce #'min affected))))

      (loop for opt in options
            collect (f opt)))))


(defun normalize-rref (matrix)
  "Divide each row by its leading non-zero coefficient."
  (let* ((rows (array-dimension matrix 0))
         (cols (array-dimension matrix 1))
         (m (copy-matrix matrix)))
    (dotimes (i rows)
      (let ((lead-val nil)
            (lead-col nil))
        ;; Find leading entry
        (dotimes (j (1- cols))
          (unless (zerop (aref m i j))
            (setf lead-val (aref m i j))
            (setf lead-col j)
            (return)))
        ;; Divide entire row by leading value
        (when lead-val
              (dotimes (j cols)
                (setf (aref m i j) (/ (aref m i j) lead-val))))))
    m))

(defun extract-solution (rref-matrix)
  "Extract solution vector from RREF matrix."
  (let* ((rows (array-dimension rref-matrix 0))
         (cols (array-dimension rref-matrix 1))
         (solution (make-array (1- cols) :initial-element nil)))
    (dotimes (i rows)
      ;; Find leading coefficient
      (dotimes (j (1- cols))
        (unless (zerop (aref rref-matrix i j))
          (setf (aref solution j)
            (/ (aref rref-matrix i (1- cols))
               (aref rref-matrix i j)))
          (return))))
    solution))


(defun enumerate-solutions (rref-matrix free-vars max-values)
  "Enumerate all valid solutions given free variables and max constraints.
   rref-matrix: normalized RREF matrix
   free-vars: list of free variable indices
   max-values: vector/list of max values for each coefficient"
  (let* ((num-vars (1- (array-dimension rref-matrix 1)))
         (solutions '()))

    (labels ((compute-dependent-vars (free-var-values)
                                     "Given values for free variables, compute dependent variables"
                                     (let ((solution (make-array num-vars :initial-element 0)))
                                       ;; Set free variable values
                                       (loop for fv in free-vars
                                             for val in free-var-values
                                             do (setf (aref solution fv) val))

                                       ;; Compute dependent variables from RREF
                                       (dotimes (i (array-dimension rref-matrix 0))
                                         (let ((pivot-col nil)
                                               (pivot-val nil))
                                           ;; Find pivot column in this row
                                           (dotimes (j num-vars)
                                             (unless (zerop (aref rref-matrix i j))
                                               (setf pivot-col j)
                                               (setf pivot-val (aref rref-matrix i j))
                                               (return)))

                                           (when pivot-col
                                                 ;; Calculate: x_pivot = (b - sum of other terms) / pivot_val
                                                 (let ((rhs (aref rref-matrix i num-vars))
                                                       (sum 0))
                                                   (dotimes (j num-vars)
                                                     (unless (= j pivot-col)
                                                       (incf sum (* (aref rref-matrix i j)
                                                                    (aref solution j)))))
                                                   (setf (aref solution pivot-col)
                                                     (/ (- rhs sum) pivot-val))))))
                                       solution))

             (valid-solution-p (solution)
                               "Check if solution satisfies all constraints"
                               (loop for i from 0 below num-vars
                                       always (and (>= (aref solution i) 0)
                                                   (<= (aref solution i) (nth i max-values))
                                                   (integerp (aref solution i)))))

             (generate-free-var-combos (vars remaining-maxes)
                                       "Generate all combinations of free variable values"
                                       (if (null vars)
                                           '(())
                                           (let ((current-var (first vars))
                                                 (max-val (first remaining-maxes)))
                                             (loop for val from 0 to max-val
                                                     append (mapcar (lambda (rest-combo)
                                                                      (cons val rest-combo))
                                                                (generate-free-var-combos
                                                                 (rest vars)
                                                                 (rest remaining-maxes))))))))

      ;; Get max values for free variables only
      (let ((free-var-maxes (mapcar (lambda (fv) (nth fv max-values))
                                free-vars)))
        ;; Generate all combinations of free variable values
        (dolist (free-combo (generate-free-var-combos free-vars free-var-maxes))
          (let ((sol (compute-dependent-vars free-combo)))
            (when (valid-solution-p sol)
                  (push sol solutions))))))

    (nreverse solutions)))

(defun weighted-solutions (solutions)
  "Given a list of solution vectors, compute their weights (sum of elements)."
  (sort (mapcar (lambda (sol)
                  (list (reduce #'+ sol) sol))
            solutions) (lambda (x y) (< (first x) (first y)))))

(defun explore (p &key (debug nil))
  (let* ((matrix (puzzle-to-matrix p))
         (rref (gauss-jordan-integer matrix))
         ;  (normalized-rref (normalize-rref rref))
         (max-pushes (max-button-pushes p))
         (free-vars (free-variables rref))
         ;  (solution (extract-solution normalized-rref))
         (all-solutions (weighted-solutions (enumerate-solutions rref free-vars max-pushes))))

    (when debug
          (format t "Puzzle: ~A~%" p)
          (format t "RREF:~%")
          (print-matrix rref)
          ; (print-matrix normalized-rref)
          (format t "Free Variables: ~A~%" free-vars)
          (format t "Max Pushes: ~A~%~%" max-pushes)
          ; (format t "Solution: ~A~%" solution)
          (format t "All Solutions: ~A~%" all-solutions))

    ;; a minimum weight for part 2
    (first (first all-solutions))))

(defun part2 (path)
  (let ((ps (file->puzzles path)))
    (reduce #'+ (mapcar #'explore ps))))


; cl-user>(time (part2 "10/input"))
; Evaluation took:
;   0.867 seconds of real time
;   0.924053 seconds of total run time (0.905278 user, 0.018775 system)
;   [ Real times consist of 0.026 seconds GC time, and 0.841 seconds non-GC time. ]
;   [ Run times consist of 0.026 seconds GC time, and 0.899 seconds non-GC time. ]
;   106.57% CPU
;   250,810,256 bytes consed
