(defun copy-matrix (matrix)
  "Create a copy of a 2D array matrix."
  (let* ((rows (array-dimension matrix 0))
         (cols (array-dimension matrix 1))
         (copy (make-array (list rows cols))))
    (dotimes (i rows)
      (dotimes (j cols)
        (setf (aref copy i j) (aref matrix i j))))
    copy))

(defun swap-rows (matrix row1 row2)
  "Swap two rows in a matrix."
  (let ((cols (array-dimension matrix 1)))
    (dotimes (j cols)
      (let ((temp (aref matrix row1 j)))
        (setf (aref matrix row1 j) (aref matrix row2 j))
        (setf (aref matrix row2 j) temp)))))

(defun gcd-extended (a b)
  "Extended Euclidean algorithm. Returns (gcd x y) where gcd = ax + by."
  (if (zerop b)
      (list a 1 0)
      (destructuring-bind (g x1 y1) (gcd-extended b (mod a b))
        (list g y1 (- x1 (* (floor a b) y1))))))

(defun gauss-jordan-integer (matrix)
  "Perform Gauss-Jordan elimination on an integer matrix.
   Matrix should be augmented [A|b] where last column is b.
   Returns the reduced row echelon form."
  (let* ((m (copy-matrix matrix))
         (rows (array-dimension m 0))
         (cols (array-dimension m 1))
         (lead 0))

    (dotimes (r rows)
      (when (>= lead (1- cols)) ; don't pivot on augmented column
            (return-from gauss-jordan-integer m))

      ;; Find pivot
      (let ((i r))
        (loop while (zerop (aref m i lead))
              do (incf i)
                (when (>= i rows)
                      (setf i r)
                      (incf lead)
                      (when (>= lead (1- cols))
                            (return-from gauss-jordan-integer m))))

        ;; Swap rows if needed
        (unless (= i r)
          (swap-rows m r i))

        ;; Eliminate column entries
        (dotimes (i rows)
          (unless (= i r)
            (when (not (zerop (aref m r lead)))
                  (let* ((a (aref m i lead))
                         (b (aref m r lead))
                         (g (gcd a b)))
                    (unless (zerop g)
                      (let ((mult-i (/ b g))
                            (mult-r (/ a g)))
                        ;; Row i = mult-i * row_i - mult-r * row_r
                        (dotimes (j cols)
                          (setf (aref m i j)
                            (- (* mult-i (aref m i j))
                               (* mult-r (aref m r j)))))))))))

        (incf lead)))
    m))

(defun print-matrix (matrix)
  "Print a matrix in readable form."
  (let ((rows (array-dimension matrix 0))
        (cols (array-dimension matrix 1)))
    (dotimes (i rows)
      (dotimes (j cols)
        (format t "~6D " (aref matrix i j)))
      (format t "~%"))))


(defun free-variables (rref-matrix)
  "Extract free variables from the reduced row echelon form matrix."
  (let* ((rows (array-dimension rref-matrix 0))
         (cols (array-dimension rref-matrix 1))
         (pivot-cols '())
         (free-vars '()))
    ;; Find pivot columns (leading 1s in each row)
    (dotimes (i rows)
      (dotimes (j (1- cols))
        (unless (zerop (aref rref-matrix i j))
          (push j pivot-cols)
          (return))))
    ;; Free variables are non-pivot columns
    (dotimes (j (1- cols))
      (unless (member j pivot-cols)
        (push j free-vars)))
    (nreverse free-vars)))


;; Example usage:
;; (defvar *test* (make-array '(3 4) :initial-contents
;;                            '((2 1 -1 8)
;;                              (-3 -1 2 -11)
;;                              (-2 1 2 -3))))
;; (print-matrix (gauss-jordan-integer *test*))
