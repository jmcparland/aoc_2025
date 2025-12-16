(defparameter *test* "07/test")
(defparameter *input* "07/input")

; accumulated start row encounters sequence of splitters
; accumulate starts colliding with splitters
; accumulator (collisions . vector of starts)
; accumulate path counts along the way, just in case...
(defun accumulate-line-data (acc line)

  ; scan line characters for splitters "^" and starts "S"
  (let ((len (length line))
        (collisions (car acc))
        (starts (copy-seq (cdr acc))))

    (loop for i from 0 below len do
            (let ((ch (char line i)))

              (cond
               ((char= ch #\S)
                 ; this is the first line of the file (initialize)
                 (setf (aref starts i) 1)
                 (setf collisions 0)
                 (return (cons collisions starts)))

               ((and (char= ch #\^) (> (aref starts i) 0))
                 ; splitter encountered with effect
                 (setf collisions (1+ collisions))
                 ;  (setf collisions (+ collisions (aref starts i)))
                 (setf (aref starts (1- i)) (+ (aref starts (1- i)) (aref starts i)))
                 (setf (aref starts (1+ i)) (+ (aref starts (1+ i)) (aref starts i)))
                 (setf (aref starts i) 0)))))

    (cons collisions starts)))

(defun zero-accumulator (length)
  (cons 0 (make-array length :initial-element 0)))


(defun read-lines (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun solve (path)
  (let* ((lines (read-lines path))
         (line-length (length (first lines)))
         (acc (zero-accumulator line-length))
         (result (reduce #'accumulate-line-data lines :initial-value acc))
         (collisions (car result))
         (starts (cdr result))
         (total-starts (reduce #'+ starts)))
    (format t "Collisions: ~A~%Timelines: ~A~%Starts: ~A~%" collisions total-starts starts)))