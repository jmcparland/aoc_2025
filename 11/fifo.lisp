(defstruct queue
  (front '())
  (rear '()))

(defun enqueue (q x)
  (setf (queue-rear q) (cons x (queue-rear q)))
  q)

(defun dequeue (q)
  (cond
   ((queue-front q)
     (let ((x (car (queue-front q))))
       (setf (queue-front q) (cdr (queue-front q)))
       x))
   ((queue-rear q)
     (setf (queue-front q) (reverse (queue-rear q)))
     (setf (queue-rear q) '())
     (dequeue q))
   (t nil))) ; queue is empty

(defun queue-empty-p (q)
  (and (null (queue-front q)) (null (queue-rear q))))