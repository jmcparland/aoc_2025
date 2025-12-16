(defun parse-pattern (pattern)
  "Parse pattern block like (\"0:\" \"###\" \"##.\" \"##.\")"
  ;; First line contains the ID, rest are the pattern lines
  (let* ((first-line (first pattern))
         (colon-pos (position #\: first-line))
         (id (parse-integer (subseq first-line 0 colon-pos)))
         (lines (rest pattern)))
    (cons id lines)))

(defun parse-requirements (requirements)
  "Parse requirements like (\"4x4: 0 0 0 0 2 0\" \"12x5: 1 0 1 0 2 2\" ...)"
  (mapcar (lambda (req-line)
            (let* ((colon-pos (position #\: req-line))
                   (dims-str (subseq req-line 0 colon-pos))
                   (x-pos (position #\x dims-str))
                   (width (parse-integer (subseq dims-str 0 x-pos)))
                   (height (parse-integer (subseq dims-str (1+ x-pos))))
                   (numbers-str (subseq req-line (+ colon-pos 2)))
                   (numbers (mapcar #'parse-integer
                              (split-sequence:split-sequence #\Space numbers-str))))
              (list width height numbers)))
      requirements))

(defun parse-file (path)
  (let* ((slurp (with-open-file (stream path)
                  (loop for line = (read-line stream nil)
                        while line
                        collect line)))
         (blocks (split-sequence:split-sequence-if
                   (lambda (line) (string= line ""))
                   slurp))
         (requirements-block (car (last blocks)))
         (pattern-blocks (butlast blocks)))

    (list (mapcar #'parse-pattern pattern-blocks)
          (parse-requirements requirements-block))))

(defun block-weight (block)
  "((0 #.# ### ##.) (1 ..# .## ##.) (2 ### ### #..) (3 #.. ##. ###)
  (4 ### #.# #.#) (5 ### .#. ###))"

  (let* ((id (first block))
         (lines (rest block))
         (weight (reduce #'+
                   (mapcar (lambda (line)
                             (count #\# line))
                       lines))))

    (cons id weight)))

; ((0 . 7) (1 . 5) (2 . 7) (3 . 6) (4 . 7) (5 . 7))

(defun dot-product (vec1 vec2)
  (reduce #'+
    (mapcar #'* vec1 vec2)))

(defun available-required (weights requirements)

  (let* ((ws (mapcar #'cdr weights)))

    (mapcar (lambda (req)
              (let* ((available (* (first req) (second req)))
                     (req-nums (third req))
                     (required (dot-product ws req-nums)))
                (list available required)))

        requirements)))

(defun filtered-delta (ars)
  (loop for pair in ars
        for available = (first pair)
        for required = (second pair)
          when (>= available required)
        collect (/ (float available) required)))

; cl-user>(reduce #'min (filtered-delta ars))
; 1.3608978

; cl-user>(length (filtered-delta ars))
; 583
