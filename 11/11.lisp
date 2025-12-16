(defun process-line (line)
  (let* ((parts (split-sequence:split-sequence #\: line))
         (node-name (string-trim '(#\Space #\Tab #\Newline) (first parts)))
         (connections (mapcar (lambda (s)
                                (string-trim '(#\Space #\Tab #\Newline) s))
                          (split-sequence:split-sequence #\Space (second parts)))))
    (cons node-name connections)))

(defun read-graph (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil :eof)
          until (eq line :eof)
            unless (string= "" (string-trim '(#\Space #\Tab #\Newline) line))
          collect (process-line line))))

(defun topological-sort (graph)
  "Perform topological sort on graph. Graph is list of (node . children)."
  (let ((in-degree (make-hash-table :test 'equal))
        (adj-list (make-hash-table :test 'equal))
        (result '())
        (queue '()))

    ;; Initialize all nodes with in-degree 0
    (dolist (entry graph)
      (let ((node (car entry)))
        (unless (gethash node in-degree)
          (setf (gethash node in-degree) 0))
        (setf (gethash node adj-list) (cdr entry))))

    ;; Calculate in-degrees
    (dolist (entry graph)
      (dolist (child (cdr entry))
        (unless (gethash child in-degree)
          (setf (gethash child in-degree) 0))
        (incf (gethash child in-degree))))

    ;; Find all nodes with in-degree 0
    (maphash (lambda (node degree)
               (when (zerop degree)
                     (push node queue)))
             in-degree)

    ;; Process queue
    (loop while queue
          do (let ((node (pop queue)))
               (push node result)
               (dolist (child (gethash node adj-list))
                 (decf (gethash child in-degree))
                 (when (zerop (gethash child in-degree))
                       (push child queue)))))

    (nreverse result)))


(defun following-nodes (graph node &key (include-self nil))
  "Return list of nodes that can be reached from NODE in GRAPH."
  (let ((visited (make-hash-table :test 'equal))
        (result '()))
    (labels ((dfs (n)
                  (unless (gethash n visited)
                    (setf (gethash n visited) t)
                    (push n result)
                    (dolist (child (cdr (assoc n graph :test 'equal)))
                      (dfs child)))))
      (dfs node))
    (if include-self
        result
        (remove node result))))

(defun predecessor-nodes (graph node &key (include-self nil))
  "Return list of nodes that can reach NODE in GRAPH."
  (let ((reverse-graph (make-hash-table :test 'equal)))
    ;; Build reverse graph
    (dolist (entry graph)
      (let ((n (car entry)))
        (dolist (child (cdr entry))
          (push n (gethash child reverse-graph)))))
    ;; DFS on reverse graph
    (let ((visited (make-hash-table :test 'equal))
          (result '()))
      (labels ((dfs (n)
                    (unless (gethash n visited)
                      (setf (gethash n visited) t)
                      (push n result)
                      (dolist (parent (gethash n reverse-graph))
                        (dfs parent)))))
        (dfs node))
      (if include-self
          result
          (remove node result)))))

; cl-user>(member "dac" (following-nodes graph "fft") :test 'equal)
; (dac xyr fqw dwy mzf ukl fen kbu kqe zqx xva scf you pdz zwa yty jip edr doy
;  zjc xtr hzz jqm fwm kwi lhn bzy sny yyb qus gnf zor gpr tbq yre aya ffv ndh
;  dxn agz qmv wiw dsw hem mxn dgo vlc rtf nib ehv zwz qvw nrp jxw onk kru qcb
;  xhf qvm ull sqf aey olu kvg frv xrk obg cni srp dth bli amw ogu ome jgm teu
;  qvi hid nzn jce oja zwx xnq rkk juj sao nxy kom tpo qaq ggn ctv ygp akr iqg
;  gep nan wpg amx wxl lcw kcq shc bcf gxm esl cvu kwn hih moq tqv ooi gdp zsg
;  kjt aza zjd mjw iki zon fop snp gjq pxj gof mxv zrr aeo otr riu nwz tkr zod
;  bhg liv yxn out ojk naw uyt roc uiq eht apm say sva ylk ejg afc cqa zmj cut
;  qqv rfl ceh rbn mfp vwd bux )
; cl-user>(member "fft" (following-nodes graph "dac") :test 'equal)
; NIL
; cl-user>(member "out" (following-nodes graph "dac") :test 'equal)
; (out nan wpg akr qaq juj yyb you dbu hcz )
; svr < fft < dac < out

(defun limit-graph-to-nodes (graph nodes)
  "Return subgraph containing only NODES and edges between them."
  (mapcar (lambda (entry)
            (cons (car entry)
                  (remove-if-not (lambda (child)
                                   (member child nodes :test 'equal))
                      (cdr entry))))
      (remove-if (lambda (entry)
                   (not (member (car entry) nodes :test 'equal)))
          graph)))

(defun count-paths (graph topo-sort)
  "Count number of paths from first to last node in topologically sorted graph."
  (let ((path-count (make-hash-table :test 'equal))
        (start (first topo-sort))
        (end (car (last topo-sort))))

    ;; Initialize: start node has 1 path (itself)
    (setf (gethash start path-count) 1)

    ;; Process nodes in topological order
    (dolist (node topo-sort)
      (let ((current-paths (gethash node path-count 0))
            (children (cdr (assoc node graph :test 'equal))))
        ;; Distribute paths to all children
        (dolist (child children)
          (incf (gethash child path-count 0) current-paths))))

    ;; Return number of paths to end node
    (gethash end path-count 0)))

(defun paths-between (graph start end)
  "Limit the graph to nodes following START and preceding END,
     topologically sort the limited graph,
     then count paths from START to END."

  (let* ((before (predecessor-nodes graph end :include-self t))
         (after (following-nodes graph start :include-self t))
         (common (intersection before after :test 'equal))
         (filtered-graph (limit-graph-to-nodes graph common))
         (sorted (topological-sort filtered-graph))
         (count (count-paths filtered-graph sorted)))

    (format t "~%Successors of ~A:~%~A~%" start after)
    (format t "~%Predecessors of ~A:~%~A~%" end before)
    (format t "~%Intersection:~%~A~%" common)
    (format t "~%Filtered graph:~%~A~%" filtered-graph)
    (format t "~%From ~A:~%~A~%" start (assoc start filtered-graph :test 'equal))
    (format t "~%Sorted:~%~A~%" sorted)
    (format t "~%Count of paths from ~A to ~A: ~A~%" start end count)

    count))


(defun part2 ()
  "We've verified the graph is acyclic, fully connected, and svr < fft < dac < out.
  We just need to multiply the paths from svr to fft, fft to dac, and dac to out."

  (let* ((graph (read-graph "11/input"))
         (paths-svr-fft (paths-between graph "svr" "fft"))
         (paths-fft-dac (paths-between graph "fft" "dac"))
         (paths-dac-out (paths-between graph "dac" "out"))
         (total-paths (* paths-svr-fft paths-fft-dac paths-dac-out)))
    (format t "~%Total paths from svr to out via dac and fft: ~A~%" total-paths)
    total-paths))


; Evaluation took:
;   0.036 seconds of real time
;   0.048279 seconds of total run time (0.043687 user, 0.004592 system)
;   133.33% CPU
;   20,345,008 bytes consed
