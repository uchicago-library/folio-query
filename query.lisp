(load #P"~/quicklisp/setup.lisp")
(ql:quickload '#:com.inuoe.jzon)
(sb-ext:add-package-local-nickname '#:jzon '#:com.inuoe.jzon)

(defun my-echo ()
(loop
  for line = (read-line nil nil)
  when (null line)
    do (return)
  when (string= line "end")
    do (return)
  do (write-line "preface")
  do (write-line line)))

(defun parse-cli-json ()
  (let ((input (with-output-to-string (s)
                 (loop for line = (read-line nil nil)
                       while (and line (plusp (length line)))
                       do (write-string line s)))))
    (jzon:parse input)))

(defun list-nodes (adj-list)
  (loop for node in adj-list
	do (write-line (format nil "~A" (first node)))))

(defun list-edges (adj-list)
  (loop for node in adj-list
	for cur = (first node)
	do (loop for next in (cadr node)
		 do (write-line (format nil "~A to ~A" cur next)))))

(defstruct (queue (:constructor make-queue ()))
  (head nil :type list)
  (tail nil :type list))

(defun enqueue (item queue)
  (let ((cell (list item)))
    (if (queue-tail queue)
        (setf (cdr (queue-tail queue)) cell)
        (setf (queue-head queue) cell))
    (setf (queue-tail queue) cell)))

(defun dequeue (queue)
  (let ((cell (queue-head queue)))
    (when cell
      (setf (queue-head queue) (cdr cell))
      (unless (queue-head queue)
        (setf (queue-tail queue) nil))
      (car cell))))

(defun adj-of (adj-list val)
  (loop for node in adj-list
	when (equalp (first node) val)
	  return (cadr node)))

(defun backtrace-path (parent-list start dest)
  (let ((curr dest)
	(path ()))
    (loop while (not (equalp curr start))
	  do (push curr path)
	  do (setf curr (gethash curr parent-list))
	  when (null curr)
	    do (return-from nil))
    (push curr path)
    (if (null curr)
	nil
	path)))
	

(defun get-shortest-path (adj-list start dest)
  (let ((visited ())
	(parent (make-hash-table :test #'equal))
	(queue (make-queue)))
    (enqueue start queue)
    (push start visited)
    (loop while (queue-head queue)
	  for curr = (dequeue queue)
	  for edges = (adj-of adj-list curr)
	  do (loop for next in edges
		   if (not (member next visited :test #'equal))
		     do (push next visited)
		     and do (enqueue next queue)
		     and do (setf (gethash next parent) curr)))
    (backtrace-path parent start dest)))

(defun reachable (adj-list start)
  (let ((seen ())
	(visited ())
	(queue (make-queue)))
    (enqueue start queue)
    (push start visited)
    (loop while (queue-head queue)
	  for curr = (dequeue queue)
	  for edges = (adj-of adj-list curr)
	  do (push curr seen)
	  do (loop for next in edges
		   if (not (member next visited))
		     do (push next visited)
		     and do (enqueue next queue)))
    seen))

(defun node-func-binds ()
  (let ((binds (make-hash-table)))
    (setf (gethash 'A binds) (lambda () (format t "Node A~%")))
    (setf (gethash 'B binds) (lambda () (format t "Node B~%")))
    (setf (gethash 'C binds) (lambda () (format t "Node C~%")))
    (setf (gethash 'D binds) (lambda () (format t "Node D~%")))
    (setf (gethash 'E binds) (lambda () (format t "Node E~%")))
    (setf (gethash 'F binds) (lambda () (format t "Node F~%")))
    binds))

(defun print-path (path)
  (let ((nf-binds (node-func-binds)))
    (loop for node in path
	  do (funcall (gethash node nf-binds)))))

(defun jobject-leaves (job)
  (let ((acc ()))
    (labels ((walk (node)
               (cond
                 ((hash-table-p node)
                  (maphash (lambda (key value)
                             (declare (ignore key))
                             (walk value))
                           node))
		 ((stringp node) (push node acc))
                 ((vectorp node)
                  (loop for el across node
                        do (walk el)))
                 (t (push node acc)))))
      (walk job))
    acc))

(defun fetch-from-jobject (category job)
  (block desired-val
    (labels ((walk (target node)
               (cond
                 ((hash-table-p node)
                  (maphash (lambda (key value)
                             (if (equal target key)
                                 (return-from desired-val value)
                                 (walk target value)))
                           node))
		 ((stringp node) nil)
                 ((vectorp node)
                  (loop for el across node
                        do (walk target el))))))
      (walk category job))))

(defun jobject-categories (job)
  (let ((acc ()))
    (labels ((walk (node)
               (cond
                 ((hash-table-p node)
                  (maphash (lambda (key value)
                             (push key acc)
                             (walk value))
                           node))
		 ((stringp node) nil)
                 ((vectorp node)
                  (loop for el across node
                        do (walk el))))))
      (walk job))
    acc))

(defun sanitize-jobject-fetch (fetched-val)
  (if (hash-table-p fetched-val)
      (jzon:stringify fetched-val)
      fetched-val))

(defparameter *test-graph-1* '((A (B C D E*)) (B (F)) (C (G)) (D ()) (E* ()) (F (I)) (G (H)) (H (I)) (I (L)) (L ())))

(defparameter *test-graph-2* '((E (J K*)) (J ()) (K* ())))

(defparameter *graph-hash* (make-hash-table))
(setf (gethash 'E* *graph-hash*) '(E *test-graph-2*))

(defun graph-merge (f-graph)
  (let ((key-out *graph-hash*))
    (loop for ne-pair in f-graph
	  for node = (first ne-pair)
	  for key-out-val = (gethash node key-out)
	  when (not (null key-out-val))
	    do (setf (cadr ne-pair) (adj-of (symbol-value (cadr key-out-val)) (car key-out-val)))
	    and do (nconc f-graph (rest (symbol-value (cadr key-out-val))))))
  f-graph)

(defun add-node (node adj-list)
  (if (assoc node adj-list :test #'equal)
      adj-list
      (cons (list node ()) adj-list)))

(defun add-edge (from to adj-list)
  (loop for ne-pair in adj-list
	for node = (first ne-pair)
	when (equal node from)
	  do (push to (second ne-pair)))
  adj-list)

(defun json-to-adj-list (job)
  (let ((acc ()))
    (labels ((walk (parent jo)
	       (cond
		 ((hash-table-p jo)
		  (maphash (lambda (key value)
			     (setf acc (add-node key acc))
			     (when parent
			       (add-edge parent key acc))
			     (walk key value))
			   jo))
		 ((stringp jo) nil)
		 ((vectorp jo)
		  (loop for el across jo
			do (walk parent el))))))
      (walk nil job))
    acc))

(defun uuid-p (str)
  (and (stringp str)
       (= (length str) 36)
       (char= (char str 8) #\-)
       (char= (char str 13) #\-)
       (char= (char str 18) #\-)
       (char= (char str 23) #\-)
       (every (lambda (letter)
                (or (digit-char-p letter 16)
                    (char= letter #\-)))
              str)))

(defstruct (stack (:constructor make-stack ()))
  (top nil :type list))

(defun push-stack (item stack)
  (push item (stack-top stack)))

(defun pop-stack (stack)
  (pop (stack-top stack)))

(defun peek-stack (stack)
  (car (stack-top stack)))

(defun stack-path (stack)
  (format nil "~{~A~^.~}" (reverse (stack-top stack))))

(defun stack-path-tag (stack)
  (let ((top (stack-top stack)))
    (format nil "~A.~A" (cadr top) (car top))))

;; wraps in a root-node that represents api endpoint
;; need to mark in a way that's recoverable when giving path
(defun json-to-adj-list-2 (job root-name)
  (let ((acc (list (list root-name ())))
        (stack (make-stack)))
    (push-stack root-name stack)
    (labels ((walk (parent jo)
               (cond
                 ((hash-table-p jo)
                  (maphash (lambda (key value)
                             (push-stack key stack)
                             (let ((node-name (if (assoc key acc :test #'equal)
                                                  (stack-path-tag stack)
                                                  key)))
                               (setf acc (add-node node-name acc))
                               (when parent
                                 (add-edge parent node-name acc))
                               (walk node-name value))
                             (pop-stack stack))
                           jo))
                 ((stringp jo) nil)
                 ((vectorp jo)
                  (loop for el across jo
                        do (walk parent el))))))
      (walk root-name job))
    acc))

(defun ends-with (suffix str)
  (and (>= (length str) (length suffix))
       (string= str suffix :start1 (- (length str) (length suffix)))))

(defun after-char (char str)
  (let ((pos (position char str :from-end t)))
    (if pos
	(subseq str (1+ pos))
	str)))

;; hacky workaround just used for testing
(defun prune-tag (adj-list)
  (loop for ne-pair in adj-list
	when (ends-with "Id" (first ne-pair))
	  do (setf (first ne-pair) (concatenate 'string (after-char #\. (first ne-pair)) (string #\*)))
	do (loop for item on (second ne-pair)
		 when (ends-with "Id" (car item))
		   do (setf (car item) (concatenate 'string (after-char #\. (car item)) (string #\*)))))
  adj-list)

(defparameter *json-test-hash* (make-hash-table :test #'equal))
(setf (gethash "holdingsRecordId*" *json-test-hash*) '("holdings-endpoint" *test-json-3*))
(setf (gethash "instanceId*" *json-test-hash*) '("instances-endpoint" *test-json-1*))

(defun json-graph-merge (f-graph)
  (let ((key-out *json-test-hash*))
    (loop for ne-pair in f-graph
	  for node = (first ne-pair)
	  for key-out-val = (gethash node key-out)
	  when (not (null key-out-val))
	    do (setf (cadr ne-pair) (list (car key-out-val)))
	    and do (nconc f-graph (rest (symbol-value (cadr key-out-val))))))
  f-graph)

(defun prepend-endpoint (base-name root-name)
  (format nil "~A.~A" root-name base-name))

;; prepends endpoint as well
(defun json-to-adj-list-3 (job root-name)
  (let ((acc (list (list root-name ())))
        (stack (make-stack)))
    (push-stack root-name stack)
    (labels ((walk (parent jo)
               (cond
                 ((hash-table-p jo)
                  (maphash (lambda (key value)
                             (push-stack key stack)
                             (let ((node-name (if (assoc (prepend-endpoint key root-name) acc :test #'equal)
                                                  (prepend-endpoint (stack-path-tag stack) root-name)
                                                  (prepend-endpoint key root-name))))
                               (setf acc (add-node node-name acc))
                               (when parent
                                 (add-edge parent node-name acc))
                               (walk node-name value))
                             (pop-stack stack))
                           jo))
                 ((stringp jo) nil)
                 ((vectorp jo)
                  (loop for el across jo
                        do (walk parent el))))))
      (walk root-name job))
    acc))

(defun prune-all-tags (adj-list)
  (loop for ne-pair in adj-list
	when (find #\. (first ne-pair))
	  do (setf (first ne-pair) (after-char #\. (first ne-pair)))
	do (loop for item on (second ne-pair)
		 when (find #\. (car item))
		   do (setf (car item) (after-char #\. (car item)))))
  adj-list)

(defun make-hash-many (&rest pairs)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (key val) on pairs by #'cddr
	  do (setf (gethash key ht) val))
    ht))

(defun make-set-many (&rest vals)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for val in vals
	  do (setf (gethash val ht) t))
    ht))

;; Should end up being the real function for schema parsing
(defun json-schema-to-adj-list (job root-name)
  (let ((acc (list (list root-name ())))
        (stack (make-stack)))
    (push-stack root-name stack)
    (labels ((walk (parent jo in-properties)
               (when (hash-table-p jo)
                 (if in-properties
                     (maphash (lambda (key value)
                                (push-stack key stack)
                                (let ((node-name (if (assoc (prepend-endpoint key root-name) acc :test #'equal)
                                                     (prepend-endpoint (stack-path-tag stack) root-name)
                                                     (prepend-endpoint key root-name))))
                                  (setf acc (add-node node-name acc))
                                  (when parent
                                    (add-edge parent node-name acc))
                                  (walk node-name value nil))
                                (pop-stack stack))
                              jo)
                     (maphash (lambda (key value)
                                (cond
                                  ((equal key "properties")
                                   (walk parent value t))
                                  ((and (equal key "items") (hash-table-p value))
                                   (walk parent value nil))
                                  (t nil)))
                              jo)))))
      (walk root-name job nil))
    acc))
