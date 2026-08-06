(eval-when (:compile-toplevel :load-toplevel :execute)
  (load #P"~/quicklisp/setup.lisp")
  (ql:quickload '#:com.inuoe.jzon)
  (sb-ext:add-package-local-nickname '#:jzon '#:com.inuoe.jzon)
  (load #P"outedges.lisp"))

;; bind read-eval to nil with a let around reads

;; Simple test function for I/O. Liable to be deleted
(defun my-echo ()
(loop
  for line = (read-line nil nil)
  when (null line)
    do (return)
  when (string= line "end")
    do (return)
  do (write-line "preface")
  do (write-line line)))

;; Used for REPL json input. No longer used, now that graphs are build automatically
(defun parse-cli-json ()
  (let ((input (with-output-to-string (s)
                 (loop for line = (read-line nil nil)
                       while (and line (plusp (length line)))
                       do (write-string line s)))))
    (jzon:parse input)))

;; Prints all nodes in an adj-list to stdout in a "NodeA" format
(defun list-nodes (adj-list)
  (loop for node in adj-list
	do (write-line (format nil "~A" (first node)))))

;; Prints all edges in an adj-list to stdout in "NodeA to NodeB" format
(defun list-edges (adj-list)
  (loop for node in adj-list
	for cur = (first node)
	do (loop for next in (cadr node)
		 do (write-line (format nil "~A to ~A" cur next)))))

;; Basic queue struct
(defstruct (queue (:constructor make-queue ()))
  (head nil :type list)
  (tail nil :type list))

;; Enqueues an item into the queue. (enqueue item queue)
(defun enqueue (item queue)
  (let ((cell (list item)))
    (if (queue-tail queue)
        (setf (cdr (queue-tail queue)) cell)
        (setf (queue-head queue) cell))
    (setf (queue-tail queue) cell)))

;; Dequeues an item from the queue and returns it
(defun dequeue (queue)
  (let ((cell (queue-head queue)))
    (when cell
      (setf (queue-head queue) (cdr cell))
      (unless (queue-head queue)
        (setf (queue-tail queue) nil))
      (car cell))))

;; Returns the nodes adjacent to val. (adj-of adj-list val). Maybe swap param order?
(defun adj-of (adj-list val)
  (loop for node in adj-list
	when (equalp (first node) val)
	  return (cadr node)))

;; Backtraces the path from start to dest, or returns nil if none. (backtrace-path parent-list start dest)
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
	
;; BFS shortest path for an adj-list. 
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

;; Returns a list of reachable nodes from start in the given adj-list (reachable adj-list start)
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

;; Test function for binding anonymous functions to tree traversal. Idea was excessive for final version. Defunct.
(defun node-func-binds ()
  (let ((binds (make-hash-table)))
    (setf (gethash 'A binds) (lambda () (format t "Node A~%")))
    (setf (gethash 'B binds) (lambda () (format t "Node B~%")))
    (setf (gethash 'C binds) (lambda () (format t "Node C~%")))
    (setf (gethash 'D binds) (lambda () (format t "Node D~%")))
    (setf (gethash 'E binds) (lambda () (format t "Node E~%")))
    (setf (gethash 'F binds) (lambda () (format t "Node F~%")))
    binds))

;; Prints the provided path. Used for adj-list. Defunct.
(defun print-path (path)
  (let ((nf-binds (node-func-binds)))
    (loop for node in path
	  do (funcall (gethash node nf-binds)))))

;; Returns the terminal values in a jobject (parsed by jzon). Largely defunct
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

;; Fetches the value from a category in a jobject. Potentially useful if program does curls for user
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

;; Returns all categories of the provided jobject
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

;; I don't really remember what this does. Remind me to test it later
(defun sanitize-jobject-fetch (fetched-val)
  (if (hash-table-p fetched-val)
      (jzon:stringify fetched-val)
      fetched-val))

;; A test graph used for proof of concept, and merge checks
(defparameter *test-graph-1* '((A (B C D E*)) (B (F)) (C (G)) (D ()) (E* ()) (F (I)) (G (H)) (H (I)) (I (L)) (L ())))

;; A second test graph meant to be merged into 1
(defparameter *test-graph-2* '((E (J K*)) (J ()) (K* ())))

;; A hash that defines test foreign-keys and graphs to merge
(defparameter *graph-hash* (make-hash-table))
(setf (gethash 'E* *graph-hash*) '(E *test-graph-2*))

;; A proof-of-concept graph merge algorithm. Defunct.
(defun graph-merge (f-graph)
  (let ((key-out *graph-hash*))
    (loop for ne-pair in f-graph
	  for node = (first ne-pair)
	  for key-out-val = (gethash node key-out)
	  when (not (null key-out-val))
	    do (setf (cadr ne-pair) (adj-of (symbol-value (cadr key-out-val)) (car key-out-val)))
	    and do (nconc f-graph (rest (symbol-value (cadr key-out-val))))))
  f-graph)

;; Adds a node with no edges to the adj-list
(defun add-node (node adj-list)
  (if (assoc node adj-list :test #'equal)
      adj-list
      (cons (list node ()) adj-list)))

;; Add an edge from the first input to the second input in the provided adj-list
(defun add-edge (from to adj-list)
  (loop for ne-pair in adj-list
	for node = (first ne-pair)
	when (equal node from)
	  do (push to (second ne-pair)))
  adj-list)

;; Takes a parsed jobject and returns an adj-list
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

;; Returns true if the provided string is a uuid, and false if not. Defunct.
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

;; Basic stack struct
(defstruct (stack (:constructor make-stack ()))
  (top nil :type list))

;; Pushes the given item to the stack
(defun push-stack (item stack)
  (push item (stack-top stack)))

;; Pops the top item from the stack and returns it
(defun pop-stack (stack)
  (pop (stack-top stack)))

;; Returns the top item on the stack without popping it
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

;; Returns t if the str ends with the provided suffix and nil otherwise. (ends-with suffix str)
(defun ends-with (suffix str)
  (and (>= (length str) (length suffix))
       (string= str suffix :start1 (- (length str) (length suffix)))))

;; Returns str.split(char)[-1].
(defun after-char (char str)
  (let ((pos (position char str :from-end t)))
    (if pos
	(subseq str (1+ pos))
	str)))

;; hacky workaround just used for testing (apparently a real part of the program now. Remind me to just build this into my functions so I don't have to do two passes)
(defun prune-tag (adj-list)
  (loop for ne-pair in adj-list
	when (ends-with "Id" (first ne-pair))
	  do (setf (first ne-pair) (concatenate 'string (after-char #\. (first ne-pair)) (string #\*)))
	do (loop for item on (second ne-pair)
		 when (ends-with "Id" (car item))
		   do (setf (car item) (concatenate 'string (after-char #\. (car item)) (string #\*)))))
  adj-list)

;; Defunct hash used for testing
(defparameter *json-test-hash* (make-hash-table :test #'equal))
(setf (gethash "holdingsRecordId*" *json-test-hash*) '("holdings-endpoint" *test-json-3*))
(setf (gethash "instanceId*" *json-test-hash*) '("instances-endpoint" *test-json-1*))

;; Test function used for merging json graphs. Defunct
(defun json-graph-merge (f-graph)
  (let ((key-out *json-test-hash*))
    (loop for ne-pair in f-graph
	  for node = (first ne-pair)
	  for key-out-val = (gethash node key-out)
	  when (not (null key-out-val))
	    do (setf (cadr ne-pair) (list (car key-out-val)))
	    and do (nconc f-graph (rest (symbol-value (cadr key-out-val))))))
  f-graph)

;; Prepends the root-name onto the base-name
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

;; Builds a single adj-list for the given endpoint based on the schema-path
(defun build-endpoint-graph (schema-path base-dir schema-to-endpoint-hash)
  (let ((endpoint (gethash schema-path schema-to-endpoint-hash))
        (schema (jzon:parse (pathname (merge-pathnames schema-path base-dir)))))
    (when endpoint
      (json-schema-to-adj-list schema endpoint))))

;; Builds all graphs and returns a list of adj-lists
(defun build-all-graphs (base-dir)
  (let* ((raml-pairs (parse-all-raml base-dir))
         (rev (reverse-mapping-hash raml-pairs)))
    (loop for (endpoint schema) in raml-pairs
          for graph = (build-endpoint-graph schema base-dir rev)
          when graph
            collect graph)))

;; The default path for schema and ramls
(defparameter *schema-and-raml-path* "mod-inventory-storage/ramls/")

;; A global variable to hold all graphs in a hash table
(defparameter *all-graphs* (make-hash-table :test #'equal))

(defparameter *field-disambiguation* (make-hash-table :test #'equal))

;; Build all graphs at runtime and hash them into *all-graphs*
(loop for graph in (build-all-graphs *schema-and-raml-path*)
      for root = (caar (last graph))
      do (setf (gethash root *all-graphs*) graph)
      do (loop for (tagged-field) in graph
	       for field = (when (find #\. tagged-field)
			     (after-char #\. tagged-field))
	       when field
		 do (push tagged-field (gethash field *field-disambiguation*))))

;; override: holdingsRecordView.json 
(setf (gethash "/holdings-storage/holdings" *all-graphs*)
      (json-schema-to-adj-list
       (jzon:parse (merge-pathnames "schemas/holdings-storage/holdingsRecord.json"
                                    *schema-and-raml-path*))
       "/holdings-storage/holdings"))

;; currently broken. Repeated id names overwrite
(defparameter *id-to-endpoint-map* (make-hash-table :test #'equal))
(loop for (id endpoint) in (id-to-endpoint-db *schema-and-raml-path* "schema.json" "table-to-schema-map.txt")
      do (setf (gethash id *id-to-endpoint-map*) endpoint))

;; Populates *id-to-endpoint-map* at runtime
(loop for pair in (parse-all-endpoints *schema-and-raml-path*)
      for id = (first pair)
      for endpoint = (second pair)
      unless (gethash id *id-to-endpoint-map*)
      do (setf (gethash id *id-to-endpoint-map*) endpoint))

;; needed until I can disambiguate each schema's local "id" meaning
(remhash "id" *id-to-endpoint-map*)

;; Simple test-function that prints a hashtable to stdout
(defun print-hash (ht)
  (maphash (lambda (key value)
	     (format t "Key: ~A, Val: ~A~%" key value))
	   ht))

;; Merges the graph at id to another graph
(defun selective-merge (id graph)
  (let* ((node (assoc id graph :test #'equal))
         (target-graph (prune-tag (copy-tree (gethash (gethash (string-right-trim "*" id) *id-to-endpoint-map*) *all-graphs*))))
         (target-root (caar (last target-graph))))
    (when (and node target-graph)
      (setf (cadr node) (list target-root))
      (nconc graph target-graph))
    graph))

;; Returns a path from start-endpoint to the target value, or the complete merged graph if no path.
(defun merge-from-start-endpoint (start-endpoint target)
  (let ((visited ())
	(queue (make-queue))
	(start-graph (prune-tag (copy-tree (gethash start-endpoint *all-graphs*))))
	(parent (make-hash-table :test #'equal)))
    (enqueue start-endpoint queue)
    (push start-endpoint visited)
    (loop while (queue-head queue)
	  for curr = (dequeue queue)
	  for edges = (adj-of start-graph curr)
	  do (cond ((string-equal curr target)
		    (return-from merge-from-start-endpoint (backtrace-path parent start-endpoint target)))
		   ((find #\* curr)
		    ;; might be tossing away an intermediate representation I want
		    (setf start-graph (selective-merge curr start-graph))
		    (setf edges (adj-of start-graph curr)))
		   (t nil))
	  do (loop for next in edges
		     unless (member next visited :test #'equal)
		       do (push next visited)
		       and do (enqueue next queue)
		       and do (setf (gethash next parent) curr)))
    nil))

;; Simple function that checks if the provided str is an id. Separated so that it can be easily modified if I change how ids are labeled.
(defun is-id (str)
  (ends-with "*" str))

;; Global var that tracks the base path for curls, before specific endpoints
(defparameter *base-folio-path* "https://uchicago-test-okapi.folio.indexdata.com")

(defparameter *base-curl* "curl -w '\n' -H \"Accept: application/json\" -H \"Content-Type: application/json\" -H \"X-Okapi-Tenant: uchicago\" -H \"X-Okapi-Token: ~A\" \"~A\"")

;; Takes a path of form (id, endpoint, id, endpoint...) and prints it to stdout in curlable format
(defun print-curls (path)
  (loop for curr in path
	and prev = nil then curr
	when (and prev (is-id prev))
	  do (format t "~A~A/{~A}~%" *base-folio-path* curr (string-right-trim "*" prev))))

;; Takes a path of form (id, endpoint, id, endpoint...) and prints it to stdout in curlable format
(defun format-endpoint-curls (path)
  (loop for curr in path
	and prev = nil then curr
	when (and prev (is-id prev))
	  collect (format nil "~A~A/{~A}" *base-folio-path* curr (string-right-trim "*" prev))))

(defun format-full-curls (path)
  (let ((all-curls (format-endpoint-curls path)))
    (loop for dest in all-curls
	  do (format t *base-curl* "" dest)
	  do (format t "~%~%"))))

;; Pathfinds from an id to the target and prints the curls to stdout
(defun pathfind-id (start-id target &key full)
    (let ((path (cons (format nil "~A*" start-id)
                    (merge-from-start-endpoint (gethash start-id *id-to-endpoint-map*) target))))
    (if full
        (format-full-curls path)
        (print-curls path))))

(defun disambiguate-target (field candidates)
  (let ((chosen nil))
    (block pick
      (labels ((bind-restarts (remaining)
                 (if (null remaining)
                     (error "Ambiguous target field \"~A\":~{~%  ~A~}" field candidates)
                     (let ((candidate (car remaining)))
                       (restart-bind
                           ((use-target
                             (lambda ()
                               (setf chosen candidate)
                               (return-from pick))
                             :report-function (lambda (s) (format s "Use ~A" candidate))))
                         (bind-restarts (cdr remaining)))))))
        (bind-restarts candidates)))
    chosen))

(defun pathfind-field (start-id target-field &key full)
  (let ((hom-fields (gethash target-field *field-disambiguation*)))
    (cond
      ((null hom-fields)
       (format t "No field \"~A\" found in any endpoint~%" target-field))
      ((= (length hom-fields) 1)
       (pathfind-id start-id (first hom-fields) :full full))
      (t
       (pathfind-id start-id (disambiguate-target target-field hom-fields) :full full)))))

(defparameter *x-okapi-token* nil)
(defparameter *config-file* "config.txt")
(defparameter *okapi-username* nil)
(defparameter *okapi-password* nil)

(defun load-login-info ()
    (with-open-file (stream *config-file*)
      (loop for line = (read-line stream nil)
	    while line
	    when (search "username" line)
	    collect (string-trim " " (after-char #\: line))
	    when (search "password" line)
	      collect (string-trim " " (after-char #\: line)))))

(defun store-login-info (login-list)
  (cond ((/= (length login-list) 2) (format t "Length ~A does not match (username password) structure required~%" (length login-list)))
	(t
	 (setf *okapi-username* (first login-list)
	       *okapi-password* (second login-list))
	 (values))))

(defparameter *login-curl-base* nil)

(defun load-login-curl ()
  (setf *login-curl-base* "curl -w '\\n' -X POST -H \"Accept: application/json\" -H \"Content-Type: application/json\" -H \"X-Okapi-Tenant: uchicago\" https://uchicago-test-okapi.folio.indexdata.com/authn/login -d '{\"username\": \"~A\", \"password\": \"~A\"}' --include"))

(load-login-curl)

(defun print-login-curl ()
  (when (or (null *okapi-username*) (null *okapi-password*))
    (format t "Username or password is invalid~%")
    (return-from print-login-curl nil))
  (format t *login-curl-base* *okapi-username* *okapi-password*))

;; do curl
;; print shell command to do curls
;; print path
;; cache token
;; debugging
;; statusId
;; make sure you don't intentionally put in security vulnerabilities
;; func param that prints that sequence (takes dexador as opt parameter)
;; Sting literals as first thing in body (whoops)
;; documentation generators
;; staple generator (generates documentation automatically)
;; emacs macro to convert comments to docstrings
;; use restarts for disambiguation of target
;; let-over-lambda safety
;; nuke provided info if it has too many repeated characters
;; Start building CLI (tentative)
;; Need flags and args
;; Crunches (look for config if it doesn't find the crunch, error if no config)
;; Makefile
;; Makerules (automate repo pulling) 

;; add func that trims {} and replaces it with ~A
;; dexador rather than uiop
;; user-friendly graphviz dump

