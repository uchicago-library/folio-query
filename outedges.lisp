(eval-when (:compile-toplevel :load-toplevel :execute)
  (load #P"~/quicklisp/setup.lisp")
  (ql:quickload '#:com.inuoe.jzon)
  (sb-ext:add-package-local-nickname '#:jzon '#:com.inuoe.jzon)
  (ql:quickload "cl-yaml"))

;; Parses all folio:linkBase and folio:linkFromField edges and returns them in ((out to) (out to)...) format. Since folio: internal graph edges are limited, this function alone cannot sufficiently populate the edge map
(defun parse-edges (file-name)
  (let ((job (jzon:parse (merge-pathnames file-name *default-pathname-defaults*))) (acc '()))
    (labels ((walk (node)
                   (cond
                    ((hash-table-p node)
                      (maphash (lambda (key value)
                                 (cond
                                  ((string= "folio:linkBase" key) (push (list (concatenate 'string "/" value)) acc))
                                  ((string= "folio:linkFromField" key) (push value (first acc)))
                                  (t (walk value))))
                               node))
                    ((stringp node) nil)
                    ((vectorp node)
                      (loop for el across node
                            do (walk el))))))
      (walk job))
    acc))

;; Hashes a list of ((out to)...) into an edge hashmap 
(defun make-hash-edges (pairs)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for pair in pairs
          for source = (first pair)
          for dest = (second pair)
          do (setf (gethash source ht) dest))
    ht))

;; Parses all of the folio: internal graph edges across all jsons
(defun parse-all-endpoints (dir)
  (loop for path in (directory (merge-pathnames "**/*.json" dir))
          append (parse-edges path)))

;; Extracts all of the types from a raml hash
(defun extract-types (raml-hash)
  (let ((types (gethash "types" raml-hash))
        (ht (make-hash-table :test #'equal)))
    (when (hash-table-p types)
          (maphash (lambda (key value)
                     (when (stringp value)
                           (setf (gethash key ht) value)))
                   types))
    ht))

;; Returns t if str starts with prefix and nil o/w. (starts-with prefix str)
(defun starts-with (prefix str)
  (and (>= (length str) (length prefix))
       (string= str prefix :end1 (length prefix))))

;; Extracts all endpoints from the provided raml given the types-map
(defun extract-endpoints (raml-hash types-map)
  (let ((acc ()))
    (labels ((walk (node current-path)
                   (when (hash-table-p node)
                         (maphash (lambda (key value)
                                    (when (stringp key)
                                          (cond
                                           ((starts-with "/" key)
                                             (if (find #\{ key)
                                                 nil
                                                 (walk value (concatenate 'string current-path key))))
                                           ((equal key "type")
                                             (when (hash-table-p value)
                                                   (maphash (lambda (rt-key rt-value)
                                                              (declare (ignore rt-key))
                                                              (when (hash-table-p rt-value)
                                                                    (let ((schema-name (or (gethash "schemaItem" rt-value)
                                                                                           (gethash "schema" rt-value))))
                                                                      (when schema-name
                                                                            (let ((schema-file (gethash schema-name types-map)))
                                                                              (when schema-file
                                                                                    (push (list current-path schema-file) acc)))))))
                                                            value)))
                                           ((hash-table-p value)
                                             (walk value current-path)))))
                                  node))))
      (walk raml-hash ""))
    acc))

;; Parses the provided raml's endpoints
(defun parse-raml-endpoints (file-path)
  (let* ((raml (yaml:parse file-path))
         (types-map (extract-types raml)))
    (extract-endpoints raml types-map)))

;; Parses all of the raml in the directory and stores their endpoints (in the wrong order) in a list of pairs
(defun parse-all-raml (dir)
  (loop for path in (directory (merge-pathnames "*.raml" dir))
          append (parse-raml-endpoints path)))

;; Takes a list of pairs and hashes it in reverse order such that (val key) becomes key: val in the hash
(defun reverse-mapping-hash (pairs)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (endpoint schema) in pairs
          do (setf (gethash schema ht) endpoint))
    ht))

;; Simple parsing function for how I stored my internal database name -> schema name list
(defun parse-arrow-line (line)
  (let ((pos (search " -> " line)))
    (when pos
          (values (subseq line 0 pos)
            (subseq line (+ pos 4))))))

;; Returns a hash of database nicknames to real endpoints used when parsing the schema.json key to endpoint map
(defun database-endpoint-nickname (file-path)
  (let ((ht (make-hash-table :test #'equal)))
    (with-open-file (stream file-path)
      (loop for line = (read-line stream nil)
            while line
            do (multiple-value-bind (dbname endpoint) (parse-arrow-line line)
                 (when dbname
                       (setf (gethash dbname ht) endpoint)))))
    ht))

;; Parses all of the foreign-key -> endpoint mappings in schema.json, using the nickname file to map sql endpoint names to real endpoint names.
(defun parse-db-foreign-keys (schema-file nickname-file)
  (let* ((schema (jzon:parse (merge-pathnames schema-file *default-pathname-defaults*)))
         (nicknames (database-endpoint-nickname nickname-file))
         (tables (gethash "tables" schema))
         (acc ()))
    (loop for table across tables
          when (hash-table-p table)
          do (let ((foreign-keys (gethash "foreignKeys" table)))
               (when (vectorp foreign-keys)
                     (loop for fk across foreign-keys
                           when (hash-table-p fk)
                           do (let ((field-name (gethash "fieldName" fk))
                                    (target-table (gethash "targetTable" fk)))
                                (when (and field-name target-table)
                                      (let ((endpoint (gethash target-table nicknames)))
                                        (when endpoint
                                              (pushnew (list field-name endpoint)
                                                       acc
                                                       :test #'equal)))))))))
    acc))

;; A function to be run once that rewrote my sql internal name to endpoint map file to one that uses the full endpoint name rather than the partial one that I wrote.
(defun write-full-path-nicknames (nickname-file raml-dir output-file)
  (let ((nicknames (database-endpoint-nickname (merge-pathnames nickname-file *default-pathname-defaults*)))
         (raml-pairs (parse-all-raml raml-dir))
         (filename-to-full-path (make-hash-table :test #'equal)))
    (loop for (endpoint schema-path) in raml-pairs
          for bare = (file-namestring (pathname schema-path))
          do (setf (gethash bare filename-to-full-path) schema-path))
    (with-open-file (out output-file :direction :output :if-exists :supersede)
      (maphash (lambda (table-name bare-json)
                 (let ((full-path (gethash bare-json filename-to-full-path)))
                   (when full-path
                         (format out "~A -> ~A~%" table-name full-path))))
               nicknames))))

;; Maps ids to the target endpoint 
(defun id-to-endpoint-db (raml-dir schema-file nickname-file)
  (let ((schema-to-endpoint (reverse-mapping-hash (parse-all-raml raml-dir)))
	(fk-pairs (parse-db-foreign-keys schema-file nickname-file)))
    (loop for (field-name json-file) in fk-pairs
	  for endpoint = (gethash json-file schema-to-endpoint)
          when endpoint
            collect (list field-name endpoint))))
