(eval-when (:compile-toplevel :load-toplevel :execute)
  (load #P"~/quicklisp/setup.lisp")
  (ql:quickload '#:com.inuoe.jzon)
  (sb-ext:add-package-local-nickname '#:jzon '#:com.inuoe.jzon)
  (ql:quickload "cl-yaml"))

(defun parse-edges (file-name)
  (let ((job (jzon:parse (merge-pathnames file-name *default-pathname-defaults*))) (acc '()))
    (labels ((walk (node)
	       (cond
		 ((hash-table-p node)
		  (maphash (lambda (key value)
			     (cond
			       ((string= "folio:linkBase" key) (push (list (concatenate 'string "/" value)) acc))
			       ((string= "folio:linkFromField" key) (push  value (first acc)))
				 (t (walk value))))
			   node))
		 ((stringp node) nil)
		 ((vectorp node)
		  (loop for el across node
			do (walk el))))))
      (walk job))
    acc))

(defun make-hash-edges (pairs)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for pair in pairs
	  for source = (first pair)
	  for dest = (second pair)
	  do (setf (gethash source ht) dest))
    ht))

(defun parse-all-endpoints (dir)
  (loop for path in (directory (merge-pathnames "**/*.json" dir))
	append (parse-edges  path)))

(defun extract-types (raml-hash)
  (let ((types (gethash "types" raml-hash))
        (ht (make-hash-table :test #'equal)))
    (when (hash-table-p types)
      (maphash (lambda (key value)
                 (when (stringp value)
                   (setf (gethash key ht) value)))
               types))
    ht))

(defun starts-with (prefix str)
  (and (>= (length str) (length prefix))
       (string= str prefix :end1 (length prefix))))

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

(defun parse-raml-endpoints (file-path)
  (let* ((raml (yaml:parse file-path))
         (types-map (extract-types raml)))
    (extract-endpoints raml types-map)))

(defun parse-all-raml (dir)
  (loop for path in (directory (merge-pathnames "*.raml" dir))
	append (parse-raml-endpoints  path)))

(defun reverse-mapping-hash (pairs)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (endpoint schema) in pairs
          do (setf (gethash schema ht) endpoint))
    ht))
