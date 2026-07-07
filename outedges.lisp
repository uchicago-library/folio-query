(load #P"~/quicklisp/setup.lisp")
(ql:quickload '#:com.inuoe.jzon)
(sb-ext:add-package-local-nickname '#:jzon '#:com.inuoe.jzon)

(defun parse-edges (file-name)
  (let ((job (jzon:parse (merge-pathnames file-name *default-pathname-defaults*))) (acc '()))
    (labels ((walk (node)
	       (cond
		 ((hash-table-p node)
		  (maphash (lambda (key value)
			     (cond
			       ((string= "folio:linkBase" key) (push (list value) acc))
			       ((string= "folio:linkFromField" key) (push value (first acc)))
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

