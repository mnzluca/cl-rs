(load "rs.lisp")

(defun perform-tests (size density prefix type out)
  (with-open-file (f out :direction :output :if-exists :append :if-does-not-exist :create)
    (loop for test from 0 to 29 do
	 (let ((filename (concatenate 'string
				      prefix
				      "/" (write-to-string size)
				      "/" (write-to-string density)
				      "/test_"
				      (write-to-string test)
				      "_orsa_" (write-to-string size)
				      "x" (write-to-string size)
				      "_bc" (write-to-string density)
				       ".orsa"))
	       (start-time 0)
	       (end-time 0))
	   (cond ((eq type :set-theory)
		  (progn
		    (setf start-time (get-internal-real-time))
		    (simulate-rs filename #'simulator-set-theory "/dev/null")
		    (setf end-time (get-internal-real-time))
		    (format f "set-theory, ~a, ~a, ~a, ~a~%" test size density (- end-time start-time))))
		 ((eq type :dependency-graph)
		  (progn
		    (setf start-time (get-internal-real-time))
		    (simulate-rs filename #'simulator-dependency-graph "/dev/null")
		    (setf end-time (get-internal-real-time))
		    (format f "dependency-graph, ~a, ~a, ~a, ~a~%" test size density (- end-time start-time))))
		 (t (error "unrecognized simulator")))
	   (gc :full t)))))

;; example of how to perform all the tests
;; (loop for n in '(10 100 1000) do
;;      (loop for d in '(0.01 0.05 0.1) do
;; 	  (perform-tests n d "./test-models/" :set-theory "results.txt")
;; 	  (perform-tests n d "./test-models/" :dependency-graph "results.txt")))
