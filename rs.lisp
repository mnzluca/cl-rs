(ql:quickload :cl-ppcre)
(use-package :cl-ppcre)

;; (require :sb-sprof)

;; (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
(declaim (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0) (space 3)))
;; (declaim (optimize (speed 0) (debug 3) (safety 3)))

(defparameter *entity-table* nil)
(defparameter *free-entity-id* 0)
(defparameter *id-table* nil)

(defstruct reaction
  (reactants (make-array 0 :element-type 'fixnum) :type (array fixnum (*)))
  (inhibitors (make-array 0 :element-type 'fixnum) :type (array fixnum (*)))
  (products (make-array 0 :element-type 'fixnum) :type (array fixnum (*))))

(defun tokenize (code)
  (declare (type string code)
	   (type fixnum *free-entity-id*))
  (cond ((null code) nil)
	((scan "^,$" code) :set-separator)
	((scan "^---+$" code) :context-begin)
	((scan "^[a-zA-Z0-9_:]+$" code)
	 (progn
	   (unless (gethash code *entity-table*)
	     (setf (gethash code *entity-table*) *free-entity-id*)
	     (setf (gethash *free-entity-id* *id-table*) code)
	     (incf *free-entity-id*))
	   (cons :entity code)))
	((scan "^.$" code) :empty-set)
	((scan "^$" code) :empty-set)
	(t (error code))))

(defun tokenize-rs-file (filename)
  (declare (type string filename))
    (with-open-file (source filename :direction :input)
      (loop for line = (read-line source nil nil)
	 while line
	 appending
	   (let* ((norm-line (regex-replace-all "," line " ,"))
		  (c-line (regex-replace "#.*$" norm-line "")))
	     (declare (type string c-line))
	     (unless (string= c-line "")
	       (append (mapcar #'tokenize (split "\\s+" c-line)) (list :eol)))))))

(defun consume (lst elem)
  (if (and (not (null lst)) (eq (car lst) elem))
      (cdr lst)
      (error lst)))

(defun trim (lst elem)
  (loop for x in lst
     while (eq x elem)
     do (setf lst (cdr lst)))
  lst)

(defun peek (lst elem)
  (and (not (null lst)) (eq (car lst) elem)))

(defun take-set (lst)
  (if (peek lst :empty-set)
      (values (make-array 0 :element-type 'fixnum)
	      (consume lst :empty-set))
      (flet ((is-entity (elem)
	       (and (listp elem)
		    (eq (car elem) :entity)))
	     (name-to-id (elem)
	       (gethash (cdr elem) *entity-table*)))
	(let ((set (make-array
		    '(1)
		    :element-type 'fixnum
		    :adjustable T
		    :fill-pointer 0)))
	  (loop for x in lst
	     while (is-entity x)
	     do (pop lst)
	       (vector-push-extend (name-to-id x) set))
	  (values set lst)))))

(defun parse-rs-reaction (line)
  (let* ((reactants nil)
	 (inhibitors nil)
	 (products nil)
	 (rest line)
	 (set nil))
    (flet ((next-set ()
	     (multiple-value-bind (x y)
		 (take-set rest)
	       (setf set x)
	       (setf rest y))))
      (next-set)
      (setf reactants set)
      (setf rest (consume rest :set-separator))
      (next-set)
      (setf inhibitors set)
      (setf rest (consume rest :set-separator))
      (next-set)
      (setf products set)
      (setf rest (consume rest :eol))
      (values (make-reaction
	       :reactants reactants
	       :inhibitors inhibitors
	       :products products)
	      rest))))

(defun read-rs-from-file (filename)
  (declare (type string filename))
  (let ((tokens (tokenize-rs-file filename))
	(A
	 (make-array
	  1
	  :element-type 'reaction
	  :adjustable T
	  :fill-pointer 0
	  :initial-element (make-reaction)))
	(context
	 (make-array
	  1
	  :element-type '(or (vector fixnum) nil)
	  :adjustable T
	  :fill-pointer 0
	  :initial-element (make-array '(0) :element-type 'fixnum))))
    (loop  until (or (null tokens) (peek tokens :context-begin))
       do (multiple-value-bind (reaction rest)
	      (parse-rs-reaction tokens)
	    (vector-push-extend reaction A)
	    (setf tokens rest)))
    (setf tokens (trim (consume tokens :context-begin) :eol))
    (loop until (null tokens)
       do (multiple-value-bind (x y)
	      (take-set tokens)
	    (if (null y)
		(setf tokens y)
		(setf tokens (consume y :eol)))
	    (vector-push-extend x context)))
    (values A context)))

(defun print-state (state &optional (stream *standard-output*))
  (declare (type stream stream))
  (loop for i fixnum from 0 to (1- (array-dimension state 0)) do
       (loop for j fixnum from 0 to (1- *free-entity-id*) do
	    (when (= (bit state i j) 1)
	      (format stream "~A " (gethash j *id-table*))))
       (format stream "~%")))

(defun apply-reaction (reaction state i)
  (declare (type fixnum i)
	   (type reaction reaction)
	   (type (array bit) state))
  (when (and (loop for r-id fixnum across (reaction-reactants reaction)
		always (= (bit state i r-id) 1))
	     (loop for i-id fixnum across (reaction-inhibitors reaction)
		never (= (bit state i i-id) 1)))
      (loop for p-id fixnum across (reaction-products reaction) do
	   (setf (bit state (1+ i) p-id) 1))))
 
(defun simulate-rs (filename simulator out)
  (declare (type string filename))
  (let ((*entity-table* (make-hash-table :test 'equal))
	(*free-entity-id* 0)
	(*id-table* (make-hash-table)))
    (multiple-value-bind (A context) (read-rs-from-file filename)
      (declare (type (vector (vector fixnum)) context))
      (let ((state (make-array (list (1+ (length context)) *free-entity-id*)
			       :element-type 'bit
			       :initial-element 0)))
	(funcall simulator A context state)
	(with-open-file (f out :direction :output :if-exists :overwrite)
	  (print-state state f))))))
  
(defun simulator-set-theory (A context state)
  (declare (type (vector reaction) A)
	   (type (vector (vector fixnum)) context)
	   (type (array bit) state))
  (loop for ctx across context
     for i fixnum from 0 do
       (loop for j fixnum across ctx do
	    (setf (bit state i j) 1))
       (loop for r across A do
	    (apply-reaction r state i))))

;; RS WITH DEPENDENCY GRAPH

(defparameter *dependency-reactants* nil)
(defparameter *dependency-inhibitors* nil)
(defparameter *entity-positive* nil)
(defparameter *entity-negative* nil)
(defparameter *enabled-reaction* nil)

(defun make-reactions-graph (A)
  (declare (type (array bit) *dependency-reactants*)
	   (type (array bit) *dependency-inhibitors*)
	   (type (vector (vector fixnum)) *entity-positive*)
  	   (type (vector (vector fixnum)) *entity-negative*)
	   (type fixnum *free-entity-id*))
  (loop for react across A
     for i fixnum from 0 do
       (loop for x fixnum across (reaction-reactants react) do
	    (setf (bit *dependency-reactants* i x) 1))
       (loop for x fixnum across (reaction-inhibitors react) do
	    (setf (bit *dependency-inhibitors* i x) 1)))
  (loop for i fixnum from 0 to (1- (length A)) do
       (loop for j fixnum from 0 to (1- *free-entity-id*) do
	    (when (= (aref *dependency-reactants* i j) 1)
	      (vector-push-extend i (aref *entity-positive* j)))
	    (when (= (aref *dependency-inhibitors* i j) 1)
	      (vector-push-extend i (aref *entity-negative* j))))))

(defun half-apply-reaction (reaction state i)
  (declare (type fixnum i)
	   (type reaction reaction)
	   (type (array bit) state))
  (when (loop for r-id fixnum across (reaction-reactants reaction)
	   always (= (bit state i r-id) 1))
      (loop for p-id fixnum across (reaction-products reaction) do
	   (setf (bit state (1+ i) p-id) 1))))

(defun apply-possibly-enabled-reactions (state i A)
  (declare (type (array bit) state)
	   (type fixnum i)
	   (type (vector bit) *enabled-reaction*)
	   (type (vector reaction) A))
  (loop for r bit across *enabled-reaction*
       for j fixnum from 0 do
       (when (= r 1)
	 (half-apply-reaction (aref A j) state i)
	 (setf (bit *enabled-reaction* j) 0))))

(defun set-possibly-enabled-reactions (state i)
  (declare (type (array bit) state)
	   (type fixnum i)
	   (type fixnum *free-entity-id*)
	   (type (vector (vector fixnum)) *entity-positive*)
	   (type (vector (vector fixnum)) *entity-negative*)
	   (type (vector bit) *enabled-reaction*))
  (loop for j fixnum from 0 to (1- *free-entity-id*) do
       (when (= (aref state i j) 1)
	 (loop for k across (aref *entity-positive* j) do
	      (setf (bit *enabled-reaction* k) 1))))
  (loop for j fixnum from 0 to (1- *free-entity-id*) do
       (when (= (aref state i j) 1)
	 (loop for k across (aref *entity-negative* j) do
	      (setf (bit *enabled-reaction* k) 0)))))

(defun simulator-dependency-graph (A context state)
  (declare (type (vector reaction) A)
	   (type (vector (vector fixnum)) context)
	   (type (array bit) state)
	   (type fixnum *free-entity-id*))
  (let ((*dependency-reactants* (make-array
				 (list (length A) *free-entity-id*)
				 :element-type 'bit
				 :initial-element 0))
	(*dependency-inhibitors* (make-array
				 (list (length A) *free-entity-id*)
				 :element-type 'bit
				 :initial-element 0))
	(*entity-positive* (make-array
			    *free-entity-id*
			    :element-type '(vector fixnum)
			    :initial-element (make-array 0 :element-type 'fixnum :fill-pointer 0)))
	(*entity-negative* (make-array
			    *free-entity-id*
			    :element-type '(vector fixnum)
			    :initial-element (make-array 0 :element-type 'fixnum :fill-pointer 0)))
	(*enabled-reaction* (make-array
			     (length A)
			     :element-type 'bit
			     :initial-element 0)))
    (loop for i fixnum from 0 to (1- *free-entity-id*) do
	 (setf (aref *entity-positive* i)
	       (make-array 0 :element-type 'fixnum :fill-pointer 0 :adjustable T))
	 (setf (aref *entity-negative* i)
	       (make-array 0 :element-type 'fixnum :fill-pointer 0 :adjustable T)))
    (make-reactions-graph A)
    (loop for ctx across context
       for i fixnum from 0 do
	 (loop for c fixnum across ctx do
	      (setf (bit state i c) 1))
	 (set-possibly-enabled-reactions state i)
	 (apply-possibly-enabled-reactions state i A))))

;; RS runtime compilation

;; (defun unroll-reaction (react)
;;   (declare (type reaction react))
;;   (compile nil
;; 	   `(lambda (state x)
;; 	      (when (and ,@(loop for r across (reaction-reactants react)
;; 			      collecting `(= (bit state x ,r) 1))
;; 			 (not (or ,@(loop for i across (reaction-inhibitors react)
;; 				       collecting `(= (bit state x ,i) 1)))))
;; 		,@(loop for p across (reaction-products react)
;; 		     collecting `(setf (bit state (1+ x) ,p) 1))))))

;; (defun simulator-jit (A context state)
;;    (declare (type (vector reaction) A)
;; 	    (type (vector (vector fixnum)) context)
;; 	    (type (array bit) state)
;; 	    (type fixnum *free-entity-id*))
;;    (let ((reactions (make-array (length A))))
;;      (loop for r across A
;; 	  for i fixnum from 0 do
;; 	  (setf (aref reactions i) (unroll-reaction r)))
;;      (loop for ctx across context
;; 	for i fixnum from 0 do
;; 	  (loop for c fixnum across ctx do
;; 	       (setf (bit state i c) 1))
;; 	  (loop for r across reactions do
;; 	       (funcall r state i)))))
  

;; (defun unroll-all-reactions (A f)
;;   (declare (type (vector reaction) A)
;; 	   (type stream f))
;;   (let ((rnames (make-array (length A))))
;;     (loop for r across A
;;        for i from 0 do
;; 	 (setf (aref rnames i) )
;; 	 (print
;; 	  `(defun ,(aref rnames i) (state x)
;; 	     (declare (type (array bit) state) (type fixnum x))
;; 	  ,(unroll-reaction r))
;; 	  f))
;;     (print `(defun rs-jit (state i)
;; 	      ,@(loop for i from 0 to (1- (length A))
;; 		     collecting `(,(aref rnames i) state i)))
;; 	   f)))

;; (defun simulator-jit (A context state)
;;    (declare (type (vector reaction) A)
;; 	   (type (vector (vector fixnum)) context)
;; 	   (type (array bit) state)
;; 	   (type fixnum *free-entity-id*))
;;    (with-open-file (f "compiled-rs.lisp"
;; 		      :direction :output
;; 		      :if-exists :supersede
;; 		      :if-does-not-exist :create)
;;      (unroll-all-reactions A f))
;;    (compile-file "compiled-rs.lisp")
;;    (load "compiled-rs.lisp")
;;    (loop for ctx across context
;;       for i from 0 do
;; 	(loop for c across ctx do
;; 	     (setf (aref state i c) 1))
;; 	(rs-jit state i)))
   

;; RS MATRIX (WARNING: SLOW)

;; (defun make-rs-matrix (A)
;;   (declare (type fixnum *free-entity-id*))
;;   (let ((M (make-array (list (length A) *free-entity-id* )
;; 		       :element-type 'fixnum
;; 		       :initial-element 0))
;; 	(N (make-array (list *free-entity-id* (length A))
;; 		       :element-type 'fixnum
;; 		       :initial-element 0)))
;;     (loop for react across A	 
;;        for i from 0 do
;; 	 (loop for x across (reaction-reactants react)
;; 	    with y = T do
;; 	      (if y (progn
;; 		      (setf (aref M i x)
;; 			    (1+ (- *free-entity-id* (length (reaction-reactants react)))))
;; 		      (setf y nil))
;; 		  (setf (aref M i x) 1)))
;; 	 (loop for  x across (reaction-inhibitors react) do
;; 	      (setf (aref M i x) (- *free-entity-id*)))
;; 	 (loop for x across (reaction-products react) do
;; 	      (setf (aref N x i) 1)))
;;     (values M N)))

;; (defparameter *tmp-mult-vec* nil)

;; (defun clamped-mat-vec-mul (M v res limit)
;;   (declare (type (vector fixnum)  *tmp-mult-vec*))
;;   (flet ((clamp (x limit)
;; 	   (declare (type fixnum x))
;; 	   (if (>= x limit) 1 0)))
;;     (declare (type (array fixnum (* *)) M)
;; 	     (type (vector bit) v)
;; 	     (type (vector bit) v))
;;     (let ((row (array-dimension M 0))
;; 	  (col (array-dimension M 1)))
;;       (declare (type fixnum row)
;; 	       (type fixnum col))
;;       (loop for i from 0 to (1- row) do
;; 	   (setf (aref *tmp-mult-vec* i) 0)
;; 	   (loop for j from 0 to (1- col) do
;; 		       (setf (aref *tmp-mult-vec* i)
;; 			     (+ (aref *tmp-mult-vec* i)
;; 				(* (aref M i j) (aref v j)))))
;; 	   (setf (aref res i)
;; 		 (clamp (aref *tmp-mult-vec* i)  limit))))))

;; (defun simulator-matrix-multiplication (A context state)
;;   (declare (type (vector reaction) A)
;; 	   (type (vector (vector fixnum)) context)
;; 	   (type (array bit (* *)) state)
;; 	   (type fixnum *free-entity-id*))
;;   (let ((res-reactions (make-array (length A)
;; 				   :element-type 'bit
;; 				   :initial-element 0))
;;     	(*tmp-mult-vec* (make-array *free-entity-id*
;; 				    :element-type 'fixnum
;; 				    :initial-element 0)))
;;     (multiple-value-bind (M N) (make-rs-matrix A)
;;       (loop for ctx across context
;; 	   for i from 0 do
;; 	   (loop for c across ctx do
;; 		(setf (aref state i c) 1))
;; 	   (let ((cs (make-array *free-entity-id*
;; 				 :element-type 'bit
;; 				 :displaced-to state
;; 				 :displaced-index-offset (* i *free-entity-id*)))
;; 		 (next-cs (make-array *free-entity-id*
;; 				      :element-type 'bit
;; 				      :displaced-to state
;; 				      :displaced-index-offset (* (1+ i) *free-entity-id*))))
;; 	     (clamped-mat-vec-mul M cs res-reactions *free-entity-id*)
;; 	     (clamped-mat-vec-mul N res-reactions next-cs 1))))))
	     

;;  (defun profile-set ()
;;   (sb-sprof:profile-call-counts "CL-USER")
;;   (sb-sprof:with-profiling (:max-samples 200
;; 					 :report :flat
;; 					 :loop t
;; 					 :show-progress t)
;;     (simulate-rs "./models/erbb.rsy" #'simulator-dependency-graph "/dev/null")))

;; (defun perform-tests (out)
;;   (with-open-file (f out :direction :output :if-exists :overwrite :if-does-not-exist :create)
;;     (loop for test from 0 to 29 do
;; 	 (loop for density in '("0.01" "0.05" "0.1") do
;; 	      (let ((filename (concatenate 'string
;; 					   "tests-2000-0.01-0.05-0.10/test_"
;; 					   (write-to-string test)
;; 					   "_orsa_2000x2000_bc"
;; 					   density
;; 					   ".orsa"))
;; 		    (start-time 0)
;; 		    (end-time 0))
;; 		(setf start-time (get-internal-real-time))
;; 		(simulate-rs filename #'simulator-set-theory "/dev/null")
;; 		(setf end-time (get-internal-real-time))
;; 		(format f "~s: ~fms~%" filename (- end-time start-time)))))))

;; (defun perform-tests-erbb (out)
;;   (with-open-file (f out :direction :output :if-exists :overwrite :if-does-not-exist :create)
;;     (loop for tests from 0 to 29 do
;; 	 (let ((filename "./tests-erbb/erbb-heresy.rsy")
;; 	       (start-time 0)
;; 	       (end-time 0))
;; 	   (setf start-time (get-internal-real-time))
;; 	   (simulate-rs filename #'simulator-set-theory "/dev/null")
;; 	   (setf end-time (get-internal-real-time))
;; 	   (format f "~s: ~fms~%" filename (- end-time start-time))))))
