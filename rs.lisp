(ql:quickload :cl-ppcre)
(use-package :cl-ppcre)

;; (require :sb-sprof)

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
 
(defun simulate-rs (filename simulator &optional out)
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
	(if out
	    (with-open-file (f out :direction :output :if-exists :overwrite)
	      (print-state state f))
	    (print-state state))))))
  
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
(defparameter *entity-positive* nil)
(defparameter *enabled-reaction* nil)

(defun make-reactions-graph (A)
  (declare (type (array bit) *dependency-reactants*)
	   (type (vector (vector fixnum)) *entity-positive*)
	   (type fixnum *free-entity-id*))
  (loop for react across A
     for i fixnum from 0 do
       (loop for x fixnum across (reaction-reactants react) do
	    (setf (bit *dependency-reactants* i x) 1)))
  (loop for i fixnum from 0 to (1- (length A)) do
       (loop for j fixnum from 0 to (1- *free-entity-id*) do
	    (when (= (aref *dependency-reactants* i j) 1)
	      (vector-push-extend i (aref *entity-positive* j))))))

(defun deps-apply-reaction (reaction state i turn)
  (declare (type fixnum i)
	   (type fixnum turn)
	   (type reaction reaction)
	   (type (array bit) state)
	   (type (array bit) *enabled-reaction*)
	   (type (vector (vector fixnum)) *entity-positive*))
  (when (and (loop for r-id fixnum across (reaction-reactants reaction)
		always (= (bit state i r-id) 1))
	     (loop for i-id fixnum across (reaction-inhibitors reaction)
		never (= (bit state i i-id) 1)))
    (loop for p-id fixnum across (reaction-products reaction) do
	 (setf (bit state (1+ i) p-id) 1)
	 (loop for r fixnum across (aref *entity-positive* p-id) do
	      (setf (bit *enabled-reaction* r (- 1 turn)) 1)))))

(defun apply-possibly-enabled-reactions (state i A turn)
  (declare (type (array bit) state)
	   (type fixnum i)
	   (type fixnum turn)
	   (type (array bit) *enabled-reaction*)
	   (type (vector reaction) A))
  (loop for j fixnum from 0 to (1- (length A)) do
       (when (= (aref *enabled-reaction* j turn) 1)
	 (deps-apply-reaction (aref A j) state i turn)
	 (setf (bit *enabled-reaction* j  turn) 0))))

(defun simulator-dependency-graph (A context state)
  (declare (type (vector reaction) A)
	   (type (vector (vector fixnum)) context)
	   (type (array bit) state)
	   (type fixnum *free-entity-id*))
  (let ((*dependency-reactants* (make-array
				 (list (length A) *free-entity-id*)
				 :element-type 'bit
				 :initial-element 0))
	(*entity-positive* (make-array
			    *free-entity-id*
			    :element-type '(vector fixnum)
			    :initial-element (make-array 0 :element-type 'fixnum :fill-pointer 0)))
	(*enabled-reaction* (make-array
			     (list (length A) 2)
			     :element-type 'bit
			     :initial-element 0))
	(turn 0))
    (loop for i fixnum from 0 to (1- *free-entity-id*) do
	 (setf (aref *entity-positive* i)
	       (make-array 0 :element-type 'fixnum :fill-pointer 0 :adjustable T)))
    (make-reactions-graph A)
    (loop for ctx across context
       for i fixnum from 0 do
	 (loop for c fixnum across ctx do
	      (setf (bit state i c) 1)
	      (loop for r fixnum across (aref *entity-positive* c) do
		   (setf (bit *enabled-reaction* r turn) 1)))
	 (apply-possibly-enabled-reactions state i A turn)
	 (setf turn (- 1 turn)))))

;;  (defun profile-set ()
;;   (sb-sprof:profile-call-counts "CL-USER")
;;   (sb-sprof:with-profiling (:max-samples 200
;; 					 :report :flat
;; 					 :loop t
;; 					 :show-progress t)
;;     (simulate-rs "../models/erbb.rsy" #'simulator-dependency-graph "/dev/null")))

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
	   (if (eq type :set-theory)
	       (progn
		 (setf start-time (get-internal-real-time))
		 (simulate-rs filename #'simulator-set-theory "/dev/null")
		 (setf end-time (get-internal-real-time))
		 (format f "set-theory, ~a, ~a, ~a, ~a~%" test size density (- end-time start-time)))
	       (progn
		 (setf start-time (get-internal-real-time))
		 (simulate-rs filename #'simulator-dependency-graph "/dev/null")
		 (setf end-time (get-internal-real-time))
		 (format f "dependency-graph, ~a, ~a, ~a, ~a~%" test size density (- end-time start-time))))))))

;; example of how to perform all the tests
;; (loop for n in '(10 100 1000 2000) do
;;      (loop for d in '(0.01 0.05 0.1) do
;; 	  (perform-tests n d "./test-models/" :set-theory "results.txt")
;; 	  (perform-tests n d "./test-models/" :dependency-graph "results.txt")))
