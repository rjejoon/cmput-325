(defmacro with-gensyms (syms &body body)
  `(let ,(loop for s in syms collect `(,s (gensym)))
     ,@body))

(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defun report-result (result form)
  "Report the results of a single case. Called by 'check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)
;;----------------------------------------------------------------------

;;EXAMPLE

;(deftest test-arithmetic ()
  ;(combine-results
   ;(test-+)
   ;(test-*)))

;(deftest test-+ ()
  ;(check
   ;(= (+ 1 2) 3)
   ;(= (- 1 2) -1)
   ;(= (+ -1 -3) -4)))

;(deftest test-* ()
  ;(check
   ;(= (- 1 2) -1)
   ;(= (* 3 4) 12)))


;; Write unit tests down here
;(print (test-arithmetic))
