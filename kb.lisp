;---------------------------
; Global variables/structs
;---------------------------
(defstruct kb-struct (clauses (make-hash-table)))

;--------------------------------------------------------------------------------------------
; OTTER theorem prover from AIAMA figure 9.14
;--------------------------------------------------------------------------------------------
(defun otter (kb sos)
  (if (null sos)
      (return-from otter 'fail)
      ; sorts sos by length and finds shortest clause in sos and saves it as shrtest-clause
      (let* ((sos (sort sos #'< :key #'length)) (shrtest-clause (pop sos))
		  ; Adds shrtest-clause from sos into knowledge base and calling this usable.
		  (usable (add kb shrtest-clause))
	          ;PROCESS(INFER(clause,usable),sos) from textbook
		  (sos (process (infer shrtest-clause usable) sos)))
	     (if (eq sos t) (return-from otter 'negated_query_is_false))
	     ;run otter until either sos is empty or refutation has been found
	     (otter usable sos))))

; INFER(clause, usable) from AIAMA figure 9.14
(defun infer (clause usable)
  (resolve clause usable))

; From AIAMA textbook figure 9.14
(defun process (clauses sos)
  ;for each clause in clauses
  (dolist (clause clauses)
    (if (eq (length clause) 0) (return-from process t))
    ;merges clause into sos
    (setf sos (merge 'list (list clause) sos  #'< :key #'length)))
  sos)  	  

;--------------------------------------------------------------------------------------------
; Misc function definitions 
;--------------------------------------------------------------------------------------------
; Tell algorithm "tells" the knowledge base a set of clauses
(defun tell (a-kb-struct clauses)
  (cond ((null clauses) a-kb-struct)
	(t (let ((a-kb-struct (add a-kb-struct (car clauses)))) (tell a-kb-struct (cdr clauses))))))

; Adds clause to kb by splitting each hash entry into "first" and "second", where "first" contains positive literals and "second" contains
; negative literals.
(defun add (kb clause)
  (dolist (literal clause)
    (let* ((key (get-key literal))
	   (entry-val (get-entry-val key kb)))
      (if (eq (if (listp literal) (car literal)) '!)
	  (setf (second entry-val) (cons clause (second entry-val)))
	  (setf (first entry-val) (cons clause (first entry-val))))
      (setf (gethash key (kb-struct-clauses kb)) entry-val))) kb)

; Returns key for hasing from literal
(defun get-key (literal)
  (if (eq (when (listp literal) (car literal)) '!) (cadr literal) (when (listp literal) (car literal))))

; Returns value of entry from knowledge base hash-table using the key
(defun get-entry-val (key kb)
  (or (gethash key (kb-struct-clauses kb)) (list nil nil)))

;-----------------------------

; Unify from AIAMA figure 9.1
(defun unify (x y &optional (z nil))
  (cond ((eql z 'fail) 'fail)
	((eql x y) z)
	((varp x) (unify-var x y z))
	((varp y) (unify-var y x z))
	;((and (compound-p x) (compund-p y)) (unify (args x) (args y) (unify (op x) (op y) z)))
	((and (listp x) (listp y)) (unify (cdr x) (cdr y) (unify (car x) (car y) z)))
	(t 'fail)))

; Unify-var from AIAMA figure 9.1
(defun unify-var (var x z)
  (let ((varbinding (assoc var z)) (xbinding (assoc x z)))
    (cond (varbinding (unify (cdr varbinding) x z))
	  (xbinding (unify var (cdr xbinding) z))
	  ;((occursp var x) 'fail)
	  (t (cons (cons var x) z)))))

; varp checks if x is indeed a variable (for unification)
(defun varp(x)
 (if (and (symbolp x) (eql (char (symbol-name x) 0) #\$))
 (return-from varp T)
 (return-from varp nil)))

;-----------------------------

; resolves clause with each member of usable
(defun resolve (clause usable)
  (let* ((new-clauses))
    ;for each literal in clause...
    (dolist (literal clause)
      ; Creates a list of potential resolvers by finding literals of the opposite sign.
      (let ((potential-resolvers (get-potential-resolvers literal usable)))
	; For each potential-clause in potential-resolver and then each potential-literal in the potential-clause
	(dolist (potential-clause potential-resolvers) (dolist (potential-literal potential-clause)
	    (let ((substitutions (unify potential-literal (get-other-literal literal))))
          ; Perform substitution 
	      (if (not (eq substitutions 'fail))
		(let ((new-clause (unify-subst (append (remove literal clause) (remove potential-literal potential-clause)) substitutions)))
		  (print-out clause potential-clause new-clause)	  
		  (setf new-clauses (merge 'list (list new-clause) new-clauses #'(lambda (x y) t))))))))))
    new-clauses))

; returns a list of potential resolvers
(defun get-potential-resolvers (literal usable)
  (if (eq (if (listp literal) (car literal)) '!)
				     (first (gethash (cadr literal) (kb-struct-clauses usable)))
				     (second (gethash (if (listp literal) (car literal)) (kb-struct-clauses usable)))))

; returns literal to be unified
(defun get-other-literal (literal)
  (if (not (eq (when (listp literal) (car literal)) '!))
      (append (list '!) literal)
      (cdr literal)))

; Perform substution ("during" unification)
(defun unify-subst (a b)
  (cond ((eql b nil) a)
	((and (varp a) (assoc a b)) (unify-subst (rest (assoc a b)) b))
	((atom a) a)
	(t (cons (unify-subst (car a) b) (unify-subst (cdr a) b)))))

; outputs results of unification
(defun print-out (clause potential-clause new-clause)
  (format t "Unifying~T~S~Tand~T~S~Tgives:~%~S~%~%~%" clause potential-clause new-clause))

;--------------------------------------------------------------------------------------------
; Running automated theorem prover (Main function)
;--------------------------------------------------------------------------------------------
(defun atp (input-kb neg-query)
  (let ((a-kb-struct (make-kb-struct))) (let ((kb (tell a-kb-struct input-kb))) (otter kb neg-query))))
