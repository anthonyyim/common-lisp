(defun match (p f &optional (b T))
  (cond
    ((equal nil b) nil) ;if b is nil, there was an error (e.g. non-equal atoms) somewhere, return nil.
    ((and (null p) (null f)) b)	;check if it reached end of lists.
    ((and (null p) (not (null f))) nil) ;check if p is at end and f isn't.
    ((and (not (null p)) (null f)) nil) ;check if p is not at end but f is. 

    ;if p is a list and f is an atom, this may mean there is a & operator. Check each item in list against the fact.
    ((and (listp p) (not (listp f))) (eval (cons 'and (loop for i in p collect (list 'quote (match i f b))))))
    
    ;if p and f are atoms, compare p to f.
    ((not (and (listp p) (listp f))) (cond
				       ;check if symbol contains a "=" if so, check to see whether a binding should be made or an equality check should be done.
				       ((string= (symbol-name p) "=" :start1 0 :end1 1 :start2 0 :end2 1) (cond
													    ((equal T b) (cons (list (intern (subseq (symbol-name p) 1)) f) nil))
													    ((not (equal nil (assoc (intern (subseq (symbol-name p) 1)) b)))  (if (equalp f (cadr (assoc (intern (subseq (symbol-name p) 1)) b)))
																						  b
																						  nil))					      		    
													    (T (cons (list (intern (subseq (symbol-name p) 1)) f) b))))
				       ;check if symbol contains a "!". 
				       ((string= (symbol-name p) "!" :start1 0 :end1 1 :start2 0 :end2 1) (cond
													    ((and (equal T b) (equalp p f)) b)
													    ((not (equal nil (assoc (intern (subseq (symbol-name p) 1)) b)))  (if (equalp f (cadr (assoc (intern (subseq (symbol-name p) 1)) b)))
																						  nil
																						  b))					      		    
													    (T nil)))
				       ;check if symbol contains a "<". 
				       ((string= (symbol-name p) "<" :start1 0 :end1 1 :start2 0 :end2 1) (cond
													    ((and (equal T b) (equalp p f)) b)
													    ((not (equal nil (assoc (intern (subseq (symbol-name p) 1)) b)))  (if (< f (cadr (assoc (intern (subseq (symbol-name p) 1)) b)))
																						  b
																						  nil))					      		    
													    (T nil)))				       
				       ;check if symbol contains a ">". 
				       ((string= (symbol-name p) ">" :start1 0 :end1 1 :start2 0 :end2 1) (cond
													    ((and (equal T b) (equalp p f)) b)
													    ((not (equal nil (assoc (intern (subseq (symbol-name p) 1)) b)))  (if (> f (cadr (assoc (intern (subseq (symbol-name p) 1)) b)))
																						  b
																						  nil))					      		    
													    (T nil)))
				       ((string= (symbol-name p) "&") b)
				       ((and (not (equal T b)) (equalp p f)) b) ;if b is a list and p and f are equal, return the list
				       ((equal T b) (equalp p f)))) ;if b is a T, comapare p and f , return the T or nil.
    
    ;if p and f are lists, recursively match car p and car f, then cdr
    ;p and cdr f.
    ((and (listp p) (listp f)) (match (cdr p) (cdr f) (match (car p) (car f) b)))
  )
)
