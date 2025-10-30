;;;;;MY-LENGTH;;;;;
(defun MY-LENGTH(L)
    (cond
        ((null L) 0)
        (t (+ 1 (MY-LENGTH(cdr L))))
    )
)

;;;;;MY-MEMQ;;;;;
(defun MY-MEMQ(A L)
	(cond
		((null L) nil)
		((eq A (car L)) L) 
		(t (MY-MEMQ A (cdr L)))
	)
)

;;;;;MY-APPEND;;;;;
(defun MY-APPEND(L1 L2)
	(cond
		((and (null L1) (null L2)) nil)
		((null L1) L2)
		((null L2) L1)
		(t (cons (car L1) (MY-APPEND (cdr L1) L2)))
	)
)

;;;;;MY-REVERSE;;;;;
(defun MY-REVERSE(L)
	(cond
		((null L) nil)
		(t (MY-APPEND (MY-REVERSE (cdr L)) (list(car L))))
	)

)

;;;;;MY-MAPCAR;;;;
(defun MY-MAPCAR(F L)
	(cond
		((null L) nil)
		(t (cons (APPLY F (list (car L))) (MY-MAPCAR F (cdr L))))
	)
)


;;;;;MY-COPY;;;;;
(defun MY-COPY(L)
	(cond
		((atom L) L)
		(t (cons (MY-COPY (car L)) (MY-COPY (cdr L)))) 
	)
)

;;;;;MY-NTH;;;;;
(defun MY-NTH (L N)
	(cond
		((= N 0) (cons nil L))
		((= N 1) L)
		((null L) nil)
		(t (MY-NTH(cdr L) (- N 1)))
	)
)

;;;;;MY-REMOVE;;;;;
(defun MY-REMOVE (X L)
	(cond
		((null L) nil)
		((equal X (car L)) (MY-REMOVE X (cdr L)))
		(t (cons (car L) (MY-REMOVE X (cdr L))))
	)
)

;;;;;MY-SUBST;;;;;
(defun MY-SUBST (X Y Z)
	(cond
		((atom Z)
		(cond ((EQ Z X) Y)(T Z)))
		(t (cons (MY-SUBST X Y (car Z)) (MY-SUBST X Y (cdr Z))))
	)
)

;;;;;MY-SUBSET;;;;;
(defun MY-SUBSET (FN L)
	(cond
    	((null L) nil)
    	((apply FN (list (car L))) (cons (car L) (MY-SUBSET FN (cdr L))))
    	(t (MY-SUBSET FN (cdr L)))
	)
)

;;;;;MY-MERGE;;;;;
(defun MY-MERGE (L1 L2)
	(cond
		((null L1) L2)
		((null L2) L1)
		((< (car L1) (car L2))(cons (car L1)(MY-MERGE (cdr L1) L2)))
		(t (cons (car L2)(MY-MERGE L1 (cdr L2))))
	)
)
;;;;;MY-SUBLIST;;;;;
(defun MY-SUBLIST (L1 L2)
	(cond
		((null L1) t)
    	((null L2) nil)
    	((eq (car L1) (car L2))
    	(cond
    		((MY-SUBLIST (cdr L1) (cdr L2)) t)
    		(t (MY-SUBLIST L1 (cdr L2))))
		)
    	(t (MY-SUBLIST L1 (cdr L2)))
	)
)

;;;;;MY-ASSOC;;;;;
(defun MY-ASSOC (A ALIST)
  (cond
    ((null ALIST) nil)
    ((eq A (car (car ALIST))) (car ALIST))
    (t (MY-ASSOC A (cdr ALIST)))))

;;;;;MY-REPLACE;;;;;
(defun MY-REPLACE (alist l)
  (cond
    ((atom l)
     (cond
       ((MY-ASSOC l alist) (cdr (MY-ASSOC l alist)))
       (t l)))
    ((null l) nil)
    (t (cons (my-replace alist (car l)) (my-replace alist (cdr l))))
	)
)
