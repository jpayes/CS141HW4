

(defun MY-LENGTH(L)
    (cond
        ((null L) 0)
        (t (+ 1 (MY-LENGTH(cdr L))))
    )
)

(defun MY-MEMQ(A L)
	(cond
		((null L) nil)
		((eq A (car L)) L) 
		(t (MY-MEMQ A (cdr L)))
	)
)

(defun MY-APPEND(L1 L2)
	(cond
		((and (null L1) (null L2)) nil)
		((null L1) L2)
		((null L2) L1)
		(t (cons (car L1) (MY-APPEND (cdr L1) L2)))
	)
)

(defun MY-REVERSE(L)
	(cond
		((null L) nil)
		(t (MY-APPEND (MY-REVERSE (cdr L)) (list(car L))))
	)

)

(defun MY-MAPCAR(F L)
	(cond
		((null L) nil)
		(t (cons (APPLY F (list (car L))) (MY-MAPCAR F (cdr L))))
	)
)

(defun MY-COPY(L)
	(cond
		((atom L) L)
		(t (cons (MY-COPY (car L)) (MY-COPY (cdr L)))) 
	)
)

(defun MY-NTH (L N)
	(cond
		((= N 0) (cons nil L))
		((= N 1) L)
		((null L) nil)
		(t (MY-NTH(cdr L) (- N 1)))
	)
)


(defun MY-REMOVE (X L)
	(cond
		((null L) nil)
		((equal X (car L)) (MY-REMOVE X (cdr L)))
		(t (cons (car L) (MY-REMOVE X (cdr L))))
	)
)

(defun MY-SUBST (X Y Z)
	(cond
		((atom Z)
		(cond ((EQ Z X) Y)(T Z)))
		(t (cons (MY-SUBT X Y (car Z)) (MY-SUBT X Y (cdr Z))))
	)
)

(defun MY-SUBSET (FN L)
	(cond
    	((null L) nil)
    	((apply FN (list (car L))) (cons (car L) (MY-SUBSET FN (cdr L))))
    	(t (MY-SUBSET FN (cdr L)))
	)
)

(defun MY-MERGE (L1 L2)
	(cond
		((null L1) L2)
		((null L2) L1)
		((< (car L1) (car L2))(cons (car L1)(MY-MERGE (cdr L1) L2)))
		(t (cons (car L2)(MY-MERGE L1 (cdr L2))))
	)
)