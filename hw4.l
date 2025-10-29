

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
		(t (cons (APPLY F (list (car L))) (MY-MAPCAR(F (cdr L)))
	)
)
