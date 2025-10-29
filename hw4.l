

(defunc my-length(L)
    (cond
        ((null L) 0)
        (t (+ 1 (my-length(cdr(L)))))
    )
)

