(var xs [I32])
(fun + (x Int y Int) -> Int
    (for (var i Int 0) (< i 10) (++ i)
        )
)

(fun >>= ((a A?) mapper)
    (switch a
        (case :yes a' A))
            (<| return yes mapper a')
        (case :nothing)
            (return nothing))

(fun >>= (ptr A* mapper M) -> B*
    (if (null? ptr)
        (return (null))
    :else
        (<| return alloc mapper * ptr)))

(fun fizz-buzz ()
    
)