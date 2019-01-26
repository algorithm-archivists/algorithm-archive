(define euclid-sub
    (lambda (a b)
        (cond 
            ((or (negative? a) (negative? b))
            (euclid-sub (abs a) (abs b)))
            (else (cond
                ((eq? a b) a)
                (else (cond
                    ((> a b)
                    (euclid-sub (- a b) b))
                    (else
                        (euclid-sub a (- b a))))))))))
                  
(define euclid-mod
    (lambda (a b)
        (cond
            ((or (negative? a) (negative? b))
            (euclid-mod (abs a) (abs b)))
        (else (cond
            ((zero? b) a)
            (else
                (euclid-mod b (modulo a b))))))))

(display (euclid-mod (* 64 67) (* 64 81))) (newline)
(display (euclid-sub (* 64 12) (* 64 27))) (newline)