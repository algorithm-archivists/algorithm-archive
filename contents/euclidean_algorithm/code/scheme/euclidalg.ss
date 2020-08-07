(define (euclid-sub a b)
    (cond                                                                                                                                                                                                                                                                         
        [(or (negative? a)(negative? b))(euclid-sub (abs a)(abs b))]                                                                                                                                                                                                              
        [(eq? a b) a]
        [(> a b)(euclid-sub(- a b) b)]
        [else
            (euclid-sub a (- b a))]))

(define (euclid-mod a b)
    (if (zero? b)
        a
        (euclid-mod b (modulo a b))))

(display (euclid-mod (* 64 67) (* 64 81))) (newline)
(display (euclid-sub (* 64 12) (* 64 27))) (newline)
