Integer>>euclidSub: secondNumber
    "Euclidean algorithm with subtraction"
    | a b |
    a := self abs.
    b := secondNumber abs.
    [ a == b ] whileFalse: [ 
        a > b ifTrue: [ 
            a := a - b.
        ] ifFalse: [ 
            b := b - a.
        ].
    ].
    ^a.

Integer>>euclidMod: secondNumber
    "Euclidean algorithm with modulus."
    | a b oldB |
    a := self abs.
    b := secondNumber abs.
    [ b == 0 ] whileFalse: [ 
        oldB := b.
        b := a % b.
        a := oldB.
    ].
    ^a.

Transcript show: ((64 * 67) euclidSub: (64 * 81)).
Transcript cr.
Transcript show: ((128 * 12) euclidMod: (128 * 77)).
