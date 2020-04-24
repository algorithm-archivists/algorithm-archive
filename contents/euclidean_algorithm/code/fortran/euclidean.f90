INTEGER FUNCTION euclid_sub(a, b)
    IMPLICIT NONE
    INTEGER, INTENT(INOUT) :: a, b

    a = ABS(a)
    b = ABS(b)

    DO WHILE (a /= b)
    
        IF (a > b) THEN
            a = a - b
        ELSE
            b = b - a
        END IF
    END DO

    euclid_sub = a

END FUNCTION euclid_sub 

INTEGER FUNCTION euclid_mod(a, b)
    IMPLICIT NONE
    INTEGER, INTENT(INOUT) :: a, b
    INTEGER                :: temp

    DO WHILE (b > 0)
        temp = b
        b = MODULO(a,b)
        a = temp
    END DO

    euclid_mod = a

END FUNCTION euclid_mod

PROGRAM euclidean

    IMPLICIT NONE
    INTEGER :: a, b, euclid_sub, euclid_mod
    
    a = 24
    b = 27
    WRITE(*,*) 'Subtraction method: GCD is: ', euclid_sub(a, b)
    
    a = 24
    b = 27
    WRITE(*,*) 'Modulus method:     GCD is: ', euclid_mod(a, b)

END PROGRAM euclidean 
