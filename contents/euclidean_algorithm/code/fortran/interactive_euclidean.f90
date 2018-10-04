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
    INTEGER             :: temp


    DO WHILE (b > 0)
        temp = b
        b = MODULO(a,b)
        a = temp
    END DO

    euclid_mod = a

END FUNCTION euclid_mod

PROGRAM euclidean
    
    IMPLICIT NONE
    INTEGER :: a, b, euclid_sub, euclid_mod, temp_a, temp_b, ioerror
    
    DO
        WRITE(*,*) 'Calculate greatest common divisor. Give two integers:'
        READ(*, '(i10)', iostat=ioerror) temp_a, temp_b
        
        IF (ioerror == 0) THEN
            EXIT
        END IF
    
        WRITE(*,*) 'Entered numbers are not integers. Try again.'
    END DO
    
    a = temp_a
    b = temp_b
    WRITE(*,*) 'Subtraction method: GCD is: ', euclid_sub(a, b)
    
    a = temp_a 
    b = temp_b 
    WRITE(*,*) 'Modulus method:     GCD is: ', euclid_mod(a, b)
END PROGRAM euclidean 
