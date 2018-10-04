PROGRAM bogo
    IMPLICIT NONE
    REAL(8), DIMENSION(5) :: array

    array = (/ 1d0, 1d0, 0d0, 3d0, 7d0 /)

    CALL bogo_sort(array)

    WRITE(*,*) array 

contaINs

    LOGICAL FUNCTION is_sorted(array) 
        REAL(8), DIMENSION(:), INTENT(IN) :: array
        INTEGER                           :: i

         DO i = 1, SIZE(array)
            IF (array(i+1) < array(i)) THEN
                is_sorted = .FALSE.
            END IF
         END DO
    END FUNCTION is_sorted

    SUBROUTINE bogo_sort(array)
        REAL(8), DIMENSION(:), INTENT(INOUT) :: array

        DO WHILE (is_sorted(array) .EQV. .FALSE.)

            CALL shuffle(array)

        END DO
    END SUBROUTINE bogo_sort
    
    SUBROUTINE shuffle(array)
        REAL(8), DIMENSION(:), INTENT(INOUT) :: array
        INTEGER                              :: i, randpos
        REAL(8)                              :: r, temp

        DO i = size(array), 2, -1
            CALL RANDOM_NUMBER(r)
            randpos    = INT(r * i) + 1
            temp       = array(randpos)
            array(randpos) = array(i)
            array(i)       = temp
        END DO

    END SUBROUTINE shuffle
END PROGRAM bogo
