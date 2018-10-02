PROGRAM main

    IMPLICIT NONE
    REAL(8), DIMENSION(10) :: A

    A = (/ 1d0, 3d0, 2d0, 4d0, 5d0, 10d0, 50d0, 7d0, 1.5d0, 0.3d0 /)

    WRITE(*,*) 'Input vector'
    WRITE(*,'( F6.2 )') A
    WRITE(*,*) ' '

    CALL bubblesort(A)

    WRITE(*,*) 'Output vector'
    WRITE(*,'(F6.2)') A

CONTAINS

SUBROUTINE bubblesort(array)

    IMPLICIT NONE
    INTEGER                              :: array_length, i, j, n
    REAL(8)                              :: tmp
    REAL(8), DIMENSION(:), INTENT(INOUT) :: array

    array_length = size(array)
    n = array_length
    
    DO i=1, n
        DO j=1, n-1
            IF ( array(j) > array(j+1) ) THEN

                tmp        = array(j+1)
                array(j+1) = array(j)
                array(j)   = tmp
                                
            END IF
        END DO
    END DO
END SUBROUTINE bubblesort
    
END PROGRAM main 
