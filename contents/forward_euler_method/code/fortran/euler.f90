PROGRAM euler
    
    IMPLICIT NONE
    LOGICAL                            :: is_approx
    REAL(8), DIMENSION(:), ALLOCATABLE :: vec
    REAL(8)                            :: time_step, threshold
    INTEGER                            :: n

    time_step = 0.01d0
    n         = 100
    threshold = 0.01d0
    
    ALLOCATE(vec(n))
    CALL forward_euler(time_step, n, vec)
    is_approx = check_result(vec, threshold, time_step)

    WRITE(*,*) is_approx

    DEALLOCATE(vec)
    
CONTAINS

    SUBROUTINE forward_euler(time_step, n, vec)
        
        IMPLICIT NONE
        REAL(8), DIMENSION(:), INTENT(OUT)   :: vec
        REAL(8), INTENT(IN)                  :: time_step
        INTEGER, INTENT(IN)                  :: n
        INTEGER                              :: i

        vec(1) = 1d0

        DO i=1, n-1
        
            vec(i+1) = vec(i) - 3d0 * vec(i) * time_step

        END DO
    END SUBROUTINE

    LOGICAL FUNCTION check_result(euler_result, threshold, time_step) 
    
        IMPLICIT NONE
        REAL(8), DIMENSION(:), INTENT(IN) :: euler_result
        REAL(8), INTENT(IN)               :: threshold, time_step 
        REAL(8)                           :: time, solution
        INTEGER                           :: i

        check_result = .TRUE.

        DO i = 1, SIZE(euler_result)

            time = (i - 1) * time_step
            solution = EXP(-3d0 * time)
             
            IF (ABS(euler_result(i) - solution) > threshold) THEN
                
                WRITE(*,*) euler_result(i), solution
                check_result = .FALSE.

            END IF
        END DO
    END FUNCTION
END PROGRAM euler

