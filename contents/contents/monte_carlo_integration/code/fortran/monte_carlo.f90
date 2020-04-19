FUNCTION in_circle(pos_x, pos_y, r)
    IMPLICIT NONE
    REAL(16), INTENT(IN) :: pos_x, pos_y, r
    LOGICAL              :: in_circle

    in_circle = (pos_x ** 2 + pos_y ** 2) < r ** 2

END FUNCTION in_circle 

PROGRAM monte_carlo
    
    IMPLICIT NONE
    
    INTERFACE
        FUNCTION in_circle(pos_x, pos_y, r) 
            IMPLICIT NONE
            REAL(16), INTENT(IN) :: pos_x, pos_y, r
            LOGICAL              :: in_circle
        END FUNCTION in_circle 
    END INTERFACE
    
    INTEGER  :: i,n
    REAL(16) :: pos_x,pos_y, r, pi_est, pi_count, pi_error, pi
    
    ! Calculate Pi from trigonometric functions as reference
    pi       = DACOS(-1.d0)
    n        = 1000000
    r        = 1d0
    pos_x    = 0d0
    pos_y    = 0d0
    pi_count = 0d0
    
    DO i=0,n
    
        CALL RANDOM_NUMBER(pos_x)
        CALL RANDOM_NUMBER(pos_y)
    
        IF (in_circle(pos_x, pos_y, r) .EQV. .TRUE.) THEN 
    
            pi_count = pi_count + 1d0
    
        END IF
    END DO
    
    pi_est   = 4d0 * pi_count / n
    pi_error = 100d0 * (abs(pi_est - pi)/pi)
    
    WRITE(*,'(A, F12.4)') 'The pi estimate is: ', pi_est
    WRITE(*,'(A, F12.4, A)') 'Percent error is: ', pi_error, ' %'
    
END PROGRAM monte_carlo
