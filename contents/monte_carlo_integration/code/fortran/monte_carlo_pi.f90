FUNCTION in_circle(pos_x, pos_y, r) 
IMPLICIT NONE
REAL(16), INTENT(IN) :: pos_x, pos_y, r
LOGICAL :: in_circle
in_circle = (pos_x ** 2 + pos_y ** 2) < r ** 2
END FUNCTION in_circle 

PROGRAM monte_carlo
IMPLICIT NONE

INTEGER :: i,n
REAL(16) :: pos_x,pos_y, r, pi_est, pi_count, pi_error, pi
LOGICAL :: in_circle

! Calculate Pi from trigonometric functions as reference
pi = DACOS(-1.d0)  
n  = 100000
r  = 1
pos_x = 0
pos_y = 0
pi_count = 0

DO i=0,n
    CALL RANDOM_NUMBER(pos_x)
    CALL RANDOM_NUMBER(pos_y)
       
    IF (in_circle(pos_x, pos_y, r) .EQV. .TRUE.) THEN 
        pi_count = pi_count + 1
    END IF
END DO

pi_est   = 4 * pi_count / n
pi_error = 100 * (abs(pi_est - pi)/pi)

WRITE(*,*) 'The pi estimate is: ', pi_est
WRITE(*,*) 'Percent error is: ', pi_error, ' %'

END PROGRAM monte_carlo
