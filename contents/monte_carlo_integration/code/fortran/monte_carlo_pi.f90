FUNCTION in_circle(pos_x, pos_y, r) ! RESULT(in_circle)
      IMPLICIT NONE
      REAL(64), INTENT(IN) :: pos_x, pos_y, r
      LOGICAL :: in_circle
in_circle = (pos_x ** 2 + pos_y ** 2) < r
END FUNCTION in_circle 

PROGRAM monte_carlo
IMPLICIT NONE

INTEGER :: i,n
REAL(64) :: pos_x,pos_y, r, pi_est, pi_count
LOGICAL :: in_circle

! TEST CASE WRITE(*,*) in_circle(0.5d0, 0.5d0, 1d0) ! is true
n = 100000
pi_count = 0
      DO i=0,n
        CALL RANDOM_NUMBER(pos_x)
        CALL RANDOM_NUMBER(pos_y)
        
        IF in_circle(pos_x, pos_y, f)
        THEN pi_count = pi_count + 1

END PROGRAM monte_carlo
