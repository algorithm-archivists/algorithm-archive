SUBROUTINE verlet(pos, acc, dt, time) 
    IMPLICIT NONE
    REAL(8), INTENT(INOUT) :: pos, acc, dt, time
    REAL(8)                :: prev_pos, next_pos


    prev_pos = pos
    time     = 0d0

    DO
        IF (pos > 0d0) THEN
            time     = time + dt
            next_pos = pos * 2d0 - prev_pos + acc * dt ** 2
            prev_pos = pos
            pos      = next_pos
        ELSE
            EXIT
        END IF
    END DO
END SUBROUTINE verlet

SUBROUTINE stormer_verlet(pos, acc, dt, time, vel) 
    IMPLICIT NONE
    REAL(8), INTENT(INOUT) :: pos, acc, dt, time, vel
    REAL(8)                :: prev_pos, next_pos

    prev_pos = pos 
    time     = 0d0
    vel      = 0d0

    DO
        IF (pos > 0d0) THEN
            time     = time + dt
            next_pos = pos * 2 - prev_pos + acc * dt ** 2
            prev_pos = pos
            pos      = next_pos
            vel      = vel + acc * dt
        ELSE
            EXIT
        END IF
    END DO
END SUBROUTINE stormer_verlet 

SUBROUTINE velocity_verlet(pos, acc, dt, time, vel) 
    IMPLICIT NONE
    REAL(8), INTENT(INOUT) :: pos, acc, dt, time, vel

    time     = 0d0
    vel      = 0d0

    DO
        IF (pos > 0d0) THEN
            time = time + dt
            pos  = pos + vel * dt + 0.5d0 * acc * dt ** 2 
            vel  = vel + acc * dt
        ELSE
            EXIT
        END IF
    END DO
END SUBROUTINE velocity_verlet 

PROGRAM verlet_integration

    IMPLICIT NONE 
    REAL(8) :: pos,acc, dt, time, vel
    
    INTERFACE
        SUBROUTINE verlet(pos, acc, dt, time)
        REAL(8), INTENT(INOUT) :: pos, acc, dt, time
        REAL(8)                :: prev_pos, next_pos
        END SUBROUTINE
    END INTERFACE 
    
    INTERFACE 
        SUBROUTINE stormer_verlet(pos, acc, dt, time, vel) 
            REAL(8), INTENT(INOUT) :: pos, acc, dt, time, vel
            REAL(8)                :: prev_pos, next_pos
        END SUBROUTINE 
    END INTERFACE 
    
    INTERFACE 
        SUBROUTINE velocity_verlet(pos, acc, dt, time, vel) 
            REAL(8), INTENT(INOUT) :: pos, acc, dt, time, vel
            REAL(8)                :: prev_pos, next_pos 
        END SUBROUTINE 
    END INTERFACE 
    
    pos = 5d0
    acc = -10d0
    dt  = 0.01d0
    ! Verlet 
    CALL verlet(pos, acc, dt, time)
    
    WRITE(*,*) 'Time for Verlet integration: ', time 
    
    ! stormer Verlet 
    pos = 5d0
    CALL stormer_verlet(pos, acc, dt, time, vel)
    
    WRITE(*,*) 'Time for Stormer-Verlet integration: ', time 
    
    ! Velocity Verlet
    pos = 5d0
    CALL velocity_verlet(pos, acc, dt, time, vel)
    
    WRITE(*,*) 'Time for Velocity-Verlet integration: ', time 
    
END PROGRAM verlet_integration
