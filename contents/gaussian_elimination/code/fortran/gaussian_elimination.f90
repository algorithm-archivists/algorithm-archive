PROGRAM main

    IMPLICIT NONE
    INTEGER                  :: i 
    REAL(8), DIMENSION(3, 4) :: A, B_transposed
    REAL(8), DIMENSION(4, 3) :: B

    DATA B / 2d0, 3d0, 4d0, &
    6d0, 1d0, 2d0, &
    3d0, 4d0, 3d0, &
    -4d0, 0d0, 10d0 /

    ! Arrays are initialized column by column up to the size or
    ! dimensionality of the column. Therefore here the
    ! 'horizontally' written rows will be the columns of an
    ! virtually-sized 4x3 matrix. In
    ! order to match the example matrix transpose operation must be
    ! invoked.

    A = transpose( reshape(&
    (/ 2d0, 3d0, 4d0, 6d0, &
       1d0, 2d0, 3d0, 4d0, &
       3d0, -4d0, 0d0, 10d0 /), &
       (/ size(A,2), size(A,1) /) ) )

    !DO i = 1, SIZE(A, 1)
    !        WRITE(*,*) i, A(i,:)
    !END DO

    ! Alternatively one can use the old DATA instruction which assigns
    ! the array elements the data given in the same column-first
    ! principle. The matrix must be transposed as well but by using an
    ! additional declared array with the proper shape.

    B_transposed = transpose(B)

    !DO i = 1, SIZE(B_transposed, 1)
    !        WRITE(*,*) i, B_transposed(i,:)
    !END DO

    WRITE(*,*) "Rows: ", size(A,1), "Cols: ", size(A,2)



!CONTAINS 
!
!    SUBROUTINE gaussian_elimination(A)
!        
!

END PROGRAM main
