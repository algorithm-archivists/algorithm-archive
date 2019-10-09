PROGRAM main

    IMPLICIT NONE
    INTEGER                  :: i 
    REAL(8), DIMENSION(3, 4) :: A !, B_transposed
    !REAL(8), DIMENSION(4, 3) :: B

    !DATA B / 2d0, 3d0, 4d0, &
    !6d0, 1d0, 2d0, &
    !3d0, 4d0, 3d0, &
    !-4d0, 0d0, 10d0 /

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

    DO i = 1, SIZE(A, 1)
            WRITE(*,*) i, A(i,:)
    END DO

    ! Alternatively one can use the old DATA instruction which assigns
    ! the array elements the data given in the same column-first
    ! principle. The matrix must be transposed as well but by using an
    ! additional declared array with the proper shape.

    !B_transposed = transpose(B)

    !DO i = 1, SIZE(B_transposed, 1)
    !        WRITE(*,*) i, B_transposed(i,:)
    !END DO

    CALL gaussian_elimination(A)

CONTAINS 

    SUBROUTINE gaussian_elimination(A)
        REAL(8), DIMENSION(3,4) :: A
        REAL(8), DIMENSION(4)   :: temp_vector
        REAL(8)                 :: frac 
        INTEGER                 :: cols = size(A,2), rows = size(A,1), &
                                   row = 1, col = 1, max_index, i, j
        !LOGICAL                 :: singular = .false. 


        ! Main loop going through all columns

        DO col = 1, cols-1

            ! find the element with the highest value

            max_index = maxloc(abs(A(:,col)),1)
            !WRITE(*,*) "max_index: ", max_index, "max_col_value: ", A(max_index, col) 
            
            ! Check if matrix is singular

            IF (A(max_index, col) == 0) THEN
                WRITE(*,*) "Matrix is singular!"
                CONTINUE
            END IF

            ! Swap row with highest value for that column to the top

            temp_vector = A(max_index, :)
            A(max_index, :) = A(row, :)
            A(row, :) = temp_vector

            DO i = 1, SIZE(A, 1)
                WRITE(*,*) i, A(i,:)
            END DO

            DO i = (row + 1), rows
            
                ! finding fraction
                frac = A(i, col)/A(row, col)
            END DO

            ! loop through the colum elements of that row

            DO j = (col + 1), cols
                ! calculate all differences with the fractions row-elements
                A(i, j) = A(i, j) - A(row, j)*frac
            END DO

            ! Set lower elements to 0

            A(i, col) = 0

        END DO
        
        row = row + 1

    END SUBROUTINE
END PROGRAM main
