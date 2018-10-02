program bogo
    implicit none
    real(8), dimension(:), allocatable :: array

    array = [1, 1, 0, 3, 7]   

    call bogo_sort(array)
contains

    logical function is_sorted(array) 
        real(8), dimension(:), intent(in) :: array
        integer                           :: i

         do i = 1, size(array)
            if (array(i+1) > array(i)) then
                is_sorted = .false.
            end if
         end do
    end function is_sorted

    subroutine bogo_sort(array)
        real(8), dimension(:), intent(inout) :: array
        integer                              :: i
        logical                              :: is_sorted

        do while (is_sorted(array) = .FALSE. )

            CALL shuffle(array)
                
        end do
    end subroutine bogo_sort
    
    subroutine shuffle(array)
        real(8), dimension(:), intent(inout) :: array
        integer                              :: i, randpos, temp
        real(8)                              :: r

        do i = size(array), 2, -1
            call random_number(r)
            randpos    = int(r * i) + 1
            temp       = array(randpos)
            array(randpos) = array(i)
            array(i)       = temp
        end do

    end subroutine shuffle
end program bogo
