program day01
  implicit none

  call part1
  call part2

contains

  subroutine part1
    use, intrinsic :: iso_fortran_env, only: iostat_end

    integer :: cals, total_cals, max_cals
    integer :: u, iostat

    iostat = 0
    cals = 0
    total_cals = 0
    max_cals = 0

    open (newunit=u, file='app/day01-input.txt', action='read')
    do while (iostat /= iostat_end)
       read (u, '(i8)', iostat=iostat) cals
       if (cals == 0 .or. iostat == iostat_end) then
          max_cals = max(max_cals, total_cals)
          total_cals = 0
          if (iostat == iostat_end) exit
       else
          total_cals = total_cals + cals
       end if
    end do
    close (u)
    print *,  max_cals
  end subroutine part1


  subroutine part2
    use, intrinsic :: iso_fortran_env, only: iostat_end

    integer :: cals, total_cals, max_cals(3)
    integer :: u, iostat, i

    iostat = 0
    cals = 0
    total_cals = 0
    max_cals = 0

    open (newunit=u, file='app/day01-input.txt', action='read')
    do while (iostat /= iostat_end)
       read (u, '(i8)', iostat=iostat) cals
       if (cals == 0 .or. iostat == iostat_end) then
          i = minloc(max_cals, 1)
          if (total_cals > max_cals(i)) max_cals(i) = total_cals
          total_cals = 0
          if (iostat == iostat_end) exit
       else
          total_cals = total_cals + cals
       end if
    end do
    close (u)
    print *,  sum(max_cals)
  end subroutine part2

end program day01
