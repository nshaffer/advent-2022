program day04
  implicit none

  call part1
  call part2

contains

  subroutine part1
    use, intrinsic :: iso_fortran_env, only: iostat_end
    
    integer :: u, iostat, i
    character(80) :: str(2)
    integer :: sec1(2), sec2(2), num_contained

    num_contained = 0
    open (newunit=u, file='app/day04-input.txt')
    do
      read (u, *, iostat=iostat) str
      if (iostat == iostat_end) exit
      
      i = index(str(1), '-')
      str(1)(i:i) = ' '
      read (str(1), *) sec1
      
      i = index(str(2), '-')
      str(2)(i:i) = ' '
      read (str(2), *) sec2

      if ( (sec1(1) <= sec2(1) .and. sec1(2) >= sec2(2)) &
        .or. (sec2(1) <= sec1(1) .and. sec2(2) >= sec1(2)) ) then
        num_contained = num_contained + 1
      end if
    end do
    close (u)
    print *, num_contained
  end subroutine part1

  subroutine part2
        use, intrinsic :: iso_fortran_env, only: iostat_end
    
    integer :: u, iostat, i
    character(80) :: str(2)
    integer :: sec1(2), sec2(2), num_overlapping

    num_overlapping = 0
    open (newunit=u, file='app/day04-input.txt')
    do
      read (u, *, iostat=iostat) str
      if (iostat == iostat_end) exit
      
      i = index(str(1), '-')
      str(1)(i:i) = ' '
      read (str(1), *) sec1
      
      i = index(str(2), '-')
      str(2)(i:i) = ' '
      read (str(2), *) sec2
      
      if (sec1(1) <= sec2(2) .and. sec2(1) <= sec1(2)) then
        num_overlapping = num_overlapping + 1
      end if
    end do
    close (u)
    print *, num_overlapping
  end subroutine part2

end program day04
