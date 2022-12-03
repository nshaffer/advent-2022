program day03
  use, intrinsic :: iso_fortran_env, only: iostat_end
  implicit none

  call part1
  call part2

contains

  subroutine part1
    character(80) :: line
    character(:), allocatable :: first, second
    character(*), parameter :: abc = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'

    integer :: u, iostat, n, i, priority

    priority = 0
    open (newunit=u, file='app/day03-input.txt')
    iostat = 0
    do
      read (u, '(a)', iostat=iostat) line
      if (iostat == iostat_end) exit
      n = len_trim(line)/2
      first = line(1:n)
      second = line(n+1:2*n)

      i = scan(first, second)
      priority = priority + index(abc, first(i:i))
    end do
    close (u)
    print *, priority
  end subroutine part1

  subroutine part2
    character(80) :: line
    character(:), allocatable :: first, second, third
    character(*), parameter :: abc = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'

    integer :: u, iostat, n, i, priority

    priority = 0
    open (newunit=u, file='app/day03-input.txt')
    iostat = 0
    do
       
      read (u, '(a)', iostat=iostat) line
      if (iostat == iostat_end) exit
      first = trim(line)

      read (u, '(a)', iostat=iostat) line
      if (iostat == iostat_end) exit
      second = trim(line)

      read (u, '(a)', iostat=iostat) line
      if (iostat == iostat_end) exit
      third = trim(line)
      
      do i = 1, len(first)
        if (index(second, first(i:i)) > 0) then
          if (index(third, first(i:i)) > 0) then
            priority = priority + index(abc, first(i:i))
            exit
          end if
        end if
      end do
 
    end do
    close (u)
    print *, priority    
  end subroutine part2
  
end program day03
