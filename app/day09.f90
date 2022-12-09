program day09
  implicit none

  call part1
  call part2

contains

  subroutine part1
    use, intrinsic :: iso_fortran_env, only: iostat_end, int32, int64

    character :: direction
    integer(int32) :: distance
    integer(int32) :: head(2), tail(2), delta(2)
    integer(int64), allocatable :: visited(:)
    integer(int64) :: v
    integer :: u, iostat, i, j

    head = [0, 0]
    tail = [0, 0]
    visited = [transfer(tail, v)]

    open (newunit=u, file='app/day09-input.txt')
    do
      read (u, *, iostat=iostat) direction, distance
      if (iostat == iostat_end) exit
      select case (direction)
      case ('U')
        delta = [0, 1]
      case ('D')
        delta = [0, -1]
      case ('L')
        delta = [-1, 0]
     case ('R')
        delta = [1, 0]
      end select
      do i = 1, distance
        if (any(abs(head + delta - tail) > 1)) then
          tail = head
          v = transfer(tail, v)
          if (.not.any(visited == v)) visited = [visited, v]
        end if
        head = head + delta
      end do
    end do
    close (u)

    print *, size(visited)
  end subroutine part1


  subroutine part2
    use, intrinsic :: iso_fortran_env, only: iostat_end, int32, int64

    integer, parameter :: length = 10
    character :: direction
    integer(int32) :: distance
    integer(int32) :: rope(length,2), delta(2), diff(2)
    integer(int64), allocatable :: visited(:)
    integer(int64) :: v
    integer :: u, iostat, i, j

    rope = 0
    visited = [transfer(rope(length,:), v)]

    open (newunit=u, file='app/day09-input.txt')
    do
      read (u, *, iostat=iostat) direction, distance
      if (iostat == iostat_end) exit
      
      select case (direction)
      case ('U')
        delta = [0, 1]
      case ('D')
        delta = [0, -1]
      case ('L')
        delta = [-1, 0]
      case ('R')
        delta = [1, 0]
      end select

      do i = 1, distance
        rope(1,:) = rope(1,:) + delta
        do j = 2, length
          diff = rope(j-1,:) - rope(j,:)
          if (all(abs(diff) < 2)) exit
          where (abs(diff) == 2) diff = diff/2
          rope(j,:) = rope(j,:) + diff
        end do
        v = transfer(rope(length,:), v)
        if (.not.any(visited == v)) visited = [visited, v]
      end do

    end do
    close (u)

    print *, size(visited)
  end subroutine part2


end program day09
