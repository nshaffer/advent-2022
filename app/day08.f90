program day08
  implicit none

  call part1
  call part2

contains

  subroutine part1
    use, intrinsic :: iso_fortran_env, only: iostat_end

    integer :: u, iostat, nx, ny, x, y, num_visible
    integer, allocatable :: h(:,:)
    character(256) :: line


    open (newunit=u, file='app/day08-input.txt')
    nx = 0
    do
      read (u, '(a)', iostat=iostat) line
      if (iostat == iostat_end) then
        ny = len_trim(line)
        exit
      else
        nx = nx + 1
      end if
    end do
    allocate (h(nx,ny))
    rewind (u)
    do y = 1, ny
      read (u, '(*(i1))') h(:,y)
    end do
    close (u)

    num_visible = 2*(nx + ny - 2)

    do y = 2, ny-1
      do x = 2, nx-1
        if (all(h(x,y) > h(x,:y-1)) .or. & 
          all(h(x,y) > h(x,y+1:)) .or. &
          all(h(x,y) > h(:x-1,y)) .or. &
          all(h(x,y) > h(x+1:,y))) num_visible = num_visible + 1
      end do
    end do

    print *, num_visible    
  end subroutine part1


  subroutine part2
    use, intrinsic :: iso_fortran_env, only: iostat_end, int64

    integer :: u, iostat, nx, ny, x, y, i
    integer(int64) ::  score(4), max_score
    integer, allocatable :: h(:,:)
    character(256) :: line

    open (newunit=u, file='app/day08-input.txt')
    nx = 0
    do
      read (u, '(a)', iostat=iostat) line
      if (iostat == iostat_end) then
        ny = len_trim(line)
        exit
      else
        nx = nx + 1
      end if
    end do
    allocate (h(nx,ny))
    rewind (u)
    do y = 1, ny
      read (u, '(*(i1))') h(:,y)
    end do
    close (u)

    max_score = 0
    do y = 2, ny-1
      do x = 2, nx-1

        ! up
        score(1) = y - 1
        do i = y-1, 1, -1
          if (h(x,i) >= h(x,y)) then
            score(1) = y - i
            exit
          end if
        end do

        ! left
        score(2) = x - 1
        do i = x-1, 1, -1
          if (h(i,y) >= h(x,y)) then
            score(2) = x - i
            exit
          end if
        end do

        ! right
        score(3) = nx - x
        do i = x+1, nx
          if (h(i,y) >= h(x,y)) then
            score(3) = i - x
            exit
          end if
        end do

        ! down
        score(4) = ny - y 
        do i = y+1, ny
          if (h(x,i) >= h(x,y)) then
            score(4) = i - y
            exit
          end if
        end do

        max_score = max(product(score), max_score)
      end do
    end do

    print *, max_score
  end subroutine part2

end program day08
