program day07

  call part1
  call part2

contains

  subroutine part1
    use, intrinsic :: iso_fortran_env, only: int64, iostat_end
    implicit none

    integer, parameter :: maxlen = 80
    integer :: u, iostat, cwd, p
    integer(int64) :: file_size
    integer(int64), allocatable :: sizes(:)
    character(maxlen), allocatable :: dirnames(:)
    integer, allocatable :: parents(:)
    character(maxlen) :: line

    cwd = 0
    allocate (parents(0))
    allocate (sizes(0))
    allocate (dirnames(0))

    open (newunit=u, file='app/day07-input.txt')
    main: do
      read (u, '(a)', iostat=iostat) line
      if (iostat == iostat_end) exit main
      if (line(1:4) == '$ cd') then
        if (line(6:7) == '..') then ! pop up
          cwd = parents(cwd)
        else ! descend to new directory
          dirnames = [character(maxlen) :: dirnames, line(6:)]
          parents = [integer :: parents, cwd]
          sizes = [integer(int64) :: sizes, 0_int64]
          cwd = size(dirnames)
          read (u, '(a)') line ! the `ls` line
          contents: do
            read (u, '(a)', iostat=iostat) line
            if (iostat == iostat_end) exit main
            if (line(1:1) == '$') then ! end of current dir contents
              backspace (u)
              exit contents
            else if (line(1:3) /= 'dir') then ! regular file
              read (line, *) file_size
              sizes(cwd) = sizes(cwd) + file_size
            end if
          end do contents
        end if
      end if
    end do main
    close (u)

    do cwd = size(dirnames), 1, -1
      p = parents(cwd)
      if (p > 0) sizes(p) = sizes(p) + sizes(cwd)
    end do

    print *, sum(sizes, mask=sizes <= 100000)
  end subroutine part1


  subroutine part2
    use, intrinsic :: iso_fortran_env, only: int64, iostat_end
    implicit none

    integer, parameter :: maxlen = 80
    integer(int64), parameter :: avail = 70000000, goal = 30000000
    integer :: u, iostat, cwd, p
    integer(int64) :: file_size, unused
    integer(int64), allocatable :: sizes(:)
    character(maxlen), allocatable :: dirnames(:)
    integer, allocatable :: parents(:)
    character(maxlen) :: line

    cwd = 0
    allocate (parents(0))
    allocate (sizes(0))
    allocate (dirnames(0))

    open (newunit=u, file='app/day07-input.txt')
    main: do
      read (u, '(a)', iostat=iostat) line
      if (iostat == iostat_end) exit main
      if (line(1:4) == '$ cd') then
        if (line(6:7) == '..') then ! pop up
          cwd = parents(cwd)
        else ! descend to new directory
          dirnames = [character(maxlen) :: dirnames, line(6:)]
          parents = [integer :: parents, cwd]
          sizes = [integer(int64) :: sizes, 0_int64]
          cwd = size(dirnames)
          read (u, '(a)') line ! the `ls` line
          contents: do
            read (u, '(a)', iostat=iostat) line
            if (iostat == iostat_end) exit main
            if (line(1:1) == '$') then ! end of current dir contents
              backspace (u)
              exit contents
            else if (line(1:3) /= 'dir') then ! regular file
              read (line, *) file_size
              sizes(cwd) = sizes(cwd) + file_size
            end if
          end do contents
        end if
      end if
    end do main
    close (u)

    do cwd = size(dirnames), 1, -1
      p = parents(cwd)
      if (p > 0) sizes(p) = sizes(p) + sizes(cwd)
    end do

    unused = avail - sizes(1)
    print *, minval(sizes, mask=unused + sizes >= goal)
  end subroutine part2


end program day07
