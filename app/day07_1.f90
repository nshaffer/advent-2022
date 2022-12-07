program day07_1
  use, intrinsic :: iso_fortran_env, only: int64, iostat_end
  implicit none

  
  integer :: u, iostat, cwd
  integer(int64) :: file_size
  integer(int64), allocatable :: sizes(:)
  character(80), allocatable :: dirnames(:)
  integer, allocatable :: parents(:)
  character(80) :: line

  cwd = 0
  allocate (parents(0))
  allocate (sizes(0))
  allocate (dirnames(0))
  
  open (newunit=u, file='app/day07-input.txt')
  main: do
    read (u, '(a)', iostat=iostat) line
    if (iostat == iostat_end) exit main
    print *, line
    if (line(1:4) == '$ cd') then
      if (line(6:7) == '..') then ! pop up
        cwd = parents(cwd)
      else ! descend to new directory
        dirnames = [character(80) :: dirnames, line(6:)]
        parents = [integer :: parents, cwd]
        sizes = [integer(int64) :: sizes, 0_int64]
        cwd = size(dirnames)
        read (u, '(a)') line ! the `ls` line
        print *, line
        contents: do
          read (u, '(a)', iostat=iostat) line
          if (iostat == iostat_end) exit main
          print *, line
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
!!TODO: understand why my root dir has a bogus size
  
  do cwd = 1, size(dirnames)
    print *, trim(dirnames(cwd)), parents(cwd), sizes(cwd)
  end do
end program day07_1
