program day05
  use cstack_type_mod
  implicit none

  call part1
  call part2

contains

  subroutine part1
    use, intrinsic :: iso_fortran_env, only: iostat_end

    ! since each crate is one letter, is bracketed with [], and has
    ! trailing space --> 4 characters per crate, with the letter in
    ! 2nd position
    integer, parameter :: width = 4, offset = 2
    
    integer :: u, max_len, stack_height, i, j, k, iostat, from, to
    character(80) :: line
    character, allocatable :: diagram(:,:)
    character(:), allocatable :: message
    type(cstack_type), allocatable :: stacks(:)

    max_len = 0
    stack_height = 0
    open (newunit=u, file='app/day05-input.txt')
    do
      read (u, '(a)') line
      if (index(line, '[') == 0) then
        ! end of the stack diagram
        exit
      else
        max_len = max(max_len, len_trim(line))
        stack_height = stack_height + 1
      end if
    end do

    allocate (diagram(stack_height, max_len))
    rewind (u)
    do i = 1, stack_height
      read (u, '(*(a1))') diagram(i,:)
    end do
      
    allocate (stacks((max_len+1)/width))
    do i = 1, size(stacks)
      call init(stacks(i), 10*stack_height) ! 10x is just a guess
      j = offset + width*(i-1)
      do k = stack_height, 1, -1
        if (diagram(k,j) == ' ') exit
        call push(stacks(i), diagram(k,j))
      end do
    end do

    read (u, *) line ! blank line

    do
      read (u, *, iostat=iostat) line(1:4), i, line(5:8), from, line(9:10), to
      if (iostat == iostat_end) exit
      do j = 1, i
        associate (p => pop(stacks(from)))
          call push(stacks(to), p)
        end associate
      end do
    end do

    close (u)

    message = ''
    do i = 1, size(stacks)
      message = message // pop(stacks(i))
      call free(stacks(i))
    end do
    print *, message
  end subroutine part1

  
  subroutine part2
    use, intrinsic :: iso_fortran_env, only: iostat_end

    ! since each crate is one letter, is bracketed with [], and has
    ! trailing space --> 4 characters per crate, with the letter in
    ! 2nd position
    integer, parameter :: width = 4, offset = 2
    
    integer :: u, max_len, stack_height, i, j, k, iostat, from, to
    character(80) :: line
    character, allocatable :: diagram(:,:)
    character(:), allocatable :: message, p
    type(cstack_type), allocatable :: stacks(:)


    max_len = 0
    stack_height = 0
    open (newunit=u, file='app/day05-input.txt')
    do
      read (u, '(a)') line
      if (index(line, '[') == 0) then
        ! end of the stack diagram
        exit
      else
        max_len = max(max_len, len_trim(line))
        stack_height = stack_height + 1
      end if
    end do

    allocate (diagram(stack_height, max_len))
    rewind (u)
    do i = 1, stack_height
      read (u, '(*(a1))') diagram(i,:)
    end do
      
    allocate (stacks((max_len+1)/width))
    do i = 1, size(stacks)
      call init(stacks(i), 10*stack_height) ! 10x is just a guess
      j = offset + width*(i-1)
      do k = stack_height, 1, -1
        if (diagram(k,j) == ' ') exit
        call push(stacks(i), diagram(k,j))
      end do
    end do

    read (u, *) line ! blank line

    do
      read (u, *, iostat=iostat) line(1:4), i, line(5:8), from, line(9:10), to
      if (iostat == iostat_end) exit
      p = ''
      do j = 1, i
        p = p // pop(stacks(from))
      end do
      do j = i, 1, -1
        call push(stacks(to), p(j:j))
      end do
    end do

    close (u)

    message = ''
    do i = 1, size(stacks)
      message = message // pop(stacks(i))
      call free(stacks(i))
    end do
    print *, message
  end subroutine part2


end program day05
  
