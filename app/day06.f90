program day06
  implicit none

  call part1
  call part2

contains

  subroutine part1
    integer :: u, iostat, i, n
    character :: c
    character(4) :: buf
    logical :: has_dup


    buf = ''
    open (newunit=u, file='app/day06-input.txt')
    read (u, '(4a)', advance='no') buf
    n = len(buf)
    has_dup = .false.
    do i = 1, len(buf)
      if (index(buf, buf(i:i)) /= i) then
        has_dup = .true.
        exit
      end if
    end do
    if (.not. has_dup) then
      print *, n
      close (u)
      return
    end if

    do 
      read (u, '(a)', advance='no', iostat=iostat) c
      if (iostat /= 0) exit
      n = n + 1
      buf(1:len(buf)-1) = buf(2:len(buf))
      buf(len(buf):len(buf)) = c
      has_dup = .false.
      do i = 1, len(buf)
        if (index(buf, buf(i:i)) /= i) then
          has_dup = .true.
          exit
        end if
      end do
      if (.not. has_dup) then
        print *, n
        exit
      end if
    end do
    close (u)
  end subroutine part1

  subroutine part2
    integer :: u, iostat, i, n
    character :: c
    character(14) :: buf
    logical :: has_dup


    buf = ''
    open (newunit=u, file='app/day06-input.txt')
    read (u, '(4a)', advance='no') buf
    n = len(buf)
    has_dup = .false.
    do i = 1, len(buf)
      if (index(buf, buf(i:i)) /= i) then
        has_dup = .true.
        exit
      end if
    end do
    if (.not. has_dup) then
      print *, n
      close (u)
      return
    end if

    do 
      read (u, '(a)', advance='no', iostat=iostat) c
      if (iostat /= 0) exit
      n = n + 1
      buf(1:len(buf)-1) = buf(2:len(buf))
      buf(len(buf):len(buf)) = c
      has_dup = .false.
      do i = 1, len(buf)
        if (index(buf, buf(i:i)) /= i) then
          has_dup = .true.
          exit
        end if
      end do
      if (.not. has_dup) then
        print *, n
        exit
      end if
    end do
    close (u)
  end subroutine part2

end program day06
