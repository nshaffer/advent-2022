module cstack_type_mod
  implicit none

  type :: cstack_type
    private
    character, allocatable :: vals(:)
    integer :: top ! index of top value on the stack
  end type cstack_type

contains

  subroutine init(stack, capacity)
    type(cstack_type), intent(out) :: stack
    integer, intent(in) :: capacity

    allocate (stack%vals(capacity), source='.')
    stack%top = 0
  end subroutine init
  

  subroutine push(stack, val)
    type(cstack_type), intent(in out) :: stack
    character, intent(in) :: val

    if (stack%top >= size(stack%vals)) then
      error stop 'Cannot push onto a full stack.'
    end if
    
    stack%top = stack%top + 1
    stack%vals(stack%top) = val
  end subroutine push


  function pop(stack) result(val)
    type(cstack_type), intent(in out) :: stack
    character :: val

    if (stack%top <= 0) error stop 'Cannot pop an empty stack.'
    
    val = stack%vals(stack%top)
    stack%top = stack%top - 1
  end function pop


  subroutine free(stack)
    type(cstack_type), intent(in out) :: stack

    if (allocated(stack%vals)) deallocate (stack%vals)
    stack%top = 0
  end subroutine free


  subroutine disp(stack)
    type(cstack_type), intent(in) :: stack

    print *, '[',stack%vals(1:stack%top), ']'
  end subroutine disp
  
end module cstack_type_mod
