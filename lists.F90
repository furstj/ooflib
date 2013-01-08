module lists

  use iterators

  private

  type :: list_item
     class(*), allocatable    :: value
     class(list_item), pointer :: next     => null()
     class(list_item), pointer :: previous => null()
  end type list_item

  type, public :: list
     private
     integer :: list_size = 0
     class(list_item), pointer :: first_item => null()
     class(list_item), pointer :: last_item  => null()
   contains
     procedure, public :: size      => lst_size
     procedure, public :: is_empty  => lst_is_empty
     procedure, public :: add       => lst_add
     procedure, public :: iterator  => lst_iterator
     procedure, public :: clear     => lst_clear
  end type list

  type, public, extends(iterator) :: list_iterator
     private
     class(list), pointer      :: parent    => null()
     class(list_item), pointer :: next_item => null()
     class(list_item), pointer :: this_item => null()
   contains
     procedure, public :: has_next => li_has_next
     procedure, public :: next     => li_next
     procedure, public :: remove   => li_remove
  end type list_iterator

  interface size
     module procedure lst_size
  end interface size
  public :: size

contains

  integer function lst_size(self)
    class(list), intent(in) :: self
    lst_size = self%list_size
  end function lst_size

  logical function lst_is_empty(self)
    class(list), intent(in) :: self
    lst_is_empty = (self%list_size == 0)
  end function lst_is_empty

  subroutine lst_add(self, val)
    class(list), intent(inout) :: self
    class(*), intent(in)       :: val
    type(list_item), pointer   :: item

    allocate(item)
    allocate(item%value, source=val)

    if ( self%is_empty() ) then
       self%first_item => item
    else
       item%previous => self%last_item
       self%last_item%next => item
    end if
    self%last_item => item
    self%list_size = self%list_size + 1 
  end subroutine lst_add

  function lst_iterator(self)
    class(list), intent(in), target :: self
    type(list_iterator)     :: lst_iterator
    lst_iterator%parent => self
    lst_iterator%next_item => self%first_item
  end function lst_iterator

  subroutine lst_clear(self)
    class(list), intent(inout) :: self
    type(list_iterator)        :: it
    class(*), pointer          :: p
    it = self%iterator()
    do while (it%has_next())
       p => it%next()
       call it%remove()
    end do
  end subroutine lst_clear

  logical function li_has_next(self)
    class(list_iterator), intent(in) :: self
    li_has_next = associated(self%next_item) 
  end function li_has_next

  function li_next(self)
    class(list_iterator), intent(inout) :: self
    class(*), pointer                   :: li_next
    self%this_item => self%next_item
    self%next_item => self%this_item%next
    li_next => self%this_item%value
  end function li_next

  subroutine li_remove(self)
    class(list_iterator), intent(inout) :: self
    class(list_item), pointer :: item
    item => self%this_item
    if (associated(item%previous)) then
       item%previous%next => item%next
    else
       self%parent%first_item => item%next
    end if

    if (associated(item%next)) then
       item%next%previous => item%previous
    else
       self%parent%last_item => item%previous
    end if
    self%parent%list_size = self%parent%list_size - 1

    deallocate(item%value)
    deallocate(item)
    self%this_item=>null()
  end subroutine li_remove

end module lists

#ifdef _TEST

program lists_test

  use lists
  use iterators
  use iso_varying_string

  type(list) :: l
  type(list_iterator) :: li
  class(*), pointer   :: p
  type(varying_string):: str

  print *, "Empty list size = ", size(l)

  str = "Ahoj"
  call l%add(str)
  call l%add(42)
  call l%add(3.14)

  print *
  print *, "Full list size = ", size(l)
  call dump( l%iterator() )

  li = l%iterator()
  p => li%next()   ! p => "Ahoj"
  call li%remove()
  p => li%next()   ! p => "Ahoj"
  call li%remove()

  print *
  print *, "Short list size = ", size(l)
  call dump( l%iterator() )

!!$  call l%add( var_str("Kuku") )
!!$  call l%add(1)
!!$  call l%add( var_str("Baf") )
!!$  call l%add(2)

  print *
  print *, "Long list size = ", size(l)
  call dump( l%iterator() )

  call l%clear()
  print *
  print *, "Empty list size = ", size(l)
  call dump( l%iterator() )


contains

  subroutine dump(it)
    class(iterator)   :: it
    class(*), pointer :: v

    do while(it%has_next())
       v => it%next()

       select type(v)
       type is (real)
          print *, "real    =>", v
       type is (integer)
          print *, "integer =>", v
       type is (character(len=*))
          print *, "string  =>", v
       type is (varying_string)
          print *, "varying_string  =>", char(v)
          
       class default
          stop "UNKNOWN CLASS"
       end select
    end do
  end subroutine dump

end program lists_test

#endif
