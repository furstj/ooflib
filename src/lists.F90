module lists

  !! author: Jiri Furst
  !! version: v0.2
  !! license: Boost Software License, Version 1.0.
  !!
  !! The module defines a generic list
  !!
  !! The basic usage:
  !!    
  !!    type(list) :: l
  !!    
  !!    call l%add(42)    
  !!    call l%add("Hello")
  !!    

  use objects
  use iterators
  use collections

  private


  type, extends(object) :: list_item
     class(list_item), pointer :: next => null()
     class(*), allocatable     :: val
  end type list_item


  type, public, extends(collection) :: list
     !! The single linked list of generic objects 
     private
     integer :: size_ = 0
     type(list_item)  :: first_ptr    !! pointer to first item 
     type(list_item)  :: last_ptr     !! pointer to last item
   contains
     procedure, public :: size      => lst_size      !! returns number of elements in the list
     procedure, public :: add       => lst_add       !! adds an element to the end 
     procedure, public :: iterator  => lst_iterator  !! returns an iterator 
  end type list

  type, extends(iterator_base) :: list_iterator
     private
     class(list), pointer      :: parent    => null()
     class(list_item), pointer :: item => null()
     class(list_item), pointer :: last => null()
   contains
     procedure, public :: has_next => li_has_next
     procedure, public :: next     => li_next
     procedure, public :: remove   => li_remove
  end type list_iterator


contains

  integer function lst_size(self)
    class(list), intent(in) :: self
    lst_size = self%size_
  end function lst_size


  subroutine lst_add(self, o)
    class(list), intent(inout) :: self
    class(*), intent(in)       :: o
    type(list_item), pointer   :: item

    allocate(item)
    allocate(item%val, source=o)

    if ( self%is_empty() ) then
       self%first_ptr%next => item
    else
       self%last_ptr%next%next => item
    end if
    self%last_ptr%next => item
    self%size_ = self%size_ + 1 
  end subroutine lst_add


  function lst_iterator(self)
    class(list), target, intent(in) :: self
    type(iterator)                  :: lst_iterator
    allocate(list_iterator :: lst_iterator%pimpl)
    select type(it=>lst_iterator%pimpl)
    type is (list_iterator)
       it%parent => self
       it%item   => self%first_ptr
    end select
  end function lst_iterator


  !======================================================================
  ! list_iterator
  !======================================================================
  logical function li_has_next(self)
    class(list_iterator), intent(in) :: self
    li_has_next = associated(self%item%next) 
  end function li_has_next
 

  function li_next(self)
    class(list_iterator), intent(inout) :: self
    class(*), pointer                   :: li_next
    self%last => self%item
    self%item => self%item%next
    li_next => self%item%val
  end function li_next


  subroutine li_remove(self)
    class(list_iterator), intent(inout) :: self
    class(list_item), pointer :: tmp
    if (.not. associated(self%last)) stop "Error in list_iterator::remove"
    self%last%next => self%item%next
    self%parent%size_ = self%parent%size_ - 1
    tmp => self%item
    self%item => self%last
    deallocate(tmp)
  end subroutine li_remove

  
end module lists

