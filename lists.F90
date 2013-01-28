!======================================================================
!
!          Copyright Jiri Furst 2013
! Distributed under the Boost Software License, Version 1.0.
!    (See accompanying file LICENSE_1_0.txt or copy at
!          http://www.boost.org/LICENSE_1_0.txt)
!
!----------------------------------------------------------------------
!
!  MODULE: lists
!
!> @author Jiri Furst
!
!  DESCRIPTION: 
!> The module defines single linked list of generic objects
!
!======================================================================

module lists

  use objects
  use iterators
  use collections

  private

  type, extends(object) :: list_item
     class(list_item), pointer :: next => null()
     class(*), allocatable     :: val
  end type list_item

  !> The single linked list of generic objects
  !!
  type, public, extends(collection) :: list
     private
     integer :: size_ = 0
     type(list_item)  :: first_ptr    !< pointer to first item 
     type(list_item)  :: last_ptr     !< pointer to last item
   contains
     procedure, public :: size      => lst_size
     procedure, public :: add       => lst_add
     procedure, public :: iterator  => lst_iterator
  end type list

  type, public, extends(iterator) :: list_iterator
     private
     class(list), pointer      :: parent    => null()
     class(list_item), pointer :: item => null()
     class(list_item), pointer :: last => null()
   contains
     procedure, public :: has_next => li_has_next
     procedure, public :: next     => li_next
     procedure, public :: remove   => li_remove
  end type list_iterator

  public :: lists_test

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
    class(iterator), allocatable    :: lst_iterator
    allocate(list_iterator :: lst_iterator)
    select type(lst_iterator)
    type is (list_iterator)
       lst_iterator%parent => self
       lst_iterator%item   => self%first_ptr
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

  
  !======================================================================
  ! TEST
  !======================================================================
  subroutine lists_test
    print *, "Homogeneous iterator tests:"
    
    call integer_list_test()
    
    print *
    
  contains
    
    subroutine integer_list_test()
      type(list) :: l
      class(iterator), allocatable :: iter
      integer :: i
      class(*), pointer :: o

      print *, " - checking homogeneous integer list"
      if (l%size() /= 0) stop "empty list size /= 0!"

      call l%add(1)
      call l%add(2)
      call l%add(3)
      if (l%size() /= 3) stop "short list size /= 3!"

      call check_array(l%iterator(), [1, 2, 3] )
      
      call l%clear()
      if (l%size() /= 0) stop "empty list size /= 0!"

      call l%add(2)
      call l%add(3)
      call l%add(4)
      call check_array(l%iterator(), [2, 3, 4] )
      
      allocate(iter, source=l%iterator())
      o=>iter%next()  ! Points to 2
      o=>iter%next()  ! Points to 4
      call iter%remove()
      if (l%size() /= 2) stop "short list size /= 2!"
      deallocate(iter)

      call check_array(l%iterator(), [2, 4] )

      
      call l%clear()
      
    end subroutine integer_list_test
    
    subroutine check_array(iter, array)
      class(iterator) :: iter
      integer, intent(in) :: array(:)
      class(*), pointer :: o
      integer :: i
      i = 0
      do while (iter%has_next())
         o => iter%next()
         i = i + 1
         select type(o)
         type is (integer)
            if (o /= array(i)) stop "wrong data in the list"
         class default
            stop "wrong data type in the list"
         end select
      end do
    end subroutine check_array

  end subroutine lists_test

end module lists

