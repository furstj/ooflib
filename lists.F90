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
!> The module defines abstract listss
!
!======================================================================

module lists

  use iterators
  use collections

  private

  type :: list_item
     class(*), allocatable    :: value
     class(list_item), pointer :: next     => null()
     class(list_item), pointer :: previous => null()
  end type list_item

  type, public, extends(collection) :: list
     private
     integer :: size_ = 0
     class(list_item), pointer :: first_item => null()
     class(list_item), pointer :: last_item  => null()
   contains
     procedure, public :: size      => lst_size
     procedure, public :: add       => lst_add
     procedure, public :: iterator  => lst_iterator
     procedure, public :: remove    => lst_remove
  end type list

  type, public, extends(iterator) :: list_iterator
     private
     class(list), pointer      :: parent    => null()
     class(list_item), pointer :: next_item => null()
     class(list_item), pointer :: this_item => null()
   contains
     procedure, public :: has_next => li_has_next
     procedure, public :: next     => li_next
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
    allocate(item%value, source=o)

    if ( self%is_empty() ) then
       self%first_item => item
    else
       item%previous => self%last_item
       self%last_item%next => item
    end if
    self%last_item => item
    self%size_ = self%size_ + 1 
  end subroutine lst_add

  function lst_iterator(self)
    class(list), target, intent(in) :: self
    class(iterator), allocatable    :: lst_iterator
    allocate(list_iterator :: lst_iterator)
    select type(lst_iterator)
    type is (list_iterator)
       lst_iterator%parent => self
       lst_iterator%next_item => self%first_item
    end select
  end function lst_iterator

  subroutine lst_remove(self, iter)
    class(list), intent(inout)     :: self
    class(iterator), intent(inout) :: iter
    class(list_item), pointer :: item
    select type(iter)
    class is (list_iterator)
       item => iter%this_item
       if (associated(item%previous)) then
          item%previous%next => item%next
       else
          self%first_item => item%next
       end if
       
       if (associated(item%next)) then
          item%next%previous => item%previous
       else
          self%last_item => item%previous
       end if
       self%size_ = self%size_ - 1
       
       deallocate(item%value)
       deallocate(item)
       iter%this_item=>null()
    class default
       stop "inconsistency of iterator type in list::remove"
    end select
  end subroutine lst_remove



  !======================================================================
  ! list_iterator
  !======================================================================
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
      call l%remove(iter)
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

