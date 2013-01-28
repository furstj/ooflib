!======================================================================
!
!          Copyright Jiri Furst 2013
! Distributed under the Boost Software License, Version 1.0.
!    (See accompanying file LICENSE_1_0.txt or copy at
!          http://www.boost.org/LICENSE_1_0.txt)
!
!----------------------------------------------------------------------
!
!  MODULE: queues
!
!> @author Jiri Furst
!
!  DESCRIPTION: 
!> The module defines simple queue 
!
!======================================================================

module queues

  use objects
  use lists
  use iterators
  use collections

  private

  !> The simple queue
  !!
  type, public, extends(collection) :: queue
     type(list), private :: container

   contains
     procedure :: is_empty
     procedure :: size
     procedure :: push
     procedure :: add
     procedure :: top
     procedure :: pop
     procedure :: clear
     procedure :: iterator => q_iterator
  end type queue

  public :: queues_test

contains

  logical function is_empty(self)
    class(queue), intent(in) :: self
    is_empty = self%container%is_empty()
  end function is_empty

  integer function size(self)
    class(queue), intent(in) :: self
    size = self%container%size()
  end function size

  subroutine clear(self)
    class(queue), intent(inout) :: self
    call self%container%clear()
  end subroutine clear

  function q_iterator(self)
    class(queue), target, intent(in) :: self
    class(iterator), allocatable    :: q_iterator
    allocate(q_iterator, source = self%container%iterator())
  end function q_iterator

  subroutine push(self, o)
    class(queue), intent(inout) :: self
    class(*), intent(in) :: o
    call self%container%add(o)
  end subroutine push

  subroutine add(self, o)
    class(queue), intent(inout) :: self
    class(*), intent(in) :: o
    call self%container%add(o)
  end subroutine add

  function top(self)
    class(queue), intent(inout)  :: self
    class(*), pointer            :: top
    class(iterator), allocatable :: iter
    allocate(iter, source = self%container%iterator())
    top => iter%next()
  end function top

  subroutine pop(self)
    class(queue), intent(inout)  :: self
    class(iterator), allocatable :: iter
    class(*), pointer            :: top
    allocate(iter, source = self%container%iterator())
    top => iter%next()
    call iter%remove()
  end subroutine pop
  
  !======================================================================
  ! TEST
  !======================================================================
  subroutine queues_test
    print *, "Homogeneous queue tests:"
    
    call integer_queue_test()
    call generic_queue_test()
    
    print *
    
  contains
    
    subroutine integer_queue_test()
      type(queue) :: q
      integer :: i
      class(*), pointer :: o

      print *, " - checking integer queue"
      if (q%size() /= 0) stop "empty queue size /= 0!"

      call q%push(1)
      call q%push(2)
      call q%push(3)

      if (q%size() /= 3) stop "short queue size /= 3!"

      call check_array(q%iterator(), [1, 2, 3] )
      
      o => q%top()
      select type(o)
      type is (integer)
         if (o/=1) stop "Wrong element, q%top() /= 1"
      end select

      o => q%top()
      select type(o)
      type is (integer)
         if (o/=1) stop "Wrong element, q%top() /= 1"
      end select

      call q%pop()
      if (q%size() /= 2) stop "short queue size /= 2!"
      
      o => q%top()
      select type(o)
      type is (integer)
         if (o/=2) stop "Wrong element, q%top() /= 2"
      end select
      
      call q%clear()
      if (q%size() /= 0) stop "empty list size /= 0!"

    end subroutine integer_queue_test
    

    subroutine generic_queue_test
      type(queue) :: q
      class(*), pointer :: o

      print *, " - checking generic queue"

      call q%push("Hi")
      call q%push(42)
      call q%push(3.14d0)
      if (q%size() /= 3) stop "short queue size /= 3!"

      o=>q%top()
      select type(o)
      type is (character(len=*))
         if (o/="Hi") stop "Wrong element, q%top() /= 'Hi'"
      class default
         stop "wrong class in the queue!"
      end select
      call q%pop()

      o=>q%top()
      select type(o)
      type is (integer)
         if (o/=42) stop "Wrong element, q%top() /= 42"
      class default
         stop "wrong class in the queue!"
      end select
      call q%pop()

      o=>q%top()
      select type(o)
      type is (real(kind(0.d0)))
         if (o/=3.14d0) stop "Wrong element, q%top() /= 3.14"
      class default
         stop "wrong class in the queue!"
      end select
      call q%pop()

      
    end subroutine generic_queue_test

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

  end subroutine queues_test

end module queues

