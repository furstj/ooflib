!======================================================================
!
!          Copyright Jiri Furst 2013
! Distributed under the Boost Software License, Version 1.0.
!    (See accompanying file LICENSE_1_0.txt or copy at
!          http://www.boost.org/LICENSE_1_0.txt)
!
!----------------------------------------------------------------------
!
!  MODULE: array_iterators
!
!> @author Jiri Furst
!
!  DESCRIPTION: 
!> The module defines iterators for rank-1 arrays
!
!======================================================================

module array_iterators

  use iterators

  private
  
  !> The iterator over an array
  !> @extends iterators::iterator
   
  type, public, extends(iterator_base) :: array_iterator
     private
     integer           :: idx
     class(*), pointer :: array(:) => null()
   contains
     procedure :: has_next => ai_has_next
     procedure :: next => ai_next
  end type array_iterator


  public :: forward_array_iterator
  public :: reverse_array_iterator
  public :: array_iterators_test

contains

  !> Creates an iterator going an array in ascending index order
  !!
  !! \param [in] array    - the rank-1 array
  !! \result the iterator
  !
  function forward_array_iterator(array) result(it)
    class(*), target     :: array(:)
    type(iterator)       :: it
    allocate(array_iterator :: it%pimpl)
    select type(i => it%pimpl)
    type is (array_iterator)
       i%array => array(:)
       i%idx = lbound(i%array,1)
    end select
  end function forward_array_iterator

  !> Creates an iterator going an array in descending index order
  !!
  !! \param [in] array    - the rank-1 array
  !! \result the iterator
  !
  function reverse_array_iterator(array) result(it)
    class(*), target     :: array(:)
    type(iterator)       :: it
    allocate(array_iterator :: it%pimpl)
    select type(i => it%pimpl)
    type is (array_iterator)
       i%array => array(::-1)
       i%idx = lbound(i%array,1)
    end select
  end function reverse_array_iterator

  function ai_has_next(self)
    class(array_iterator), intent(in) :: self
    logical                           :: ai_has_next
    ai_has_next = self%idx <= ubound(self%array,1)
  end function ai_has_next

  function ai_next(self)
    class(array_iterator), intent(inout) :: self
    class(*), pointer                    :: ai_next
    ai_next => self%array(self%idx)
    self%idx = self%idx + 1
  end function ai_next


  !======================================================================
  ! test
  !
  !> Simple test for array iterators
  !
  subroutine array_iterators_test
    real :: x(-1:8) = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
    integer :: i(5)  = [ 4, 5, 6, 7, 8 ]
 
    print *, "Array iterator tests:"
    call forward_iterator_real(forward_array_iterator(x), x)
    call forward_iterator_int(forward_array_iterator(i), i)

    call reverse_iterator_real(reverse_array_iterator(x), x)

    print *

  contains
    
    subroutine forward_iterator_real(it, z)
      class(iterator)   :: it
      real              :: z(:)
      class(*), pointer :: v
      integer           :: i
      print *, " - checking forward iterator (real)"
      i = lbound(z,1)-1
      do while(it%has_next())
         v => it%next()
         i = i + 1
         select type(v)
         type is (real)
            if (v/=z(i)) stop "error in forward_iterator"
         class default
            stop "UNKNOWN CLASS in forward_iterator"
         end select
      end do
    end subroutine forward_iterator_real

    
    subroutine forward_iterator_int(it, z)
      class(iterator)   :: it
      integer           :: z(:)
      class(*), pointer :: v
      integer           :: i
      print *, " - checking forward iterator (integer)"
      i = lbound(z,1)-1
      do while(it%has_next())
         v => it%next()
         i = i + 1
         select type(v)
         type is (integer)
            if (v/=z(i)) stop "error in forward_iterator"
         class default
            stop "UNKNOWN CLASS in forward_iterator"
         end select
      end do
    end subroutine forward_iterator_int

    subroutine reverse_iterator_real(it, z)
      class(iterator)   :: it
      real              :: z(:)
      class(*), pointer :: v
      integer           :: i
      print *, " - checking reverse iterator (real)"
      i = ubound(z,1)+1
      do while(it%has_next())
         v => it%next()
         i = i - 1
         select type(v)
         type is (real)
            if (v/=z(i)) stop "error in reverse_iterator"
         class default
            stop "UNKNOWN CLASS in reverse_iterator"
         end select
      end do
    end subroutine reverse_iterator_real
    
  end subroutine array_iterators_test

end module array_iterators

