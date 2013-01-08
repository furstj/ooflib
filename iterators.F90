!======================================================================
!
!          Copyright Jiri Furst 2013
! Distributed under the Boost Software License, Version 1.0.
!    (See accompanying file LICENSE_1_0.txt or copy at
!          http://www.boost.org/LICENSE_1_0.txt)
!
!----------------------------------------------------------------------
!
!  MODULE: iterators
!
!> @author Jiri Furst
!
!  DESCRIPTION: 
!> The module defines an abstract class iterator
!
!======================================================================
 
module iterators

  private
  
  !======================================================================
  !> Abstract iterator
  !!
  !! The class iterator is an abstract iterator designed in the style of 
  !! Java Iterator.
  !!
  !! The iterator should be able to iterate over an collection using
  !! following idiom:
  !! 
  !! @code{.f90}
  !!
  !! class(iterator), allocatable :: iter
  !! class(*), pointer :: d
  !! allocate(iter, source=collection%iterator())
  !!
  !! do while (iter%has_next())
  !!   d => iter%next()
  !!   operate on d      
  !! end do
  !!
  !! @endcode
  type, abstract, public :: iterator
   contains
     !> Returns `.true.` if the iterator has more elements.
     !> @return `.true.` if the iterator has more elements.
     procedure(iterator_has_next), deferred :: has_next

     !> Returns the pointer to next element
     !> @return the pointer to next element
     procedure(iterator_next), deferred     :: next

  end type iterator


  abstract interface
     !> \interface iterator_has_next
     function iterator_has_next(self)
       import iterator
       class(iterator), intent(in) :: self
       logical                     :: iterator_has_next
     end function iterator_has_next

     !> \interface iterator_next
     function iterator_next(self)
       import iterator
       class(iterator), intent(inout) :: self
       class(*), pointer              :: iterator_next
     end function iterator_next
  end interface


end module iterators
