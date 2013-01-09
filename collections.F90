!======================================================================
!
!          Copyright Jiri Furst 2013
! Distributed under the Boost Software License, Version 1.0.
!    (See accompanying file LICENSE_1_0.txt or copy at
!          http://www.boost.org/LICENSE_1_0.txt)
!
!----------------------------------------------------------------------
!
!  MODULE: collections
!
!> @author Jiri Furst
!
!  DESCRIPTION: 
!> The module defines abstract collections
!
!======================================================================

module collections

  use iterators

  implicit none

  private

  !======================================================================
  !> Abstract collection
  !!
  !! The root of collection hierarchy. A collection represents a group
  !! of elements and provides some basic operations with the elements and 
  !! collections
  !!
  type, public, abstract :: collection
     
   contains

     !> Returns the number of elements in this collection
     !> \result number of elements
     procedure(coll_size), deferred, public     :: size

     !> Test if the collection is empty
     !> \result `.true.` if the collection is empty
     procedure, public                          :: is_empty

     !> Adds an element to the collection
     procedure(coll_add), deferred, public      :: add

     !> Returns an iterator
     !> \result an iterator, `class(iterator), allocatable`
     !procedure(coll_iterator), deferred, public :: iterator
     procedure, public :: iterator => coll_iterator

     !> Removes an element given by the itereator
     procedure(coll_remove), deferred, public   :: remove 

     !> Clears the collections
     procedure, public :: clear => coll_clear
  end type collection


  abstract interface

     integer function coll_size(self)
       import collection
       class(collection), intent(in) :: self
     end function coll_size


     !> Interface for collection::add
     !> \param[in] o - the object to add
     subroutine coll_add(self, o)
       import collection
       class(collection), intent(inout) :: self
       class(*), intent(in)             :: o
     end subroutine coll_add


     !> Interface for collection::add
     !> \param[in] o - the object to add
     function coll_iterator(self)
       use iterators
       import collection
       class(collection), target, intent(in) :: self
       class(iterator), allocatable          :: coll_iterator
     end function coll_iterator

     
     subroutine coll_remove(self, iter)
       use iterators
       import collection
       class(collection), intent(inout) :: self
       class(iterator), intent(inout)   :: iter
     end subroutine coll_remove

  end interface



contains

  logical function is_empty(self)
    class(collection), intent(in) :: self
    is_empty = (self%size() == 0)
  end function is_empty

  subroutine coll_clear(self)
    class(collection), intent(inout) :: self
    class(iterator), allocatable     :: iter
    class(*), pointer                :: o
    allocate(iter, source=self%iterator())
    do while (iter%has_next())
       o => iter%next()
       call self%remove(iter)
    end do
  end subroutine coll_clear

end module collections
