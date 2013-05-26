!======================================================================
!
!          Copyright Jiri Furst 2013
! Distributed under the Boost Software License, Version 1.0.
!    (See accompanying file LICENSE_1_0.txt or copy at
!          http://www.boost.org/LICENSE_1_0.txt)
!
!----------------------------------------------------------------------
!
!  MODULE: objects
!
!> @author Jiri Furst
!
!  DESCRIPTION: 
!> The module defines class object which is a root of the class hierarchy
!
!======================================================================
module objects

  private


  !> The root of class hierarchy
  type, public :: object
     
   contains
     
     !> indicates whether an object is equall to this one
     !!
     !! @param[in] o    the other object
     !! @returns   true if both objects equals
     procedure :: equals

  end type object

  interface operator(==)
     module procedure equals
     module procedure equals_reverse
  end interface operator(==)
  public :: operator(==)


contains
  
  !> Default equals method
  !!
  !! @return false
  logical function equals(self, o)
    class(object), intent(in) :: self      !< this object
    class(*), intent(in)      :: o         !< other object
    equals = false
  end function equals

  !> Reversed equals method
  !!
  !! @return false
  logical function equals_reverse(o, self)
    class(object), intent(in) :: self      !< this object
    class(*), intent(in)      :: o         !< other object
    equals_reverse = equals(self, o)
  end function equals_reverse

end module objects
