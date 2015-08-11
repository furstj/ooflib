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
!  @author Jiri Furst
!
!  DESCRIPTION: 
!  
!
!======================================================================
module objects

  !! author: Jiri Furst
  !!
  !! The module defines class object which is a root of the class hierarchy

  private

  type, public :: object
  !! The root of class hierarchy
     
   contains
     
     generic :: operator(==) => equals
     !! Compares with other object

     procedure :: equals
  end type object

contains
  

  logical function equals(self, o)
    !! Default equals method
    class(object), intent(in) :: self       !! this object
    class(*), intent(in)      :: o          !! other object
    equals = .false.
  end function equals


end module objects
