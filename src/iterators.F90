module iterators

  !! author: Jiri Furst
  !! version: v0.2
  !! license: Boost Software License, Version 1.0.
  !!
  !! The module defines an abstract class iterator and its PIMPL class
  !!
  !! The iterator approximates Java iterators. The basic usage:
  !!    
  !!    subroutine traverse_collection(c)
  !!    class(collection) :: c
  !!    type(iterator)    :: it
  !!    class(*), pointer :: d
  !!    
  !!    it = c%iterator()
  !!    while (it%has_next())
  !!      d => it%next()
  !!      call do_the_job_with(d)
  !!    end while
  !!

  use objects

  private


  type, abstract, extends(object), public :: iterator_base
     !! This class is intended as a base for concrete iterators
   contains
     procedure(ibase_has_next), deferred :: has_next               !! Returns if collection has next element
     procedure(ibase_next), deferred     :: next                   !! Returns pointer to next element
     procedure                           :: remove => ibase_remove !! Remove this element from collection
  end type iterator_base


  type, public, extends(iterator_base) :: iterator
     class(iterator_base), allocatable :: pimpl                    !! Pointer to concrete iterator
   contains
     procedure, non_overridable :: has_next                        !! Returns if collection has next element
     procedure, non_overridable :: next                            !! Returns pointer to next element
     procedure, non_overridable :: remove                          !! Remove this element from collection
  end type iterator


  abstract interface
     function ibase_next(self)
       import iterator_base
       class(iterator_base), intent(inout) :: self
       class(*), pointer                   :: ibase_next
     end function ibase_next

     function ibase_has_next(self)
       import iterator_base
       class(iterator_base), intent(in) :: self
       logical                          :: ibase_has_next
     end function ibase_has_next
  end interface

contains

  subroutine ibase_remove(self)
    class(iterator_base), intent(inout) :: self
    stop "iterator:remove has to be overridden"
  end subroutine ibase_remove


  logical function has_next(self)
    class(iterator), intent(in) :: self
    has_next = self%pimpl%has_next()
  end function has_next


  function next(self)
    class(iterator), intent(inout) :: self
    class(*), pointer              :: next
    next => self%pimpl%next()
  end function next


  subroutine remove(self)
    class(iterator), intent(inout) :: self
    call self%pimpl%remove()
  end subroutine remove

end module iterators
