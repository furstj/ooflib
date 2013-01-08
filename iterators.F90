module iterators

  private
  
  type, abstract, public :: iterator
   contains
     procedure(iterator_has_next), deferred :: has_next
     procedure(iterator_next), deferred     :: next
  end type iterator


  abstract interface
     function iterator_has_next(self)
       import iterator
       class(iterator), intent(in) :: self
       logical                     :: iterator_has_next
     end function iterator_has_next

     function iterator_next(self)
       import iterator
       class(iterator), intent(inout) :: self
       class(*), pointer              :: iterator_next
     end function iterator_next
  end interface


end module iterators
