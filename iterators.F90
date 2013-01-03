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


  type, public, extends(iterator) :: array_iterator
     private
     integer           :: idx
     class(*), pointer :: array(:) => null()
   contains
     procedure :: has_next => ai_has_next
     procedure :: next => ai_next
  end type array_iterator

  interface create_iterator
     module procedure ai_create
  end interface create_iterator
  public :: create_iterator

contains

  function ai_create(array) result(it)
    class(*), target     :: array(:)
    type(array_iterator) :: it

    it%array => array
    it%idx = lbound(it%array,1)
  end function ai_create

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

  
end module iterators

#ifdef _TEST

program iterators_test

  use iterators

  real :: x(-1:8) = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
  
  call dump(create_iterator(x))

contains

  subroutine dump(it)
    class(iterator)   :: it
    class(*), pointer :: v

    do while(it%has_next())
       v => it%next()

       select type(v)
       type is (real)
          print *, "=>", v
       class default
          stop "UNKNOWN CLASS"
       end select
    end do
  end subroutine dump

end program iterators_test

#endif
