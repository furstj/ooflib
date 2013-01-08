module array_iterators

  use iterators

  private
  
  !> The iterator over an array
  !> @extends iterators::iterator
   
  type, public, extends(iterator) :: array_iterator
     private
     integer           :: idx
     class(*), pointer :: array(:) => null()
   contains
     procedure :: has_next => ai_has_next
     procedure :: next => ai_next
  end type array_iterator

  interface forward_array_iterator
     module procedure ai_create
  end interface forward_array_iterator

  interface reverse_array_iterator
     module procedure rai_create
  end interface reverse_array_iterator


  public :: array_iterators_test

contains

  function ai_create(array) result(it)
    class(*), target     :: array(:)
    type(array_iterator) :: it

    it%array => array(:)
    it%idx = lbound(it%array,1)
  end function ai_create

  function rai_create(array) result(it)
    class(*), target     :: array(:)
    type(array_iterator) :: it
    it%array => array(::-1)
    it%idx = lbound(it%array,1)
  end function rai_create

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
  !======================================================================
  
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

