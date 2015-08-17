subroutine lists_test
  
    use lists
    use iterators

    print *, "Homogeneous iterator tests:"
    
    call integer_list_test()
    
    print *
    
  contains
    
    subroutine integer_list_test()
      type(list) :: l
      type(iterator) :: iter
      integer :: i
      class(*), pointer :: o

      print *, " - checking homogeneous integer list"
      if (l%size() /= 0) stop "empty list size /= 0!"

      call l%add(1)
      call l%add(2)
      call l%add(3)
      if (l%size() /= 3) stop "short list size /= 3!"

      call check_array(l%iterator(), [1, 2, 3] )
      
      call l%clear()
      if (l%size() /= 0) stop "empty list size /= 0!"

      call l%add(2)
      call l%add(3)
      call l%add(4)
      call check_array(l%iterator(), [2, 3, 4] )

      iter =l%iterator() 
      o=>iter%next()  ! Points to 2
      o=>iter%next()  ! Points to 4
      call iter%remove()
      if (l%size() /= 2) stop "short list size /= 2!"

      call check_array(l%iterator(), [2, 4] )

      
      call l%clear()
      
    end subroutine integer_list_test
    
    subroutine check_array(iter, array)
      type(iterator) :: iter
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

  end subroutine lists_test
