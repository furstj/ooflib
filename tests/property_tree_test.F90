subroutine property_tree_test

  use property_tree

  print *, "property_tree tests:"

  call test_empty_tree()

  print *

contains

  subroutine test_empty_tree()
    type(ptree) :: p
    print *, " - empty tree"
  end subroutine test_empty_tree

  subroutine test1()

    type(ptree), pointer :: p, q
    character(len=256) :: line
    allocate(p)

    print *, "p%size() = ", p%size() 
    print *, "p%empty() = ", p%empty() 

    !q => p%get_child("first")
    !print *, "q%empty() = ", q%empty()

    call p%add("first", "1-1")
    call p%add("first", "1-2")
    call p%add("second.third", "23-1")
    call p%add("second.third", "23-2")
    call p%add("integer", 1)
    call p%add("double", atan(1.d0)*4)
    call p%add("double", 2*atan(1.d0)*4)
    
    allocate(q)
    call q%add("zero", 0.d0)
    call p%put_child("double", q)

    call p%print()

    call p%put("integer", 42)
    call p%print()

    line = p%get_string("first")
    print *, trim(line)

    line = p%get_string("second.third")
    print *, trim(line)

    line = p%get("second.fourth", "nothing")
    print *, trim(line)

    deallocate(p)

  end subroutine test1

end subroutine property_tree_test
