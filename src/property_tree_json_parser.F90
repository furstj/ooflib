module property_tree_json_parser
  !! author: Jiri Furst
  !! version: v0.1
  !! license: Boost Software License, Version 1.0.
  !!
  !! The module implements *read_json* and *write_json* functions for reading/writting property tree in/to
  !! json format
  !!
  !! @note The module uses external *json_module* (version 4.2)

  implicit none
  private

  public :: read_json
  public :: write_json

contains

  subroutine read_json(filename, pt, iostat)
    use json_module
    use property_tree

    character(len=*), intent(in)   :: filename
    type(ptree), intent(inout)     :: pt
    integer, intent(out), optional :: iostat
    type(json_file)           :: jfile
    type(json_value), pointer :: jroot, jval
    integer :: i

    call json_initialize()

    if (present(iostat)) iostat = 0

    call jfile%load_file(filename)
    if (json_failed()) then
      if (present(iostat)) then
        iostat = -1
        return
      else
        stop "Error reading json file"
      end if
    end if

    call jfile%get(jroot)
    do i = 1, json_count(jroot)
      call json_get_child(jroot, i, jval)
      call json_to_ptree(jval, pt)
    end do

    call jfile%destroy()

  contains

    recursive subroutine json_to_ptree(jval, pt)
      use, intrinsic :: iso_fortran_env, only : real64
      type(json_value), pointer    :: jval
      type(ptree)                  :: pt
      type(json_value), pointer    :: jchild
      type(ptree), pointer         :: pchild
      integer                       :: i, n_children, var_type
      character(len=:), allocatable :: name, str
      logical :: bool
      real(real64) :: d

      call json_info(jval, var_type, n_children, name)

      select case (var_type)

        case (json_logical)
          call json_get(jval, bool)
          call pt%add(name, bool)

        case (json_integer)
          call json_get(jval, i)
          call pt%add(name, i)

        case (json_double)
          call json_get(jval, d)
          call pt%add(name, d)

        case (json_string)
          call json_get(jval, str)
          call pt%add(name, str)

        case (json_object)
          allocate(pchild)
          do i = 1, n_children
            call json_get_child(jval, i, jchild)
            call json_to_ptree(jchild, pchild)
          end do
          call pt%add_child(name, pchild)

      end select

    end subroutine

  end subroutine
  
  
  subroutine write_json(filename, pt, iostat)
    use json_module
    use property_tree
    character(len=*), intent(in)   :: filename
    type(ptree), intent(inout)     :: pt
    integer, intent(out), optional :: iostat
    type(json_value), pointer :: js

    if (present(iostat)) iostat = 0

    call json_initialize()
    call json_create_object(js,"")

    call ptree_to_json(pt, js)

    call json_print(js, filename)

    call json_destroy(js)

  contains

    recursive subroutine ptree_to_json(pt, js)
      type(ptree)               :: pt
      type(json_value), pointer :: js
      type(json_value), pointer     :: jchild
      type(ptree), pointer          :: pchild
      character(len=:), allocatable :: name
      character(len=:), pointer     :: val
      integer :: i

      do i = 1, pt%size()
        pchild => pt%get_child(i, name)
        val => pchild%data()

        if (associated(val)) then
          call json_add(js, name, val)
        else if (pchild%size()>0) then
          call json_create_object(jchild, name)
          call ptree_to_json(pchild, jchild)
          call json_add(js, jchild)
        end if

      end do

    end subroutine

  end subroutine
  

end module property_tree_json_parser

