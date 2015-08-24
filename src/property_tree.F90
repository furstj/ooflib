module property_tree

  !! author: Jiri Furst
  !! version: v0.1
  !! license: Boost Software License, Version 1.0.
  !!
  !! The module approximates property_tree from boost library

  implicit none
  private

  type :: value_type
    character(len=:), pointer :: first   => null()
    class(*), pointer         :: second  => null()
  contains
    procedure :: free => value_type_free
  end type value_type


  type, public :: ptree
    private
    character(len=:), pointer :: data_        => null()
    type(value_type), pointer :: children_(:) => null()
     
  contains

    procedure :: size  => size_    !! Number of direct children of this node.
    procedure :: empty             !! Whether there are any direct children.

    procedure :: data

    generic   :: get_child => get_child_by_name, get_child_at
    generic   :: get => get_string_default, get_int_default, get_double_default, get_logical_default
    procedure :: get_string
    procedure :: get_int
    procedure :: get_double
    procedure :: get_logical
    procedure, private :: get_child_by_name
    procedure, private :: get_child_at
    procedure, private :: get_string_default
    procedure, private :: get_int_default
    procedure, private :: get_double_default
    procedure, private :: get_logical_default

    procedure :: add_child
    generic   :: add => add_string, add_int, add_double, add_logical
    procedure, private :: add_string
    procedure, private :: add_int
    procedure, private :: add_double
    procedure, private :: add_logical

    procedure :: put_child
    generic   :: put => put_string, put_int, put_double, put_logical
    procedure, private :: put_string
    procedure, private :: put_int
    procedure, private :: put_double
    procedure, private :: put_logical

    procedure :: clear => ptree_clear !! Clear this tree completely, of both data and children.

    procedure :: print => ptree_print

    procedure, private :: find        !! Find (optionally k-th) child with given name
    procedure, private :: find_index  !! Find (optionally k-th) child with given name

    final :: ptree_finalize

  end type ptree


  public :: read_command_line

contains


  recursive subroutine value_type_free(self)
    class(value_type), intent(inout) :: self
    if (associated(self%first)) deallocate(self%first)
    if (associated(self%second)) deallocate(self%second)
  end subroutine value_type_free


  recursive subroutine ptree_clear(self)
    class(ptree), intent(inout) :: self
    integer :: i
    if (associated(self%data_)) deallocate(self%data_)
    if (associated(self%children_)) then
      do i = 1, size(self%children_)
        call self%children_(i)%free()
      end do
      deallocate(self%children_)
    end if
  end subroutine ptree_clear

  
  subroutine ptree_finalize(self)
    type(ptree), intent(inout) :: self
    call self%clear()
  end subroutine ptree_finalize


  pure integer function size_(self)
    class(ptree), intent(in) :: self
    if (associated(self%children_)) then
      size_ = size(self%children_)
    else
      size_ = 0
    end if
  end function 


  pure logical function empty(self)
    class(ptree), intent(in) :: self
    empty = (self%size() == 0)
  end function empty

  
  function data(self)
    class(ptree), intent(in)  :: self
    character(len=:), pointer :: data
    data => self%data_
  end function


  !======================================================================
  ! GET
  !======================================================================

  recursive function get_child_by_name(self, path) result(p)
    class(ptree), intent(in)      :: self
    character(len=*), intent(in ) :: path
    type(ptree), pointer          :: p
    integer :: i
    i = index(path, ".")
    if (i>0) then
      p => self%find(path(1:i-1))
      if (associated(p)) p => p%get_child_by_name(path(i+1:))
    else
      p => self%find(path)
    end if
  end function


  function get_child_at(self, i, child_name) result(p)
    class(ptree), intent(in) :: self
    integer, intent(in)      :: i
    character(len=:), allocatable, optional :: child_name
    type(ptree), pointer     :: p

    if (i<1 .or. i>self%size()) stop "Index out of range in ptree%get_child(i)"

    select type (c => self%children_(i)%second)
      type is (ptree)
      p => c
    end select
    if (present(child_name)) child_name = self%children_(i)%first
  end function

  subroutine get_string_or_nothing(self, path, str)
    type(ptree), intent(in)                       :: self
    character(len=*), intent(in)                  :: path
    character(len=:), intent(inout), allocatable  :: str
    type(ptree), pointer :: p
    p => self%get_child(path)
    if (associated(p)) then
      if (associated(p%data_)) then
        allocate(str, source=p%data_)
      end if
    end if
  end subroutine get_string_or_nothing
  
  
  function get_string(self, path, istat) result(str)
    class(ptree), intent(in)        :: self
    character(len=*), intent(in)    :: path
    integer, intent(out), optional  :: istat
    character(len=:), allocatable   :: str
    if (present(istat)) istat = 0
    call get_string_or_nothing(self, path, str)
    if (.not. allocated(str)) then
      if (present(istat)) then
        istat = -1
      else
        print *, "ptree%get_string : path '", path, "' not found!"
        stop
      end if
    end if
  end function get_string


  function get_int(self, path, istat) result(i)
    class(ptree), intent(in)        :: self
    character(len=*), intent(in)    :: path
    integer, intent(out), optional  :: istat
    integer  :: i
    character(len=:), allocatable   :: str
    if (present(istat)) istat = 0
    call get_string_or_nothing(self, path, str)
    if (.not. allocated(str)) then
      if (present(istat)) then
        istat = -1
      else
        print *, "ptree%get_int : path '", path, "' not found!"
        stop
      end if
    end if
    read(str,*) i
  end function get_int


  function get_double(self, path, istat) result(d)
    use, intrinsic :: iso_fortran_env, only : real64
    class(ptree), intent(in)        :: self
    character(len=*), intent(in)    :: path
    integer, intent(out), optional  :: istat
    real(real64)                    :: d
    character(len=:), allocatable   :: str
    if (present(istat)) istat = 0
    call get_string_or_nothing(self, path, str)
    if (.not. allocated(str)) then
      if (present(istat)) then
        istat = -1
      else
        print *, "ptree%get_double : path '", path, "' not found!"
        stop
      end if
    end if
    read(str,*) d
  end function get_double


  function get_logical(self, path, istat) result(l)
    use, intrinsic :: iso_fortran_env, only : real64
    class(ptree), intent(in)        :: self
    character(len=*), intent(in)    :: path
    integer, intent(out), optional  :: istat
    logical                         :: l
    character(len=:), allocatable   :: str
    if (present(istat)) istat = 0
    call get_string_or_nothing(self, path, str)
    if (.not. allocated(str)) then
      if (present(istat)) then
        istat = -1
      else
        print *, "ptree%get_logical : path '", path, "' not found!"
        stop
      end if
    end if
    read(str,*) l
  end function get_logical
  
  
  function get_string_default(self, path, default) result(str)
    class(ptree), intent(in)       :: self
    character(len=*), intent(in)   :: path
    character(len=*), intent(in)   :: default
    character(len=:), allocatable  :: str
    call get_string_or_nothing(self, path, str)
    if (.not. allocated(str)) allocate(str, source=default)
  end function get_string_default


  function get_int_default(self, path, default) result(i)
    class(ptree), intent(in)       :: self
    character(len=*), intent(in)   :: path
    integer, intent(in)            :: default
    integer                        :: i
    character(len=:), allocatable  :: str
    call get_string_or_nothing(self, path, str)
    if (allocated(str)) then
      read(str,*) i
    else
      i = default
    end if
  end function get_int_default


  function get_double_default(self, path, default) result(d)
    use, intrinsic :: iso_fortran_env, only : real64
    class(ptree), intent(in)       :: self
    character(len=*), intent(in)   :: path
    real(real64), intent(in)       :: default
    real(real64)                   :: d
    character(len=:), allocatable  :: str
    call get_string_or_nothing(self, path, str)
    if (allocated(str)) then
      read(str,*) d
    else
      d = default
    end if
  end function get_double_default


  function get_logical_default(self, path, default) result(d)
    use, intrinsic :: iso_fortran_env, only : real64
    class(ptree), intent(in)       :: self
    character(len=*), intent(in)   :: path
    logical, intent(in)            :: default
    logical                        :: d
    character(len=:), allocatable  :: str
    call get_string_or_nothing(self, path, str)
    if (allocated(str)) then
      read(str,*) d
    else
      d = default
    end if
  end function get_logical_default
  

  !======================================================================
  ! ADD
  !======================================================================

  subroutine add_child(self, path, child)
    class(ptree), intent(inout)           :: self
    character(len=*), intent(in)          :: path
    type(ptree), pointer, intent(inout)   :: child
    type(ptree), pointer         :: p
    integer :: i
    p => force_path(self,path)
    i = index(path, ".", back=.true.)
    call append_child(p, path(i+1:), child)
  end subroutine add_child


  subroutine add_string(self, path, val)
    class(ptree), intent(inout)  :: self
    character(len=*), intent(in) :: path
    character(len=*), intent(in) :: val
    type(ptree), pointer :: child
    allocate(child)
    allocate(child%data_, source=val)
    call self%add_child(path, child)
  end subroutine add_string


  subroutine add_int(self, path, val)
    class(ptree), intent(inout)  :: self
    character(len=*), intent(in) :: path
    integer, intent(in)  :: val
    character(len=40) :: str
    write(str,*) val
    call self%add(path, trim(adjustl(str)))
  end subroutine add_int


  subroutine add_double(self, path, val)
    use, intrinsic :: iso_fortran_env, only : real64
    class(ptree), intent(inout)  :: self
    character(len=*), intent(in) :: path
    real(real64), intent(in)     :: val
    character(len=40) :: str
    write(str,*) val
    call self%add(path, trim(adjustl(str)))
  end subroutine add_double


  subroutine add_logical(self, path, val)
    use, intrinsic :: iso_fortran_env, only : real64
    class(ptree), intent(inout)  :: self
    character(len=*), intent(in) :: path
    logical, intent(in)          :: val
    character(len=40) :: str
    write(str,*) val
    call self%add(path, trim(adjustl(str)))
  end subroutine add_logical

  !======================================================================
  ! put
  !======================================================================

  subroutine put_child(self, path, child)
    class(ptree), intent(inout)           :: self
    character(len=*), intent(in)          :: path
    type(ptree), pointer, intent(inout)   :: child
    type(ptree), pointer         :: p
    integer :: i, idx
    p => force_path(self,path)
    i = index(path, ".", back=.true.)
    idx = p%find_index(path(i+1:))
    if (idx>0) then
      select type (pt => p%children_(idx)%second)
        type is (ptree)
        deallocate(pt)
      end select
      p%children_(idx)%second => child
    else
      call append_child(p, path(i+1:), child)
    end if
  end subroutine put_child

  
  subroutine put_string(self, path, val)
    class(ptree), intent(inout)  :: self
    character(len=*), intent(in) :: path
    character(len=*), intent(in) :: val
    type(ptree), pointer :: child
    allocate(child)
    allocate(child%data_, source=val)
    call self%put_child(path, child)
  end subroutine put_string


  subroutine put_int(self, path, val)
    class(ptree), intent(inout)  :: self
    character(len=*), intent(in) :: path
    integer, intent(in)  :: val
    character(len=40) :: str
    write(str,*) val
    call self%put(path, trim(adjustl(str)))
  end subroutine put_int


  subroutine put_double(self, path, val)
    use, intrinsic :: iso_fortran_env, only : real64
    class(ptree), intent(inout)  :: self
    character(len=*), intent(in) :: path
    real(real64), intent(in)     :: val
    character(len=40) :: str
    write(str,*) val
    call self%add(path, trim(adjustl(str)))
  end subroutine put_double


  subroutine put_logical(self, path, val)
    use, intrinsic :: iso_fortran_env, only : real64
    class(ptree), intent(inout)  :: self
    character(len=*), intent(in) :: path
    logical, intent(in)          :: val
    character(len=40) :: str
    write(str,*) val
    call self%add(path, trim(adjustl(str)))
  end subroutine put_logical

  !======================================================================
  ! internals
  !======================================================================

  function find(self, name, which) result(p)
    class(ptree), intent(in)      :: self
    character(len=*), intent(in ) :: name
    integer, intent(in), optional :: which
    type(ptree), pointer          :: p
    integer :: idx
    idx = find_index(self, name, which)
    if (idx>0) then
      select type(pt=>self%children_(idx)%second)
        type is (ptree)
        p => pt
        return
        class default
        error stop "Internal error in ptree_find"
      end select
    else
      p => null()
    end if
  end function find


  function find_index(self, name, which) result(idx)
    class(ptree), intent(in)      :: self
    character(len=*), intent(in ) :: name
    integer, intent(in), optional :: which
    integer                       :: idx
    integer :: i, w, count

    w = 1
    if (present(which)) w = which
    idx = 0

    count = 0
    do i = 1, self%size()
      if (self%children_(i)%first == name) then
        count = count + 1
        if (count == w) then
          idx = i
          return
        end if
      end if
    end do
    
  end function find_index

  
  recursive function force_path(self, path) result(p)
    type(ptree), intent(inout), target   :: self
    character(len=*), intent(in)         :: path
    type(ptree), pointer                 :: p
    integer :: i

    i = index(path, ".")
    if (i==0) then
      p => self
      return
    end if

    p => self%find(path(1:i-1))
    if (.not. associated(p)) then
      allocate(p)
      call append_child(self, path(1:i-1), p)
    end if
    p => force_path(p, path(i+1:))

  end function force_path


  subroutine append_child(self, name, child) 
    type(ptree), intent(inout)       :: self
    character(len=*), intent(in)     :: name
    type(ptree), pointer, intent(in) :: child
    type(value_type), pointer        :: tmp(:)

    allocate(tmp(self%size()+1))
    if (self%size()>0) then
      tmp(1:self%size()) = self%children_
      deallocate(self%children_)
    end if
    self%children_ => tmp
    allocate(self%children_(self%size())%first, source = name)
    self%children_(self%size())%second => child
  end subroutine append_child


  recursive subroutine ptree_print(self, iu)
    use, intrinsic :: iso_fortran_env, only : output_unit 
    class(ptree), intent(in)      :: self
    integer, intent(in), optional :: iu
    integer :: iu_
    character(len=2) :: indent = "  "

    iu_ = output_unit
    if (present(iu)) iu_ = iu
    
    call ptree_print_worker(self, iu_, "")
    
  contains

    recursive subroutine ptree_print_worker(pt, iu, spaces)
      class(ptree), intent(in)      :: pt
      integer, intent(in)          :: iu
      character(len=*), intent(in) :: spaces
      integer :: i
      write(iu,'(A)') "{"
      do i = 1, pt%size()
        write(iu,'(A)',advance='no') spaces//indent//pt%children_(i)%first//' = '
        select type(p=>pt%children_(i)%second)
          type is (ptree)
          if (associated(p%data_)) then
            write(iu,'(A)') p%data_ // ';'
          else
            call ptree_print_worker(p, iu, spaces//indent)
          end if
        end select
      end do
      write(iu,'(A)') spaces//"};"      
    end subroutine ptree_print_worker

  end subroutine ptree_print

  !=====================================================================================

  subroutine read_command_line(pt)
    !! Reads whole command line and stores it's content into the tree
    !!
    !! Rules:
    !! - arguments in the form "--key=value" are stored as key=value
    !! - arguments in the form "--key" are stored as key="true"
    !! - arguments without "--" are stored into "arg" array
    !!
    !! Example:
    !!    program --output=file.out --debug.level=2 --debug.output=file.dbg input1 input2
    !!
    !! will be stored as
    !!    {
    !!       arg = {
    !!               1 = input1,
    !!               2 = input2
    !!       },
    !!       output = file.out,
    !!       debug  = {
    !!               level = 2,
    !!               output = file.dbg,
    !!       },
    !!    }
    type(ptree), intent(inout) :: pt
    integer :: i, iarg, e
    character(len=256) :: arg, name

    iarg = 0
    do i = 1, command_argument_count()
      call get_command_argument(i, arg)
      if (arg(1:2)=="--") then
        e = index(arg, "=")
        if (e>0) then
          call pt%put(arg(3:e-1), trim(arg(e+1:)))
        else
          call pt%put(trim(arg(3:e-1)), "true")
        endif
      else
        iarg = iarg+1
        write(name,*) iarg
        call pt%put("arg." // trim(adjustl(name)), trim(arg))
      end if
    end do

  end subroutine

end module property_tree

