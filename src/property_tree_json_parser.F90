module property_tree_json_parser

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

    if (present(iostat)) iostat = 0

  end subroutine
  
  
  subroutine write_json(filename, pt, iostat)
    use json_module
    use property_tree
    character(len=*), intent(in)   :: filename
    type(ptree), intent(inout)     :: pt
    integer, intent(out), optional :: iostat

    if (present(iostat)) iostat = 0

  end subroutine
  

end module property_tree_json_parser

