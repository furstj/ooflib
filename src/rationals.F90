!======================================================================
!
!          Copyright Jiri Furst 2013
! Distributed under the Boost Software License, Version 1.0.
!    (See accompanying file LICENSE_1_0.txt or copy at
!          http://www.boost.org/LICENSE_1_0.txt)
!
!----------------------------------------------------------------------
!
!  MODULE: rationals
!
!> @author Jiri Furst
!
!  DESCRIPTION: 
!> The module implements oprations with rational numbers
!
!======================================================================

module rationals

  private

  type, public :: rational
     integer :: numerator   = 0
     integer :: denominator = 1
  end type 

  interface real
     module procedure real_rat
  end interface 
  public :: real

  interface assignment(=)
     module procedure assign_int
  end interface 

  interface operator(+)
     module procedure add_rat_rat
     module procedure add_int_rat
     module procedure add_rat_int
  end interface 
  public :: operator(+)

  interface operator(-)
     module procedure sub_rat_rat
     module procedure sub_int_rat
     module procedure sub_rat_int
  end interface 
  public :: operator(-)

  interface operator(*)
     module procedure mul_rat_rat
     module procedure mul_int_rat
     module procedure mul_rat_int
  end interface 
  public :: operator(*)

  interface operator(/)
     module procedure div_rat_rat
     module procedure div_rat_int
     module procedure div_int_rat
  end interface 
  public :: operator(/)


  public :: rationals_test

contains

  function real_rat(rat)
    type(rational), intent(in)    :: rat
    integer, parameter  :: dbl = selected_real_kind(15)
    real(dbl)           :: real_rat
    real_rat = rat%numerator / real(rat%denominator, dbl)
  end function real_rat


  subroutine assign_int(c, a)
    integer, intent(in)         :: a
    type(rational), intent(out) :: c
    c = rational(a, 1)
  end subroutine assign_int

  !----------------------------------------------------------------------

  function add_rat_rat(a, b) result(c)
    type(rational), intent(in) :: a, b
    type(rational)             :: c
    c%numerator   = a%numerator * b%denominator + a%denominator * b%numerator
    c%denominator = a%denominator * b%denominator
    call normalize(c)
  end function add_rat_rat

  function add_rat_int(a, b) result(c)
    type(rational), intent(in) :: a
    integer, intent(in)        :: b
    type(rational)             :: c
    c = a
    c%numerator = c%numerator + b*c%denominator 
  end function add_rat_int

  function add_int_rat(b, a) result(c)
    type(rational), intent(in) :: a
    integer, intent(in)        :: b
    type(rational)             :: c
    c = a
    c%numerator = c%numerator + b*c%denominator 
  end function add_int_rat

  !----------------------------------------------------------------------

  function sub_rat_rat(a, b) result(c)
    type(rational), intent(in) :: a, b
    type(rational)             :: c
    integer :: denom
    c%numerator   = a%numerator * b%denominator - a%denominator * b%numerator
    c%denominator = a%denominator * b%denominator
    call normalize(c)
  end function sub_rat_rat

  function sub_int_rat(b, a) result(c)
    type(rational), intent(in) :: a
    integer, intent(in)        :: b
    type(rational)             :: c
    c%numerator = b*a%denominator - a%numerator
    c%denominator = a%denominator
  end function sub_int_rat

  function sub_rat_int(a, b) result(c)
    type(rational), intent(in) :: a
    integer, intent(in)        :: b
    type(rational)             :: c
    c%numerator = a%numerator - b*a%denominator
    c%denominator = a%denominator
  end function sub_rat_int

  !----------------------------------------------------------------------

  function mul_rat_rat(a, b) result(c)
    type(rational), intent(in) :: a, b
    type(rational)             :: c
    c%numerator   = a%numerator * b%numerator
    c%denominator = a%denominator * b%denominator
    call normalize(c)
  end function mul_rat_rat

  function mul_rat_int(a, b) result(c)
    type(rational), intent(in) :: a
    integer, intent(in)        :: b
    type(rational)             :: c
    c%numerator   = a%numerator * b
    c%denominator = a%denominator 
    call normalize(c)
  end function mul_rat_int

  function mul_int_rat(b, a) result(c)
    type(rational), intent(in) :: a
    integer, intent(in)        :: b
    type(rational)             :: c
    c%numerator   = a%numerator * b
    c%denominator = a%denominator 
    call normalize(c)
  end function mul_int_rat

  !----------------------------------------------------------------------

  function div_rat_rat(a, b) result(c)
    type(rational), intent(in) :: a, b
    type(rational)             :: c
    integer :: denom
    c%numerator   = a%numerator * b%denominator
    c%denominator = a%denominator * b%numerator
    call normalize(c)
  end function div_rat_rat

  function div_rat_int(a, b) result(c)
    type(rational), intent(in) :: a
    integer, intent(in)        :: b
    type(rational)             :: c
    c%numerator   = a%numerator
    c%denominator = a%denominator * b
    call normalize(c)
  end function div_rat_int

  function div_int_rat(b, a) result(c)
    type(rational), intent(in) :: a
    integer, intent(in)        :: b
    type(rational)             :: c
    c%numerator   = b * a%denominator
    c%denominator = a%numerator 
    call normalize(c)
  end function div_int_rat


  !======================================================================

  subroutine normalize(rat)
    type(rational), intent(inout) :: rat
    integer :: gcd

    if (rat%denominator<0) then
       rat%numerator = - rat%numerator
       rat%denominator = - rat%denominator
    end if
    
    gcd = greatest_common_divisor(rat%numerator, rat%denominator)
 
    rat%numerator   = rat%numerator / gcd
    rat%denominator = rat%denominator / gcd
  end subroutine normalize

  function greatest_common_divisor(x, y)
    integer, intent(in) :: x, y
    integer :: greatest_common_divisor
    integer :: a, b
    a = abs(x)
    b = abs(y)
    do while(a /= b)
       if (a>b) then
          a = a - b
       else
          b = b - a
       end if
    end do
    greatest_common_divisor = a
  end function greatest_common_divisor

  !======================================================================
  subroutine rationals_test()

    print *, "Rationals tests:"

    call rationals_constructors()
    call rationals_operators()

    print *

  contains
    
    subroutine rationals_constructors()
      type(rational) :: a, b, c
      print *, " - constructors"

      a = rational(2,3)
      if (abs(real(a) - 0.666) > 0.001) stop "error in rational(2,3)"

      b = 2
      if (abs(real(b) - 2) > 0.001) stop "error in b = 2"

      c = a
      if (abs(real(c) - 0.666) > 0.001) stop "error in c = a"

    end subroutine rationals_constructors

    subroutine rationals_operators()
      type(rational) :: a, b, c, d
      print *, " - operators"

      a = rational(1,2)
      b = rational(3,8)
      c = rational(4,3)

      d = a + b
      if (d%numerator /= 7 .or. d%denominator /= 8) &
           stop "error in 1/2 + 3/8"

      d = b - a
      if (d%numerator /= -1 .or. d%denominator /= 8) &
           stop "error in 1/2 - 3/8"

      d = (a+b) + (a-b)
      if (d%numerator /= 1 .or. d%denominator /= 1) &
           stop "error in (1/2+b) - (1/2-b)"

      d = a * b
      if (d%numerator /= 3 .or. d%denominator /= 16) &
           stop "error in 1/2 * 3/8"

      d = a / b
      if (d%numerator /= 4 .or. d%denominator /= 3) &
           stop "error in (1/2) / (3/8)"

      d = 2 * a
      if (d%numerator /= 1 .or. d%denominator /= 1) &
           stop "error in 2 * (1/2)"

      d = b * 4
      if (d%numerator /= 3 .or. d%denominator /= 2) &
           stop "error in (3/8) * 4"

      d = 2 / a
      if (d%numerator /= 4 .or. d%denominator /= 1) &
           stop "error in 2 / (1/2)"

      d = b / 4
      if (d%numerator /= 3 .or. d%denominator /= 32) &
           stop "error in (3/8) / 4"


    end subroutine rationals_operators

  end subroutine rationals_test

end module rationals
