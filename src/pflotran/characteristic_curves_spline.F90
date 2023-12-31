module Characteristic_Curves_spline_module
#include "petsc/finclude/petscsys.h"

use Characteristic_Curves_Base_module ! Base type
use slatec_pchip_module ! Imported SLATEC PCHIP library
use petscsys ! PETSC_TRUE / PETSC_FALSE
use PFLOTRAN_Constants_module ! UNINITIALIZED_DOUBLE
use Option_module ! Unused argument 'option'

implicit none

private

! **************************************************************************** !
!
! Author: Matthew Paul
! Date:   05/01/2023
!
! This module contains the methods to construct, evaluate, and deconstruct 
! cubic splines for any monotonic capillary pressure or relative permeability
! function or data set. This can be done to use available data or to accelerate
! computationally expensive but analytical functions.

! Cubic splines are the simpliest arithmetic expression that ensure continuity
! and smoothness over an arbitrary data set. When hysteresis in imbibition and
! drainage are not modeled, the model seeks the minimum free energy for a given
! degree of saturation, and conseqently, capillary pressure is necessarially a
! monotonic function of saturation.
!
! Monotonic splines are calculated using the SLATEC Piecewise Cubic Hermite
! Interpolation Polynomial (PCHIP) package, a public domain library.
! PCHIP was optimized for graphing, thus the evaluation function assumes the
! points being evaluated are ordered and consquently a linear search was used
! to evaluate the entire list. Here, the PCHIP splines are cached as standard
! form polynomials and a binary search is used to locate the the correct spline.
! Additionally, binary search is written to be branchless, supporting future
! vectorization.
!
! Caveat, monotonic splines are not recommended for use with Richard's mode.
! Different degrees of saturation can yield identical matrix potentials. Where
! this occurs, capillary pressure and saturation are not uniquely invertible.
! Consequently, Richard's equation has degenerate roots and is ill-defined.
! Additionally, calcuation of the cubic polynomial inverse is inefficient.
!
! References:
!
! Ross, P (1992) "Cubic approximation of hydraulic properties for simulations of
! unsaturated flow", Water Resour. Res. 28(10):2617-2620.
! doi: 10.1029/92WR01310a
!
! Fritsch, FN and Butland, J (1984) "A method for constucting local monotone
! piecewise cubic interpolants", SIAM J. Sci. Stat. Comput., 5(2):300-304.
! doi: 10.1137/0905021
!
! Fritsch, FN and Carlson, RE (1980) "Monotone piecewise cubic interpolation",
! SIAM J. Numer. Anal., 17(2):238-246.
! doi: 10.1137/0717021
!
! **************************************************************************** !

type, public, extends(sat_func_base_type) :: sf_pchip_type
  private
    PetscInt  :: n ! Number of knots, 1 more than splines
    PetscReal, dimension(:), allocatable :: Sw ! Saturation reference points
    PetscReal, dimension(:,:), allocatable :: Pc ! Cubic polynomials
  contains
    procedure, public :: Init              => SFPCHIPInit
    procedure, public :: CapillaryPressure => SFPCHIPCapillaryPressure
    procedure, public :: Saturation        => SFPCHIPSaturation
    procedure, public :: D2SatDP2          => SFPCHIPD2SatDP2
    procedure, public :: Test              => SFPCHIPTest
    final :: SFPCHIPDtor
end type sf_pchip_type

! **************************************************************************** !

type, public, extends(rel_perm_func_base_type) :: rpf_pchip_type
  private
    PetscInt :: n ! Number of knots, 1 more than splines
    PetscReal, dimension(:), allocatable :: Sw ! Saturation reference points
    PetscReal, dimension(:,:), allocatable :: Kr ! Cubic polynomials
  contains
    procedure, public :: Init                 => RPFPCHIPInit
    procedure, public :: RelativePermeability => RPFPCHIPRelativePermeability
    procedure, public :: Test                 => RPFPCHIPTest
    final :: RPFPCHIPDtor
end type rpf_pchip_type

! **************************************************************************** !

private PCHIPCoefficients ! Calculates SF and RPF coefficients via SLATEC/PCHIP
 
private SFPCHIPAllocate
public  SFPCHIPCreate
public  SFPCHIPCtorFunction
public  SFPCHIPCtorArray

private RPFPCHIPAllocate
public  RPFPCHIPCreate
public  RPFPCHIPCtorFunction
public  RPFPCHIPCtorArray

contains

! **************************************************************************** !
! SLATEC/PCHIP/PCHIM Wrapper
! **************************************************************************** !

subroutine PCHIPCoefficients(n, x, y, dy, c2, c3)
  implicit none
  PetscInt, intent(in)   :: n ! Number of knots, 1 more than splines
  PetscReal, intent(in)  :: x(n), y(n)
  PetscReal, intent(out) :: dy(n), c2(n), c3(n)
  PetscInt :: i
  PetscReal :: h, delta, del1, del2

  ! Use SLATEC/PCHIP/PCHIM to calculate first derivatives for monotonic splines
  ! I is part of the SLATEC error handling, but it is not used here.
  i = 0
  call PCHIM(n, x, y, dy, 1, i)

  ! Note, Hermite polynomials fully define the cubic using the value and
  ! 1st derivative of the bounding knots.
  ! Here, the Hermite polynomials are cached instead in standard polynomial form
  do i = 1, n-1
    h     =  x(i+1)  - x(i)
    delta = (y(i+1)  - y(i))/h
    del1  = (dy(i)   - delta)/h
    del2  = (dy(i+1) - delta)/h

    c2(i) = -(del1+del1+del2)
    c3(i) =  (del1     +del2)/h
  end do
  ! Set cubic and quadratic coefficients of final knot to 0
  c2(n) = 0d0
  c3(n) = 0d0
end subroutine

! **************************************************************************** !
! Saturation Function Spline Methods
! **************************************************************************** !

function SFPCHIPCreate() result (new)
  implicit none
  class(sf_pchip_type), pointer :: new
! Create a placeholder object of the PCHIP type for the parser
  allocate(new)
  call new%Init()
end function

! **************************************************************************** !

subroutine SFPCHIPInit(this)
  implicit none
  class(sf_pchip_type) :: this
! Unused objects in base class that must be nullified
  nullify(this%sat_poly)
  nullify(this%pres_poly)
! Unitialized values, which will to be set in ctor, but not Create()
  this%Sr = UNINITIALIZED_DOUBLE
  this%pcmax = UNINITIALIZED_DOUBLE
! Common initial values regardless of ctor
  this%analytical_derivative_available = PETSC_TRUE
  this%calc_int_tension = PETSC_FALSE
  this%calc_vapor_pressure = PETSC_FALSE
end subroutine SFPCHIPInit

! **************************************************************************** !

function SFPCHIPAllocate(n) result (new)
  implicit none
  PetscInt, intent(in) :: n
  class(sf_pchip_type), pointer :: new

  nullify(new)

  ! Return null as no interpolation is possible with less than 2 knots
  if (n < 2) return

  ! Allocate spline object
  allocate(new)
  if (.not. associated(new)) return ! Return null as allocation failed

  ! Default initialization
  call new%Init()
  new%n = n

  ! Allocate subordinate dynamic objects
  allocate(new%Sw(n))
  allocate(new%Pc(0:3, n)) ! Zero-indexing to match polynomial indices
  if (.not. allocated(new%Sw) .or. .not. allocated(new%Pc)) then
    deallocate(new)
    nullify(new)
  end if ! Return null if any subordinate allocation failed

end function

! **************************************************************************** !

subroutine SFPCHIPDtor(this)
  implicit none
  type(sf_pchip_type) :: this
  ! Deallocate subordinate dynamic objects
  if (allocated(this%Sw)) deallocate(this%Sw)
  if (allocated(this%Pc)) deallocate(this%Pc)
end subroutine

! **************************************************************************** !

function SFPCHIPCtorFunction(n, sf_analytic) result (new)
  implicit none
  PetscInt, intent(in) :: n ! Number of splines
  class(sat_func_base_type), intent(in) :: sf_analytic
  class(sf_pchip_type), pointer :: new

  PetscInt  :: i
  type(option_type) :: option ! Placeholder

  new => SFPCHIPAllocate(n+1) ! Allocate space for N+1 knots
  if (.not. associated(new)) return

  ! Copy attributes of saturation function to be approximated
  new%Sr    = sf_analytic%Sr
  new%Pcmax = sf_analytic%Pcmax

  ! Generate N+1 evenly spaced knots starting at 0 and ending at 1
  do i = 1, n+1
    new%Sw(i) = dble(i-1)/dble(n)
    call sf_analytic%CapillaryPressure(new%Sw(i), new%Pc(0,i), new%Pc(1,i), &
                                       option)
  end do
 
! Calculate the PCHIP splines
  call PCHIPCoefficients(new%n, new%Sw, new%Pc(0,:), new%Pc(1,:), &
                                        new%Pc(2,:), new%Pc(3,:))

end function SFPCHIPCtorFunction

! **************************************************************************** !

function SFPCHIPCtorArray(n, Sw, Pc) result (new)
  implicit none
  PetscInt, intent(in) :: n ! Number of knots
  PetscReal, Dimension(n), intent(in) :: Sw, Pc
  class(sf_pchip_type), pointer :: new

  new => SFPCHIPAllocate(n) ! Allocate space for N knots
  if (.not. associated(new)) return

! Vector copy passed array
  new%Sw = Sw
  new%Pc(0,:) = Pc

! Set base class attributes with first knot
  new%Sr    = new%Sw(1)
  new%Pcmax = new%Pc(0,1)

! Calculate the PCHIP splines
  call PCHIPCoefficients(new%n, new%Sw, new%Pc(0,:), new%Pc(1,:), &
                                        new%Pc(2,:), new%Pc(3,:))

end function SFPCHIPCtorArray

! **************************************************************************** !

subroutine SFPCHIPCapillaryPressure(this, liquid_saturation, &
                                   capillary_pressure, dPc_dSatl, option)
  implicit none
  class(sf_pchip_type) :: this
  PetscReal, intent(in)   :: liquid_saturation
  PetscReal, intent(out)  :: capillary_pressure, dPc_dSatl
  type(option_type), intent(inout) :: option

  PetscInt :: i, j, k
  PetscReal :: Sw
  PetscBool :: mask

! Truncate saturation to be within bounds
  Sw = min(max(liquid_saturation,this%Sw(1)),this%Sw(this%n))

! Branchless binary search for polynomial index
  i = 1
  j = this%n
  do while (j - i > 1) 
    k = (i + j)/2
    mask = this%Sw(k) > Sw
    ! merge(tsource, fsource, mask)
    i = merge(i, k, mask)
    j = merge(k, j, mask)
  end do

! Polynomials are defined with respect to the nearest knot for precision
  Sw  = Sw - this%Sw(i)

! Horner's method utilizes fused add-multiply
  capillary_pressure    =   this%Pc(0,i) + Sw * &
                        (   this%Pc(1,i) + Sw * &
                        (   this%Pc(2,i) + Sw * &
                            this%Pc(3,i) ))

  dPc_dSatl             =   this%Pc(1,i) + Sw * &
                        ( 2*this%Pc(2,i) + Sw * &
                          3*this%Pc(3,i) )

end subroutine SFPCHIPCapillaryPressure

! **************************************************************************** !

subroutine SFPCHIPSaturation(this, capillary_pressure, &
                            liquid_saturation, dsat_dpres, option)
  implicit none
  class(sf_pchip_type) :: this
  PetscReal, intent(in)  :: capillary_pressure
  PetscReal, intent(out) :: liquid_saturation, dsat_dpres
  type(option_type), intent(inout) :: option

  PetscReal, parameter :: PI = 4*atan(1d0)
  PetscInt :: i, j, k
  PetscReal :: Sw, Pc, dPc
  PetscReal :: a, b, c
  PetscReal :: q, r, s, t
  PetscBool :: mask

  ! Truncate capillary pressure to be within bounds
  Pc = min(max(capillary_pressure, this%Pc(0,this%n)), this%Pc(0,1))

  ! Branchless binary search for polynomial index
  i = 1
  j = this%n
  do while (j - i > 1)
    k = (i + j) / 2
    mask = this%Pc(0,k) < Pc
    ! merge(tsource, fsource, mask)
    i = merge(i, k, mask)
    j = merge(k, j, mask)
  end do

  ! Find root of polynomial of up to 3rd order
  if (this%Pc(3,i) /= 0d0) then ! Cubic
    a =  this%Pc(2,i)       / this%Pc(3,i)
    b =  this%Pc(1,i)       / this%Pc(3,i)
    c = (this%Pc(0,i) - Pc) / this%Pc(3,i)

    q = (    a**2 - 3d0*  b         ) /  9d0
    r = (2d0*a**3 - 9d0*a*b + 27d0*c) / 54d0
    s = r**2 - q**3

    if (s < 0d0) then
      ! Three real roots, but only one in the domain
      t = acos(r/sqrt(q**3)) ! Angle in radians
      ! 1st root
      Sw = -2d0*sqrt(q)*cos(t/3d0) - a/3d0
      if (Sw < 0d0 .or. Sw > (this%Sw(i+1) - this%Sw(i))) then
        ! 2nd root
        Sw = -2d0*sqrt(q)*cos((t + 2d0*PI)/3d0) - a/3d0
        if (Sw < 0d0 .or. Sw > (this%Sw(i+1) - this%Sw(i))) then
          ! 3rd root
          Sw = -2d0*sqrt(q)*cos((t - 2d0*PI)/3d0) - a/3d0
        end if
      end if
    else ! One real root, complex roots are ignored
      t = sign( (abs(r)+sqrt(s))**(1d0/3d0), -r)
      if (t /= 0d0) then
        Sw = t + q/t - a/3d0
      else
        Sw = -a/3d0
      end if
    end if

    dPc =     this%Pc(1,i) + Sw * &
          ( 2*this%Pc(2,i) + Sw * &
            3*this%Pc(3,i) )

  else if (this%Pc(2,I) /= 0d0) then ! Quadratic
    a = this%Pc(2,I)
    b = this%Pc(1,I)
    c = this%Pc(0,I) - Pc

    q = (b + sign(sqrt(b**2 - 4d0*a*c), b)) / (-2d0)

    Sw = q/a ! Check if this root is within spline domain
    if (Sw < 0d0 .or. Sw > (this%Sw(i+1) - this%Sw(i))) then
      Sw = c/q ! If not, the other must be
    end if

    dPc =     this%Pc(1,i) + Sw * &
            2*this%Pc(2,i)

  else if (this%Pc(1,i) /= 0d0) then ! Linear
    Sw = (Pc - this%Pc(0,i)) / this%Pc(1,i)

    dPc = this%Pc(1,i)

  else ! Degenerate
    Sw = 0d0
    dPc = tiny(this%Pc(0,i))
  end if

  liquid_saturation = Sw + this%Sw(i) ! Linear translation to saturation space
  dsat_dpres = 1d0 / dPc ! Invert 1st derivative

end subroutine SFPCHIPSaturation

! **************************************************************************** !

subroutine SFPCHIPD2SatDP2(this,Pc, d2s_dp2, option)
  implicit none
  class(sf_pchip_type) :: this
  PetscReal, intent(in) :: Pc
  PetscReal, intent(out) :: d2s_dp2
  type(option_type), intent(inout) :: option

  PetscInt :: i, j, k
  PetscReal :: Sw, dSw_dPc

  ! Using inverse/first derivative rather than repeating cubic formula etc.
  call this%Saturation(Pc, Sw, dSw_dPc, option)

  ! Repeat binary search to get index for 2nd derivative
  i = 1
  j = this%n
  do while (j - 1 > 1)
    k = (i + j) /2
    if (this%Pc(0,i) > Pc) then
     j = k
    else
     i = k
    end if
  end do

  d2s_dp2 = -( 2*this%Pc(2,i) + 6*Sw*this%Pc(3,i) ) / dSw_dPc**3

end subroutine SFPCHIPD2SatDP2

! **************************************************************************** !

subroutine SFPCHIPTest(this,cc_name,option)
  implicit none
  class(sf_pchip_type) :: this
  character(len=MAXWORDLENGTH) :: cc_name
  type(option_type), intent(inout) :: option
  character(len=MAXSTRINGLENGTH) :: string

  PetscInt :: i

! Write knots and spline derivatives to file
  write(string,*) cc_name
  string = trim(cc_name) // '_pc_knots.dat'
  open(unit=87,file=string)
  write(87,*) '#Index, Sw, Pc, dPc/dSw'

  do i = 1, this%n
    write(87,*) i, this%Sw(i), this%Pc(0,i), this%Pc(1,i)
  end do

! Also call base test
  call SFBaseTest(this, cc_name, option)

end subroutine SFPCHIPTest

! **************************************************************************** !
! Relative Permeability PCHIP Methods
! **************************************************************************** !

function RPFPCHIPCreate() result (new)
  implicit none
  class(rpf_pchip_type), pointer :: new
  allocate(new)
  call new%Init()
end function

! **************************************************************************** !
subroutine RPFPCHIPInit(this)
  implicit none
  class(rpf_pchip_type) :: this
! Unused objects in base class that must be nullified
  nullify(this%poly)
! Unitialized values, which ought to be set in ctor
  this%Sr = UNINITIALIZED_DOUBLE
  this%Srg = UNINITIALIZED_DOUBLE
! Common values regardless of ctor
  this%analytical_derivative_available = PETSC_TRUE
end subroutine RPFPCHIPInit

! **************************************************************************** !

function RPFPCHIPAllocate(n) result (new)
  ! Perform allocation for both data or function defined splines
  implicit none
  PetscInt, intent(in) :: n
  class(rpf_pchip_type), pointer :: new

  nullify(new)

  ! Return null as no interpolation is possible with less than 2 knots
  if (n < 2) return

  ! Allocate spline object
  allocate(new)
  if (.not. associated(new)) return ! Return null as allocation failed
  call new%Init()
  new%n = n

  ! Allocate subordinate dynamic objects
  allocate(new%Sw(n))
  allocate(new%Kr(0:3,n)) ! Zero-indexing to match polynomial coefficients
  if (.not. allocated(new%Sw) .or. .not. allocated(new%Kr)) then
    ! Note, final method will deallocate if only one is allocated
    deallocate(new)
    nullify(new)
  end if ! Return null if any subordinate allocation failed
end function

! **************************************************************************** !

subroutine RPFPCHIPDtor(this)
  implicit none
  type(rpf_pchip_type) :: this
  ! Deallocate subordinate dynamic objects
  ! These will not have been allocated if "create" is used instead of a ctor
  if (allocated(this%Sw)) deallocate(this%Sw)
  if (allocated(this%Kr)) deallocate(this%Kr)
end subroutine RPFPCHIPDtor

! **************************************************************************** !

function RPFPCHIPCtorFunction(n, rpf_analytic) result (new)
  implicit none
  PetscInt, intent(in) :: n
  class(rel_perm_func_base_type), intent(in) :: rpf_analytic
  class(rpf_pchip_type), pointer :: new

  PetscInt  :: i
  type(option_type) :: option

  new => RPFPCHIPAllocate(n+1) ! Allocate space for N+1 knots
  if (.not. associated(new)) return

! Copy attributes of relative permeability function to be approximated
  new%Sr  = rpf_analytic%Sr
  new%Srg = rpf_analytic%Srg
! Caveat, liquid relative perm functions may have an "uninitialized" gas residual
  if (rpf_analytic%Srg == UNINITIALIZED_DOUBLE) new%Srg = 0d0

! Generate N+1 evenly spaced knots starting at Sr and ending at 1-Srg
  do i = 1, n
    new%Sw(i) = (1d0 - new%Srg - new%Sr) * dble(i-1)/dble(n) + new%Sr
    call rpf_analytic%RelativePermeability(new%Sw(i), new%Kr(0,i), new%Kr(1,i),&
                                           option)
  end do
  ! Ensure gas residual is precise
  i = n + 1
  new%Sw(i) = 1d0 - new%Srg 
  call rpf_analytic%RelativePermeability(new%Sw(i), new%Kr(0,i), new%Kr(1,i),  &
                                         option)

! Calculate the PCHIP splines
  call PCHIPCoefficients(new%n, new%Sw, new%Kr(0,:), new%Kr(1,:), &
                                        new%Kr(2,:), new%Kr(3,:))

end function RPFPCHIPCtorFunction

! **************************************************************************** !

function RPFPCHIPCtorArray(n, Sw, Kr) result (new)
  implicit none
  PetscInt, intent(in)  :: n
  PetscReal, Dimension(n), intent(in) :: Sw, Kr
  class(rpf_pchip_type), pointer :: new

  new => RPFPCHIPAllocate(n)
  if (.not. associated(new)) return

! Vector copy passed array to internal array
  new%Sw = Sw
  new%Kr(0,:) = Kr

! Set base class attributes with limiting knots
  new%Sr = new%Sw(1)
  new%Srg = 1d0 - new%Sw(n)

! Calculate the PCHIP splines
  call PCHIPCoefficients(new%n, new%Sw, new%Kr(0,:), new%Kr(1,:), &
                                        new%Kr(2,:), new%Kr(3,:))

end function RPFPCHIPCtorArray

! **************************************************************************** !

subroutine RPFPCHIPRelativePermeability(this, liquid_saturation, &
                                   relative_permeability, dkr_sat, option)
  implicit none
  class(rpf_pchip_type) :: this
  PetscReal, intent(in)   :: liquid_saturation
  PetscReal, intent(out)  :: relative_permeability, dkr_sat
  type(option_type), intent(inout) :: option

  PetscInt :: i, j, k
  PetscReal :: Sw
  PetscBool :: mask

! Truncate saturation to be within bounds
  Sw = min(max(liquid_saturation,this%Sw(1)), this%Sw(this%n))

! Branchless binary search for polynomial index
  i = 1
  j = this%n
  do while (j - i > 1)
    k = (i + j)/2
    mask = this%Sw(k) > Sw
    ! merge(tsource, fsource, mask)
    i = merge(i, k, mask)
    j = merge(k, j, mask)
  end do

! Note, polynomials are defined with respect to the nearest knot for precision
  Sw  = Sw - this%Sw(i)

! Horner's method leverages fused add-multiple
  relative_permeability =   this%Kr(0,i) + Sw * &
                        (   this%Kr(1,i) + Sw * &
                        (   this%Kr(2,i) + Sw * &
                            this%Kr(3,i) ))

  dkr_sat               =   this%Kr(1,i) + Sw * &
                        ( 2*this%Kr(2,i) + Sw * &
                          3*this%Kr(3,i) )

end subroutine RPFPCHIPRelativePermeability

! **************************************************************************** !

subroutine RPFPCHIPTest(this,cc_name,phase,option)
  implicit none
  class(rpf_pchip_type) :: this
  character(len=MAXWORDLENGTH) :: cc_name
  type(option_type), intent(inout) :: option
  character(len=MAXWORDLENGTH) :: phase
  character(len=MAXSTRINGLENGTH) :: string

  PetscInt :: i

! Write knots and spline derivatives to file
  write(string,*) cc_name
  string = trim(cc_name) // '_' //  trim(phase) // '_Kr_knots.dat'
  open(unit=87,file=string)
  write(87,*) '#Index, Sw, Kr, dKr/dSw'
  do i = 1, this%n
    write(87,*) i, this%Sw(i), this%Kr(0,i), this%Kr(1,i)
  end do
  close(87)

! Also call base test
  call RPFBaseTest(this, cc_name, phase, option)
    
end subroutine RPFPCHIPTest

! **************************************************************************** !

end module
