module Reaction_Sandbox_ONE_class

#include "petsc/finclude/petscsys.h"
  use petscsys

  use Reaction_Sandbox_Base_class
  use PFLOTRAN_Constants_module

  implicit none

  private

  type, public, &
    extends(reaction_sandbox_base_type) :: reaction_sandbox_one_type
    PetscInt :: auxiliary_offset
    PetscInt :: species_Cx_id
    PetscInt :: species_Cs_id
    PetscInt :: species_Xim_id
    PetscInt :: nb_c_simple
    PetscReal :: yield
    PetscReal :: k_decay
    PetscReal :: min_cx_allowed
    PetscReal :: min_xim_allowed
    PetscBool :: molarity_units

    PetscReal :: k_cat
    PetscReal :: k_ana
    PetscReal :: ks_cx
    PetscReal :: nc_ed

    PetscReal, pointer :: stoich(:)

  contains
    procedure, public :: ReadInput => ONEReadInput
    procedure, public :: Setup => ONESetup
    procedure, public :: AuxiliaryPlotVariables => ONEAuxiliaryPlotVariables
    procedure, public :: Evaluate => ONEEvaluate
    procedure, public :: Destroy => ONEDestroy
  end type reaction_sandbox_one_type

  public :: ONECreate

contains

! ************************************************************************** !

function ONECreate()
  !
  ! Allocates flexible biodegradation reaction object.
  !
  implicit none

  class(reaction_sandbox_one_type), pointer :: ONECreate

  allocate(ONECreate)
  ONECreate%auxiliary_offset = UNINITIALIZED_INTEGER
  ONECreate%nb_c_simple = UNINITIALIZED_INTEGER
  ONECreate%yield = UNINITIALIZED_DOUBLE
  ONECreate%k_decay = UNINITIALIZED_DOUBLE
  ONECreate%min_cx_allowed = UNINITIALIZED_DOUBLE
  ONECreate%min_xim_allowed = UNINITIALIZED_DOUBLE
  ONECreate%molarity_units = PETSC_TRUE

  ONECreate%k_cat = UNINITIALIZED_DOUBLE
  ONECreate%k_ana = UNINITIALIZED_DOUBLE
  ONECreate%ks_cx = UNINITIALIZED_DOUBLE
  ONECreate%nc_ed = UNINITIALIZED_DOUBLE

  nullify(ONECreate%stoich)
  nullify(ONECreate%next)

end function ONECreate

! ************************************************************************** !

subroutine ONEReadInput(this,input,option)
  !
  ! Reads flexible biodegradation reaction parameters
  !
  use Option_module
  use Input_Aux_module
  use String_module

  implicit none

  class(reaction_sandbox_one_type) :: this
  type(input_type), pointer :: input
  type(option_type) :: option

  character(len=MAXWORDLENGTH) :: word
  character(len=MAXSTRINGLENGTH) :: error_string

  error_string = 'CHEMISTRY,REACTION_SANDBOX,AVAILABLE OC'
  call InputPushBlock(input,option)
  do
    call InputReadPflotranString(input,option)
    if (InputError(input)) exit
    if (InputCheckExit(input,option)) exit

    call InputReadCard(input,option,word)
    call InputErrorMsg(input,option,'keyword',error_string)
    call StringToUpper(word)
!
    select case(word)
      case('AQUEOUS_CONCENTRATION_UNITS')
        call InputReadCard(input,option,word)
        call InputErrorMsg(input,option,word,error_string)
        call StringToUpper(word)
        select case(word)
          case('MOLARITY')
            this%molarity_units = PETSC_TRUE
          case('MOLALITY')
            this%molarity_units = PETSC_FALSE
          case default
            call InputKeywordUnrecognized(input,word, &
                         trim(error_string)//&
                         'AQUEOUS_CONCENTRATION_UNITS',option)
        end select
      case('YIELD')
        call InputReadDouble(input,option,this%yield)
        call InputErrorMsg(input,option,word,error_string)
      case('BIOMASS_DECAY_RATE_CONSTANT')
        call InputReadDouble(input,option,this%k_decay)
        call InputErrorMsg(input,option,word,error_string)
        call InputReadAndConvertUnits(input,this%k_decay,'1/sec', &
                                      trim(error_string)//','//word,option)
      case('MIN_ALLOWED_CX_CONCENTRATION')
        call InputReadDouble(input,option,this%min_cx_allowed)
        call InputErrorMsg(input,option,word,error_string)
      case('MIN_ALLOWED_XIM_CONCENTRATION')
        call InputReadDouble(input,option,this%min_xim_allowed)
        call InputErrorMsg(input,option,word,error_string)
      case default
        call InputKeywordUnrecognized(input,word,error_string,option)
    end select
  enddo
  call InputPopBlock(input,option)
  error_string = ''
  if (Uninitialized(this%k_decay)) then
    error_string = trim(error_string) // 'BIOMASS_DECAY_RATE_CONSTANT,'
  endif
  if (Uninitialized(this%yield)) then
    error_string = trim(error_string) // 'YIELD,'
  endif

  if (len_trim(error_string) > 0) then
    option%io_buffer = 'Reaction Sandbox Available OC has &
      &uninitialized parameters: ' &
      // error_string(1:len_trim(error_string)-1)
    call PrintErrMsg(option)
  endif

end subroutine ONEReadInput

! ************************************************************************** !

subroutine ONESetup(this,reaction,option)
  !
  ! Sets up the flexible biodegradation reaction with hardwired parameters
  !
  use Reaction_Aux_module, only : reaction_rt_type, GetPrimarySpeciesIDFromName
  use Reaction_Immobile_Aux_module, only : GetImmobileSpeciesIDFromName
  use Option_module
  use Reactive_Transport_Aux_module
  use Reaction_Aux_module

  implicit none

  class(reaction_sandbox_one_type) :: this
  class(reaction_rt_type) :: reaction
  type(option_type) :: option

  character(len=MAXWORDLENGTH) :: word

  ! 1. Allocate memory auxiliary array
  this%auxiliary_offset = reaction%nauxiliary
  reaction%nauxiliary = reaction%nauxiliary + 3
  ! rate_cs, rate_cx, rate_xb

  ! Aqueous species
  word = 'Cx'
  this%species_Cx_id = &
    GetPrimarySpeciesIDFromName(word,reaction,option)
  word = 'Cs'
  this%species_Cs_id = &
    GetPrimarySpeciesIDFromName(word,reaction,option)

  ! Immobile species
  word = 'Xim'
  this%species_Xim_id = &
    GetImmobileSpeciesIDFromName(word,reaction%immobile,option)


  allocate(this%stoich(reaction%ncomp))
  this%stoich = 0.d0
  this%stoich(this%species_Cx_id) = -1.d0
  this%stoich(this%species_Cs_id) = 1.d0
  this%stoich(this%species_Xim_id+reaction%offset_immobile) = this%yield

  ! Acetate: moles C per mol acetate
  this%nb_c_simple = 2

  ! Rates
  this%k_cat = 3.74d-18
  this%k_ana = 1.14247261d-6
  this%ks_cx = 8.85d-9
  this%nc_ed = 15.d0

end subroutine ONESetup

! ************************************************************************** !

subroutine ONEAuxiliaryPlotVariables(this,list,reaction,option)
  !
  ! Adds available oc auxiliary plot variables to output list
  !
  use Option_module
  use Reaction_Aux_module
  use Output_Aux_module
  use Variables_module, only : REACTION_AUXILIARY

  class(reaction_sandbox_one_type) :: this
  type(output_variable_list_type), pointer :: list
  type(option_type) :: option
  class(reaction_rt_type) :: reaction

  character(len=MAXWORDLENGTH) :: word
  character(len=MAXWORDLENGTH) :: units

  ! Rate Cs
  word = 'Rate Cs'
  units = 'mol/L-sec'
  call OutputVariableAddToList(list,word,OUTPUT_RATE,units, &
                                REACTION_AUXILIARY, &
                                this%auxiliary_offset + 1)

  ! Rate Cx
  word = 'Rate Cx'
  units = 'mol/L-sec'
  call OutputVariableAddToList(list,word,OUTPUT_RATE,units, &
                                REACTION_AUXILIARY, &
                                this%auxiliary_offset + 2)

  ! Rate Cx
  word = 'Rate Xb'
  units = 'mol/L-sec'
  call OutputVariableAddToList(list,word,OUTPUT_RATE,units, &
                                REACTION_AUXILIARY, &
                                this%auxiliary_offset + 3)

end subroutine ONEAuxiliaryPlotVariables

! ************************************************************************** !

subroutine ONEEvaluate(this,Residual,Jacobian,compute_derivative, &
                               rt_auxvar,global_auxvar,material_auxvar, &
                               reaction,option)
  !
  ! Evaluates flexible biodegradation reaction storing residual and Jacobian
  !
  use Option_module
  use Reaction_Aux_module
  use Reactive_Transport_Aux_module
  use Global_Aux_module
  use Material_Aux_module

  implicit none

  class(reaction_sandbox_one_type) :: this
  type(option_type) :: option
  class(reaction_rt_type) :: reaction
  PetscBool :: compute_derivative
  PetscReal :: Residual(reaction%ncomp) ! [mole / sec]
  PetscReal :: Jacobian(reaction%ncomp,reaction%ncomp) ! [kg water / sec]
  type(reactive_transport_auxvar_type) :: rt_auxvar
  type(global_auxvar_type) :: global_auxvar
  type(material_auxvar_type) :: material_auxvar

  PetscInt, parameter :: iphase = 1
  PetscReal :: volume               ! [m^3 bulk volume]
  PetscReal :: molality_to_molarity ! [kg water / L water]
  PetscReal :: Cx, Cs             ! [mole / L water] or [mole / kg water]
  PetscReal :: Xim                  ! [mole biomass / m^3 bulk volume]
  PetscReal :: Ir_Xim, Ir_cx, Ir_cs   ! [mole rxn / m^3 bulk volume-sec]
  PetscInt :: Xim_offset
  PetscReal :: L_water

  Xim_offset = this%species_Xim_id + reaction%offset_immobile
  volume = material_auxvar%volume

  if (this%molarity_units) then
    molality_to_molarity = global_auxvar%den_kg(iphase)*1.d-3
  else
    molality_to_molarity = 1.d0
  endif

  ! Concentrations
  Xim = rt_auxvar%immobile(this%species_Xim_id) ! mol / m3 bulk
  Xim = Xim / (1.d3 * material_auxvar%porosity) ! molt / m3 bulk -> mol / L
  Cx = rt_auxvar%pri_molal(this%species_Cx_id)*molality_to_molarity !mol / L
  Cs = rt_auxvar%pri_molal(this%species_Cs_id)*molality_to_molarity ! mol / L


  if (Cx>this%min_cx_allowed .AND. Xim>this%min_xim_allowed) then
    Ir_cs = Xim * this%k_cat * Cx / (Cx + this%ks_cx)! rate simple organic [mol / L sec]
    Ir_cx = (Xim * this%k_cat * Cx / (Cx + this%ks_cx)) * (this%nc_ed / this%nb_c_simple) ! rate complex organic [mol / L sec]
    Ir_Xim = (Xim * this%k_ana * Cx / (Cx + this%ks_cx)) - (this%k_decay * Xim) ! rate biomass [mol / L sec]

    if (Cx < this%min_cx_allowed .OR. Xim < this%min_xim_allowed) then
      Ir_cs = 0.d0
      Ir_cx = 0.d0
      Ir_Xim = 0.d0
    endif

  else
    Ir_cs = 0.d0
    Ir_cx = 0.d0
    Ir_Xim = 0.d0
  endif

  ! Save rates
  rt_auxvar%auxiliary_data(this%auxiliary_offset + 1) = Ir_cs
  rt_auxvar%auxiliary_data(this%auxiliary_offset + 2) = Ir_cx
  rt_auxvar%auxiliary_data(this%auxiliary_offset + 3) = Ir_Xim

  ! Residual
  L_water = 1.d3 * volume * material_auxvar%porosity ! volumen water in L
  Residual(this%species_Cs_id) = Residual(this%species_Cs_id) - this%stoich(this%species_Cs_id) * Ir_cs * L_water ! mol / s
  Residual(this%species_Cx_id) = Residual(this%species_Cx_id) - this%stoich(this%species_Cx_id) * Ir_cx * L_water ! mol / s
  Residual(Xim_offset) = Residual(Xim_offset) - this%yield * Ir_Xim * L_water ! mol / s


end subroutine ONEEvaluate

! ************************************************************************** !

subroutine ONEDestroy(this)
  !
  ! Deallocates dynamic memory
  !
  use Utility_module, only : DeallocateArray

  implicit none
  class(reaction_sandbox_one_type) :: this

  call DeallocateArray(this%stoich)

end subroutine ONEDestroy

end module Reaction_Sandbox_ONE_class
