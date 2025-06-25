module type_mod
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  implicit none

  private

  public t_location, t_address, t_person, t_boss, print_info, t_rectangle, area

  type :: t_location
    character(len=:), allocatable :: loc_latitude
    character(len=:), allocatable :: loc_logitude
  end type

  type :: t_address
    character(len=:), allocatable :: add_street
    character(len=:), allocatable :: add_number
    character(len=:), allocatable :: add_city
    character(len=:), allocatable :: add_state
    integer :: add_zip_code = 0
    type(t_location) :: add_exact_location ! Derived types could have other derived types
  end type

  type :: t_person
    character(len=:), allocatable :: person_name
    integer :: person_age
    type(t_address) :: person_address
  end type

  type, extends(t_person) :: t_boss ! Derived types could inherit properties and procedures from other derived types
    type(t_person) :: boss_data
    real(dp) :: salary = 0.0_dp
    integer :: vacation_days = 0

  contains
    procedure :: print_info ! Derived types could have procedures known as methods
  end type

  type :: t_rectangle
    real(dp) :: height = 0.0_dp
    real(dp) :: width = 0.0_dp

  contains
    procedure :: area
  end type

contains

  subroutine print_info(self)
    class(t_boss), intent(in) :: self
    print *, 'Name: ', self%boss_data%person_name
    print *, 'Age: ', self%boss_data%person_age
    print *, 'Address street name: ', self%boss_data%person_address%add_street
    print *, 'Address number: ', self%boss_data%person_address%add_number
    print *, 'Address city: ', self%boss_data%person_address%add_city
    print *, 'Address state: ', self%boss_data%person_address%add_state
    print *, 'Address zip code: ', self%boss_data%person_address%add_zip_code
    print *, 'Address location latitude: ', self%boss_data%person_address%add_exact_location%loc_latitude
    print *, 'Address location longitude: ', self%boss_data%person_address%add_exact_location%loc_logitude
    print *, 'Salary: ', self%salary
    print *, 'Vacation days: ', self%vacation_days
  end subroutine print_info

  real(dp) function area(self) result(norm)
    class(t_rectangle), intent(in) :: self ! This refers to the instance object of the derived type itself.
    norm = self%height * self%width
  end function area
end module type_mod

program Prog_7_Derived_Types
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  use type_mod
  implicit none

  type(t_location) :: address_location
  type(t_address) :: address
  type(t_person) :: person
  type(t_boss) :: boss
  type(t_rectangle) :: rectangle

  address_location%loc_latitude = '34.130839'
  address_location%loc_logitude = '-118-297340'

  address%add_street = '5th Ave'
  address%add_number = '725'
  address%add_city = 'NY'
  address%add_state = 'New York'
  address%add_zip_code = 10022
  address%add_exact_location = address_location

  person%person_name = 'John'
  person%person_age = 75
  person%person_address = address

  boss%boss_data = person
  boss%salary = 120000.0
  boss%vacation_days = 30

  rectangle%height = 3.0_dp
  rectangle%width = 4.0_dp

  call boss%print_info()

  print *, 'Rectangle area: ', rectangle%area()

  deallocate(address%add_exact_location%loc_logitude)
  deallocate(address%add_exact_location%loc_latitude)
  deallocate(address%add_street)
  deallocate(address%add_number)
  deallocate(address%add_city)
  deallocate(address%add_state)
  deallocate(person%person_name)
end program Prog_7_Derived_Types