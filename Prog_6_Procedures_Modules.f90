module procedure_mod
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  implicit none

  !All entities created below this statement are accessible only inside this module
  private private_module_num

  !All entities created below this statement are accessible to all the program
  public public_module_num, print_procedure_num, initialize_matrix, print_matrix, save_names, select_section, total_price

  integer :: public_module_num = 1
  integer :: private_module_num = 1

contains

  subroutine print_procedure_num()
    print *, 'Module private variable value: ', private_module_num
  end subroutine print_procedure_num

  subroutine initialize_matrix(x, y, matrix)
    implicit none

    integer, intent(in) :: x
    integer, intent(in) :: y
    character, intent(inout) :: matrix(:,:)

    integer :: i
    integer :: j

    do i = 1, x
      do j = 1, y
        matrix(i,j) = '*'
      end do
    end do
  end subroutine initialize_matrix

  subroutine print_matrix(matrix)
    implicit none

    character, intent(in) :: matrix(:,:)
    integer :: i

    do i = 1, size(matrix, 1) !size() is a Fortran built-in procedure to get the size of the specified dimension array
      print *, matrix(i,:)
    end do
  end subroutine print_matrix

  subroutine save_names(matrix)
    implicit none

    character, intent(inout) :: matrix(:,:)
    character :: names_char

    integer :: names_number
    integer :: names_size
    integer :: name_size
    integer :: i
    integer :: j

    names_number = size(matrix, 1)
    names_size = size(matrix, 2)

    do i = 1, names_number
      print *, 'Enter the number of characters for the ', i, ' name: '
      read(*,*) name_size

      if(name_size > names_size) then
        do while(name_size > names_size)
          print *, 'The size of the name most be less than ', names_size
          print *, 'Enter the number of characters for the ', i, 'name: '
          read(*,*) name_size
        end do
      end if

      do j = 1, name_size
        print *, 'Enter the character ', MOD(j, names_size), ' from name ', i, ': '
        read(*,*) names_char
        matrix(i,j) = names_char
      end do
    end do
  end subroutine save_names

  function select_section(age) result(norm)
    implicit none

    integer, intent(in) :: age
    character :: section_char
    character :: norm

    if(age >= 21) then
      print *, 'Which section you prefer A, B, C or D?'
      read(*,*) section_char

      select case(section_char)
        case('A')
          print *, 'Welcome to section A, the price is $100.'
        case('B')
          print *, 'Welcome to section B, the price is $200.'
        case('C')
          print *, 'Welcome to section C, the price is $300.'
        case('D')
          print *, 'Welcome to section D, the price is $400.'
        case default
          print *, 'Welcome to section E, this section is free.'
      end select
    else
      print *, 'Which section you prefer A, B?'
      read(*,*) section_char

      select case(section_char)
        case('A')
          print *, 'Welcome to section A, the price is $100.'
        case('B')
          print *, 'Welcome to section B, the price is $200.'
        case default
          print *, 'Welcome to section E, this section is free.'
      end select
    end if

    norm = section_char
  end function select_section

  function total_price(section, people, discount) result(norm)
    use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
    implicit none

    character, intent(in) :: section
    integer, intent(in) :: people
    real(dp), intent(in), optional :: discount
    real(dp) :: norm

    if(section == 'A') then
      norm = people * 100.0_dp
    else if(section == 'B') then
      norm = people * 200.0_dp
    else if(section == 'C') then
      norm = people * 300.0_dp
    else if(section == 'D') then
      norm = people * 400.0_dp
    else
      norm = 0.0
    end if
    
    if(present(discount)) then
      norm = norm - (norm * discount)
    end if
  end function total_price
end module procedure_mod

program Prog_6_Procedures_Modules
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  use procedure_mod
  implicit none

  integer :: age
  integer :: names_number
  integer :: names_size
  integer :: name_size
  integer :: i
  integer :: j

  character :: names_char
  character :: section_char
  character, dimension(:), allocatable :: name
  character, dimension(:,:), allocatable :: names_1

  real(dp) :: total
  real(dp) :: discount = 0.3_dp

  print *, 'How many characters has your name?'
  read(*,*) name_size

  allocate(name(name_size))

  print *, 'Hi, plase spell your name?'
  read(*,*) name
  print *, 'How old are you?'
  read(*,*) age

  !Nested if else
  if(age >= 21) then
    print *, 'Welcome ', name, '!'
  else
    if(age >= 18) then
      print *, 'Welcome ', name, ', we have a special place for 18-20 years old people.'
    else
      print *, 'You need to have at least 18 years old to enter this place.'
    end if
  end if

  !Else-id instead of using nested if-else
  if(age >= 21) then
    print *, 'Welcome ', name, '!'
  else if(age >= 18) then
    print *, 'Welcome ', name, ', we have a special place for 18-20 years old people.'
  else
    print *, 'You need to have at least 18 years old to enter this place.'
  end if

  if(age >= 18) then
    section_char = select_section(age)

    print *, 'Enter the number of names for your table: '
    read(*,*) names_number
    print *, 'Enter the number of characters of the largest name: '
    read(*,*) names_size

    !Allocate 2D dynamic array while program execution
    allocate(names_1(names_number, names_size))

    call initialize_matrix(names_number, names_size, names_1)
    call save_names(names_1)
    call print_matrix(names_1)

    print *, 'Total price: ', total_price(section_char, names_number)
    print *, 'Total price with 30% discount: ', total_price(section_char, names_number, discount)

    deallocate(names_1)
  end if

  print *, 'Module public variable value: ', public_module_num
  !print *, 'Module private variable value: ', private_module_num !Error, we can't access to private members outs of program scope
  call print_procedure_num

  deallocate(name)
end program Prog_6_Procedures_Modules