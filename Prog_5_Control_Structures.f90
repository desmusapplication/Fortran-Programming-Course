program Prog_5_Control_Structures
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

    print *, 'Enter the number of names for your table: '
    read(*,*) names_number
    print *, 'Enter the number of characters of the largest name: '
    read(*,*) names_size

    !Allocate 2D dynamic array while program execution
    allocate(names_1(names_number, names_size))

    !This nested loops would help us to initialize our 2D array
    do i = 1, names_number
      do j = 1, names_size
        names_1(i,j) = '*'
      end do
    end do

    !This would help us to insert the names inside the 2D array
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
        names_1(i,j) = names_char
      end do
    end do

    !This would print the 2D array
    print *, 'Your table names: '
    do i = 1, names_number
      print *, names_1(i,:)
    end do

    deallocate(names_1)
  end if

  deallocate(name)
end program Prog_5_Control_Structures