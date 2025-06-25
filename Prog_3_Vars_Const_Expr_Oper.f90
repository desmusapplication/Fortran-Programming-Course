program Prog_3_Vars_Const_Expr_Oper
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  implicit none

  integer :: x_1
  integer :: y_1

  real(sp) :: x_2
  real(sp) :: y_2

  real(sp), parameter :: pi = 3.14_sp

  logical :: p_t
  logical :: p_f

  x_1 = 0
  y_1 = 0

  x_2 = 0.0_sp
  y_2 = 0.0_sp

  p_t = .true.
  p_f = .false.

  print *, 'Add two numbers'
  print *, 'Insert an integer number: '
  read(*,*) x_1
  print *, 'Insert another integer number: '
  read(*,*) y_1
  print *, (x_1 + y_1)

  print *, 'Sub two numbers'
  print *, 'Insert an integer number: '
  read(*,*) x_1
  print *, 'Insert another integer number: '
  read(*,*) y_1
  print *, (x_1 - y_1)

  print *, 'Multiply two numbers'
  print *, 'Insert an integer number: '
  read(*,*) x_1
  print *, 'Insert another integer number: '
  read(*,*) y_1

  print *, (x_1 * y_1)
  print *, 'Divide two numbers'
  print *, 'Insert an integer number: '
  read(*,*) x_1
  print *, 'Insert another integer number: '
  read(*,*) y_1
  print *, (x_1 / y_1)

  print *, 'Exp of a numbers'
  print *, 'Insert an integer number: '
  read(*,*) x_1
  print *, 'Insert another integer number: '
  read(*,*) y_1
  print *, (x_1 ** y_1)

  print *, 'Add two numbers'
  print *, 'Insert an integer number: '
  read(*,*) x_2
  print *, 'Insert another integer number: '
  read(*,*) y_2
  print *, (x_2 + y_2)

  print *, 'Sub two numbers'
  print *, 'Insert an integer number: '
  read(*,*) x_2
  print *, 'Insert another integer number: '
  read(*,*) y_2
  print *, (x_2 - y_2)

  print *, 'Multiply two numbers'
  print *, 'Insert an integer number: '
  read(*,*) x_2
  print *, 'Insert another integer number: '
  read(*,*) y_2
  print *, (x_2 * y_2)

  print *, 'Divide two numbers'
  print *, 'Insert an integer number: '
  read(*,*) x_2
  print *, 'Insert another integer number: '
  read(*,*) y_2
  print *, (x_2 / y_2)

  print *, 'Exp of a numbers'
  print *, 'Insert an integer number: '
  read(*,*) x_2
  print *, 'Insert another integer number: '
  read(*,*) y_2
  print *, (x_2 ** y_2)

  print *, 'Comparison between numbers'
  print *, (x_1 == y_1)
  print *, (x_1 /= y_1)
  print *, (x_1 > y_1)
  print *, (x_1 < y_1)
  print *, (x_1 >= y_1)
  print *, (x_1 <= y_1)

  print *, 'True table of and operation'
  print *, (p_t .and. p_t)
  print *, (p_t .and. p_f)
  print *, (p_f .and. p_t)
  print *, (p_f .and. p_f)

  print *, 'True table of or operation'
  print *, (p_t .or. p_t)
  print *, (p_t .or. p_f)
  print *, (p_f .or. p_t)
  print *, (p_f .or. p_f)

  print *, 'True table of not operation'
  print *, (.not. p_t)
  print *, (.not. p_f)

  print *, 'Equivalence and Inequivalence'
  print *, (p_t .eqv. p_t)
  print *, (p_t .neqv. p_f)
  print *, (p_f .eqv. p_t)
  print *, (p_f .neqv. p_f)
end program Prog_3_Vars_Const_Expr_Oper