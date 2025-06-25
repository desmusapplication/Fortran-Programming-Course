program Prog_2_Variables
  use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
  implicit none

  integer :: age
  real(sp) :: score_sp
  real(dp) :: score_dp
  complex :: frequency
  character :: section_type
  logical :: status

  age = 23
  score_sp = 8.2_sp
  score_dp = 8.2_dp
  frequency = (1.0, -0.5)
  section_type = 'C'
  status = .true.

  print *, 'Age: ', age
  print *, 'Score sp: ', score_sp
  print *, 'Score dp: ', score_dp
  print *, 'Frequency: ', frequency
  print *, 'Section type: ', section_type
  print *, 'Status: ', status
  
end program Prog_2_Variables