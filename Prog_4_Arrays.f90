program Prog_4_Arrays
  implicit none

  integer, dimension(10) :: numbers_1
  integer :: numbers_2(10)
  integer :: numbers_3(0:9) !Custom lower and upper index bounds (From 0 to 9 instead of 1 to 10)
  integer :: numbers_4(3,3)

  character :: names_1(3,6) !2D array
  character, allocatable :: names_2(:,:) !2D dynamic array
  character(:), allocatable :: name !1D dynamic array

  allocate(names_2(3,6))

  numbers_1 = [1,2,3,4,5,6,7,8,9,10]
  numbers_1(5) = 20 !Access the element located at index 5 and store the value 20
  numbers_2 = [10,20,30,40,50,60,70,80,90,100]
  numbers_3 = [1,2,3,4,5,6,7,8,9,10]
  numbers_3(0) = 10
  numbers_3(9) = 100

  !2D integer array initialization
  data numbers_4(1,:)/1,2,3/
  data numbers_4(2,:)/4,5,6/
  data numbers_4(3,:)/7,8,9/

  name = 'Ashley'

  print *, numbers_1
  print *, numbers_2
  print *, numbers_3
  print *, numbers_4
  print *, name

  !In other video we would see how to initialize 2D arrays using loops

  !Array Slicing
  numbers_1(:) = 0 !Set all elements to zero
  numbers_1(1:5) = 1 ! Set first five elements to one
  numbers_1(6:) = 2 !See all elements after five to two

  print *, numbers_1(1:10:2) !Array at odd indices
  print *, numbers_1(10:1:1) !Array in reverse
  print *, numbers_4(1,:) !Print out the first row in a 2D array
  print *, numbers_4(:,1) !Print out the first column in a 2D array

  deallocate(name)
  deallocate(names_2)
end program Prog_4_Arrays