program advect
  use commons
  use read_params

  implicit none

  integer :: i, j, k,fu

  integer :: istat ! Used to check status of allocatable arrays

  real(dp), parameter :: pi = 3.14_dp

  character (len = 13) :: out_file
  character (len = 100):: filename

  call GET_COMMAND_ARGUMENT(1, filename)

  call read_file(filename)

!  xa = 0.0_dp
!  xb = 1.0_dp
!  N = 32
!  ngc = 2

  call initBlock()

  call initu()

!  cfl = 0.9_dp
!  a = 1.0_dp

  dt = cfl*dx/abs(a)

!  Ncycle = 1
  tmax = Ncycle*(xb-xa)/abs(a)

  Ca = a*dt/(dx)

  j = 0
  t = 0
  do while(t < tmax) ! time stepping looop
    call advection()

    u(:) = uNew(:)

    write (out_file, '("uNew_",I4.4, ".txt")') j
    call write_output(trim(out_file))

    j = j +1
    t = t + dt
  end do

end program
