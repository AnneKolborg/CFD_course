module commons
  use read_params, only: dp
  implicit none
  real(dp), dimension(:), allocatable:: x, u, uNew, uInit

contains
  subroutine initBlock()
    use read_params, only: dx, xa, xb, N, ngc

    implicit none

    integer:: i, istat
    real(dp):: cell_center
    !real(kind = 8)::dx

    dx = (xb - xa)/N

    allocate(x(N+2*ngc), stat = istat)
    if (istat .ne. 0)then
      write(*, '(A, I10)') 'Buffer commons failed, x: STAT =', istat
      STOP
    endif

    do i = ngc+1, N + ngc
      cell_center = i - (ngc+1) + 0.5_dp
      x(i) = xa + cell_center*dx
    end do

    x(1) = xa - 1.5_dp*dx
    x(2) = xa - 0.5_dp*dx

    x(N + ngc + 1) = xb + 0.5_dp*dx
    x(N + ngc + 2) = xb + 1.5_dp*dx

  end subroutine

  subroutine initu()
    use read_params, only: N, ngc, InitCond, Method

    implicit none

    integer :: i, k, fu, istat

    real(dp), parameter :: pi = 3.14_dp

    character(len=13)::out_file

    allocate(uInit(size(x)), stat = istat)
    if (istat .ne. 0)then
      write(*, '(A, I10)') 'Buffer commons failed, uInit: STAT =', istat
      STOP
    endif

    ! initialize u
    write(*,*) InitCond, Method
    if (InitCond == 'Sinus')then
        write(*,*) 'Using sinus intial condition'
        do i = ngc+1, N + ngc
          uInit(i) = sin(2.0_dp*pi*x(i))
        end do
    elseif (InitCond == 'Shock') then
        write(*,*) 'Using shock initial condition'
        do i = ngc +1, N + ngc
          if (abs(x(i)) < 1.0_dp/3.0_dp)then
            uInit(i) = 1.0_dp
          else
            uInit(i) = 0.0_dp
          endif
        end do
    else
        write(*,*) 'Unknown intial condition'
    endif

    call BC(uInit)

    allocate(u(size(x)), stat = istat)
    if (istat .ne. 0)then
      write(*, '(A, I10)') 'Buffer commons failed, u: STAT =', istat
      STOP
    endif

    allocate(uNew(size(x)), stat = istat)
    if (istat .ne. 0)then
      write(*, '(A, I10)') 'Buffer commons failed, uNew: STAT =', istat
      STOP
    endif

    uNew = uInit
    u = uInit

    out_file = 'init.txt'
    call write_output(out_file)

  end subroutine

  subroutine BC(placeholder_array)
!    use commons, only: dp
    use read_params, only: N, ngc
    implicit none

    real(dp), dimension(N + 2*ngc), intent(out)::placeholder_array
    !Apply periodic boundary conditions
    placeholder_array(1) = placeholder_array(N+1)
    placeholder_array(2) = placeholder_array(N+ngc)
    placeholder_array(N + ngc +1) = placeholder_array(ngc+1)
    placeholder_array(N + ngc + 2) = placeholder_array(ngc +2)
  end subroutine

  subroutine advection()
!    use commons, only: dp, u, uNew
    use read_params, only: Ca, N, ngc, Method

    implicit none

    integer::i

    if ((Method == 'Lax Friedrichs') .or. (Method == 'LF')) then
      do i = ngc+1, N + ngc
        uNew(i) = 0.5_dp*(u(i+1) + u(i-1)) + 0.5_dp*Ca*(u(i+1) - u(i-1))
      end do
    else if ((Method == 'Lax Wendroff') .or. (Method == 'LW')) then
      do i = ngc +1, N + ngc
        uNew(i) = u(i) - 0.5_dp*Ca*(u(i+1) - u(i-1)) + 0.5_dp*Ca*Ca*(u(i+1) - 2*u(i) + u(i-1))
      end do
    else
      write(*,*) 'Unknown method'
    end if

    call BC(uNew)
  end subroutine

  subroutine write_output(filename)
!    use commons, only: dp, x, uInit, u, uNew
    implicit none

    character(len=13), INTENT(IN)::filename

    integer::k, fu

  2 format(es23.16, es23.16, es23.16, es23.16)

    fu = 5
    open(unit =fu, action = 'write', file = filename, status = 'replace')
    do k = 1, size(x)
      write(fu,*) x(k), uInit(k), u(k), uNew(k)
    end do

    close(fu)

  end subroutine

end module commons
