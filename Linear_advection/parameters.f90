module read_params
  implicit none

  integer, parameter:: dp=kind(0.d0)

  character(len=100)::buffer, label
  integer::pos
  integer, parameter::fh = 15
  integer::ios=0
  integer::line=0

  !Program varialbles
  real(dp)::xa, xb, a, cfl, dx, dt, tmax, t, Ca
  integer::N, ngc, Ncycle
  character(len=100)::Method, InitCond

contains
  subroutine read_file(filename)
    implicit none
    character(len=100)::filename

    open(fh, file = filename)

    do while(ios == 0)
      read(fh, '(A)', iostat=ios) buffer
      if (ios == 0) then
        line = line +1

        pos = scan(buffer, '    ')
        label = buffer(1:pos)
        buffer = buffer(pos+1:)

        pos = scan(buffer, '=')
        buffer = buffer(pos+1:)

        select case(label)
        case('xa')
          read(buffer, *, iostat = ios) xa
          !print *, 'Read xa = ', xa

        case('xb')
          read(buffer, *, iostat = ios) xb
          !print *, 'Read xb = ', xb

        case('N')
          read(buffer, *, iostat = ios) N
          !print *, 'Read N = ', N

        case('ngc')
          read(buffer, *, iostat = ios) ngc
          !print *, 'Read number of ghost zones = ', ngc

        case('a')
          read(buffer, *, iostat = ios) a
          !print *, 'Read a = ', a

        case('cfl')
          read(buffer, *, iostat = ios) cfl
          !print *, 'Read CFL = ', cfl

        case('Ncycle')
          read(buffer, *, iostat = ios) Ncycle
          !print *, 'Read Ncycle = ', Ncycle

        case('Method')
          read(buffer, *, iostat = ios) Method

        case('InitialCondition')
          read(buffer, *, iostat = ios) InitCond
          !print *, 'Read initial condition:', InitCond

        end select
      end if
    end do
  end subroutine



end module read_params
