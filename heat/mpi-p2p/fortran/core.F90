! Main solver routines for heat equation solver
module core
  use heat

contains

  ! Exchange the boundary data between MPI tasks
  subroutine exchange(field0)
    use mpi

    implicit none

    type(field), intent(inout) :: field0

    integer :: ierr,rc,source,source2,dest,dest2,myid,ntasks
    integer :: requests(4)
    integer :: status(MPI_STATUS_SIZE,2)
    call mpi_comm_rank(MPI_COMM_WORLD, myid, rc)
    call mpi_comm_size(MPI_COMM_WORLD, ntasks, rc)



    ! TODO start: implement halo exchange
    ! Send to left, receive from right
    if((myid < ntasks-1) .and. (myid > 0)) then
      dest=myid-1
      source=myid+1
    elseif (myid < ntasks-1) then
      dest=mpi_proc_null
      source=myid+1
    elseif (myid > 0) then
      dest=myid-1
      source=mpi_proc_null
    end if
  !  call mpi_sendrecv(field0%data(:,1),field0%nx+2,mpi_double_precision,dest,11,field0%data(:,field0%ny+1), &
	!& field0%nx+2,mpi_double_precision,source,11,mpi_comm_world,mpi_status_ignore,rc)
    call mpi_isend(field0%data(:,1),field0%nx+2,mpi_double_precision,dest,11,mpi_comm_world,requests(1),rc)
    call mpi_irecv(field0%data(:,field0%ny+1),field0%nx+2,mpi_double_precision,source,11,mpi_comm_world,requests(2),rc)
    ! Send to right, receive from left
    dest2=source
    source2=dest
  !  call mpi_sendrecv(field0%data(:,field0%ny-1),field0%nx+2,mpi_double_precision,dest2,12,field0%data(:,0), &
	!& field0%nx+2,mpi_double_precision,source2,12,mpi_comm_world,mpi_status_ignore,rc)
    call mpi_isend(field0%data(:,field0%ny-1),field0%nx+2,mpi_double_precision,dest2,12,mpi_comm_world,requests(3),rc)
    call mpi_irecv(field0%data(:,0),field0%nx+2,mpi_double_precision,source2,12,mpi_comm_world,requests(4),rc)
    ! TODO end
    call mpi_waitall(size(requests),requests,status,rc)
  end subroutine exchange

  ! Compute one time step of temperature evolution
  ! Arguments:
  !   curr (type(field)): current temperature values
  !   prev (type(field)): values from previous time step
  !   a (real(dp)): update equation constant
  !   dt (real(dp)): time step value
  subroutine evolve(curr, prev, a, dt)

    implicit none

    type(field), intent(inout) :: curr, prev
    real(dp) :: a, dt
    integer :: i, j, nx, ny

    nx = curr%nx
    ny = curr%ny

    do j = 1, ny
       do i = 1, nx
          curr%data(i, j) = prev%data(i, j) + a * dt * &
               & ((prev%data(i-1, j) - 2.0 * prev%data(i, j) + &
               &   prev%data(i+1, j)) / curr%dx**2 + &
               &  (prev%data(i, j-1) - 2.0 * prev%data(i, j) + &
               &   prev%data(i, j+1)) / curr%dy**2)
       end do
    end do
  end subroutine evolve

end module core
