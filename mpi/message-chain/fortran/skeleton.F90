program basic
  use mpi
  use iso_fortran_env, only : REAL64

  implicit none
  integer, parameter :: size = 10000000
  integer :: rc, myid, ntasks, dest, source
  integer :: message(size)
  integer :: receiveBuffer(size)
  integer :: status(MPI_STATUS_SIZE)

  real(REAL64) :: t0, t1

  call mpi_init(rc)
  call mpi_comm_rank(MPI_COMM_WORLD, myid, rc)
  call mpi_comm_size(MPI_COMM_WORLD, ntasks, rc)

  message = myid

  ! Start measuring the time spent in communication
  call mpi_barrier(mpi_comm_world, rc)
  t0 = mpi_wtime()

  ! TODO: Send and receive as defined in the assignment
!  if ( myid < ntasks-1 ) then
!     call mpi_send(message,size,mpi_integer,myid+1,myid+1,mpi_comm_world,rc)
!     write(*,'(A10,I3,A20,I8,A,I3,A,I3)') 'Sender: ', myid, &
!          ' Sent elements: ',size, &
!          '. Tag: ', myid+1, '. Receiver: ', myid+1
!  end if
!
!  if ( myid > 0 ) then
!     call mpi_recv(receiveBuffer,size,mpi_integer,myid-1,myid,mpi_comm_world,status,rc)
!     write(*,'(A10,I3,A,I3)') 'Receiver: ', myid, &
!          ' First element: ', receiveBuffer(1)
!  end if
  if((myid < ntasks-1) .and. (myid > 0)) then
    dest=myid+1
    source=myid-1
  elseif (myid < ntasks-1) then
    dest=myid+1
    source=mpi_proc_null
  elseif (myid > 0) then
    dest=mpi_proc_null
    source=myid-1
  end if

  call mpi_sendrecv(message,size,mpi_integer,dest,myid+1,receiveBuffer,size,mpi_integer,source,mpi_any_tag,mpi_comm_world,status,rc)
  write(*,'(A10,I3,A20,I8,A,I3,A,I3)') 'Sender: ', myid, &
	' Sent elements: ',size, &
	'. Tag: ', myid+1, '. Receiver: ', myid+1, &
	'status=', status
!  elseif(myid < ntasks-1) then
!     call mpi_sendrecv(message,size,mpi_integer,myid+1,myid+1,receiveBuffer,size,mpi_integer,mpi_proc_null,mpi_any_tag,mpi_comm_world,status,rc)
!     write(*,'(A10,I3,A20,I8,A,I3,A,I3)') 'Sender: ', myid, &
!          ' Sent elements: ',size, &
!          '. Tag: ', myid+1, '. Receiver: ', myid+1
!  elseif(myid > 0) then
!     call mpi_sendrecv(message,size,mpi_integer,mpi_proc_null,myid+1,receiveBuffer,size,mpi_integer,myid-1,mpi_any_tag,mpi_comm_world,status,rc)
!     write(*,'(A10,I3,A,I3)') 'Receiver: ', myid, &
!          ' First element: ', receiveBuffer(1)
!  end if

  ! Finalize measuring the time and print it out
  t1 = mpi_wtime()
  call mpi_barrier(mpi_comm_world, rc)
  call flush(6)

  write(*, '(A20, I3, A, F6.3)') 'Time elapsed in rank', myid, ':', t1-t0

  call mpi_finalize(rc)

end program basic
