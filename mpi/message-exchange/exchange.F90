program exchange
  use mpi
  implicit none
  integer, parameter :: size = 100
  integer :: rc, myid, ntasks, count
  integer :: status(MPI_STATUS_SIZE)
  integer :: message(size)
  integer :: receiveBuffer(size)

  call mpi_init(rc)
  call mpi_comm_rank(MPI_COMM_WORLD, myid, rc)
  call mpi_comm_size(MPI_COMM_WORLD, ntasks, rc)

  message = myid

  ! TODO: Implement sending and receiving as defined in the assignment
  if ( myid == 0 ) then
     receiveBuffer=0
     call mpi_send(message,size,mpi_integer,1,123,mpi_comm_world,rc)
     call mpi_recv(receiveBuffer,size,mpi_integer,1,321,mpi_comm_world,status,rc)
     write(*,'(A10,I3,A10,I3)') 'Rank: ', myid, &
          ' received ', receiveBuffer(1)
  else if (myid == 1) then
     receiveBuffer=1
     call mpi_recv(receiveBuffer,size,mpi_integer,0,123,mpi_comm_world,status,rc)
     call mpi_send(message,size,mpi_integer,0,321,mpi_comm_world,rc)
     write(*,'(A10,I3,A10,I3)') 'Rank: ', myid, &
          ' received ', receiveBuffer(1)
  end if

  call mpi_finalize(rc)

end program exchange
