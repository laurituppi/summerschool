program multiple
  use mpi
  use omp_lib
  implicit none
  integer :: myid, mythread, provided, required=MPI_THREAD_MULTIPLE
  integer :: threadtag, ierr, recvthread, i, ntasks

  call mpi_init_thread(required,provided,ierr)

  if (provided>=required) then
    print*,'thread safety OK'
  else
    print*,'Arr, thread safety insufficient'
  end if

  call mpi_comm_rank(mpi_comm_world,myid,ierr)
  call mpi_comm_size(mpi_comm_world,ntasks,ierr)

  !$omp parallel default(shared) private(mythread,threadtag,ierr,i)
  mythread=omp_get_thread_num()
  threadtag=2**10+mythread
  if (myid==0) then
    do i=1,ntasks-1
      call mpi_send(mythread,1,mpi_integer,i,threadtag,mpi_comm_world,ierr)
    end do
    print*,'sender ',myid,'thread ', mythread
  else
    call mpi_recv(recvthread,1,mpi_integer,0,threadtag,mpi_comm_world,mpi_status_ignore,ierr)
    print*,'receiver ',myid,'thread ', mythread,'received ',recvthread
  end if

  !$omp end parallel

  call mpi_finalize(ierr)

end program
