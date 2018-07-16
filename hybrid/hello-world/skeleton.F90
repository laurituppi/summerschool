program hello
  use omp_lib
  use mpi
  implicit none
  integer :: my_id, tid, rc, omp_rank
  integer :: provided, required=MPI_THREAD_FUNNELED

  ! TODO: Initialize MPI with thread support.
  call mpi_init_thread(required,provided,rc)

  ! TODO: Find out the MPI rank and thread ID of each thread and print
  !       out the results.
  !$omp parallel private(omp_rank)
  call mpi_comm_rank(mpi_comm_world,my_id, rc)
  omp_rank=omp_get_thread_num()
  print*,'hello from process ', my_id, ',thread ', omp_rank
  !$omp end parallel

  ! TODO: Investigate the provided thread support level.
  if (provided>=required) then
    print*,'thread safety OK'
  else
    print*,'Arr, thread safety insufficient'
  end if

  call MPI_Finalize(rc)
end program hello
