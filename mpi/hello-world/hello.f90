program hello
  use mpi
  implicit none
  integer :: rc, rank, size
  call mpi_init(rc)
  call mpi_comm_rank(mpi_comm_world, rank, rc)
  call mpi_comm_size(mpi_comm_world, size, rc)
  if (rank==0) then
    print*,'hello mpi world', rank, size
  else
    print*,'hello mpi world', rank
  end if
  call mpi_finalize(rc)
end program
