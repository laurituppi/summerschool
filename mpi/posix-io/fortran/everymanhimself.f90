program pario
  use mpi
  use, intrinsic :: iso_fortran_env, only : error_unit, output_unit
  implicit none

  integer, parameter :: datasize = 64, writer_id = 0
  integer :: rc, my_id, ntasks, localsize, i
  integer, dimension(:), allocatable :: localvector
  integer, dimension(datasize) :: fullvector

  call mpi_init(rc)
  call mpi_comm_size(mpi_comm_world, ntasks, rc)
  call mpi_comm_rank(mpi_comm_world, my_id, rc)

  if (ntasks > 64) then
     write(error_unit, *) 'Maximum number of tasks is 64!'
     call mpi_abort(MPI_COMM_WORLD, -1, rc)
  end if

  if (mod(datasize, ntasks) /= 0) then
     write(error_unit,*) 'Datasize (64) should be divisible by number of tasks'
     call mpi_abort(MPI_COMM_WORLD, -1, rc)
  end if

  localsize = datasize / ntasks
  allocate(localvector(localsize))

  localvector = [(i + my_id * localsize, i=1,localsize)]

  call many_writer()

  deallocate(localvector)
  call mpi_finalize(rc)

contains

  subroutine many_writer()
    implicit none
    character(len=80) :: fname
    !character(len=1) :: id

    ! TODO: Implement a function that writers the whole array of elements
    !       to a file so that single process is responsible for the file io
    integer :: n,tag,funit
    !read(id,*) my_id
    write(fname,'(A,I0,A)') 'testdata' + my_id + '.dat'
    funit=22+my_id
    tag=773*my_id
    open(funit,file=fname,access='stream')
    write(funit) localvector
    close(funit)

  end subroutine many_writer

end program pario