program datatype_struct
  use mpi
  implicit none
  type particle
     real :: coords(3)
     integer :: charge
     character(len=2) :: label
  end type particle
  integer, parameter :: n = 1000
  integer :: i, ierror,  myid,  ntasks, tag
  type(particle) :: particles(n)

  integer, parameter :: cnt=3
  integer:: particle_mpi_type, temp_type, count
  integer:: types(cnt),blocklen(cnt)
  integer(KIND=MPI_ADDRESS_KIND) :: disp(cnt)
  integer(KIND=MPI_ADDRESS_KIND) :: lb, extent
  real(8) :: t1,t2
  

  count=3
  blocklen=[1,1,1]
  disp=1
  types=[mpi_real,mpi_integer,mpi_character]

  call MPI_INIT(ierror)
  call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierror)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, ntasks, ierror)

  ! insert some data for the particle struct
  if(myid == 0) then
     do i = 1, n
        call random_number(particles(i)%coords)
        particles(i)%charge = 54
        particles(i)%label = 'Xe'
     end do
  end if

  ! TODO: define the datatype for type particle
  call mpi_type_create_struct(count,blocklen,disp,types,temp_type,ierror)
  ! TODO: Check extent.
  ! (Not really neccessary on most systems.)
  call mpi_type_get_extent(temp_type,lb,extent,ierror)
  ! TODO: resize the particle_mpi_type if needed
  call mpi_type_create_resized(temp_type,lb,extent,ierror)
  if(extent /= disp(2)-disp(1)) then
     ! TODO: resize the particle_mpi_type if needed
     particle_mpi_type=temp_type
     call mpi_type_create_resized(particle_mpi_type,lb,extent,ierror)
  end if


  t1=MPI_WTIME()
  if(myid == 0) then
     do i=1,1000
        call MPI_SEND(particles, n, particle_mpi_type, 1, i, &
             MPI_COMM_WORLD,ierror)
     end do
  else if(myid == 1) then
     do i=1, 1000
        call MPI_RECV(particles, n, particle_mpi_type, 0, i, &
             MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
     end do
  end if
  t2=MPI_WTIME()

  write(*,*) "Time: ", myid, (t2-t1) / 1000d0
  write(*,*) "Check:", myid, particles(n)%coords(1)

  call MPI_TYPE_free(particle_mpi_type, ierror)

  call MPI_FINALIZE(ierror)


end program datatype_struct
