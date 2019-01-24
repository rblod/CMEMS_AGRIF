program create_coordinates
use agrif_util
use par_oce
use variables
use io_netcdf
use agrif_readwrite

integer :: narg,iargc
character(len=80) :: namelistname

integer :: status
integer :: i,j
real,dimension(:,:),allocatable :: temp_array
  
call agrif_init_grids()

  narg = iargc()

  IF (narg == 0) THEN
     namelistname = 'namelist.input'
  ELSE
     CALL getarg(1,namelistname)
  ENDIF

  ! read input file (namelist.input)
CALL read_namelist(namelistname)
 
 
  ! read parent coodinates file
  status = Read_Coordinates(TRIM(parent_coordinate_file))

  ! 
  ! define name of child coordinate file
  CALL set_child_name(parent_coordinate_file,Child_filename) 
   print *,'Child_filename = ',Child_filename
! Deduce root grid sizes

nx = SIZE(glamt,1)
ny = SIZE(glamt,2)

  IF( imax > SIZE(glamt,1) .OR. jmax > SIZE(glamt,2) .OR. imax <= imin .OR. jmax <= jmin ) THEN                    
     WRITE(*,*) 'ERROR ***** bad child grid definition ...'
     WRITE(*,*) 'please check imin,jmin,imax,jmax,jpizoom,jpjzoom values'       
     WRITE(*,*) ' '
     STOP
  ENDIF
  !                      


    !
  !  check potential longitude problems 
  print *,'CHECK ',glamt(imin,jmin) , glamt(imax,jmax)
  allocate(temp_array(nx,ny))
  print *,'size = ',size(temp_array)
  temp_array=glamt
  do j=1,ny
  i=nx-1
  do while (i>=1)
    if ((temp_array(i+1,j)<0).and.(temp_array(i,j)>=0)) then
      do while ((temp_array(i,j)>=0).AND.(i>=1))
        glamt(i,j)=glamt(i,j)-360
        i=i-1
        if (i==0) exit
      enddo
    endif
    i=i-1
  enddo
  enddo
    temp_array=glamu
  do j=1,ny
  i=nx-1
  do while (i>=1)
    if ((temp_array(i+1,j)<0).and.(temp_array(i,j)>=0)) then
      do while ((temp_array(i,j)>=0).AND.(i>=1))
        glamu(i,j)=glamu(i,j)-360
        i=i-1
        if (i==0) exit
      enddo
    endif
    i=i-1
  enddo
  enddo
  temp_array=glamv
  do j=1,ny
  i=nx-1
  do while (i>=1)
    if ((temp_array(i+1,j)<0).and.(temp_array(i,j)>=0)) then
      do while ((temp_array(i,j)>=0).AND.(i>=1))
        glamv(i,j)=glamv(i,j)-360
        i=i-1
        if (i==0) exit
      enddo
    endif
    i=i-1
  enddo
  enddo
  temp_array=glamf
  do j=1,ny
  i=nx-1
  do while (i>=1)
    if ((temp_array(i+1,j)<0).and.(temp_array(i,j)>=0)) then
      do while ((temp_array(i,j)>=0).AND.(i>=1))
        glamf(i,j)=glamf(i,j)-360
        i=i-1
        if (i==0) exit
      enddo
    endif
    i=i-1
  enddo
  enddo

  deallocate(temp_array)
  IF( glamt(imin,jmin) > glamt(imax,jmax) ) THEN    
   print *,'CHECK2 '  
     ! WHERE ( glamt < 0 ) glamt = glamt + 360.
     ! WHERE ( glamf < 0 ) glamf = glamf + 360.
  ENDIF

  call agrif_declare_var()
  
  call agrif_regrid()
stop
end
