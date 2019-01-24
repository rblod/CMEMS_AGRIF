!************************************************************************
! Fortran 95 OPA Nesting tools						*
!									*
!     Copyright (C) 2005 Florian Lemarié (Florian.Lemarie@imag.fr)	*
!									*
!************************************************************************
!
MODULE agrif_readwrite
  !
  USE variables
  !
  IMPLICIT NONE
  !
CONTAINS
  !
  !************************************************************************
  ! 									*
  ! MODULE  AGRIF_READWRITE						*
  !									*
  ! module containing subroutine used for : 				*
  !   - Coordinates files reading/writing				*
  !   - Bathymetry files reading/writing (meter and levels)		*
  !   - Naming of child grid files					*
  !									*
  !************************************************************************
  !       
  !*****************************************************
  !   function Read_Coordinates(name,Grid)
  !*****************************************************

  INTEGER FUNCTION Read_Coordinates(name,Pacifique)
    !
    USE io_netcdf
    !    
    !  file name to open
    ! 
    CHARACTER(*) name
    LOGICAL,OPTIONAL :: Pacifique
    !
    !     
    CALL read_ncdf_var('glamt',name,glamt)
  !  write(*,*) glamt

    CALL read_ncdf_var('glamu',name,glamu)
    CALL read_ncdf_var('glamv',name,glamv)
    CALL read_ncdf_var('glamf',name,glamf)
    CALL read_ncdf_var('gphit',name,gphit)
    CALL read_ncdf_var('gphiu',name,gphiu)
    CALL read_ncdf_var('gphiv',name,gphiv)
    CALL read_ncdf_var('gphif',name,gphif)
    CALL read_ncdf_var('e1t',name,e1t)
    CALL read_ncdf_var('e1u',name,e1u)
    CALL read_ncdf_var('e1v',name,e1v)
    CALL read_ncdf_var('e1f',name,e1f)
    CALL read_ncdf_var('e2t',name,e2t)
    CALL read_ncdf_var('e2u',name,e2u)
    CALL read_ncdf_var('e2v',name,e2v)
    CALL read_ncdf_var('e2f',name,e2f)
    CALL read_ncdf_var('nav_lon',name,nav_lon)
    CALL read_ncdf_var('nav_lat',name,nav_lat)       
    ! 
    !            
    WRITE(*,*) ' '
    WRITE(*,*) 'Reading coordinates file: ',name
    WRITE(*,*) ' '
    !      
    Read_Coordinates = 1
    !      
  END FUNCTION Read_Coordinates

  !*****************************************************
  !   function Write_Coordinates(name,Grid)
  !*****************************************************

  INTEGER FUNCTION Write_Coordinates(name)
    !
    USE io_netcdf
    CHARACTER(*) name
    INTEGER :: status,ncid
    CHARACTER(len=1),DIMENSION(2) :: dimnames
    !
    status = nf90_create(name,NF90_WRITE,ncid)
    status = nf90_close(ncid)
    !           
    dimnames = (/ 'x','y' /)
    CALL write_ncdf_dim(dimnames(1),name,nx)
    CALL write_ncdf_dim(dimnames(2),name,ny)
    !      
    CALL write_ncdf_var('nav_lon',dimnames,name,nav_lon,'float')      
    CALL write_ncdf_var('nav_lat',dimnames,name,nav_lat,'float')
    !
    CALL write_ncdf_var('glamt',dimnames,name,glamt,'double')
    CALL write_ncdf_var('glamu',dimnames,name,glamu,'double')
    CALL write_ncdf_var('glamv',dimnames,name,glamv,'double')
    CALL write_ncdf_var('glamf',dimnames,name,glamf,'double')
    CALL write_ncdf_var('gphit',dimnames,name,gphit,'double')
    CALL write_ncdf_var('gphiu',dimnames,name,gphiu,'double')
    CALL write_ncdf_var('gphiv',dimnames,name,gphiv,'double')
    CALL write_ncdf_var('gphif',dimnames,name,gphif,'double')      
    CALL write_ncdf_var('e1t',dimnames,name,e1t,'double')      
    CALL write_ncdf_var('e1u',dimnames,name,e1u,'double')     
    CALL write_ncdf_var('e1v',dimnames,name,e1v,'double')      
    CALL write_ncdf_var('e1f',dimnames,name,e1f,'double')
    CALL write_ncdf_var('e2t',dimnames,name,e2t,'double')
    CALL write_ncdf_var('e2u',dimnames,name,e2u,'double')
    CALL write_ncdf_var('e2v',dimnames,name,e2v,'double')
    CALL write_ncdf_var('e2f',dimnames,name,e2f,'double')
    !      
    CALL copy_ncdf_att('nav_lon',TRIM(parent_coordinate_file),name,MINVAL(nav_lon),MAXVAL(nav_lon))
    CALL copy_ncdf_att('nav_lat',TRIM(parent_coordinate_file),name,MINVAL(nav_lat),MAXVAL(nav_lat))
    CALL copy_ncdf_att('glamt',TRIM(parent_coordinate_file),name)
    CALL copy_ncdf_att('glamu',TRIM(parent_coordinate_file),name)
    CALL copy_ncdf_att('glamv',TRIM(parent_coordinate_file),name)
    CALL copy_ncdf_att('glamf',TRIM(parent_coordinate_file),name)
    CALL copy_ncdf_att('gphit',TRIM(parent_coordinate_file),name)
    CALL copy_ncdf_att('gphiu',TRIM(parent_coordinate_file),name)
    CALL copy_ncdf_att('gphiv',TRIM(parent_coordinate_file),name)
    CALL copy_ncdf_att('gphif',TRIM(parent_coordinate_file),name)
    CALL copy_ncdf_att('e1t',TRIM(parent_coordinate_file),name)
    CALL copy_ncdf_att('e1u',TRIM(parent_coordinate_file),name)
    CALL copy_ncdf_att('e1v',TRIM(parent_coordinate_file),name)
    CALL copy_ncdf_att('e1f',TRIM(parent_coordinate_file),name)
    CALL copy_ncdf_att('e2t',TRIM(parent_coordinate_file),name)
    CALL copy_ncdf_att('e2u',TRIM(parent_coordinate_file),name)
    CALL copy_ncdf_att('e2v',TRIM(parent_coordinate_file),name)
    CALL copy_ncdf_att('e2f',TRIM(parent_coordinate_file),name)            
    !
    WRITE(*,*) ' '
    WRITE(*,*) 'Writing coordinates file: ',name
    WRITE(*,*) ' '
    !
    Write_Coordinates = 1
    !      
  END FUNCTION Write_Coordinates
  
  !*****************************************************
  !   function set_child_name(Parentname,Childname)
  !*****************************************************
  !
  SUBROUTINE set_child_name(Parentname,Childname)
    !
    CHARACTER(*),INTENT(in) :: Parentname
    CHARACTER(*),INTENT(out) :: Childname
    CHARACTER(2) :: prefix
    INTEGER :: pos
    !   
    pos  = INDEX(TRIM(Parentname),'/',back=.TRUE.)
    !
    prefix=Parentname(pos+1:pos+2)
    IF (prefix == '1_') THEN
       Childname = '2_'//Parentname(pos+3:LEN(Parentname)) 
    ELSEIF (prefix == '2_') THEN
       Childname = '3_'//Parentname(pos+3:LEN(Parentname)) 
    ELSEIF (prefix == '3_') THEN
       Childname = '4_'//Parentname(pos+3:LEN(Parentname)) 
    ELSEIF (prefix == '4_') THEN
       Childname = '5_'//Parentname(pos+3:LEN(Parentname)) 
    ELSE
       Childname = '1_'//Parentname(pos+1:LEN(Parentname)) 
    ENDIF
    !   
  END SUBROUTINE set_child_name
  !
  
END MODULE agrif_readwrite