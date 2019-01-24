!************************************************************************
! Fortran 95 OPA Nesting tools						*
!									*
!     Copyright (C) 2005 Florian Lemarié (Florian.Lemarie@imag.fr)	*
!                        Laurent Debreu (Laurent.Debreu@imag.fr)	*
!************************************************************************
!
PROGRAM create_bathy
  !
  USE variables
  USE agrif_readwrite
  !
  IMPLICIT NONE
  !
  !************************************************************************
  ! 									*
  ! PROGRAM  CREATE_BATHY						*
  !									*
  ! program to implement bathymetry interpolation to generate 		*
  ! child grid bathymetry file						*
  !									*
  ! various options :							*
  !									*
  ! 1- Interpolation directly from parent bathymetry file (z-coord)	*
  ! 2- Use new topo file in meters (for example etopo)  		*
  !									*
  ! vertical coordinates permitted : z-coord and partial steps		*
  ! sigma coordinates is not yet implemented				*
  !									*
  !Interpolation is carried out using bilinear interpolation		*
  !from the AGRIF software			*
  !************************************************************************
  !
  ! variables declaration
  !        
  !
  CHARACTER(len=80) ::   namelistname
  CHARACTER*100     ::   Childmeter_file,Childlevel_file,Child_coordinates,child_ps, Child_domcfg
  INTEGER ::   nbadd,status,narg,iargc

  call agrif_init_grids()
  
  narg = iargc()      
  IF (narg == 0) THEN
     namelistname = 'namelist.input'
  ELSE
     CALL getarg(1,namelistname)
  ENDIF
  !
  ! read input file (namelist.input)
  CALL read_namelist(namelistname)
  !      
  ! define names of child grid files
  CALL set_child_name(parent_coordinate_file,Child_coordinates) 
  IF( TRIM(parent_meshmask_file) .NE. '/NULL' )    CALL set_child_name(parent_meshmask_file,Childlevel_file)            
  IF( TRIM(parent_meshmask_file) .NE. '/NULL' )    CALL set_child_name(parent_domcfg_output,Child_domcfg) 
  !
  !                                                   ! ------------------------------------------------------------------
  IF( .NOT.new_topo .AND. .NOT.partial_steps ) THEN   ! **** First option : no new topo file & no partial steps
     !                                                !                 ==> interpolate levels directly from parent file
     !                                                ! ------------------------------------------------------------------ 
     !
     !                                                ! -----------------------------------------------------
  ELSE                                                ! **** Second option : new topo file or partial steps     
     !                                                ! ----------------------------------------------------- 
     WRITE(*,*) '***Second option : new topo or partial steps'
  ENDIF     
END PROGRAM create_bathy
  
  
