module variables

     REAL,  DIMENSION(:,:),  POINTER :: nav_lon, nav_lat => NULL()
     REAL,  DIMENSION(:,:),  POINTER :: glamv, glamu, glamt, glamf => NULL()
     REAL,  DIMENSION(:,:),  POINTER :: gphit, gphiu, gphiv, gphif => NULL()
     REAL,  DIMENSION(:,:),  POINTER :: e1t, e1u, e1v, e1f => NULL()
     REAL,  DIMENSION(:,:),  POINTER :: e2t, e2u, e2v, e2f => NULL()
     REAL,  DIMENSION(:,:),  POINTER :: bathy_level => NULL()
     REAL,  DIMENSION(:,:),  POINTER :: bathy_meter => NULL()
     REAL,  DIMENSION(:,:),  POINTER :: wgt => NULL()
     REAL,  DIMENSION(:,:,:),POINTER :: fmask, umask, vmask, tmask => NULL()
     REAL,  DIMENSION(:,:,:),POINTER :: e3t_ps, e3w_ps, gdept_ps, gdepwps => NULL()
     REAL,  DIMENSION(:,:),  POINTER :: gdepw_ps => NULL()
     REAL,  DIMENSION(:),    POINTER :: gdeptht => NULL()
     INTEGER, DIMENSION(:) ,   POINTER :: time_steps => NULL()

  CHARACTER*8,DIMENSION(10) :: flxtab = (/'socliot1','socliot2','socliopl', &
       'socliocl','socliohu','socliowi','soshfldo','sohefldo','sowaflup','sofbt   '/)
  !
  !
  !**************************************************************
  ! Declaration of various input file variables (namelist.input) 
  !**************************************************************
  !
  
  INTEGER ::   irafx, irafy
  INTEGER ::   nx, ny
  

!$AGRIF_DO_NOT_TREAT
  !      
  INTEGER ::   nbghostcellsfine, imin, jmin, imax, jmax, rho, rhot
  INTEGER ::   nbghostcellsfine_x, nbghostcellsfine_y, nbghostcellsfine_tot_x, nbghostcellsfine_tot_y
  INTEGER ::   shlat
  INTEGER ::   N, type_bathy_interp
  ! 
  INTEGER ::   jpizoom, jpjzoom, npt_connect, npt_copy
  !      
  REAL*8 ::   rn_hmin
  REAL*8 ::   ppkth2, ppacr2, ppkth, ppacr, ppdzmin, pphmax, smoothing_factor, e3zps_min, e3zps_rat
  REAL*8 ::   psur, pa0, pa1, pa2, adatrj
  !       
  LOGICAL ::   ldbletanh, ln_e3_dep
  LOGICAL ::   partial_steps, smoothing, bathy_update
  LOGICAL ::   new_topo, removeclosedseas, dimg, iom_activated
  LOGICAL ::   ln_agrif_domain, ln_perio
  
  CHARACTER*100 ::   parent_meshmask_file, elevation_database, parent_bathy_meter, parent_domcfg_output
  CHARACTER*100 ::   elevation_name, parent_batmet_name
  CHARACTER*100 ::   parent_coordinate_file, restart_file, updated_parent_file, updated_parent_domcfg, restart_trc_file
  CHARACTER*100 ::   dimg_output_file, interp_type
  CHARACTER*100 :: Child_filename 
  !      
  CHARACTER(len=80) , DIMENSION(20) :: flx_Files, u_files, v_files
  CHARACTER(len=255), DIMENSION(20) :: VAR_INTERP
!$AGRIF_END_DO_NOT_TREAT

  
contains

  !************************************************************************
  !									*
  !   subroutine read_namelist						*
  !									*
  !   read variables contained in namelist.input file   			*
  !   filled in by user 							*
  !									*
  !************************************************************************
  !
  SUBROUTINE read_namelist(namelistname)
    !
    IMPLICIT NONE
    CHARACTER(len=80) :: namelistname
    CHARACTER*255 :: output
    LOGICAL :: is_it_there
    INTEGER unit_nml
    
  NAMELIST /input_output/iom_activated
  !
  NAMELIST /coarse_grid_files/parent_coordinate_file, parent_meshmask_file, parent_domcfg_output
  !      
  NAMELIST /bathymetry/new_topo, elevation_database, elevation_name, smoothing, smoothing_factor,  &
                       ln_agrif_domain, npt_connect, npt_copy, removeclosedseas, type_bathy_interp, rn_hmin      
  !      
  NAMELIST /nesting/nbghostcellsfine, ln_perio, imin, imax, jmin, jmax, rho, rhot, bathy_update, updated_parent_file, updated_parent_domcfg      
  !
  NAMELIST /vertical_grid/ppkth, ppacr, ppdzmin, pphmax, psur, pa0, pa1, N, ldbletanh, ln_e3_dep, pa2, ppkth2, ppacr2
  ! 
  NAMELIST /partial_cells/partial_steps, parent_bathy_meter, parent_batmet_name, e3zps_min, e3zps_rat      
  !
  NAMELIST /nemo_coarse_grid/ jpizoom, jpjzoom 
  !          
  NAMELIST /forcing_files/ flx_files,  u_files,  v_files 
  !           
  NAMELIST /interp/ VAR_INTERP
  !      
  NAMELIST /restart/ restart_file, shlat, dimg, dimg_output_file, adatrj, interp_type 

  NAMELIST /restart_trc/ restart_trc_file, interp_type 
  
    !      
    FLX_FILES = '/NULL'
    U_FILES = '/NULL'
    V_FILES = '/NULL'
    VAR_INTERP = 'NULL'
    unit_nml = Agrif_Get_Unit()
    !      
    INQUIRE ( FILE = namelistname , EXIST = is_it_there )      
    !
    IF ( is_it_there ) THEN 
       !
       OPEN ( FILE   = namelistname , &
            UNIT   =  unit_nml        , &
            STATUS = 'OLD'            , &
            FORM   = 'FORMATTED'      , &
            ACTION = 'READ'           , &
            ACCESS = 'SEQUENTIAL'     )
       !
       REWIND(unit_nml)
       READ (unit_nml , NML = input_output)
       READ (unit_nml , NML = coarse_grid_files)
       READ (unit_nml , NML = bathymetry)
       READ (unit_nml , NML = nesting) 
       READ (unit_nml , NML = vertical_grid)
       READ (unit_nml , NML = partial_cells)                    
       READ (unit_nml , NML = nemo_coarse_grid ) 
       READ (unit_nml , NML = forcing_files )  
       READ (unit_nml , NML = interp )   
       READ (unit_nml , NML = restart )
       READ (unit_nml , NML = restart_trc )
       CLOSE(unit_nml)
       !	
       irafx = rho
       irafy = rho
       imin = imin + jpizoom - 1
       imax = imax + jpizoom - 1
       jmin = jmin + jpjzoom - 1
       jmax = jmax + jpjzoom - 1
       !
       nbghostcellsfine_x=nbghostcellsfine     
       nbghostcellsfine_y=nbghostcellsfine 
       
       IF(ln_perio)  nbghostcellsfine_x=1  ! standard number of cells of the domain
       
       IF( ln_agrif_domain ) THEN
          IF(.NOT. ln_perio) THEN
             nbghostcellsfine_tot_x= nbghostcellsfine_x +1
             nbghostcellsfine_tot_y= nbghostcellsfine_y +1
          ELSE
            nbghostcellsfine_tot_x= 1
            nbghostcellsfine_tot_y= nbghostcellsfine_y +1
         ENDIF
       ELSE
          nbghostcellsfine = 0
       ENDIF
       
       !
    ELSE
       !
       PRINT *,'namelist file ''',TRIM(namelistname),''' not found'
       STOP 
       !
    END IF
    !
    !
  END SUBROUTINE read_namelist
  
end module variables


