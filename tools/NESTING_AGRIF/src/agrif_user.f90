subroutine agrif_user

end subroutine agrif_user

subroutine agrif_initworkspace

end subroutine agrif_initworkspace

subroutine agrif_initvalues
use par_oce
use variables
use agrif_readwrite

integer :: status

irafx = agrif_irhox()
irafy = agrif_irhoy()
       write(*,*) 'in agrif_initvalues' 

       IF( ln_agrif_domain ) THEN
          IF(.NOT. ln_perio) THEN
             nx = (nbcellsx)+2*nbghostcellsfine+2
             ny = (nbcellsy)+2*nbghostcellsfine+2
             nbghostcellsfine_tot_x= nbghostcellsfine_x +1
             nbghostcellsfine_tot_y= nbghostcellsfine_y +1
          ELSE
            nx = (nbcellsx)+2*nbghostcellsfine_x 
            ny = (nbcellsy)+2*nbghostcellsfine+2
            nbghostcellsfine_tot_x= 1
            nbghostcellsfine_tot_y= nbghostcellsfine_y +1
         ENDIF
       ELSE
          nbghostcellsfine = 0
          nx = nbcellsx+irafx
          ny = nbcellsy+irafy
       ENDIF
       
  WRITE(*,*) ' '
  WRITE(*,*)'Size of the High resolution grid: ',nx,' x ',ny
  WRITE(*,*) ' '
       
       call agrif_grid_allocate()
       
       call agrif_declare_var()
       
       call agrif_init_lonlat()

       where (glamt < -180) glamt = glamt +360.
       if (ln_perio) then
         glamt(1,:)=glamt(nx-1,:)
         glamt(nx,:)=glamt(2,:)
       endif
 
       where (glamu < -180) glamu = glamu +360.
       if (ln_perio) then
         glamu(1,:)=glamu(nx-1,:)
         glamu(nx,:)=glamu(2,:)
       endif

      where (glamv < -180) glamv = glamv +360.
       if (ln_perio) then
         glamv(1,:)=glamv(nx-1,:)
         glamv(nx,:)=glamv(2,:)
       endif

      where (glamf < -180) glamf = glamf +360.
       if (ln_perio) then
         glamf(1,:)=glamf(nx-1,:)
         glamf(nx,:)=glamf(2,:)
       endif
        
  !              
  ! Write interpolation result in child coodinates file*

  status = Write_Coordinates(TRIM(Child_filename))      
       
end subroutine agrif_initvalues

subroutine agrif_declare_var()
use par_oce
use variables
use agrif_profiles
integer :: ind1, ind2, ind3

!!----------------------------------------------------------------------

   ! 1. Declaration of the type of variable which have to be interpolated
   !---------------------------------------------------------------------


ind2 = nbghostcellsfine_tot_x + 1
ind3 = nbghostcellsfine_tot_y + 1

CALL agrif_declare_variable((/2,2/),(/ind2,ind3/),(/'x','y'/),(/1,1/),(/nx,ny/),glamt_id)

CALL agrif_declare_variable((/1,2/),(/ind2-1,ind3/),(/'x','y'/),(/1,1/),(/nx,ny/),glamu_id)
CALL agrif_declare_variable((/2,1/),(/ind2,ind3-1/),(/'x','y'/),(/1,1/),(/nx,ny/),glamv_id)
CALL agrif_declare_variable((/1,1/),(/ind2-1,ind3-1/),(/'x','y'/),(/1,1/),(/nx,ny/),glamf_id)

CALL agrif_declare_variable((/2,2/),(/ind2,ind3/),(/'x','y'/),(/1,1/),(/nx,ny/),gphit_id)
CALL agrif_declare_variable((/1,2/),(/ind2-1,ind3/),(/'x','y'/),(/1,1/),(/nx,ny/),gphiu_id)
CALL agrif_declare_variable((/2,1/),(/ind2,ind3-1/),(/'x','y'/),(/1,1/),(/nx,ny/),gphiv_id)
CALL agrif_declare_variable((/1,1/),(/ind2-1,ind3-1/),(/'x','y'/),(/1,1/),(/nx,ny/),gphif_id)

CALL Agrif_Set_bcinterp(glamt_id,interp=AGRIF_linear)
CALL Agrif_Set_interp(glamt_id,interp=AGRIF_linear)
CALL Agrif_Set_bc( glamt_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)-1/) )

CALL Agrif_Set_bcinterp(glamu_id,interp=AGRIF_linear)
CALL Agrif_Set_interp(glamu_id,interp=AGRIF_linear)
CALL Agrif_Set_bc( glamu_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)-1/) )

CALL Agrif_Set_bcinterp(glamv_id,interp=AGRIF_linear)
CALL Agrif_Set_interp(glamv_id,interp=AGRIF_linear)
CALL Agrif_Set_bc( glamv_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)-1/) )

CALL Agrif_Set_bcinterp(glamf_id,interp=AGRIF_linear)
CALL Agrif_Set_interp(glamf_id,interp=AGRIF_linear)
CALL Agrif_Set_bc( glamf_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)-1/) )

CALL Agrif_Set_bcinterp(gphit_id,interp=AGRIF_linear)
CALL Agrif_Set_interp(gphit_id,interp=AGRIF_linear)
CALL Agrif_Set_bc( gphit_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)-1/) )

CALL Agrif_Set_bcinterp(gphiu_id,interp=AGRIF_linear)
CALL Agrif_Set_interp(gphiu_id,interp=AGRIF_linear)
CALL Agrif_Set_bc( gphiu_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)-1/) )

CALL Agrif_Set_bcinterp(gphiv_id,interp=AGRIF_linear)
CALL Agrif_Set_interp(gphiv_id,interp=AGRIF_linear)
CALL Agrif_Set_bc( gphiv_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)-1/) )

CALL Agrif_Set_bcinterp(gphif_id,interp=AGRIF_linear)
CALL Agrif_Set_interp(gphif_id,interp=AGRIF_linear)
CALL Agrif_Set_bc( gphif_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)-1/) )

end subroutine agrif_declare_var

subroutine agrif_init_lonlat()
use variables
use agrif_profiles
use agrif_util
external :: init_glamt, init_glamu, init_glamv, init_glamf
external :: init_gphit, init_gphiu, init_gphiv, init_gphif

call Agrif_Init_variable(glamt_id, procname = init_glamt)
call Agrif_Init_variable(glamu_id, procname = init_glamu)
call Agrif_Init_variable(glamv_id, procname = init_glamv)
call Agrif_Init_variable(glamf_id, procname = init_glamf)

call Agrif_Init_variable(gphit_id, procname = init_gphit)
call Agrif_Init_variable(gphiu_id, procname = init_gphiu)
call Agrif_Init_variable(gphiv_id, procname = init_gphiv)
call Agrif_Init_variable(gphif_id, procname = init_gphif)

end subroutine agrif_init_lonlat

   SUBROUTINE init_glamt( ptab, i1, i2, j1, j2, before, nb,ndir)
   use variables
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpsshn  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL, DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      INTEGER                         , INTENT(in   ) ::   nb , ndir
      LOGICAL  ::   western_side, eastern_side,northern_side,southern_side
      !
      !!----------------------------------------------------------------------  
      !
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)
         southern_side = (nb == 2).AND.(ndir == 1)
         northern_side = (nb == 2).AND.(ndir == 2)
      IF( before) THEN
         ptab(i1:i2,j1:j2) = glamt(i1:i2,j1:j2)
      ELSE
         glamt(i1:i2,j1:j2)=ptab
      ENDIF
      !
   END SUBROUTINE init_glamt

    SUBROUTINE init_glamu( ptab, i1, i2, j1, j2, before, nb,ndir)
    use variables
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpsshn  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL, DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      INTEGER                         , INTENT(in   ) ::   nb , ndir
      LOGICAL  ::   western_side, eastern_side,northern_side,southern_side
      !
      !!----------------------------------------------------------------------  
      !
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)
         southern_side = (nb == 2).AND.(ndir == 1)
         northern_side = (nb == 2).AND.(ndir == 2)
      IF( before) THEN
         ptab(i1:i2,j1:j2) = glamu(i1:i2,j1:j2)
      ELSE
         glamu(i1:i2,j1:j2)=ptab
      ENDIF
      !
   END SUBROUTINE init_glamu

   SUBROUTINE init_glamv( ptab, i1, i2, j1, j2, before, nb,ndir)
   use variables
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpsshn  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL, DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      INTEGER                         , INTENT(in   ) ::   nb , ndir
      LOGICAL  ::   western_side, eastern_side,northern_side,southern_side
      !
      !!----------------------------------------------------------------------  
      !
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)
         southern_side = (nb == 2).AND.(ndir == 1)
         northern_side = (nb == 2).AND.(ndir == 2)
      IF( before) THEN
         ptab(i1:i2,j1:j2) = glamv(i1:i2,j1:j2)
      ELSE
         glamv(i1:i2,j1:j2)=ptab
      ENDIF
      !
   END SUBROUTINE init_glamv

   SUBROUTINE init_glamf( ptab, i1, i2, j1, j2, before, nb,ndir)
   use variables
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpsshn  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL, DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      INTEGER                         , INTENT(in   ) ::   nb , ndir
      LOGICAL  ::   western_side, eastern_side,northern_side,southern_side
      !
      !!----------------------------------------------------------------------  
      !
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)
         southern_side = (nb == 2).AND.(ndir == 1)
         northern_side = (nb == 2).AND.(ndir == 2)
      IF( before) THEN
         ptab(i1:i2,j1:j2) = glamf(i1:i2,j1:j2)
      ELSE
         glamf(i1:i2,j1:j2)=ptab
      ENDIF
      !
   END SUBROUTINE init_glamf

   SUBROUTINE init_gphit( ptab, i1, i2, j1, j2, before, nb,ndir)
   use variables
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpsshn  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL, DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      INTEGER                         , INTENT(in   ) ::   nb , ndir
      LOGICAL  ::   western_side, eastern_side,northern_side,southern_side
      !
      !!----------------------------------------------------------------------  
      !
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)
         southern_side = (nb == 2).AND.(ndir == 1)
         northern_side = (nb == 2).AND.(ndir == 2)
      IF( before) THEN
         ptab(i1:i2,j1:j2) = gphit(i1:i2,j1:j2)
      ELSE
         gphit(i1:i2,j1:j2)=ptab
      ENDIF
      !
   END SUBROUTINE init_gphit

    SUBROUTINE init_gphiu( ptab, i1, i2, j1, j2, before, nb,ndir)
    use variables
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpsshn  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL, DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      INTEGER                         , INTENT(in   ) ::   nb , ndir
      LOGICAL  ::   western_side, eastern_side,northern_side,southern_side
      !
      !!----------------------------------------------------------------------  
      !
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)
         southern_side = (nb == 2).AND.(ndir == 1)
         northern_side = (nb == 2).AND.(ndir == 2)
      IF( before) THEN
         ptab(i1:i2,j1:j2) = gphiu(i1:i2,j1:j2)
      ELSE
         gphiu(i1:i2,j1:j2)=ptab
      ENDIF
      !
   END SUBROUTINE init_gphiu

    SUBROUTINE init_gphiv( ptab, i1, i2, j1, j2, before, nb,ndir)
    use variables
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpsshn  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL, DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      INTEGER                         , INTENT(in   ) ::   nb , ndir
      LOGICAL  ::   western_side, eastern_side,northern_side,southern_side
      !
      !!----------------------------------------------------------------------  
      !
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)
         southern_side = (nb == 2).AND.(ndir == 1)
         northern_side = (nb == 2).AND.(ndir == 2)
      IF( before) THEN
         ptab(i1:i2,j1:j2) = gphiv(i1:i2,j1:j2)
      ELSE
         gphiv(i1:i2,j1:j2)=ptab
      ENDIF
      !
   END SUBROUTINE init_gphiv


   SUBROUTINE init_gphif( ptab, i1, i2, j1, j2, before, nb,ndir)
   use variables
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE interpsshn  ***
      !!----------------------------------------------------------------------  
      INTEGER                         , INTENT(in   ) ::   i1, i2, j1, j2
      REAL, DIMENSION(i1:i2,j1:j2), INTENT(inout) ::   ptab
      LOGICAL                         , INTENT(in   ) ::   before
      INTEGER                         , INTENT(in   ) ::   nb , ndir
      LOGICAL  ::   western_side, eastern_side,northern_side,southern_side
      !
      !!----------------------------------------------------------------------  
      !
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)
         southern_side = (nb == 2).AND.(ndir == 1)
         northern_side = (nb == 2).AND.(ndir == 2)
      IF( before) THEN
         ptab(i1:i2,j1:j2) = gphif(i1:i2,j1:j2)
      ELSE
         gphif(i1:i2,j1:j2)=ptab
      ENDIF
      !
   END SUBROUTINE init_gphif

   
subroutine agrif_grid_allocate()
use variables

    ALLOCATE(nav_lon(nx,ny),nav_lat(nx,ny))
    !
    write(*,*) 'in grid allocate', nx,ny
    ALLOCATE(glamt(nx,ny),glamu(nx,ny),glamv(nx,ny),glamf(nx,ny))
    ALLOCATE(gphit(nx,ny),gphiu(nx,ny),gphiv(nx,ny),gphif(nx,ny))
    !
    ALLOCATE(e1t(nx,ny),e1u(nx,ny),e1v(nx,ny),e1f(nx,ny))
    ALLOCATE(e2t(nx,ny),e2u(nx,ny),e2v(nx,ny),e2f(nx,ny))
    !
    ALLOCATE(bathy_level(nx,ny))
    
end subroutine agrif_grid_allocate

SUBROUTINE Agrif_detect( kg, ksizex )
      !!----------------------------------------------------------------------
      !!                      *** ROUTINE Agrif_detect ***
      !!----------------------------------------------------------------------
   INTEGER, DIMENSION(2) :: ksizex
   INTEGER, DIMENSION(ksizex(1),ksizex(2)) :: kg 
      !!----------------------------------------------------------------------
   !
   RETURN
   !
END SUBROUTINE Agrif_detect

SUBROUTINE agrif_before_regridding
END SUBROUTINE agrif_before_regridding