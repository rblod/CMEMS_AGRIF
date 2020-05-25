#if defined key_agrif
subroutine agrif_initworkspace()
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_InitWorkspace ***
      !!----------------------------------------------------------------------
end subroutine agrif_initworkspace
SUBROUTINE Agrif_InitValues
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_InitValues ***
      !!
      !! ** Purpose :: Declaration of variables to be interpolated
      !!----------------------------------------------------------------------
   USE Agrif_Util
   USE oce
   USE dom_oce
   USE nemogcm
   USE domain
   !!
   IMPLICIT NONE

   ! No temporal refinement
   call Agrif_Set_coeffreft(1)

   CALL nemo_init       !* Initializations of each fine grid

   CALL dom_nam
   CALL cfg_write         ! create the configuration file

END SUBROUTINE Agrif_InitValues

SUBROUTINE Agrif_InitValues_cont
use dom_oce
use lbclnk

    integer :: irafx, irafy
    logical :: ln_perio
    integer nx,ny

irafx = agrif_irhox()
irafy = agrif_irhoy()

nx=nlci ; ny=nlcj

   !       IF(jperio /=1 .AND. jperio/=4 .AND. jperio/=6 ) THEN
   !          nx = (nbcellsx)+2*nbghostcellsfine+2
   !          ny = (nbcellsy)+2*nbghostcellsfine+2
   !          nbghostcellsfine_tot_x= nbghostcellsfine_x +1
   !          nbghostcellsfine_tot_y= nbghostcellsfine_y +1
   !       ELSE
   !         nx = (nbcellsx)+2*nbghostcellsfine_x
   !         ny = (nbcellsy)+2*nbghostcellsfine+2
   !         nbghostcellsfine_tot_x= 1
   !         nbghostcellsfine_tot_y= nbghostcellsfine_y +1
   !      ENDIF
   !    ELSE
   !       nbghostcellsfine = 0
   !       nx = nbcellsx+irafx
   !       ny = nbcellsy+irafy

  WRITE(*,*) ' '
  WRITE(*,*)'Size of the High resolution grid: ',nx,' x ',ny
  WRITE(*,*) ' '

       call agrif_init_lonlat()
       ln_perio=.FALSE.
       if( jperio ==1 .OR. jperio==2 .OR. jperio==4) ln_perio=.TRUE.

       where (glamt < -180) glamt = glamt +360.
       where (glamt > +180) glamt = glamt -360.

       CALL lbc_lnk( 'glamt', glamt, 'T', 1._wp)
       CALL lbc_lnk( 'gphit', gphit, 'T', 1._wp)


       where (glamu < -180) glamu = glamu +360.
       where (glamu > +180) glamu = glamu -360.
       CALL lbc_lnk( 'glamu', glamu, 'U', 1._wp)
       CALL lbc_lnk( 'gphiu', gphiu, 'U', 1._wp)

       where (glamv < -180) glamv = glamv +360.
       where (glamv > +180) glamv = glamv -360.
       CALL lbc_lnk( 'glamv', glamv, 'V', 1._wp)
       CALL lbc_lnk( 'gphiv', gphiv, 'V', 1._wp)

       where (glamf < -180) glamf = glamf +360.
       where (glamf > +180) glamf = glamf -360.
       CALL lbc_lnk( 'glamf', glamf, 'F', 1._wp)
       CALL lbc_lnk( 'gphif', gphif, 'F', 1._wp)

       ! Correct South and North
       IF ((nbondj == -1).OR.(nbondj == 2)) THEN
         glamt(:,1)=glamt(:,2)
         gphit(:,1)=gphit(:,2)
         glamu(:,1)=glamu(:,2)
         gphiu(:,1)=gphiu(:,2)
         glamv(:,1)=glamv(:,2)
         gphiv(:,1)=gphiv(:,2)
       ENDIF
       IF ((nbondj == 1).OR.(nbondj == 2)) THEN
         glamt(:,jpj)=glamt(:,jpj-1)
         gphit(:,jpj)=gphit(:,jpj-1)
         glamu(:,jpj)=glamu(:,jpj-1)
         gphiu(:,jpj)=gphiu(:,jpj-1)
         glamv(:,jpj)=glamv(:,jpj-1)
         gphiv(:,jpj)=gphiv(:,jpj-1)
         glamf(:,jpj)=glamf(:,jpj-1)
         gphif(:,jpj)=gphif(:,jpj-1)
       ENDIF

       ! Correct West and East
       IF (jperio /=1) THEN
       IF ((nbondi == -1).OR.(nbondi == 2)) THEN
         glamt(1,:)=glamt(2,:)
         gphit(1,:)=gphit(2,:)
         glamu(1,:)=glamu(2,:)
         gphiu(1,:)=gphiu(2,:)
         glamv(1,:)=glamv(2,:)
         gphiv(1,:)=gphiv(2,:)
       ENDIF
       IF ((nbondi == 1).OR.(nbondi == 2)) THEN
         glamt(jpi,:)=glamt(jpi-1,:)
         gphit(jpi,:)=gphit(jpi-1,:)
         glamu(jpi,:)=glamu(jpi-1,:)
         gphiu(jpi,:)=gphiu(jpi-1,:)
         glamv(jpi,:)=glamv(jpi-1,:)
         gphiv(jpi,:)=gphiv(jpi-1,:)
         glamf(jpi,:)=glamf(jpi-1,:)
         gphif(jpi,:)=gphif(jpi-1,:)
       ENDIF
       ENDIF

       call agrif_init_scales()

       ! Correct South and North
       IF ((nbondj == -1).OR.(nbondj == 2)) THEN
         e1t(:,1)=e1t(:,2)
         e2t(:,1)=e2t(:,2)
         e1u(:,1)=e1u(:,2)
         e2u(:,1)=e2u(:,2)
         e1v(:,1)=e1v(:,2)
         e2v(:,1)=e2v(:,2)
       ENDIF
       IF ((nbondj == 1).OR.(nbondj == 2)) THEN
         e1t(:,jpj)=e1t(:,jpj-1)
         e2t(:,jpj)=e2t(:,jpj-1)
         e1u(:,jpj)=e1u(:,jpj-1)
         e2u(:,jpj)=e2u(:,jpj-1)
         e1v(:,jpj)=e1v(:,jpj-1)
         e2v(:,jpj)=e2v(:,jpj-1)
         e1f(:,jpj)=e1f(:,jpj-1)
         e2f(:,jpj)=e2f(:,jpj-1)
       ENDIF

       ! Correct West and East
       IF (jperio /=1) THEN
       IF ((nbondj == -1).OR.(nbondj == 2)) THEN
         e1t(1,:)=e1t(2,:)
         e2t(1,:)=e2t(2,:)
         e1u(1,:)=e1u(2,:)
         e2u(1,:)=e2u(2,:)
         e1v(1,:)=e1v(2,:)
         e2v(1,:)=e2v(2,:)
       ENDIF
       IF ((nbondj == 1).OR.(nbondj == 2)) THEN
         e1t(jpi,:)=e1t(jpi-1,:)
         e2t(jpi,:)=e2t(jpi-1,:)
         e1u(jpi,:)=e1u(jpi-1,:)
         e2u(jpi,:)=e2u(jpi-1,:)
         e1v(jpi,:)=e1v(jpi-1,:)
         e2v(jpi,:)=e2v(jpi-1,:)
         e1f(jpi,:)=e1f(jpi-1,:)
         e2f(jpi,:)=e2f(jpi-1,:)
       ENDIF
       ENDIF

END SUBROUTINE Agrif_InitValues_cont


subroutine agrif_declare_var()
use par_oce
use dom_oce
use agrif_profiles
use agrif_parameters

   IMPLICIT NONE

integer :: ind1, ind2, ind3
integer nx,ny
integer nbghostcellsfine_tot_x, nbghostcellsfine_tot_y
INTEGER :: irafx

External :: nemo_mapping
!!----------------------------------------------------------------------

   ! 1. Declaration of the type of variable which have to be interpolated
   !---------------------------------------------------------------------

nx=nlci ; ny=nlcj

ind2 = 2 + nbghostcells_x
ind3 = 2 + nbghostcells_y_s
nbghostcellsfine_tot_x=nbghostcells_x+1
nbghostcellsfine_tot_y=max(nbghostcells_y_s,nbghostcells_y_n)+1

irafx = Agrif_irhox()

! In case of East-West periodicity, prevent AGRIF interpolation at east and west boundaries
! The procnames will not be called at these boundaries
if (jperio == 1) then
  call Agrif_Set_NearCommonBorderX(.TRUE.)
  call Agrif_Set_DistantCommonBorderX(.TRUE.)
endif
if (.not.south_boundary_open) then
  call Agrif_Set_NearCommonBorderY(.TRUE.)
endif

CALL agrif_declare_variable((/2,2/),(/ind2,ind3/),(/'x','y'/),(/1,1/),(/nx,ny/),glamt_id)
CALL agrif_declare_variable((/1,2/),(/ind2-1,ind3/),(/'x','y'/),(/1,1/),(/nx,ny/),glamu_id)
CALL agrif_declare_variable((/2,1/),(/ind2,ind3-1/),(/'x','y'/),(/1,1/),(/nx,ny/),glamv_id)
CALL agrif_declare_variable((/1,1/),(/ind2-1,ind3-1/),(/'x','y'/),(/1,1/),(/nx,ny/),glamf_id)

CALL agrif_declare_variable((/2,2/),(/ind2,ind3/),(/'x','y'/),(/1,1/),(/nx,ny/),gphit_id)
CALL agrif_declare_variable((/1,2/),(/ind2-1,ind3/),(/'x','y'/),(/1,1/),(/nx,ny/),gphiu_id)
CALL agrif_declare_variable((/2,1/),(/ind2,ind3-1/),(/'x','y'/),(/1,1/),(/nx,ny/),gphiv_id)
CALL agrif_declare_variable((/1,1/),(/ind2-1,ind3-1/),(/'x','y'/),(/1,1/),(/nx,ny/),gphif_id)

! Horizontal scale factors

CALL agrif_declare_variable((/2,2/),(/ind2,ind3/),(/'x','y'/),(/1,1/),(/nx,ny/),e1t_id)
CALL agrif_declare_variable((/1,2/),(/ind2-1,ind3/),(/'x','y'/),(/1,1/),(/nx,ny/),e1u_id)
CALL agrif_declare_variable((/2,1/),(/ind2,ind3-1/),(/'x','y'/),(/1,1/),(/nx,ny/),e1v_id)
CALL agrif_declare_variable((/1,1/),(/ind2-1,ind3-1/),(/'x','y'/),(/1,1/),(/nx,ny/),e1f_id)

CALL agrif_declare_variable((/2,2/),(/ind2,ind3/),(/'x','y'/),(/1,1/),(/nx,ny/),e2t_id)
CALL agrif_declare_variable((/1,2/),(/ind2-1,ind3/),(/'x','y'/),(/1,1/),(/nx,ny/),e2u_id)
CALL agrif_declare_variable((/2,1/),(/ind2,ind3-1/),(/'x','y'/),(/1,1/),(/nx,ny/),e2v_id)
CALL agrif_declare_variable((/1,1/),(/ind2-1,ind3-1/),(/'x','y'/),(/1,1/),(/nx,ny/),e2f_id)

! Bathymetry

CALL agrif_declare_variable((/2,2/),(/ind2,ind3/),(/'x','y'/),(/1,1/),(/nx,ny/),bathy_id)

! Vertical scale factors
CALL agrif_declare_variable((/2,2,0/),(/ind2,ind3,0/),(/'x','y','N'/),(/1,1,1/),(/nx,ny,jpk/),e3t_id)
CALL agrif_declare_variable((/2,2,0/),(/ind2,ind3,0/),(/'x','y','N'/),(/1,1,1/),(/nx,ny,jpk/),e3t_copy_id)
CALL agrif_declare_variable((/2,2,0/),(/ind2,ind3,0/),(/'x','y','N'/),(/1,1,1/),(/nx,ny,jpk+1/),e3t_connect_id)

CALL agrif_declare_variable((/1,2,0/),(/ind2-1,ind3,0/),(/'x','y','N'/),(/1,1,1/),(/nx,ny,jpk/),e3u_id)
CALL agrif_declare_variable((/2,1,0/),(/ind2,ind3-1,0/),(/'x','y','N'/),(/1,1,1/),(/nx,ny,jpk/),e3v_id)

! Bottom level

CALL agrif_declare_variable((/2,2/),(/ind2,ind3/),(/'x','y'/),(/1,1/),(/nx,ny/),bottom_level_id)

CALL Agrif_Set_bcinterp(glamt_id,interp=AGRIF_linear)
CALL Agrif_Set_interp(glamt_id,interp=AGRIF_linear)
CALL Agrif_Set_bc( glamt_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)-1/) )

CALL Agrif_Set_bcinterp(glamu_id,interp=AGRIF_linear)
CALL Agrif_Set_interp(glamu_id,interp=AGRIF_linear)
CALL Agrif_Set_bc( glamu_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)/) )

CALL Agrif_Set_bcinterp(glamv_id,interp=AGRIF_linear)
CALL Agrif_Set_interp(glamv_id,interp=AGRIF_linear)
CALL Agrif_Set_bc( glamv_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)/) )

CALL Agrif_Set_bcinterp(glamf_id,interp=AGRIF_linear)
CALL Agrif_Set_interp(glamf_id,interp=AGRIF_linear)
CALL Agrif_Set_bc( glamf_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)/) )

CALL Agrif_Set_bcinterp(gphit_id,interp=AGRIF_linear)
CALL Agrif_Set_interp(gphit_id,interp=AGRIF_linear)
CALL Agrif_Set_bc( gphit_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)-1/) )

CALL Agrif_Set_bcinterp(gphiu_id,interp=AGRIF_linear)
CALL Agrif_Set_interp(gphiu_id,interp=AGRIF_linear)
CALL Agrif_Set_bc( gphiu_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)/) )

CALL Agrif_Set_bcinterp(gphiv_id,interp=AGRIF_linear)
CALL Agrif_Set_interp(gphiv_id,interp=AGRIF_linear)
CALL Agrif_Set_bc( gphiv_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)/) )

CALL Agrif_Set_bcinterp(gphif_id,interp=AGRIF_linear)
CALL Agrif_Set_interp(gphif_id,interp=AGRIF_linear)
CALL Agrif_Set_bc( gphif_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)/) )

!

CALL Agrif_Set_bcinterp(e1t_id,interp=AGRIF_ppm)
CALL Agrif_Set_interp(e1t_id,interp=AGRIF_ppm)
CALL Agrif_Set_bc( e1t_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)-1/) )

CALL Agrif_Set_bcinterp(e1u_id, interp1=Agrif_linear, interp2=AGRIF_ppm)
CALL Agrif_Set_interp(e1u_id, interp1=Agrif_linear, interp2=AGRIF_ppm)
CALL Agrif_Set_bc( e1u_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)/) )

CALL Agrif_Set_bcinterp(e1v_id,interp1=AGRIF_ppm, interp2=Agrif_linear)
CALL Agrif_Set_interp(e1v_id, interp1=AGRIF_ppm, interp2=Agrif_linear)
CALL Agrif_Set_bc( e1v_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)/) )

CALL Agrif_Set_bcinterp(e1f_id,interp=AGRIF_linear)
CALL Agrif_Set_interp(e1f_id,interp=AGRIF_linear)
CALL Agrif_Set_bc( e1f_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)/) )

CALL Agrif_Set_bcinterp(e2t_id,interp=AGRIF_ppm)
CALL Agrif_Set_interp(e2t_id,interp=AGRIF_ppm)
CALL Agrif_Set_bc( e2t_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)-1/) )

CALL Agrif_Set_bcinterp(e2u_id,interp1=Agrif_linear, interp2=AGRIF_ppm)
CALL Agrif_Set_interp(e2u_id,interp1=Agrif_linear, interp2=AGRIF_ppm)
CALL Agrif_Set_bc( e2u_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)/) )

CALL Agrif_Set_bcinterp(e2v_id,interp1=AGRIF_ppm, interp2=Agrif_linear)
CALL Agrif_Set_interp(e2v_id,interp1=AGRIF_ppm, interp2=Agrif_linear)
CALL Agrif_Set_bc( e2v_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)/) )

CALL Agrif_Set_bcinterp(e2f_id,interp=AGRIF_linear)
CALL Agrif_Set_interp(e2f_id,interp=AGRIF_linear)
CALL Agrif_Set_bc( e2f_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)/) )

CALL Agrif_Set_bcinterp(bathy_id,interp=AGRIF_linear)
CALL Agrif_Set_interp(bathy_id,interp=AGRIF_linear)
CALL Agrif_Set_bc( bathy_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)-1/) )

! Vertical scale factors
CALL Agrif_Set_bcinterp(e3t_id,interp=AGRIF_ppm)
CALL Agrif_Set_interp(e3t_id,interp=AGRIF_ppm)
CALL Agrif_Set_bc( e3t_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)-1/) )
CALL Agrif_Set_Updatetype( e3t_id, update = AGRIF_Update_Average)

CALL Agrif_Set_bcinterp(e3t_copy_id,interp=AGRIF_constant)
CALL Agrif_Set_interp(e3t_copy_id,interp=AGRIF_constant)
CALL Agrif_Set_bc( e3t_copy_id, (/-npt_copy*irafx,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)-1/))

CALL Agrif_Set_bcinterp(e3t_connect_id,interp=AGRIF_ppm)
CALL Agrif_Set_interp(e3t_connect_id,interp=AGRIF_ppm)
CALL Agrif_Set_bc( e3t_connect_id, (/-(npt_copy+npt_connect)*irafx-1,-npt_copy*irafx/))

CALL Agrif_Set_bcinterp(e3u_id, interp1=Agrif_linear, interp2=AGRIF_ppm)
CALL Agrif_Set_interp(e3u_id, interp1=Agrif_linear, interp2=AGRIF_ppm)
CALL Agrif_Set_bc( e3u_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)-1/) )
CALL Agrif_Set_Updatetype(e3u_id,update1 = Agrif_Update_Copy, update2 = Agrif_Update_Average)

CALL Agrif_Set_bcinterp(e3v_id,interp1=AGRIF_ppm, interp2=Agrif_linear)
CALL Agrif_Set_interp(e3v_id, interp1=AGRIF_ppm, interp2=Agrif_linear)
CALL Agrif_Set_bc( e3v_id, (/0,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)-1/) )
CALL Agrif_Set_Updatetype(e3v_id,update1 = Agrif_Update_Average, update2 = Agrif_Update_Copy)

! Bottom level
CALL Agrif_Set_bcinterp(bottom_level_id,interp=AGRIF_constant)
CALL Agrif_Set_interp(bottom_level_id,interp=AGRIF_constant)
CALL Agrif_Set_bc( bottom_level_id, (/-npt_copy*irafx,max(nbghostcellsfine_tot_x,nbghostcellsfine_tot_y)-1/))
CALL Agrif_Set_Updatetype( bottom_level_id, update = AGRIF_Update_Max)

Call Agrif_Set_ExternalMapping(nemo_mapping)

end subroutine agrif_declare_var

subroutine nemo_mapping(ndim,ptx,pty,bounds,bounds_chunks,correction_required,nb_chunks)
use dom_oce
integer :: ndim
integer :: ptx, pty
integer,dimension(ndim,2,2) :: bounds
integer,dimension(:,:,:,:),allocatable :: bounds_chunks
logical,dimension(:),allocatable :: correction_required
integer :: nb_chunks
integer :: i

if (agrif_debug_interp) then
do i=1,ndim
 print *,'direction = ',i,bounds(i,1,2),bounds(i,2,2)
enddo
endif

  if (bounds(2,2,2) > jpjglo) then
   if (bounds(2,1,2) <=jpjglo) then
    nb_chunks = 2
    allocate(bounds_chunks(nb_chunks,ndim,2,2))
    allocate(correction_required(nb_chunks))
         do i=1,nb_chunks
          bounds_chunks(i,:,:,:) = bounds
        enddo

! FIRST CHUNCK (for j<=jpjglo)

! Original indices
    bounds_chunks(1,1,1,1) = bounds(1,1,2)
    bounds_chunks(1,1,2,1) = bounds(1,2,2)
    bounds_chunks(1,2,1,1) = bounds(2,1,2)
    bounds_chunks(1,2,2,1) = jpjglo


    bounds_chunks(1,1,1,2) = bounds(1,1,2)
    bounds_chunks(1,1,2,2) = bounds(1,2,2)
    bounds_chunks(1,2,1,2) = bounds(2,1,2)
    bounds_chunks(1,2,2,2) = jpjglo

! Correction required or not
    correction_required(1)=.FALSE.

! SECOND CHUNCK (for j>jpjglo)

! Original indices
    bounds_chunks(2,1,1,1) = bounds(1,1,2)
    bounds_chunks(2,1,2,1) = bounds(1,2,2)
    bounds_chunks(2,2,1,1) = jpjglo-2
    bounds_chunks(2,2,2,1) = bounds(2,2,2)

! Where to find them
! We use the relation TAB(ji,jj)=TAB(jpiglo-ji+2,jpjglo-2-(jj-jpjglo))

    if (ptx == 2) then ! T, V points
      bounds_chunks(2,1,1,2) = jpiglo-bounds(1,2,2)+2
      bounds_chunks(2,1,2,2) = jpiglo-bounds(1,1,2)+2
    else ! U, F points
      bounds_chunks(2,1,1,2) = jpiglo-bounds(1,2,2)+1
      bounds_chunks(2,1,2,2) = jpiglo-bounds(1,1,2)+1
    endif

    if (pty == 2) then ! T, U points
      bounds_chunks(2,2,1,2) = jpjglo-2-(bounds(2,2,2) -jpjglo)
      bounds_chunks(2,2,2,2) = jpjglo-2-(jpjglo-2      -jpjglo)
    else ! V, F points
      bounds_chunks(2,2,1,2) = jpjglo-3-(bounds(2,2,2) -jpjglo)
      bounds_chunks(2,2,2,2) = jpjglo-3-(jpjglo-2      -jpjglo)
    endif
! Correction required or not
    correction_required(2)=.TRUE.

   else
    nb_chunks = 1
    allocate(bounds_chunks(nb_chunks,ndim,2,2))
    allocate(correction_required(nb_chunks))
         do i=1,nb_chunks
          bounds_chunks(i,:,:,:) = bounds
        enddo

    bounds_chunks(1,1,1,1) = bounds(1,1,2)
    bounds_chunks(1,1,2,1) = bounds(1,2,2)
    bounds_chunks(1,2,1,1) = bounds(2,1,2)
    bounds_chunks(1,2,2,1) = bounds(2,2,2)

    bounds_chunks(1,1,1,2) = jpiglo-bounds(1,2,2)+2
    bounds_chunks(1,1,2,2) = jpiglo-bounds(1,1,2)+2

    bounds_chunks(1,2,1,2) = jpjglo-2-(bounds(2,2,2)-jpjglo)
    bounds_chunks(1,2,2,2) = jpjglo-2-(bounds(2,1,2)-jpjglo)

    if (ptx == 2) then ! T, V points
      bounds_chunks(1,1,1,2) = jpiglo-bounds(1,2,2)+2
      bounds_chunks(1,1,2,2) = jpiglo-bounds(1,1,2)+2
    else ! U, F points
      bounds_chunks(1,1,1,2) = jpiglo-bounds(1,2,2)+1
      bounds_chunks(1,1,2,2) = jpiglo-bounds(1,1,2)+1
    endif

    if (pty == 2) then ! T, U points
      bounds_chunks(1,2,1,2) = jpjglo-2-(bounds(2,2,2) -jpjglo)
      bounds_chunks(1,2,2,2) = jpjglo-2-(bounds(2,1,2) -jpjglo)
    else ! V, F points
      bounds_chunks(1,2,1,2) = jpjglo-3-(bounds(2,2,2) -jpjglo)
      bounds_chunks(1,2,2,2) = jpjglo-3-(bounds(2,1,2) -jpjglo)
    endif

    correction_required(1)=.TRUE.


   endif
  elseif (bounds(1,1,2) < 1) then
   if (bounds(1,2,2) > 0) then
    nb_chunks = 2
    allocate(correction_required(nb_chunks))
    correction_required=.FALSE.
    allocate(bounds_chunks(nb_chunks,ndim,2,2))
         do i=1,nb_chunks
          bounds_chunks(i,:,:,:) = bounds
        enddo

    bounds_chunks(1,1,1,2) = bounds(1,1,2)+jpiglo-2
    bounds_chunks(1,1,2,2) = 1+jpiglo-2

    bounds_chunks(1,1,1,1) = bounds(1,1,2)
    bounds_chunks(1,1,2,1) = 1

    bounds_chunks(2,1,1,2) = 2
    bounds_chunks(2,1,2,2) = bounds(1,2,2)

    bounds_chunks(2,1,1,1) = 2
    bounds_chunks(2,1,2,1) = bounds(1,2,2)

   else
    nb_chunks = 1
    allocate(correction_required(nb_chunks))
    correction_required=.FALSE.
    allocate(bounds_chunks(nb_chunks,ndim,2,2))
         do i=1,nb_chunks
          bounds_chunks(i,:,:,:) = bounds
        enddo
    bounds_chunks(1,1,1,2) = bounds(1,1,2)+jpiglo-2
    bounds_chunks(1,1,2,2) = bounds(1,2,2)+jpiglo-2

    bounds_chunks(1,1,1,1) = bounds(1,1,2)
    bounds_chunks(1,1,2,1) = bounds(1,2,2)
   endif
  else
    nb_chunks=1
    allocate(correction_required(nb_chunks))
    correction_required=.FALSE.
    allocate(bounds_chunks(nb_chunks,ndim,2,2))
         do i=1,nb_chunks
          bounds_chunks(i,:,:,:) = bounds
        enddo
    bounds_chunks(1,1,1,2) = bounds(1,1,2)
    bounds_chunks(1,1,2,2) = bounds(1,2,2)
    bounds_chunks(1,2,1,2) = bounds(2,1,2)
    bounds_chunks(1,2,2,2) = bounds(2,2,2)

    bounds_chunks(1,1,1,1) = bounds(1,1,2)
    bounds_chunks(1,1,2,1) = bounds(1,2,2)
    bounds_chunks(1,2,1,1) = bounds(2,1,2)
    bounds_chunks(1,2,2,1) = bounds(2,2,2)

  endif

end subroutine nemo_mapping

function agrif_external_switch_index(ptx,pty,i1,isens)
use dom_oce
integer :: ptx, pty, i1, isens
integer :: agrif_external_switch_index

 if (isens == 1) then
    if (ptx == 2) then ! T, V points
      agrif_external_switch_index = jpiglo-i1+2
    else ! U, F points
      agrif_external_switch_index = jpiglo-i1+1
    endif
elseif (isens ==2) then
    if (pty == 2) then ! T, U points
      agrif_external_switch_index = jpjglo-2-(i1 -jpjglo)
    else ! V, F points
      agrif_external_switch_index = jpjglo-3-(i1 -jpjglo)
    endif
endif

end function agrif_external_switch_index

subroutine correct_field(tab2d,i1,i2,j1,j2)
use dom_oce
integer :: i1,i2,j1,j2
real,dimension(i1:i2,j1:j2) :: tab2d

integer :: i,j
real,dimension(i1:i2,j1:j2) :: tab2dtemp

tab2dtemp = tab2d

do j=j1,j2
do i=i1,i2
  tab2d(i,j)=tab2dtemp(i2-(i-i1),j2-(j-j1))
enddo
enddo


end subroutine correct_field

subroutine agrif_init_lonlat()
use agrif_profiles
use agrif_util
use dom_oce
external :: init_glamt, init_glamu, init_glamv, init_glamf
external :: init_gphit, init_gphiu, init_gphiv, init_gphif
integer :: ji,jj,i1,i2,j1,j2
real,dimension(jpi,jpj) :: tab2dtemp
integer :: ind2,ind3
integer :: irhox, irhoy
external :: longitude_linear_interp

irhox = agrif_irhox()
irhoy = agrif_irhoy()
call Agrif_Set_external_linear_interp(longitude_linear_interp)

call Agrif_Init_variable(glamt_id, procname = init_glamt)
call Agrif_Init_variable(glamu_id, procname = init_glamu)
call Agrif_Init_variable(glamv_id, procname = init_glamv)
call Agrif_Init_variable(glamf_id, procname = init_glamf)
call Agrif_UnSet_external_linear_interp()

call Agrif_Init_variable(gphit_id, procname = init_gphit)
call Agrif_Init_variable(gphiu_id, procname = init_gphiu)
call Agrif_Init_variable(gphiv_id, procname = init_gphiv)
call Agrif_Init_variable(gphif_id, procname = init_gphif)

end subroutine agrif_init_lonlat

real function longitude_linear_interp(x1,x2,coeff)
real :: x1, x2, coeff
real :: val_interp

if ((x1*x2 <= -50*50)) then
	if (x1 < 0) then
		val_interp = coeff *(x1+360.) + (1.-coeff) *x2
	else
		val_interp = coeff *x1 + (1.-coeff) *(x2+360.)
	endif
	if ((val_interp) >=180.) val_interp = val_interp - 360.
else
	val_interp = coeff * x1 + (1.-coeff) * x2
endif
longitude_linear_interp = val_interp
end function longitude_linear_interp

subroutine agrif_init_scales()
use agrif_profiles
use agrif_util
use dom_oce
use lbclnk
logical :: ln_perio
integer nx,ny

external :: init_e1t, init_e1u, init_e1v, init_e1f
external :: init_e2t, init_e2u, init_e2v, init_e2f

nx=nlci ; ny=nlcj

ln_perio=.FALSE.
if( jperio ==1 .OR. jperio==2 .OR. jperio==4) ln_perio=.TRUE.

call Agrif_Init_variable(e1t_id, procname = init_e1t)
call Agrif_Init_variable(e1u_id, procname = init_e1u)
call Agrif_Init_variable(e1v_id, procname = init_e1v)
call Agrif_Init_variable(e1f_id, procname = init_e1f)

call Agrif_Init_variable(e2t_id, procname = init_e2t)
call Agrif_Init_variable(e2u_id, procname = init_e2u)
call Agrif_Init_variable(e2v_id, procname = init_e2v)
call Agrif_Init_variable(e2f_id, procname = init_e2f)

CALL lbc_lnk( 'e1t', e1t, 'T', 1._wp)
CALL lbc_lnk( 'e2t', e2t, 'T', 1._wp)
CALL lbc_lnk( 'e1u', e1u, 'U', 1._wp)
CALL lbc_lnk( 'e2u', e2u, 'U', 1._wp)
CALL lbc_lnk( 'e1v', e1v, 'V', 1._wp)
CALL lbc_lnk( 'e2v', e2v, 'V', 1._wp)
CALL lbc_lnk( 'e1f', e1f, 'F', 1._wp)
CALL lbc_lnk( 'e2f', e2f, 'F', 1._wp)

end subroutine agrif_init_scales

   SUBROUTINE init_glamt( ptab, i1, i2, j1, j2,  before, nb,ndir)
   use dom_oce
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
      	 glamt(i1:i2,j1:j2) = ptab(i1:i2,j1:j2)
      ENDIF
      !
   END SUBROUTINE init_glamt

    SUBROUTINE init_glamu( ptab, i1, i2, j1, j2, before, nb,ndir)
    use dom_oce
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
      IF( before) THEN
         ptab(i1:i2,j1:j2) = glamu(i1:i2,j1:j2)
      ELSE
      	 glamu(i1:i2,j1:j2) = ptab(i1:i2,j1:j2)
      ENDIF
      !
   END SUBROUTINE init_glamu

   SUBROUTINE init_glamv( ptab, i1, i2, j1, j2, before, nb,ndir)
    use dom_oce
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
      IF( before) THEN
         ptab(i1:i2,j1:j2) = glamv(i1:i2,j1:j2)
      ELSE
      	 glamv(i1:i2,j1:j2) = ptab(i1:i2,j1:j2)
      ENDIF
      !
   END SUBROUTINE init_glamv

   SUBROUTINE init_glamf( ptab, i1, i2, j1, j2,  before, nb,ndir)
    use dom_oce
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
      IF( before) THEN
         ptab(i1:i2,j1:j2) = glamf(i1:i2,j1:j2)
      ELSE
      	 glamf(i1:i2,j1:j2) = ptab(i1:i2,j1:j2)
      ENDIF
      !
   END SUBROUTINE init_glamf

   SUBROUTINE init_gphit( ptab, i1, i2, j1, j2, before, nb,ndir)
   use dom_oce
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
      IF( before) THEN
         ptab(i1:i2,j1:j2) = gphit(i1:i2,j1:j2)
      ELSE
         gphit(i1:i2,j1:j2)=ptab
      ENDIF
      !
   END SUBROUTINE init_gphit

    SUBROUTINE init_gphiu( ptab, i1, i2, j1, j2, before, nb,ndir)
    use dom_oce
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
    use dom_oce
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
   use dom_oce
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


   SUBROUTINE init_e1t( ptab, i1, i2, j1, j2, before, nb,ndir)
   use dom_oce
   use agrif_parameters
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
      INTEGER :: ji,jj

         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)
         southern_side = (nb == 2).AND.(ndir == 1)
         northern_side = (nb == 2).AND.(ndir == 2)

      IF( before) THEN
        ! May need to extend at south boundary
          IF (j1<1) THEN
            IF (.NOT.agrif_child(south_boundary_open)) THEN
              IF ((nbondj == -1).OR.(nbondj == 2)) THEN
                DO jj=1,j2
                  ptab(i1:i2,jj)=e1t(i1:i2,jj)
                ENDDO
                DO jj=j1,0
                  ptab(i1:i2,jj)=e1t(i1:i2,1)
                ENDDO
              ENDIF
            ELSE
              stop "OUT OF BOUNDS"
            ENDIF
          ELSE
             ptab(i1:i2,j1:j2) = e1t(i1:i2,j1:j2)
          ENDIF
      ELSE
         e1t(i1:i2,j1:j2)=ptab/Agrif_rhoy()
      ENDIF
      !
   END SUBROUTINE init_e1t

   SUBROUTINE init_e1u( ptab, i1, i2, j1, j2, before, nb,ndir)
   use dom_oce
   use agrif_parameters
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
          IF (j1<1) THEN
            IF (.NOT.agrif_child(south_boundary_open)) THEN
              IF ((nbondj == -1).OR.(nbondj == 2)) THEN
                DO jj=1,j2
                  ptab(i1:i2,jj)=e1u(i1:i2,jj)
                ENDDO
                DO jj=j1,0
                  ptab(i1:i2,jj)=e1u(i1:i2,1)
                ENDDO
              ENDIF
            ELSE
              stop "OUT OF BOUNDS"
            ENDIF
          ELSE
             ptab(i1:i2,j1:j2) = e1u(i1:i2,j1:j2)
          ENDIF
      ELSE
         e1u(i1:i2,j1:j2)=ptab/Agrif_rhoy()
      ENDIF
      !
   END SUBROUTINE init_e1u

   SUBROUTINE init_e1v( ptab, i1, i2, j1, j2, before, nb,ndir)
   use dom_oce
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
         ptab(i1:i2,j1:j2) = e1v(i1:i2,j1:j2)
      ELSE
         e1v(i1:i2,j1:j2)=ptab/Agrif_rhoy()
      ENDIF
      !
   END SUBROUTINE init_e1v

   SUBROUTINE init_e1f( ptab, i1, i2, j1, j2, before, nb,ndir)
   use dom_oce
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
         ptab(i1:i2,j1:j2) = e1f(i1:i2,j1:j2)
      ELSE
         e1f(i1:i2,j1:j2)=ptab/Agrif_rhoy()
      ENDIF
      !
   END SUBROUTINE init_e1f

  SUBROUTINE init_e2t( ptab, i1, i2, j1, j2, before, nb,ndir)
   use dom_oce
   use agrif_parameters
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
          IF (j1<1) THEN
            IF (.NOT.agrif_child(south_boundary_open)) THEN
              IF ((nbondj == -1).OR.(nbondj == 2)) THEN
                DO jj=1,j2
                  ptab(i1:i2,jj)=e2t(i1:i2,jj)
                ENDDO
                DO jj=j1,0
                  ptab(i1:i2,jj)=e2t(i1:i2,1)
                ENDDO
              ENDIF
            ELSE
              stop "OUT OF BOUNDS"
            ENDIF
          ELSE
             ptab(i1:i2,j1:j2) = e2t(i1:i2,j1:j2)
          ENDIF
      ELSE
         e2t(i1:i2,j1:j2)=ptab/Agrif_rhoy()
      ENDIF
      !
   END SUBROUTINE init_e2t

   SUBROUTINE init_e2u( ptab, i1, i2, j1, j2, before, nb,ndir)
   use dom_oce
   use agrif_parameters
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
          IF (j1<1) THEN
            IF (.NOT.agrif_child(south_boundary_open)) THEN
              IF ((nbondj == -1).OR.(nbondj == 2)) THEN
                DO jj=1,j2
                  ptab(i1:i2,jj)=e2u(i1:i2,jj)
                ENDDO
                DO jj=j1,0
                  ptab(i1:i2,jj)=e2u(i1:i2,1)
                ENDDO
              ENDIF
            ELSE
              stop "OUT OF BOUNDS"
            ENDIF
          ELSE
             ptab(i1:i2,j1:j2) = e2u(i1:i2,j1:j2)
          ENDIF
      ELSE
         e2u(i1:i2,j1:j2)=ptab/Agrif_rhoy()
      ENDIF
      !
   END SUBROUTINE init_e2u

   SUBROUTINE init_e2v( ptab, i1, i2, j1, j2, before, nb,ndir)
   use dom_oce
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
         ptab(i1:i2,j1:j2) = e2v(i1:i2,j1:j2)
      ELSE
         e2v(i1:i2,j1:j2)=ptab/Agrif_rhoy()
      ENDIF
      !
   END SUBROUTINE init_e2v

   SUBROUTINE init_e2f( ptab, i1, i2, j1, j2, before, nb,ndir)
   use dom_oce
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
         ptab(i1:i2,j1:j2) = e2f(i1:i2,j1:j2)
      ELSE
         e2f(i1:i2,j1:j2)=ptab/Agrif_rhoy()
      ENDIF
      !
   END SUBROUTINE init_e2f


SUBROUTINE agrif_nemo_init
USE agrif_parameters
USE dom_oce
USE in_out_manager
USE lib_mpp


   !!
   IMPLICIT NONE

   INTEGER ::   ios

   NAMELIST/namagrif/ nn_cln_update,ln_spc_dyn,rn_sponge_tra,rn_sponge_dyn,ln_chk_bathy,npt_connect, npt_copy, south_boundary_open

      REWIND( numnam_ref )              ! Namelist namagrif in reference namelist : nesting parameters
      READ  ( numnam_ref, namagrif, IOSTAT = ios, ERR = 901 )
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namagrif in reference namelist', lwp )

      REWIND( numnam_cfg )              ! Namelist namzgr in configuration namelist : nesting parameters
      READ  ( numnam_cfg, namagrif, IOSTAT = ios, ERR = 902 )
902   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namagrif in configuration namelist', lwp )
      IF(lwm) WRITE ( numond, namagrif )

      IF(lwp) THEN                     ! Control print
         WRITE(numout,*)
         WRITE(numout,*) 'agrif_nemo_init : nesting'
         WRITE(numout,*) '~~~~~~~'
         WRITE(numout,*) '   Namelist namagrif : set nesting parameters'
         WRITE(numout,*) '      npt_copy             = ', npt_copy
         WRITE(numout,*) '      npt_connect          = ', npt_connect
         WRITE(numout,*) '      south_boundary_open  = ', south_boundary_open
      ENDIF

      print *,'soutyhubar = ',south_boundary_open

   ! Set the number of ghost cells according to periodicity

      nbghostcells_x = nbghostcells
      nbghostcells_y_s = nbghostcells
      nbghostcells_y_n = nbghostcells

      IF (.not.agrif_root()) THEN
        IF (jperio == 1) THEN
          nbghostcells_x = 0
        ENDIF
        IF (.NOT.south_boundary_open) THEN
          nbghostcells_y_s = 0
        ENDIF
      ENDIF

END SUBROUTINE agrif_nemo_init


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

#if defined  key_mpp_mpi
SUBROUTINE Agrif_InvLoc( indloc, nprocloc, i, indglob )
      !!----------------------------------------------------------------------
      !!                     *** ROUTINE Agrif_InvLoc ***
      !!----------------------------------------------------------------------
   USE dom_oce
   !!
   IMPLICIT NONE
   !
   INTEGER :: indglob, indloc, nprocloc, i
      !!----------------------------------------------------------------------
   !
   SELECT CASE( i )
   CASE(1)   ;   indglob = indloc + nimppt(nprocloc+1) - 1
   CASE(2)   ;   indglob = indloc + njmppt(nprocloc+1) - 1
   CASE DEFAULT
      indglob = indloc
   END SELECT
   !
END SUBROUTINE Agrif_InvLoc

SUBROUTINE Agrif_get_proc_info( imin, imax, jmin, jmax )
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_get_proc_info ***
      !!----------------------------------------------------------------------
   USE par_oce
   USE dom_oce
   !!
   IMPLICIT NONE
   !
   INTEGER, INTENT(out) :: imin, imax
   INTEGER, INTENT(out) :: jmin, jmax
      !!----------------------------------------------------------------------
   !
   imin = nimppt(Agrif_Procrank+1)  ! ?????
   jmin = njmppt(Agrif_Procrank+1)  ! ?????
   imax = imin + jpi - 1
   jmax = jmin + jpj - 1
   !
END SUBROUTINE Agrif_get_proc_info

SUBROUTINE Agrif_estimate_parallel_cost(imin, imax,jmin, jmax, nbprocs, grid_cost)
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE Agrif_estimate_parallel_cost ***
      !!----------------------------------------------------------------------
   USE par_oce
   !!
   IMPLICIT NONE
   !
   INTEGER,  INTENT(in)  :: imin, imax
   INTEGER,  INTENT(in)  :: jmin, jmax
   INTEGER,  INTENT(in)  :: nbprocs
   REAL(wp), INTENT(out) :: grid_cost
      !!----------------------------------------------------------------------
   !
   grid_cost = REAL(imax-imin+1,wp)*REAL(jmax-jmin+1,wp) / REAL(nbprocs,wp)
   !
END SUBROUTINE Agrif_estimate_parallel_cost
#endif
#endif
