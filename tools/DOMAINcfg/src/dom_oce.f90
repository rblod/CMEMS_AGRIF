MODULE dom_oce
   !!======================================================================
   !!                       ***  MODULE dom_oce  ***
   !!       
   !! ** Purpose :   Define in memory all the ocean space domain variables
   !!======================================================================
   !! History :  1.0  ! 2005-10  (A. Beckmann, G. Madec)  reactivate s-coordinate 
   !!            3.3  ! 2010-11  (G. Madec) add mbk. arrays associated to the deepest ocean level
   !!            3.4  ! 2011-01  (A. R. Porter, STFC Daresbury) dynamical allocation
   !!            3.5  ! 2012     (S. Mocavero, I. Epicoco) Add arrays associated
   !!                             to the optimization of BDY communications
   !!            3.7  ! 2015-11  (G. Madec) introduce surface and scale factor ratio
   !!             -   ! 2015-11  (G. Madec, A. Coward)  time varying zgr by default
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   Agrif_Root    : dummy function used when lk_agrif=F
   !!   Agrif_CFixed  : dummy function used when lk_agrif=F
   !!   dom_oce_alloc : dynamical allocation of dom_oce arrays
   !!----------------------------------------------------------------------
   USE par_oce        ! ocean parameters

   IMPLICIT NONE
   PUBLIC             ! allows the acces to par_oce when dom_oce is used (exception to coding rules)

   PUBLIC dom_oce_alloc  ! Called from nemogcm.F90

  !! time & space domain namelist
   !! ----------------------------
   INTEGER , PUBLIC ::   nmsh            !: = 1 create a mesh-mask file
   !                                    !!* Namelist namdom : time & space domain *
   INTEGER , PUBLIC ::   nn_bathy        !: = 0/1/2 ,compute/read the bathymetry file
   REAL(wp), PUBLIC ::   rn_bathy        !: depth of flat bottom (active if nn_bathy=0; if =0 depth=jpkm1)
   REAL(wp), PUBLIC ::   rn_hmin         !: minimum ocean depth (>0) or minimum number of ocean levels (<0)
   REAL(wp), PUBLIC ::   rn_e3zps_min    !: miminum thickness for partial steps (meters)
   REAL(wp), PUBLIC ::   rn_e3zps_rat    !: minimum thickness ration for partial steps
   INTEGER , PUBLIC ::   nn_msh          !: = 1 create a mesh-mask file

   INTEGER , PUBLIC ::   ntopo           !: = 0/1 ,compute/read the bathymetry file
   INTEGER, PUBLIC  ::   nperio          !: type of lateral boundary condition
   REAL(wp), PUBLIC ::   e3zps_min       !: miminum thickness for partial steps (meters)
   REAL(wp), PUBLIC ::   e3zps_rat       !: minimum thickness ration for partial steps

   INTEGER, PUBLIC :: nn_interp
   CHARACTER(LEN=132), PUBLIC :: cn_topo
   CHARACTER(LEN=132), PUBLIC :: cn_bath
   CHARACTER(LEN=132), PUBLIC :: cn_lon
   CHARACTER(LEN=132), PUBLIC :: cn_lat

   LOGICAL, PUBLIC ::   lzoom      =  .FALSE.   !: zoom flag
   LOGICAL, PUBLIC ::   lzoom_e    =  .FALSE.   !: East  zoom type flag
   LOGICAL, PUBLIC ::   lzoom_w    =  .FALSE.   !: West  zoom type flag
   LOGICAL, PUBLIC ::   lzoom_s    =  .FALSE.   !: South zoom type flag
   LOGICAL, PUBLIC ::   lzoom_n    =  .FALSE.   !: North zoom type flag

   LOGICAL, PUBLIC ::   ln_domclo  =  .FALSE.

   INTEGER       ::   jphgr_msh          !: type of horizontal mesh
   !                                       !  = 0 curvilinear coordinate on the sphere read in coordinate.nc
   !                                       !  = 1 geographical mesh on the sphere with regular grid-spacing
   !                                       !  = 2 f-plane with regular grid-spacing
   !                                       !  = 3 beta-plane with regular grid-spacing
   !                                       !  = 4 Mercator grid with T/U point at the equator

   REAL(wp)      ::   ppglam0            !: longitude of first raw and column T-point (jphgr_msh = 1)
   REAL(wp)      ::   ppgphi0            !: latitude  of first raw and column T-point (jphgr_msh = 1)
   !                                                        !  used for Coriolis & Beta parameters (jphgr_msh = 2 or 3)
   REAL(wp)      ::   ppe1_deg           !: zonal      grid-spacing (degrees)
   REAL(wp)      ::   ppe2_deg           !: meridional grid-spacing (degrees)
   REAL(wp)      ::   ppe1_m             !: zonal      grid-spacing (degrees)
   REAL(wp)      ::   ppe2_m             !: meridional grid-spacing (degrees)

   !! Vertical grid parameter for domzgr
   !! ==================================
   REAL(wp)      ::   ppsur              !: ORCA r4, r2 and r05 coefficients
   REAL(wp)      ::   ppa0               !: (default coefficients)
   REAL(wp)      ::   ppa1               !:
   REAL(wp)      ::   ppkth              !:
   REAL(wp)      ::   ppacr              !:
   !
   !  If both ppa0 ppa1 and ppsur are specified to 0, then
   !  they are computed from ppdzmin, pphmax , ppkth, ppacr in dom_zgr
   REAL(wp)      ::   ppdzmin            !: Minimum vertical spacing
   REAL(wp)      ::   pphmax             !: Maximum depth
   !
   LOGICAL       ::   ldbletanh          !: Use/do not use double tanf function for vertical coordinates
   REAL(wp)      ::   ppa2               !: Double tanh function parameters
   REAL(wp)      ::   ppkth2             !:
   REAL(wp)      ::   ppacr2             !:

   !!----------------------------------------------------------------------
   !! time & space domain namelist
   !! ----------------------------
   !                                   !!* Namelist namdom : time & space domain *
   LOGICAL , PUBLIC ::   ln_linssh      !: =T  linear free surface ==>> model level are fixed in time
   LOGICAL , PUBLIC ::   ln_meshmask    !: =T  create a mesh-mask file (mesh_mask.nc)
   REAL(wp), PUBLIC ::   rn_isfhmin     !: threshold to discriminate grounded ice to floating ice
   REAL(wp), PUBLIC ::   rn_rdt         !: time step for the dynamics and tracer
   REAL(wp), PUBLIC ::   rn_atfp        !: asselin time filter parameter
   INTEGER , PUBLIC ::   nn_euler       !: =0 start with forward time step or not (=1)
   LOGICAL , PUBLIC ::   ln_iscpl       !: coupling with ice sheet
   LOGICAL , PUBLIC ::   ln_crs         !: Apply grid coarsening to dynamical model output or online passive tracers

   !! Free surface parameters
   !! =======================
   LOGICAL , PUBLIC :: ln_dynspg_exp    !: Explicit free surface flag
   LOGICAL , PUBLIC :: ln_dynspg_ts     !: Split-Explicit free surface flag

   !! Time splitting parameters
   !! =========================
   LOGICAL,  PUBLIC :: ln_bt_fw         !: Forward integration of barotropic sub-stepping
   LOGICAL,  PUBLIC :: ln_bt_av         !: Time averaging of barotropic variables
   LOGICAL,  PUBLIC :: ln_bt_auto       !: Set number of barotropic iterations automatically
   INTEGER,  PUBLIC :: nn_bt_flt        !: Filter choice
   INTEGER,  PUBLIC :: nn_baro          !: Number of barotropic iterations during one baroclinic step (rdt)
   REAL(wp), PUBLIC :: rn_bt_cmax       !: Maximum allowed courant number (used if ln_bt_auto=T)
   REAL(wp), PUBLIC :: rn_bt_alpha      !: Time stepping diffusion parameter


   !                                   !! old non-DOCTOR names still used in the model
   REAL(wp), PUBLIC ::   atfp           !: asselin time filter parameter
   REAL(wp), PUBLIC ::   rdt            !: time step for the dynamics and tracer

   !                                   !!! associated variables
   INTEGER , PUBLIC ::   neuler         !: restart euler forward option (0=Euler)
   REAL(wp), PUBLIC ::   r2dt           !: = 2*rdt except at nit000 (=rdt) if neuler=0

   !!----------------------------------------------------------------------
   !! space domain parameters
   !!----------------------------------------------------------------------
   INTEGER, PUBLIC ::   jperio   !: Global domain lateral boundary type (between 0 and 7)
   !                                !  = 0 closed                 ;   = 1 cyclic East-West
   !                                !  = 2 cyclic North-South     ;   = 3 North fold T-point pivot
   !                                !  = 4 cyclic East-West AND North fold T-point pivot
   !                                !  = 5 North fold F-point pivot
   !                                !  = 6 cyclic East-West AND North fold F-point pivot
   !                                !  = 7 bi-cyclic East-West AND North-South
   LOGICAL, PUBLIC ::   l_Iperio, l_Jperio   !   should we explicitely take care I/J periodicity 

   !                                 !  domain MPP decomposition parameters
   INTEGER             , PUBLIC ::   nimpp, njmpp     !: i- & j-indexes for mpp-subdomain left bottom
   INTEGER             , PUBLIC ::   nreci, nrecj     !: overlap region in i and j
   INTEGER             , PUBLIC ::   nproc            !: number for local processor
   INTEGER             , PUBLIC ::   narea            !: number for local area
   INTEGER             , PUBLIC ::   nbondi, nbondj   !: mark of i- and j-direction local boundaries
   INTEGER, ALLOCATABLE, PUBLIC ::   nbondi_bdy(:)    !: mark i-direction local boundaries for BDY open boundaries
   INTEGER, ALLOCATABLE, PUBLIC ::   nbondj_bdy(:)    !: mark j-direction local boundaries for BDY open boundaries
   INTEGER, ALLOCATABLE, PUBLIC ::   nbondi_bdy_b(:)  !: mark i-direction of neighbours local boundaries for BDY open boundaries  
   INTEGER, ALLOCATABLE, PUBLIC ::   nbondj_bdy_b(:)  !: mark j-direction of neighbours local boundaries for BDY open boundaries  

   INTEGER, PUBLIC ::   npolj             !: north fold mark (0, 3 or 4)
   INTEGER, PUBLIC ::   nlci, nldi, nlei  !: i-dimensions of the local subdomain and its first and last indoor indices
   INTEGER, PUBLIC ::   nlcj, nldj, nlej  !: i-dimensions of the local subdomain and its first and last indoor indices
   INTEGER, PUBLIC ::   noea, nowe        !: index of the local neighboring processors in
   INTEGER, PUBLIC ::   noso, nono        !: east, west, south and north directions
   INTEGER, PUBLIC ::   nidom             !: ???

   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   mig        !: local  ==> global domain i-index
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   mjg        !: local  ==> global domain j-index
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   mi0, mi1   !: global ==> local  domain i-index (mi0=1 and mi1=0 if the global index
   !                                                                !                                             is not in the local domain)
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   mj0, mj1   !: global ==> local  domain j-index (mj0=1 and mj1=0 if the global index
   !                                                                !                                             is not in the local domain)
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   nimppt, njmppt   !: i-, j-indexes for each processor
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   ibonit, ibonjt   !: i-, j- processor neighbour existence
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   nlcit , nlcjt    !: dimensions of every subdomain
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   nldit , nldjt    !: first, last indoor index for each i-domain
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   nleit , nlejt    !: first, last indoor index for each j-domain
   INTEGER, PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) :: nfiimpp, nfipproc, nfilcit

   !!----------------------------------------------------------------------
   !! horizontal curvilinear coordinate and scale factors
   !! ---------------------------------------------------------------------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE        , DIMENSION(:,:) ::   glamt , glamu, glamv , glamf    !: longitude at t, u, v, f-points [degree]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE        , DIMENSION(:,:) ::   gphit , gphiu, gphiv , gphif    !: latitude  at t, u, v, f-points [degree]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, TARGET, DIMENSION(:,:) ::   e1t   , e2t  , r1_e1t, r1_e2t   !: t-point horizontal scale factors    [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, TARGET, DIMENSION(:,:) ::   e1u   , e2u  , r1_e1u, r1_e2u   !: horizontal scale factors at u-point [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, TARGET, DIMENSION(:,:) ::   e1v   , e2v  , r1_e1v, r1_e2v   !: horizontal scale factors at v-point [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, TARGET, DIMENSION(:,:) ::   e1f   , e2f  , r1_e1f, r1_e2f   !: horizontal scale factors at f-point [m]
   !
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE        , DIMENSION(:,:) ::   e1e2t , r1_e1e2t                !: associated metrics at t-point
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE        , DIMENSION(:,:) ::   e1e2u , r1_e1e2u , e2_e1u       !: associated metrics at u-point
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE        , DIMENSION(:,:) ::   e1e2v , r1_e1e2v , e1_e2v       !: associated metrics at v-point
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE        , DIMENSION(:,:) ::   e1e2f , r1_e1e2f                !: associated metrics at f-point
   !
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE        , DIMENSION(:,:) ::   ff_f  , ff_t                    !: Coriolis factor at f- & t-points  [1/s]

   !! s-coordinate and hybrid z-s-coordinate
   !! =----------------======---------------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)   ::   gsigt, gsigw       !: model level depth coefficient at t-, w-levels (analytic)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)   ::   gsi3w              !: model level depth coefficient at w-level (sum of gsigw)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)   ::   esigt, esigw       !: vertical scale factor coef. at t-, w-levels

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   hbatv , hbatf      !: ocean depth at the vertical of  v--f
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   hbatt , hbatu      !:                                 t--u points (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   scosrf, scobot     !: ocean surface and bottom topographies
   !                                                                           !  (if deviating from coordinate surfaces in HYBRID)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   hifv  , hiff       !: interface depth between stretching at v--f
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   hift  , hifu       !: and quasi-uniform spacing             t--u points (m)
!!gm end

   !!----------------------------------------------------------------------
   !! vertical coordinate and scale factors
   !! ---------------------------------------------------------------------
   LOGICAL, PUBLIC ::   ln_zco       !: z-coordinate - full step
   LOGICAL, PUBLIC ::   ln_zps       !: z-coordinate - partial step
   LOGICAL, PUBLIC ::   ln_sco       !: s-coordinate or hybrid z-s coordinate
   LOGICAL, PUBLIC ::   ln_isfcav    !: presence of ISF 
   !
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::  e3t_0, e3u_0 , e3v_0 , e3f_0 !: t-,u-,v-,f-vert. scale factor [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::  e3w_0, e3uw_0, e3vw_0        !: w-,uw-,vw-vert. scale factor [m]
   !
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   gdept_0 !: t- depth              [m]
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:) ::   gdepw_0 !: w- depth              [m]
   !
   INTEGER, PUBLIC ::   nla10              !: deepest    W level Above  ~10m (nlb10 - 1)
   INTEGER, PUBLIC ::   nlb10              !: shallowest W level Bellow ~10m (nla10 + 1) 
   !
   !! 1D reference  vertical coordinate
   !! =-----------------====------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)   ::   gdept_1d, gdepw_1d !: reference depth of t- and w-points (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:)   ::   e3t_1d  , e3w_1d   !: reference vertical scale factors at T- and W-pts (m)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   e3tp    , e3wp     !: ocean bottom level thickness at T and W points

   !!----------------------------------------------------------------------
   !! masks, top and bottom ocean point position
   !! ---------------------------------------------------------------------
!!gm Proposition of new name for top/bottom vertical indices
!   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   mtk_t, mtk_u, mtk_v   !: top first wet T-, U-, V-, F-level (ISF)
!   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   mbk_t, mbk_u, mbk_v   !: bottom last wet T-, U- and V-level
!!gm
   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   mbathy             !: number of ocean level (=0, 1, ... , jpk-1)
   REAL(wp) , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   bathy             !: number of ocean level (=0, 1, ... , jpk-1)
   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   mbkt, mbku, mbkv   !: bottom last wet T-, U- and V-level
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   tmask_i            !: interior domain T-point mask
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   tmask_h            !: internal domain T-point mask (Figure 8.5 NEMO book)

   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   misfdep                 !: top first ocean level             (ISF)
   INTEGER , PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   mikt, miku, mikv, mikf  !: top first wet T-, U-, V-, F-level (ISF)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   risfdep                 !: Iceshelf draft                    (ISF)

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) ::   ssmask, ssumask, ssvmask             !: surface mask at T-,U-, V- and F-pts
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:), TARGET :: tmask, umask, vmask, fmask   !: land/ocean mask at T-, U-, V- and F-pts
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:), TARGET :: wmask                        !: land/ocean mask at W- pts               

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) :: msk_opnsea  , msk_csundef                !: open ocean mask, closed sea mask (all of them)
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) :: msk_csglo   , msk_csrnf   , msk_csemp    !: closed sea masks
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:) :: msk_csgrpglo, msk_csgrprnf, msk_csgrpemp !: closed sea masks

   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:) ::   tpol, fpol          !: north fold mask (jperio= 3 or 4)

   !!----------------------------------------------------------------------
   !! calendar variables
   !! ---------------------------------------------------------------------
   INTEGER , PUBLIC ::   nyear         !: current year
   INTEGER , PUBLIC ::   nmonth        !: current month
   INTEGER , PUBLIC ::   nday          !: current day of the month
   INTEGER , PUBLIC ::   nhour         !: current hour
   INTEGER , PUBLIC ::   nminute       !: current minute
   INTEGER , PUBLIC ::   ndastp        !: time step date in yyyymmdd format
   INTEGER , PUBLIC ::   nday_year     !: current day counted from jan 1st of the current year
   INTEGER , PUBLIC ::   nsec_year     !: current time step counted in second since 00h jan 1st of the current year
   INTEGER , PUBLIC ::   nsec_month    !: current time step counted in second since 00h 1st day of the current month
   INTEGER , PUBLIC ::   nsec_week     !: current time step counted in second since 00h of last monday
   INTEGER , PUBLIC ::   nsec_day      !: current time step counted in second since 00h of the current day
   REAL(wp), PUBLIC ::   fjulday       !: current julian day 
   REAL(wp), PUBLIC ::   fjulstartyear !: first day of the current year in julian days
   REAL(wp), PUBLIC ::   adatrj        !: number of elapsed days since the begining of the whole simulation
   !                                   !: (cumulative duration of previous runs that may have used different time-step size)
   INTEGER , PUBLIC, DIMENSION(0: 2) ::   nyear_len     !: length in days of the previous/current/next year
   INTEGER , PUBLIC, DIMENSION(0:13) ::   nmonth_len    !: length in days of the months of the current year
   INTEGER , PUBLIC, DIMENSION(0:13) ::   nmonth_half   !: second since Jan 1st 0h of the current year and the half of the months
   INTEGER , PUBLIC, DIMENSION(0:13) ::   nmonth_end    !: second since Jan 1st 0h of the current year and the end of the months
   INTEGER , PUBLIC                  ::   nsec1jan000   !: second since Jan 1st 0h of nit000 year and Jan 1st 0h the current year

   !!----------------------------------------------------------------------
   !! agrif domain
   !!----------------------------------------------------------------------
#if defined key_agrif
   LOGICAL, PUBLIC, PARAMETER ::   lk_agrif = .TRUE.    !: agrif flag
#else
   LOGICAL, PUBLIC, PARAMETER ::   lk_agrif = .FALSE.   !: agrif flag
#endif

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: dom_oce.F90 10068 2018-08-28 14:09:04Z nicolasmartin $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

#if ! defined key_agrif
   !!----------------------------------------------------------------------
   !! NOT 'key_agrif'      dummy function                     No AGRIF zoom
   !!----------------------------------------------------------------------
   LOGICAL FUNCTION Agrif_Root()
      Agrif_Root = .TRUE.
   END FUNCTION Agrif_Root

   CHARACTER(len=3) FUNCTION Agrif_CFixed()
      Agrif_CFixed = '0' 
   END FUNCTION Agrif_CFixed
#endif

   INTEGER FUNCTION dom_oce_alloc()
      !!----------------------------------------------------------------------
      INTEGER, DIMENSION(11) :: ierr
      !!----------------------------------------------------------------------
      ierr(:) = 0
      !
      ALLOCATE( mig(jpi), mjg(jpj), STAT=ierr(1) )
         !
      ALLOCATE( mi0(jpiglo)   , mi1 (jpiglo),  mj0(jpjglo)   , mj1 (jpjglo) ,     &
         &      tpol(jpiglo)  , fpol(jpiglo)                                , STAT=ierr(2) )
         !
      ALLOCATE( glamt(jpi,jpj) ,    glamu(jpi,jpj) ,  glamv(jpi,jpj) ,  glamf(jpi,jpj) ,     &
         &      gphit(jpi,jpj) ,    gphiu(jpi,jpj) ,  gphiv(jpi,jpj) ,  gphif(jpi,jpj) ,     &
         &       e1t (jpi,jpj) ,     e2t (jpi,jpj) , r1_e1t(jpi,jpj) , r1_e2t(jpi,jpj) ,     &
         &       e1u (jpi,jpj) ,     e2u (jpi,jpj) , r1_e1u(jpi,jpj) , r1_e2u(jpi,jpj) ,     &
         &       e1v (jpi,jpj) ,     e2v (jpi,jpj) , r1_e1v(jpi,jpj) , r1_e2v(jpi,jpj) ,     &
         &       e1f (jpi,jpj) ,     e2f (jpi,jpj) , r1_e1f(jpi,jpj) , r1_e2f(jpi,jpj) ,     &
         &      e1e2t(jpi,jpj) , r1_e1e2t(jpi,jpj)                                     ,     &
         &      e1e2u(jpi,jpj) , r1_e1e2u(jpi,jpj) , e2_e1u(jpi,jpj)                   ,     &
         &      e1e2v(jpi,jpj) , r1_e1e2v(jpi,jpj) , e1_e2v(jpi,jpj)                   ,     &
         &      e1e2f(jpi,jpj) , r1_e1e2f(jpi,jpj)                                     ,     &
         &      ff_f (jpi,jpj) ,    ff_t (jpi,jpj)                                     , STAT=ierr(3) )
         !
      ALLOCATE( gdept_0(jpi,jpj,jpk) , gdepw_0(jpi,jpj,jpk) , STAT=ierr(4) )
         !
      ALLOCATE( e3t_0 (jpi,jpj,jpk) , e3u_0 (jpi,jpj,jpk) , e3v_0(jpi,jpj,jpk) , e3f_0(jpi,jpj,jpk) , e3w_0(jpi,jpj,jpk) ,   &
         &      e3uw_0(jpi,jpj,jpk) , e3vw_0(jpi,jpj,jpk) , STAT=ierr(5) )                       
         !
      ALLOCATE( gdept_1d(jpk) , e3tp (jpi,jpj), e3wp(jpi,jpj) ,gdepw_1d(jpk) , e3t_1d(jpk) , e3w_1d(jpk) , STAT=ierr(6) )
         !
      ALLOCATE( bathy(jpi,jpj),mbathy(jpi,jpj), tmask_i(jpi,jpj) , tmask_h(jpi,jpj) ,                        & 
         &      ssmask (jpi,jpj) , ssumask(jpi,jpj) , ssvmask(jpi,jpj) ,     &
         &      mbkt   (jpi,jpj) , mbku   (jpi,jpj) , mbkv   (jpi,jpj) , STAT=ierr(7) )
         !
      ALLOCATE( misfdep(jpi,jpj) , mikt(jpi,jpj) , miku(jpi,jpj) ,     &
         &      risfdep(jpi,jpj) , mikv(jpi,jpj) , mikf(jpi,jpj) , STAT=ierr(8) )
         !
      ALLOCATE( tmask(jpi,jpj,jpk) , umask(jpi,jpj,jpk) ,     & 
         &      vmask(jpi,jpj,jpk) , fmask(jpi,jpj,jpk) , wmask(jpi,jpj,jpk) , STAT=ierr(9) )
         !
      ALLOCATE( hbatv (jpi,jpj) , hbatf (jpi,jpj) ,     &
         &      hbatt (jpi,jpj) , hbatu (jpi,jpj) ,     &
         &      scosrf(jpi,jpj) , scobot(jpi,jpj) ,     &
         &      hifv  (jpi,jpj) , hiff  (jpi,jpj) ,     &
         &      hift  (jpi,jpj) , hifu  (jpi,jpj) , STAT=ierr(10) )

      ALLOCATE( msk_opnsea  (jpi,jpj), msk_csundef (jpi,jpj),                        &
         &      msk_csglo   (jpi,jpj), msk_csrnf   (jpi,jpj), msk_csemp   (jpi,jpj), &
         &      msk_csgrpglo(jpi,jpj), msk_csgrprnf(jpi,jpj), msk_csgrpemp(jpi,jpj), STAT=ierr(11) )
      !
      dom_oce_alloc = MAXVAL(ierr)
      !
   END FUNCTION dom_oce_alloc

   !!======================================================================
END MODULE dom_oce
