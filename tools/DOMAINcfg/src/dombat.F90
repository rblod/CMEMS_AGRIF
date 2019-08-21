MODULE dombat

   USE oce               ! ocean variables
   USE dom_oce           ! ocean domain
   !
   USE in_out_manager    ! I/O manager
   USE iom               ! I/O library
   USE lbclnk            ! ocean lateral boundary conditions (or mpp link)
   USE lib_mpp           ! distributed memory computing library
   USE timing            ! Timing
   USE agrif_modutil
   USE bilinear_interp

   IMPLICIT NONE
   PRIVATE

   PUBLIC   dom_bat        ! called by dom_zgr.F90

CONTAINS

   SUBROUTINE dom_bat

      INTEGER :: inum, id, ji, jj
      INTEGER :: iimin,iimax,jjmin,jjmax
      INTEGER :: tabdim1, tabdim2, nxhr, nyhr, nxyhr
      INTEGER, DIMENSION(2) :: ddims
      INTEGER, DIMENSION(3) :: status
      INTEGER, DIMENSION(1) :: i_min,i_max
      INTEGER, DIMENSION(1) ::j_min,j_max
      INTEGER, DIMENSION(:)  ,ALLOCATABLE     ::   src_add,dst_add 
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: trouble_points , vardep,mask_oce
      REAL(wp) :: Cell_lonmin, Cell_lonmax, Cell_latmin, Cell_latmax,zdel
      REAL(wp), DIMENSION(:)  , ALLOCATABLE ::   lon_new1D , lat_new1D, vardep1d
      REAL(wp), DIMENSION(:,:), ALLOCATABLE ::   coarselon, coarselat, coarsebathy, bathy_test
      REAL(wp), DIMENSION(:,:),ALLOCATABLE     ::   matrix,interpdata 
      LOGICAL :: lonlat_2D, ln_pacifiq
      LOGICAL :: identical_grids
      LOGICAL, DIMENSION(:,:), ALLOCATABLE     ::   masksrc
 
      CHARACTER(32) :: bathyfile, bathyname, lonname,latname       

      bathyfile=TRIM(cn_topo)
      bathyname=TRIM(cn_bath)
      lonname=TRIM(cn_lon)
      latname=TRIM(cn_lat)
   
      CALL iom_open( bathyfile, inum, lagrif=.FALSE. )
      
      ! check if lon/lat are 2D arrays
      id = iom_varid( inum, lonname, ddims )
      IF (ddims(2)==0) THEN
         lonlat_2D = .FALSE.
      ELSE
         lonlat_2D = .TRUE.
      ENDIF   
      
      id = iom_varid( inum, bathyname, ddims )
      ln_pacifiq = .FALSE.
      
      status=-1

      IF (lonlat_2D) THEN
         ! here implicitly it's old topo database (orca format)
         ALLOCATE(coarselon  (ddims(1),ddims(2)), STAT=status(1)) 
         ALLOCATE(coarselat  (ddims(1),ddims(2)), STAT=status(2)) 
         ALLOCATE(coarsebathy(ddims(1),ddims(2)), STAT=status(3)) 
         IF( sum(status) /= 0 )   CALL ctl_stop( 'STOP', 'dom_bat : unable to allocate arrays' )
         CALL iom_get  ( inum, jpdom_unknown, lonname, coarselon )
         CALL iom_get  ( inum, jpdom_unknown, latname, coarselat )
         CALL iom_get  ( inum, jpdom_unknown, bathyname, coarsebathy )
         CALL iom_close (inum)
         DO ji = 2, ddims(1)
            IF( SIGN(1.,coarselon(ji,1))  /=  SIGN(1.,coarselon(ji-1,1))) ln_pacifiq =.TRUE.
         ENDDO
         IF( ln_pacifiq ) THEN
            WHERE(coarselon < 0.001) 
                coarselon = Coarselon + 360.
             END WHERE
         ENDIF
     
         ! equivalent to new database
      ELSE
         ALLOCATE(lon_new1D(ddims(1)), lat_new1D(ddims(2)))
         CALL iom_get  ( inum, jpdom_unknown, lonname, lon_new1D )
         CALL iom_get  ( inum, jpdom_unknown, latname, lat_new1D )

         IF( MAXVAL(glamt) > 180 ) THEN  
            !          
            WHERE( lon_new1D < 0 )
                lon_new1D = lon_new1D + 360.
            END WHERE
            !     
            zdel = 0.5     
            i_min = MAXLOC(lon_new1D,mask = lon_new1D < MINVAL(glamt)-zdel)
            i_max = MINLOC(lon_new1D,mask = lon_new1D > MAXVAL(glamt)+zdel)                    
            j_min = MAXLOC(lat_new1D,mask = lat_new1D < MINVAL(gphit)-zdel)
            j_max = MINLOC(lat_new1D,mask = lat_new1D > MAXVAL(gphit)+zdel)
            !          
            tabdim1 = ( SIZE(lon_new1D) - i_min(1) + 1 ) + i_max(1)                    
            !          
            IF(j_min(1)-2 >= 1 .AND. j_max(1)+3 <= SIZE(lat_new1D,1) ) THEN
               j_min(1) = j_min(1)-2
               j_max(1) = j_max(1)+3
            ENDIF
            tabdim2 = j_max(1) - j_min(1) + 1
            !
            ALLOCATE(coarselon  (tabdim1,tabdim2), STAT=status(1))
            ALLOCATE(coarselat  (tabdim1,tabdim2), STAT=status(2))
            ALLOCATE(Coarsebathy(tabdim1,tabdim2), STAT=status(3)) 
            IF( SUM(status) /= 0 ) CALL ctl_stop( 'STOP', 'dom_bat : unable to allocate arrays' )          
            !
            DO ji = 1,tabdim1
               coarselat(ji,:) = lat_new1D(j_min(1):j_max(1))
            END DO
            !
            DO jj = 1, tabdim2                                 
               coarselon(1:SIZE(lon_new1D)-i_min(1)+1      ,jj) = lon_new1D(i_min(1):SIZE(lon_new1D))
               coarselon(2+SIZE(lon_new1D)-i_min(1):tabdim1,jj) = lon_new1D(1:i_max(1))   
            END DO
            !              
            CALL iom_get(inum, jpdom_unknown, bathyname,coarsebathy(1:SIZE(lon_new1D)-i_min(1)+1,:), &
            kstart=(/i_min(1),j_min(1)/),kcount=(/SIZE(lon_new1D)-i_min(1),tabdim2/))

            CALL iom_get(inum, jpdom_unknown, bathyname,coarsebathy(2+SIZE(lon_new1D)-i_min(1):tabdim1,:), &
            kstart=(/1,j_min(1)/),kcount=(/i_max(1),tabdim2/))                
            !
         ELSE
 
           !
            i_min = MAXLOC(lon_new1D,mask = lon_new1D < MINVAL(glamt)-zdel)
            i_max = MINLOC(lon_new1D,mask = lon_new1D > MAXVAL(glamt)+zdel)
            j_min = MAXLOC(lat_new1D,mask = lat_new1D < MINVAL(gphit)-zdel)
            j_max = MINLOC(lat_new1D,mask = lat_new1D > MAXVAL(gphit)+zdel)
            !      
            IF(i_min(1)-2 >= 1 .AND. i_max(1)+3 <= SIZE(lon_new1D,1) ) THEN
               i_min(1) = i_min(1)-2
               i_max(1) = i_max(1)+3
            ENDIF
            tabdim1 = i_max(1) - i_min(1) + 1
            !
            IF(j_min(1)-2 >= 1 .AND. j_max(1)+3 <= SIZE(lat_new1D,1) ) THEN
               j_min(1) = j_min(1)-2
               j_max(1) = j_max(1)+3
            ENDIF
            tabdim2 = j_max(1) - j_min(1) + 1
            !
            ALLOCATE(coarselon  (tabdim1,tabdim2), STAT=status(1)) 
            ALLOCATE(coarselat  (tabdim1,tabdim2), STAT=status(2)) 
            ALLOCATE(coarsebathy(tabdim1,tabdim2), STAT=status(3)) 
            IF( SUM(status) /= 0 ) CALL ctl_stop( 'STOP', 'dom_bat : unable to allocate arrays' )  
            !
            DO jj = 1,tabdim2
               coarselon(:,jj) = lon_new1D(i_min(1):i_max(1))
            END DO
            !      
            DO ji = 1,tabdim1
              coarselat(ji,:) = lat_new1D(j_min(1):j_max(1)) 
            END DO
            !
            CALL iom_get(inum,jpdom_unknown,bathyname,coarsebathy, &
            &                  kstart=(/i_min(1),j_min(1)/),kcount=(/tabdim1,tabdim2/))
            !
         ENDIF   ! > 180
    
         DEALLOCATE(lon_new1D) ; DEALLOCATE(lat_new1D)
         CALL iom_close (inum)
         coarsebathy = coarsebathy *rn_scale
        ! reset land to 0 
         WHERE (coarsebathy < 0.)        
          coarsebathy=0.
         ENDWHERE 
 
      ENDIF   ! external

      IF(lwp) THEN
         WRITE(numout,*) 'Interpolation of high resolution bathymetry on child grid'
         IF( nn_interp == 0 ) THEN
            WRITE(numout,*) 'Arithmetic average ...'
         ELSE IF( nn_interp == 1 ) THEN
            WRITE(numout,*) 'Median average ...'
         ELSE IF( nn_interp == 2 ) THEN     
            WRITE(numout,*) 'Bilinear interpolation ...'
         ELSE     
            WRITE(*,*) 'bad value for nn_interp variable ( must be 0,1 or 2 )'
            STOP 
         ENDIF
      ENDIF  
      !
      !------------------------------------
      !MEDIAN AVERAGE or ARITHMETIC AVERAGE
      !------------------------------------
      !
      IF( nn_interp == 0 .OR. nn_interp == 1 ) THEN 
         !
         ALLOCATE(trouble_points(jpi,jpj))
         trouble_points = 0
         !
         !  POINT DETECTION
         !
         !                       
         DO jj = 2,jpj-1
            DO ji = 2,jpi-1
               !     
               ! FINE GRID CELLS DEFINITION               
               !
               Cell_lonmin = MIN(glamf(ji-1,jj-1),glamf(ji,jj-1),glamf(ji,jj),glamf(ji-1,jj))
               Cell_lonmax = MAX(glamf(ji-1,jj-1),glamf(ji,jj-1),glamf(ji,jj),glamf(ji-1,jj))
               Cell_latmin = MIN(gphif(ji-1,jj-1),gphif(ji,jj-1),gphif(ji,jj),gphif(ji-1,jj))
               Cell_latmax = MAX(gphif(ji-1,jj-1),gphif(ji,jj-1),gphif(ji,jj),gphif(ji-1,jj))
               !               
               ! SEARCH FOR ALL POINTS CONTAINED IN THIS CELL
               !               
               iimin = 1
               DO WHILE( coarselon(iimin,1) < Cell_lonmin ) 
                  iimin = iimin + 1
               ENDDO
               !               
               jjmin = 1
               DO WHILE( coarselat(iimin,jjmin) < Cell_latmin ) 
                  jjmin = jjmin + 1
               ENDDO
               !                
               iimax = iimin 
               DO WHILE( coarselon(iimax,1) <= Cell_lonmax ) 
                  iimax = iimax + 1
                  iimax = MIN( iimax,SIZE(coarsebathy,1))
               ENDDO
               !                               
               jjmax = jjmin 
               DO WHILE( coarselat(iimax,jjmax) <= Cell_latmax ) 
                  jjmax = jjmax + 1
                  jjmax = MIN( jjmax,SIZE(coarsebathy,2))
               ENDDO
               !
               iimax = iimax-1
               jjmax = jjmax-1
               !               
               iimin = MAX( iimin,1 )
               jjmin = MAX( jjmin,1 )
               iimax = MIN( iimax,SIZE(coarsebathy,1))
               jjmax = MIN( jjmax,SIZE(coarsebathy,2))

               nxhr = iimax - iimin + 1
               nyhr = jjmax - jjmin + 1                    

               IF( nxhr == 0 .OR. nyhr == 0 ) THEN
                  trouble_points(ji,jj) = 1
               ELSE

                  ALLOCATE( vardep(nxhr,nyhr) )
                  ALLOCATE( mask_oce(nxhr,nyhr) )
                  mask_oce = 0       

                  vardep(:,:) = coarsebathy(iimin:iimax,jjmin:jjmax)

                  WHERE( vardep(:,:) .GT. 0. )  mask_oce = 1

!                 IF( SUM(mask_oce) == 0 ) THEN
                  IF( SUM(mask_oce) < 0.5*(nxhr*nyhr) ) THEN
                     bathy(ji,jj) = 0.
                  ELSE
                     IF( nn_interp == 0 ) THEN
                        ! Arithmetic average                   
                        bathy(ji,jj) = SUM (vardep(:,:)*mask_oce(:,:))/SUM(mask_oce)
                     ELSE
                        ! Median average       
                        !
                        vardep(:,:) = vardep(:,:)*mask_oce(:,:) - 100000*(1-mask_oce(:,:))
                        ALLOCATE(vardep1d(nxhr*nyhr))
                        vardep1d = RESHAPE(vardep,(/ nxhr*nyhr /) )
                        CALL ssort(vardep1d,nxhr*nyhr)
                        !
                        ! Calculate median
                        !
                        IF (MOD(SUM(mask_oce),2) .NE. 0) THEN
                           bathy(ji,jj) = vardep1d( SUM(mask_oce)/2 + 1)
                        ELSE
                           bathy(ji,jj) = ( vardep1d(SUM(mask_oce)/2) + vardep1d(SUM(mask_oce)/2+1) )/2.
                        END IF
                        DEALLOCATE(vardep1d)   
                     ENDIF
                  ENDIF
                  DEALLOCATE (mask_oce,vardep)
               ENDIF
            ENDDO
         ENDDO

         IF( SUM( trouble_points ) > 0 ) THEN
            CALL ctl_warn ('too much empty cells, proceed to bilinear interpolation')
            nn_interp = 2
         ENDIF

      ENDIF

     !
     ! create logical array masksrc
     !
      IF( nn_interp == 2) THEN 
         !
         identical_grids = .FALSE.

         IF( Agrif_Parent(jpi) == jpi  .AND.   &
             Agrif_Parent(jpj) == jpj  ) THEN
            IF( MAXVAL( ABS(coarselat(:,:)- gphit(:,:)) ) < 0.0001 .AND.   &
                 MAXVAL( ABS(coarselon(:,:)- gphit(:,:)) ) < 0.0001 ) THEN
               bathy(:,:)  = coarsebathy(:,:) 
                IF(lwp) WRITE(numout,*) 'same grid between ', bathyname,' and child domain'                   
               identical_grids = .TRUE.                          
            ENDIF
         ENDIF

         IF( .NOT. identical_grids ) THEN  
            ALLOCATE(masksrc(SIZE(coarsebathy,1),SIZE(coarsebathy,2)))
            ALLOCATE(bathy_test(jpi,jj))
            !
            !                    Where(G0%bathy_meter.le.0.00001) 
            !                  masksrc = .false.
            !              ElseWhere
            !
            masksrc = .TRUE.
            !
            !              End where                       
            !            
            ! compute remapping matrix thanks to SCRIP package            
            !                                  
            CALL get_remap_matrix(coarselat,gphit,   &
                 coarselon,glamt,   &
                 masksrc,matrix,src_add,dst_add)
            CALL make_remap(coarsebathy,bathy_test,jpi,jpj, &
                 matrix,src_add,dst_add)  
            !                                  
            bathy= bathy_test               
            !            
            DEALLOCATE(masksrc)
            DEALLOCATE(bathy_test) 
         ENDIF
        !            
      ENDIF

   END SUBROUTINE dom_bat

END MODULE dombat
