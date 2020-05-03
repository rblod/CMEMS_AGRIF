#if defined key_agrif
   SUBROUTINE Agrif_connection
   use dom_oce
   use agrif_parameters
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE  Agrif_Sponge ***
      !!----------------------------------------------------------------------
      INTEGER  ::   ji, jj, ind1, ind2
      INTEGER  ::   ispongearea, istart
      REAL(wp) ::   z1_spongearea
      !!----------------------------------------------------------------------
      !
         ! Define ramp from boundaries towards domain interior at T-points
         ! Store it in ztabramp

         ALLOCATE(ztabramp(jpi,jpj))
         ispongearea = 1 + npt_connect * Agrif_irhox()
         istart = npt_copy * Agrif_irhox()
         z1_spongearea = 1._wp / REAL( ispongearea )
         
         ztabramp(:,:) = 0._wp

         ! --- West --- !
         IF( (nbondi == -1) .OR. (nbondi == 2) ) THEN
            ind1 = 1+nbghostcells + istart
            ind2 = ind1 + ispongearea 
            DO jj = 1, jpj
               DO ji = ind1, ind2                
                  ztabramp(ji,jj) = REAL( ind2 - ji ) * z1_spongearea * umask(ind1,jj,1)
               END DO
            ENDDO
         ENDIF

         ! --- East --- !
         IF( (nbondi == 1) .OR. (nbondi == 2) ) THEN
            !ind1 = nlci - nbghostcells - ispongearea
            ind2 = nlci - nbghostcells - istart
            ind1 = ind2 -ispongearea
           
            
            DO jj = 1, jpj
               DO ji = ind1, ind2
                  ztabramp(ji,jj) = MAX( ztabramp(ji,jj), REAL( ji - ind1 ) * z1_spongearea * umask(ind2-1,jj,1) )
               ENDDO
            ENDDO
         ENDIF

         ! --- South --- !
         IF( (nbondj == -1) .OR. (nbondj == 2) ) THEN
            ! ind1 = 1+nbghostcells
            ! ind2 = 1+nbghostcells + ispongearea
            ind1 = 1+nbghostcells + istart
            ind2 = ind1 + ispongearea 
            DO jj = ind1, ind2 
               DO ji = 1, jpi
                  ztabramp(ji,jj) = MAX( ztabramp(ji,jj), REAL( ind2 - jj ) * z1_spongearea * vmask(ji,ind1,1) )
               END DO
            ENDDO
         ENDIF

         ! --- North --- !
         IF( (nbondj == 1) .OR. (nbondj == 2) ) THEN
            ! ind1 = nlcj - nbghostcells - ispongearea
            ! ind2 = nlcj - nbghostcells
            
            ind2 = nlcj - nbghostcells - istart
            ind1 = ind2 -ispongearea
            
            DO jj = ind1, ind2
               DO ji = 1, jpi
                  ztabramp(ji,jj) = MAX( ztabramp(ji,jj), REAL( jj - ind1 ) * z1_spongearea * vmask(ji,ind2-1,1) )
               END DO
            ENDDO
         ENDIF
      !
      !
   END SUBROUTINE Agrif_connection
   
   SUBROUTINE Agrif_make_connection
   use dom_oce
   use agrif_parameters
      !!----------------------------------------------------------------------
      !!                 *** ROUTINE  Agrif_Sponge ***
      !!----------------------------------------------------------------------
      INTEGER  ::   ji, jj, ind1, ind2
      INTEGER  ::   ispongearea, istart
      REAL(wp) ::   z1_spongearea
      !!----------------------------------------------------------------------
      !
         ! Define ramp from boundaries towards domain interior at T-points
         ! Store it in ztabramp

         ispongearea = 1 + npt_connect * Agrif_irhox()
         istart = npt_copy * Agrif_irhox()

         ! --- West --- !
         IF( (nbondi == -1) .OR. (nbondi == 2) ) THEN
            
            ind1 = 1+nbghostcells + istart
            ind2 = ind1 + ispongearea
            DO jk=1,jpk            
             DO jj = 1, jpj
               DO ji = ind1, ind2
                
                ! print *,'VAL = ',ztabramp(ji,jj)*e3t_interp(ji,jj,jk)+(1.-ztabramp(ji,jj))*e3t_0(ji,jj,jk), &
                ! e3t_0(ji,jj,jk)
                  e3t_0(ji,jj,jk) = ztabramp(ji,jj)*e3t_interp(ji,jj,jk)+(1.-ztabramp(ji,jj))*e3t_0(ji,jj,jk)
               ENDDO
              ENDDO
            ENDDO
         ENDIF

         ! --- East --- !
         IF( (nbondi == 1) .OR. (nbondi == 2) ) THEN
            ind2 = nlci - nbghostcells - istart
            ind1 = ind2 -ispongearea
            DO jk=1,jpk    
            DO jj = 1, jpj
               DO ji = ind1, ind2
                  e3t_0(ji,jj,jk) = ztabramp(ji,jj)*e3t_interp(ji,jj,jk)+(1.-ztabramp(ji,jj))*e3t_0(ji,jj,jk)
               ENDDO
            ENDDO
            ENDDO
         ENDIF

         ! --- South --- !
         IF( (nbondj == -1) .OR. (nbondj == 2) ) THEN
            ind1 = 1+nbghostcells + istart
            ind2 = ind1 + ispongearea 
            DO jk=1,jpk    
            DO jj = ind1, ind2 
               DO ji = 1, jpi
                  e3t_0(ji,jj,jk) = ztabramp(ji,jj)*e3t_interp(ji,jj,jk)+(1.-ztabramp(ji,jj))*e3t_0(ji,jj,jk)
               END DO
            ENDDO
            ENDDO
         ENDIF

         ! --- North --- !
         IF( (nbondj == 1) .OR. (nbondj == 2) ) THEN
            
            ind2 = nlcj - nbghostcells - istart
            ind1 = ind2 -ispongearea
            DO jk=1,jpk               
            DO jj = ind1, ind2
               DO ji = 1, jpi
                  e3t_0(ji,jj,jk) = ztabramp(ji,jj)*e3t_interp(ji,jj,jk)+(1.-ztabramp(ji,jj))*e3t_0(ji,jj,jk)
               END DO
            ENDDO
            ENDDO
         ENDIF
      !
      !
   END SUBROUTINE Agrif_make_connection
   
#else
subroutine agrif_connection_empty
end subroutine agrif_connection_empty
#endif