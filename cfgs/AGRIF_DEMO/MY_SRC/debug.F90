
MODULE debug
   !!======================================================================
   !!                       ***  MODULE debug   ***
   !! Ocean debug : 
   !!======================================================================
   !! History :  4.0  !  2020-02  (R. Benshila)  Original code from CROCO
   !!                             see www.croco-ocean.eu
   !!----------------------------------------------------------------------
   USE par_oce        ! ocean parameters
   USE dom_oce
   USE in_out_manager
   USE lib_mpp

   IMPLICIT NONE 
   PRIVATE

   PUBLIC :: debug_ini, check_tab

!$AGRIF_DO_NOT_TREAT
   LOGICAL :: debug_write
   INTEGER :: nbprocs_in
   INTEGER, DIMENSION(:), ALLOCATABLE :: debug_procs_units
!$AGRIF_END_DO_NOT_TREAT

#if defined key_mpp_mpi
   INCLUDE 'mpif.h'
#endif

   INTERFACE check_tab
      MODULE PROCEDURE check_tab2d, check_tab3d
   END INTERFACE

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE debug_ini
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE debug_init  ***
      !!
      !! ** Purpose :   Create or open binary file with debug informations
      !!
      !! ** Method  :   Shared memory computing, set the local processor
      !!              variables to the value of the global domain
      !!----------------------------------------------------------------------
      !
      CHARACTER(len=80) :: filename
      INTEGER :: nb, new_unit
          
 !tmeporary
      debug_write = ln_debug_write  

      filename = "check_file"
      IF( .NOT. debug_write ) THEN
         OPEN(107, file="debug_infos", form="formatted", status='OLD')
         READ(107,*) nbprocs_in
         CLOSE(107)
         ALLOCATE( debug_procs_units(0:nbprocs_in-1) )
         DO nb = 0, nbprocs_in-1
            WRITE(filename, '(a11,i0,a1,i0)') "check_file_", nbprocs_in, '_', nb
!RB : quickfix before updation conv to deal with newunit
!$AGRIF_DO_NOT_TREAT
            OPEN(newunit=new_unit,                    &                
        &        file=filename,form= 'unformatted',    &
        &        status='OLD')
!$AGRIF_END_DO_NOT_TREAT
            debug_procs_units(nb) = new_unit
         END DO     
      ELSE
#if defined key_mpp_mpi
         IF (narea == 1) THEN
            OPEN(107, file="debug_infos", form="formatted",status='REPLACE')
            WRITE(107,*) jpnij
            CLOSE(107)
         ENDIF
         WRITE(*,*) 'MYNODE = ',narea-1
         WRITE(filename, '(a11,i0,a1,i0)') "check_file_", jpnij,'_', narea-1
#else
         OPEN(107, file="debug_infos", form="formatted")   
         WRITE(107,*) 1
         CLOSE(107)
         WRITE(filename, '(a14)') "check_file_1_0"
#endif
         OPEN(107, file=filename, form='unformatted', status='REPLACE')   
      ENDIF
      !
   END SUBROUTINE debug_ini


   SUBROUTINE check_tab2d( tab, comment )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE mpp_init  ***
      !!
      !! ** Purpose :   Lay out the global domain over processors.
      !!
      !! ** Method  :   Shared memory computing, set the local processor
      !!              variables to the value of the global domain
      !!----------------------------------------------------------------------
      !
      CHARACTER(len=*), INTENT(in) :: comment
      REAL(wp), DIMENSION(jpi,jpj), INTENT(in) :: tab
      !
      LOGICAL :: mystop
      LOGICAL :: globstop
      INTEGER :: Lmseq, Mmseq
      INTEGER :: i0, j0, ji, jj, i1, j1, i2, j2
      INTEGER :: i1_r, i2_r, j1_r, j2_r, nb
      INTEGER :: iseq1, iseq2, jseq1, jseq2
      INTEGER :: ierr
      INTEGER, DIMENSION(2) :: lb, ub
      REAL(wp), DIMENSION(:,:), ALLOCATABLE :: tabread
          
      IF (debug_write) THEN

#if defined key_mpp_mpi
         i1 = nldi
         i2 = nlei
         j1 = nldj
         j2 = nlej
#endif
         WRITE(107) i1, i2, j1, j2
#if defined key_mpp_mpi
         WRITE(107) lbound(tab) + (/nimpp-1, njmpp-1/)
#else
         WRITE(107) lbound(tab)
#endif
      
#if defined key_mpp_mpi
         WRITE(107) ubound(tab) + (/nimpp-1, njmpp-1/)
#else
         WRITE(107) ubound(tab)      
#endif
         WRITE(107)tab
      ELSE
         DO nb = 0, nbprocs_in-1
            READ(debug_procs_units(nb)) i1_r, i2_r, j1_r, j2_r
            READ(debug_procs_units(nb)) lb
            READ(debug_procs_units(nb)) ub
            ALLOCATE( tabread(lb(1):ub(1),lb(2):ub(2)) )
            READ(debug_procs_units(nb)) tabread
      
            iseq1 = 1
            iseq2 = jpiglo
            jseq1 = 1
            jseq2 = jpjglo

            i1 = nldi
            i2 = nlei
            j1 = nldj
            j2 = nlej

            mystop =.FALSE.
            DebugExit : DO jj = MAX(j1,jseq1-njmpp+1), MIN(j2,jseq2-njmpp+1)
                           j0 = jj + njmpp - 1
                           DO ji = MAX(i1,iseq1-nimpp+1), MIN(i2,iseq2-nimpp+1)
                              i0 = ji + nimpp - 1
                              IF( tabread(i0,j0) /= tab(ji,jj) ) THEN
                                 WRITE(*,'(A,A,2x,5i4,3e20.12)')'BUGBIN = ',   &
     &                           comment ,narea-1, i0, j0, ji, jj,             &
     &                           tabread(i0,j0), tab(ji,jj),                   &
     &                           ABS( tabread(i0,j0)-tab(ji,jj) )            
#ifdef key_agrif
                                 WRITE(*,*) 'GRID# ', Agrif_CFixed()
#endif
                                 mystop= .TRUE.
                                 exit DebugExit
                              ENDIF
                           END DO
                        END DO DebugExit
#ifdef MPI         
            CALL MPI_ALLREDUCE( mystop, globstop, 1,       &
     &                   MPI_LOGICAL, MPI_LOR, MPI_COMM_OCE, ierr )
            mystop = globstop
#endif
            IF( mystop ) THEN
#ifdef key_mpp_mpi
               CALL MPI_BARRIER( MPI_COMM_OCE, ierr )
               CALL MPI_FINALIZE( ierr )
#endif
               STOP                            !-->  EXIT
            ENDIF            
            DEALLOCATE( tabread )
         END DO 
      ENDIF   ! READ/WRITE

#ifdef key_mpp_mpi
      CALL MPI_BARRIER( MPI_COMM_OCE, ierr )
#endif

      IF( .NOT. debug_write ) THEN
#ifndef key_agrif
         IF(lwp) WRITE(*,*) 'CHECK ', comment, ' PASSED'
#else      
         IF(lwp) WRITE(*,*) 'CHECK ', comment, ' PASSED'   &
        &                 , ' ON GRID ', Agrif_CFixed()
#endif
      ENDIF

   END SUBROUTINE check_tab2d


   SUBROUTINE check_tab3d( tab, comment )
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE mpp_init  ***
      !!
      !! ** Purpose :   Lay out the global domain over processors.
      !!
      !! ** Method  :   Shared memory computing, set the local processor
      !!              variables to the value of the global domain
      !!----------------------------------------------------------------------
      !
      REAL(wp), DIMENSION(jpi,jpj,jpk), INTENT(in) :: tab
      CHARACTER*(*), INTENT(in) :: comment
      !
      CHARACTER(len=80) :: comment_k
      LOGICAL :: mystop, globstop
      INTEGER :: Lmseq, Mmseq
      INTEGER :: ierr, ji, jj, jk
      INTEGER :: i0, j0, i1, i2, j1, j2
      INTEGER :: i1_r, i2_r, j1_r, j2_r, nb
      INTEGER :: iseq1, iseq2, jseq1, jseq2 
      INTEGER, DIMENSION(3) :: lb, ub
      REAL(wp), DIMENSION(:,:,:), ALLOCATABLE :: tabread
          
      IF( debug_write ) THEN
#if defined key_mpp_mpi
         i1 = i1 + nimpp - 1
         i2 = i2 + nimpp - 1
         j1 = j1 + njmpp - 1
         j2 = j2 + njmpp - 1
#endif
         WRITE(107) i1, i2, j1, j2
#if defined key_mpp_mpi
         WRITE(107) lbound(tab) + (/nimpp-1,njmpp-1,0/)
         WRITE(107) ubound(tab) + (/nimpp-1,njmpp-1,0/)
#else
         WRITE(107) lbound(tab)
         WRITE(107) ubound(tab)      
#endif
         WRITE(107) tab
      ELSE
         DO nb = 0, nbprocs_in-1
            READ(debug_procs_units(nb)) i1_r, i2_r, j1_r, j2_r
            READ(debug_procs_units(nb)) lb
            READ(debug_procs_units(nb)) ub
            ALLOCATE( tabread(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)) )
            READ(debug_procs_units(nb)) tabread

            iseq1 = i1_r
            iseq2 = i2_r
            jseq1 = j1_r
            jseq2 = j2_r
      
            mystop = .FALSE.
            DebugExit : DO jk = 1, jpk
                           DO jj = MAX(j1,jseq1-njmpp+1), MIN(j2,jseq2-njmpp+1)
                              j0 = jj+njmpp-1
                              DO ji = MAX(i1,iseq1-nimpp+1), MIN(i2,iseq2-nimpp+1)
                                 i0 = ji+nimpp-1
                                 IF( tabread(i0,j0,jk) /= tab(ji,jj,jk) ) THEN
                                    WRITE(*,'(A,A,2x,6i4,3e20.12)') 'BUGBIN = ',   &
     &                              comment,                                       &
     &                              narea-1, i0, j0, ji, jj, jk,                   &
     &                              tabread(i0, j0,jk), tab(ji,jj,jk),             &
     &                              ABS(tabread(i0,j0,jk)-tab(ji,jj,jk))
!                                   print *,'iif = ',iif,iic
#ifdef key_agrif
                                    WRITE(*,*) 'GRID# ', Agrif_CFixed()
#endif    
                                    mystop = .TRUE.
                                    exit DebugExit
                                 ENDIF
                              END DO
                           END DO
                        END DO DebugExit
#ifdef key_mpp_mpi   
            CALL MPI_ALLREDUCE( mystop, globstop, 1,   &
     &                   MPI_LOGICAL, MPI_LOR, MPI_COMM_OCE, ierr )
            mystop = globstop
#endif
            IF( mystop ) THEN
#ifdef key_mpp_mpi
               CALL MPI_BARRIER( MPI_COMM_OCE, ierr )
               CALL MPI_FINALIZE( ierr )
#endif
               stop                            !-->  EXIT
            ENDIF         
            DEALLOCATE( tabread )
         END DO 
      ENDIF

#ifdef key_mpp_mpi
      CALL MPI_BARRIER( MPI_COMM_OCE, ierr )
#endif

      IF( .NOT. debug_write ) THEN
#ifndef key_agrif
         IF(lwp) WRITE(*,*) 'CHECK ', comment, ' PASSED'
#else
         IF(lwp) WRITE(*,*) 'CHECK ', comment, ' PASSED'    &
     &                       , ' ON GRID ', Agrif_CFixed()
#endif
      ENDIF             
      !
   END SUBROUTINE check_tab3d    

END MODULE debug
