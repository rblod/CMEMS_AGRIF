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
   !!
   IMPLICIT NONE
   
   CALL nemo_init       !* Initializations of each fine grid
   
   print *,'JPI = ',jpi,nbcellsx
   
END SUBROUTINE Agrif_InitValues
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