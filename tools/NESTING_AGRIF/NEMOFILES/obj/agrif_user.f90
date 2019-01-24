subroutine agrif_user

      use Agrif_Types, only : Agrif_tabvars


#include "Param_BeforeCall_agrif_user.h"

end subroutine agrif_user

subroutine agrif_initworkspace

      use Agrif_Types, only : Agrif_tabvars


#include "Param_BeforeCall_agrif_initworkspace.h"

end subroutine agrif_initworkspace

subroutine agrif_initvalues

      use Agrif_Types, only : Agrif_tabvars


#include "Param_BeforeCall_agrif_initvalues.h"

end subroutine agrif_initvalues

SUBROUTINE Agrif_detect( kg, ksizex )
      use Agrif_Types, only : Agrif_tabvars


#include "Param_BeforeCall_Agrif_detect.h"
      integer, dimension(1:2) :: ksizex
      integer, dimension(1: ksizex ( 1 ),1: ksizex ( 2 )) :: kg

      !!----------------------------------------------------------------------
      !!                      *** ROUTINE Agrif_detect ***
      !!----------------------------------------------------------------------
            !!----------------------------------------------------------------------
   !
   RETURN
   !
END SUBROUTINE Agrif_detect

SUBROUTINE agrif_before_regridding

      use Agrif_Types, only : Agrif_tabvars


#include "Param_BeforeCall_agrif_before_regridding.h"
END SUBROUTINE agrif_before_regridding
