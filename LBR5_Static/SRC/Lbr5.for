!
!******************************************************************************
!
!    Main program for LBR-5.6 by Digital Visual Fortran 6.0.
!    ====================================================
!
!     Basic program (called MAIN) is called by the main program called LBR5. After all the 
!     computation, the data for drawings are stored in the files U.DES, W-V.DES
!     and so on. Another file called INDICE.DAT is also built to store INDAIG,
!     INDRAI,NETO.
!
!     Subroutine GRAPHIC reads data from U.DES, W-V.DES and so on. For chosen
!     element,the data are transfered into file FIG.DAT.
!
!     Subroutine QWSUB1 shows the distribution on QuickWindows from the data
!     in FIG.DAT.
!
!     Last Modification :  Feb 1999  (Updated by Hengfeng Wang on 25-01-1999)
!                         1-11-2000  (Version anglaise)
!                         27-7-2005  (Mode Batch)

!******************************************************************************
!
      USE DFLIB
!

!
	COMMON/LANGUE/ LANGUE,IBATCH   ! 1 Francais; 2 English	!Modif Batch

      CHARACTER*11 NOM   ! 'FichDessin\'
      CHARACTER*22 NOM1  ! 'A.MULTI.STRUC\STRUC-a\'
	COMMON/NOM/NOM,NOM1  

	IBATCH = 1 !IBATCH = 1 MEANS MODE BATCH					!Modif Batch
			   !IBATCH = 0 MEANS NO BATCH					!Modif Batch

!      
!
! Call Standard Fortran Calculations of LBR5
! ****************************************
!
 10   CALL MAIN
!
! ****************************************
!
! Open graphic Window
!
c 20	OPEN(66,FILE='USER')

      IS=1  !cas de Charge pris en compte dans GRAPHIC (au debut du processus).
  100 CONTINUE
      
 200  WRITE(*,*)' **** COMPUTATION FINISHED ****'

c      STOP

	END
