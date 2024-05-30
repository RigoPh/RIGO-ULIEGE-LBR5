C
      SUBROUTINE GRAPHIC(FNAME,INDAIG,INDRAI,NETO,NSOL,IS)
C
C******************************************************************************
C
C     Subroutine GRAPHIC
C
C     1. Read data from the files U.DES, W-V.DES, ... stored by LBR5.for
c                                                  and created in VISION.for
C     2. Store the data in file FIG.DAT for Visual Fortran subroutine QWSUB1.for
C        to show the distribution.
C
C     Hengfeng Wang, 25-01-1999
c     Rigo Philippe, 28-12-1999 (dessins relatifs aux données structurelles)
c     Rigo Philippe, fevrier 2001 (differents cas de charges, NSOL)
C
C     Modifié : 3-5-2003
C******************************************************************************
C
      IMPLICIT REAL *8(A-H,O-Z)
      CHARACTER*40 Title
      CHARACTER*10 FNAME
      CHARACTER*11 NOM   ! 'FichDessin\'
      CHARACTER*22 NOM1  ! 'A.MULTI.STRUC\STRUC-a\'
      CHARACTER*6 Units
      CHARACTER*2 NUMBER

      DIMENSION PXY(35,4)

	COMMON/LANGUE/ LANGUE   ! 1 Francais; 2 English
	COMMON/NOM/NOM,NOM1  

c	IS = Numero du cas de charge considere

 10   CONTINUE
      IF (LANGUE.EQ.2) GOTO 1

      WRITE(*,*)' ==>> CAS DE CHARGE no',IS
      WRITE(*,*)' ENTRER LE NUMERO DU DESSIN QUE VOUS SOUHAITEZ'
      WRITE(*,*)' ---------------------------------------------'
      WRITE(*,*)'  1 : U      déplacement longitudinal, OX'
      WRITE(*,*)'  2 : W-V    déplacement transversal, OY'
      WRITE(*,*)'  3 : Nx     effort longitudinal unitaire, sens OX'
      WRITE(*,*)'  4 : Ry-Ny  effort transversal unitaire,  sens OY'
      WRITE(*,*)'  5 : Mx     moment longitudinal unitaire, axe OY'
      WRITE(*,*)'  6 : My     moment transversal unitaire,  axe OX'
      WRITE(*,*)'  7 : Nxy    effort de cisaillement unitaire'
      WRITE(*,*)'  8 : Rx     effort tranchant unitaire(y compris raid)'
      WRITE(*,*)'  9 : Sx     contrainte longitudinale, bordé, z=0 (OX)'
      WRITE(*,*)' 10 : Sy     contrainte transversale,  bordé, z=0 (0Y)'
      WRITE(*,*)' 11 : Txy    contrainte de cisaillement, bordé, z=0'
      WRITE(*,*)' 12 : COMP   contrainte de comparaison,  bordé, Z=0'
      WRITE(*,*)' 13 : PRESS  diagramme des sollicitations'
      WRITE(*,*)' 14 : Ry     effort tranchant unit.  (y compris cadre)'
      IF(INDAIG.EQ.1) THEN
       WRITE(*,*)' 15 : Sy cadre, Contr. trans, Jonct. Ame-semelle, JAS'
       WRITE(*,*)' 16 : Txy cadre, Contr. Cisail, Jonct. Ame-bordé, JAB'
       WRITE(*,*)' 17 : Contr. Comp. (cadre),   Jonct. Ame-semelle, JAS'
       WRITE(*,*)' 18 : Contr. Comp. (cadre),   Jonct. Ame-semelle, JAB'
	END IF
c     IF(INDRAI.EQ.1) THEN
c        WRITE(*,*)' 17 : S raid , Contr. Long, Jonct. Ame-semelle, JAS'
c        WRITE(*,*)' 18 : T raid , Contr. Cisail,Jonct. Ame-bordé,  JAB'
c	END IF
      WRITE(*,*)' 30,31,32,33: Poids Propre; Charge CHA1; CHA2-3, Mesh'

      WRITE(*,*)' ==> Pour visualiser l''echantillonage de la structure'
      WRITE(*,*)' 50 : Epaisseur du bordé'
      WRITE(*,*)' 51 : Hauteur   de l''ame des cadres (transversaux)'
      WRITE(*,*)' 52 : Epaisseur de l''ame des cadres (transversaux)'
      WRITE(*,*)' 53 : Largeur semelle     des cadres (transversaux)'
      WRITE(*,*)' 54 : Epaisseur semelle   des cadres (transversaux)'
      WRITE(*,*)' 55 : Entredistance     entre cadres (transversaux)'
      WRITE(*,*)' 56 : Hauteur   de l''ame des raidisseurs (lisses)'
      WRITE(*,*)' 57 : Epaisseur de l''ame des raidisseurs (lisses)'
      WRITE(*,*)' 58 : Largeur semelle     des raidisseurs (lisses)'
      WRITE(*,*)' 59 : Epaisseur semelle   des raidisseurs (lisses)'
      WRITE(*,*)' 60 : Entredistance     entre raidisseurs (lisses)'
      WRITE(*,*)' -->> STOP ENTER 99 '
      WRITE(*,*)' -->> Pour CHANGER de cas de charges ENTRER "-IS"',
     *          ' avec IS le cas de charges choisi'
      WRITE(*,*)'      Exemple: pour le cas 3 : ENTRER -3'
      WRITE(*,*)' -->> Entrer votre choix SVP!'
      READ(*,*) ID
      GOTO 3

  1   CONTINUE ! English
      WRITE(*,*)' ==>> LOAD CASE no',IS
      WRITE(*,*)' ENTER the No of the DRAWING TO PLOT'
      WRITE(*,*)' -----------------------------------'
      WRITE(*,*)'  1 : U      Longitudinal Displacement  (OX)'
      WRITE(*,*)'  2 : W-V    Transverse Displacement,  (plane:OYZ)'
      WRITE(*,*)'  3 : Nx     Unitary Longitudinal Forces (OX)'
      WRITE(*,*)'  4 : Ry-Ny  Unitary Combined transverse Forces (OXY)'
      WRITE(*,*)'  5 : Mx     Unitary Longitudinal Bending Moment'
      WRITE(*,*)'  6 : My     Unitary Transverse Bending Moment'
      WRITE(*,*)'  7 : Nxy    Unitary in-plane Shear forces'
      WRITE(*,*)'  8 : Rx     Unitary reaction (Stiffener))'
      WRITE(*,*)'  9 : Sx     Longitudinal Stress in the plate, z=0(OX)'
      WRITE(*,*)' 10 : Sy     Transverse  Stress in the plate,  z=0(0Y)'
      WRITE(*,*)' 11 : Txy    Shearing Stress in the plate, z=0'
      WRITE(*,*)' 12 : COMP   Von Mises Combined Stress, plate, z=0'
      WRITE(*,*)' 13 : PRESS  Load Pattern'
      WRITE(*,*)' 14 : Ry     Unitary reaction (Frame)'
      IF(INDAIG.EQ.1) THEN
       WRITE(*,*)' 15 : Sy Frame,  Trans Stress, junct. Web-Flange, JWF'
       WRITE(*,*)' 16 : Txy Frame, Shear Stress, junct. Web-Plate,  JWP'
       WRITE(*,*)' 17 : Combined Stress (Frame), junct. Web-Flange, JWF'
       WRITE(*,*)' 18 : Combined Stress (Frame), junct. Web-Flange, JWP'
	END IF
c      IF(INDRAI.EQ.1) THEN
c        WRITE(*,*)' 17 : S stiff , Long. Stress, junct. Web-Flange, JWF'
c        WRITE(*,*)' 18 : T stiff , Shear Stress, junct. Web-Plate,  JWP'
c	END IF
      WRITE(*,*)' 30,31,32,33: Deadweight; Loads CHA1; CHA2-3, Mesh'

      WRITE(*,*)' ==>> To visualize the structure scantling'
      WRITE(*,*)' 50 : Plate Thickness'
      WRITE(*,*)' 51 : Web Height       of  Frames (transverse)'
      WRITE(*,*)' 52 : Web Thickness    of  Frames (transverse)'
      WRITE(*,*)' 53 : Flange Width     of  Frames (transverse)'
      WRITE(*,*)' 54 : Flange Thickness of  Frames (transverse)'
      WRITE(*,*)' 55 : Spacing between Frames      (transverse)'
      WRITE(*,*)' 56 : Web Height       of  Stiffeners (long.)'
      WRITE(*,*)' 57 : Web Thickness    of  Stiffeners (long.)'
      WRITE(*,*)' 58 : Flange Width     of  Stiffeners (long.)'
      WRITE(*,*)' 59 : Flange Thickness of  Stiffeners (long.)'
      WRITE(*,*)' 60 : Spacing between Stiffeners      (long.)'
      WRITE(*,*)' -->> TO STOP , ENTER  99'
      WRITE(*,*)' -->> To CHANGE the Load Case ENTRER "-IS"',
     *                ' with IS the new considered load case'
      WRITE(*,*)'      Example: for CASE 3 : ENTRER -3'
      WRITE(*,*)' -->> Input your choice, Please!'
      READ(*,*) ID

  3   CONTINUE

      IF(ID.LT.0) THEN
	  IS=-ID
	  IF(IS.GT.NSOL) IS=1
	  GOTO 10
	ENDIF

      NOM='FichDessin\'
	IF(IS.EQ.1) NUMBER='01'
	IF(IS.EQ.2) NUMBER='02'
	IF(IS.EQ.3) NUMBER='03'
	IF(IS.EQ.4) NUMBER='04'
	IF(IS.EQ.5) NUMBER='05'
	IF(IS.EQ.6) NUMBER='06'
	IF(IS.EQ.7) NUMBER='07'
	IF(IS.EQ.8) NUMBER='08'
	IF(IS.EQ.9) NUMBER='09'
	IF(IS.EQ.10)NUMBER='10'

      IF(ID.EQ.1) THEN
	  OPEN(22,file=NOM1//NOM//'U'//NUMBER//'.DES')
	  GOTO 30
      ELSE IF(ID.EQ.2) THEN
	  OPEN(22,file=NOM1//NOM//'W-V'//NUMBER//'.DES')
	  GOTO 30
	ELSE IF(ID.EQ.3)  THEN
	  OPEN(22,file=NOM1//NOM//'NX'//NUMBER//'.DES')
	  GOTO 30
	ELSE IF(ID.EQ.4)  THEN
	  OPEN(22,file=NOM1//NOM//'RY-NY'//NUMBER//'.DES')
	  GOTO 30
	ELSE IF(ID.EQ.5)  THEN
	  OPEN(22,file=NOM1//NOM//'MX'//NUMBER//'.DES')
	  GOTO 30
	ELSE IF(ID.EQ.6)  THEN
	  OPEN(22,file=NOM1//NOM//'MY'//NUMBER//'.DES')
	  GOTO 30
	ELSE IF(ID.EQ.7)  THEN
	  OPEN(22,file=NOM1//NOM//'NXY'//NUMBER//'.DES')
	  GOTO 30
	ELSE IF(ID.EQ.8)  THEN
	  OPEN(22,file=NOM1//NOM//'RX'//NUMBER//'.DES')
        GOTO 30
	ELSE IF(ID.EQ.9)  THEN
	  OPEN(22,file=NOM1//NOM//'SX'//NUMBER//'.DES')
	  GOTO 30
	ELSE IF(ID.EQ.10)  THEN
	  OPEN(22,file=NOM1//NOM//'SY'//NUMBER//'.DES')
	  GOTO 30
	ELSE IF(ID.EQ.11)  THEN
	  OPEN(22,file=NOM1//NOM//'TXY'//NUMBER//'.DES')
	  GOTO 30
	ELSE IF(ID.EQ.12)  THEN
	  OPEN(22,file=NOM1//NOM//'COMP'//NUMBER//'.DES')
	  GOTO 30
	ELSE IF(ID.EQ.13)  THEN
	  OPEN(22,file=NOM1//NOM//'PRESS'//NUMBER//'.DES')
	  GOTO 30
	ELSE IF(ID.EQ.14)  THEN
	  OPEN(22,file=NOM1//NOM//'RY'//NUMBER//'.DES')
	  GOTO 30
	END IF

	IF(INDAIG.EQ.1) THEN
	  IF(ID.EQ.15)  THEN
	    OPEN(22,file=NOM1//NOM//'S cadre'//NUMBER//'.DES')
	    GOTO 30
	  ELSE IF(ID.EQ.16)  THEN
	    OPEN(22,file=NOM1//NOM//'T cadre'//NUMBER//'.DES')
	    GOTO 30
	  END IF
c      END IF

c      IF(INDRAI.EQ.1) THEN
	  IF(ID.EQ.17) THEN
c	    OPEN(22,file=NOM1//NOM//'S raid'//NUMBER//'.DES')
	    OPEN(22,file=NOM1//NOM//'Sc JAS'//NUMBER//'.DES')
	    GOTO 30
	  ELSE IF(ID.EQ.18) THEN
c	    OPEN(22,file=NOM1//NOM//'T raid'//NUMBER//'.DES')
	    OPEN(22,file=NOM1//NOM//'Sc JAB'//NUMBER//'.DES')
	    GOTO 30
	  END IF
	END IF

	IF(ID.EQ.30)  THEN
	  OPEN(22,file=NOM1//NOM//'POIDS.DES')
	  GOTO 30
	ELSE IF(ID.EQ.31)  THEN
	  OPEN(22,file=NOM1//NOM//'CHA1.DES')
	  GOTO 30
	ELSE IF(ID.EQ.32)  THEN
	  OPEN(22,file=NOM1//NOM//'CHA2.DES')
	  GOTO 30
	ELSE IF(ID.EQ.33)  THEN
	  OPEN(22,file=NOM1//NOM//'MESH.DES')
	  GOTO 30
	END IF


      IF(ID.EQ.50) THEN
	  OPEN(22,file=NOM1//NOM//'T bordage.des')
	  GOTO 30
      ELSE IF(ID.EQ.51) THEN
	  OPEN(22,file=NOM1//NOM//'H ame Cadres.des')
	  GOTO 30
      ELSE IF(ID.EQ.52) THEN
	  OPEN(22,file=NOM1//NOM//'T ame Cadres.des')
	  GOTO 30
      ELSE IF(ID.EQ.53) THEN
	  OPEN(22,file=NOM1//NOM//'L sem Cadres.des')
	  GOTO 30
      ELSE IF(ID.EQ.54) THEN
	  OPEN(22,file=NOM1//NOM//'T sem Cadres.des')
	  GOTO 30
      ELSE IF(ID.EQ.55) THEN
	  OPEN(22,file=NOM1//NOM//'Espac Cadres.des')
	  GOTO 30
      ELSE IF(ID.EQ.56) THEN
	  OPEN(22,file=NOM1//NOM//'H ame Raid.des')
	  GOTO 30
      ELSE IF(ID.EQ.57) THEN
	  OPEN(22,file=NOM1//NOM//'T ame Raid.des')
	  GOTO 30
      ELSE IF(ID.EQ.58) THEN
	  OPEN(22,file=NOM1//NOM//'L sem Raid.des')
	  GOTO 30
      ELSE IF(ID.EQ.59) THEN
	  OPEN(22,file=NOM1//NOM//'T sem Raid.des')
	  GOTO 30
      ELSE IF(ID.EQ.60) THEN
	  OPEN(22,file=NOM1//NOM//'Espac Raid.des')
	  GOTO 30
	ENDIF
	
      IF(ID.EQ.99) GOTO 200

      WRITE(*,*)'  '
	IF(LANGUE==1) THEN
	  WRITE(*,*)' Choix incorrect!'
	  WRITE(*,*)' Press any key to continue'
	ELSE
	  WRITE(*,*)' Your choice is wrong!'
	  WRITE(*,*)' Press any key to continue'
	ENDIF
	WRITE(*,*)'  '
	WRITE(*,*)'  '
	READ(*,*)
	GOTO 10 ! restart
!
! Read general data
!
 30   CONTINUE ! end of loop

      WRITE(*,*)' Element =',ID

      READ(22,*) Vmax,Scale
      READ(22,'(A)') Units
	READ(22,'(A)') Title

	OPEN(20,file=NOM//'TEMPT')
!
! Data for first panel
!
	READ(22,*)IP,MT

	M = 11+2*MT
	DO I=1,M      ! Read the data
	  READ(22,*)IP,(PXY(I,J),J=1,4)
	END DO
!
!     Find Xmin, Ymin, Xmax, Ymax.
!     ----------------------------
!
	IF (PXY(1,1).LT.PXY(1,3)) THEN ! Set Xmin and Xmax
	  Xmin=PXY(1,1)
	  Xmax=PXY(1,3)
	ELSE
	  Xmin=PXY(1,3)
	  Xmax=PXY(1,1)
	END IF

	IF (PXY(1,2).LT.PXY(1,4)) THEN ! Set Ymin and Ymax
	  Ymin=PXY(1,2)
	  Ymax=PXY(1,4)
	ELSE
	  Ymin=PXY(1,4)
	  Ymax=PXY(1,2)
	END IF

	DO I=2,M      ! Set ranges of the coordinates
	  DO J=1,2
	    K=(J-1)*2
	    IF (PXY(I,K+1).GT.Xmax) Xmax=PXY(I,K+1)
	    IF (PXY(I,K+1).LT.Xmin) Xmin=PXY(I,K+1)
	    IF (PXY(I,K+2).GT.Ymax) Ymax=PXY(I,K+2)
	    IF (PXY(I,K+2).LT.Ymin) Ymin=PXY(I,K+2)
	  END DO
	END DO
!
! Set data for drawing
!
	ML=1   ! Main plate
	N=11    ! Points
	NP=0    ! Color NP=0 Black

	WRITE(20,*)N,NP
	DO I=1,N
	  WRITE(20,*)PXY(I,1),PXY(I,2)
	END DO

	IF (MT.GE.1) THEN  ! Crossbars
	  DO I=1,MT          ! Each crossbar 
	    IK=11+(I-1)*2
	    DO J=1,2         ! Two lines
	      ML=ML+1
	      N=2
	      NP=0
	      WRITE(20,*)N,NP
            WRITE(20,*)PXY(IK+J,1),PXY(IK+J,2)
	      WRITE(20,*)PXY(IK+J,3),PXY(IK+J,4)
	    END DO
	  END DO
	END IF

	DO I=1,10             ! Distributions
	  IF (I.LT.10) THEN
	    ML=ML+1
	    N=3
	    NP=12    ! Color NP=12 Bright Red

          WRITE(20,*)N,NP
          WRITE(20,*)PXY(I,1),PXY(I,2)
	    WRITE(20,*)PXY(I,3),PXY(I,4)
	    WRITE(20,*)PXY(I+1,3),PXY(I+1,4)
	  ELSE
	    ML=ML+1
	    N=4
	    NP=12
          WRITE(20,*)N,NP
          WRITE(20,*)PXY(I,1),PXY(I,2)
	    WRITE(20,*)PXY(I,3),PXY(I,4)
	    WRITE(20,*)PXY(I+1,3),PXY(I+1,4)
	    WRITE(20,*)PXY(I+1,1),PXY(I+1,2)
	  END IF
	END DO
!
! For other panels	
!
	IF (NETO.EQ.1) GOTO 100

	DO NEL = 2,NETO  ! Read data from other panels

	  READ(22,*)IP,MT
	  M = 11+2*MT
	  DO I=1,M      ! Read data from panel NEL
	    READ(22,*)IP,(PXY(I,J),J=1,4)
	  END DO

	  DO I=1,M      ! Set ranges of the coordinates
	    DO J=1,2
	      K=(J-1)*2
	      IF (PXY(I,K+1).GT.Xmax) Xmax=PXY(I,K+1)
	      IF (PXY(I,K+1).LT.Xmin) Xmin=PXY(I,K+1)
	      IF (PXY(I,K+2).GT.Ymax) Ymax=PXY(I,K+2)
	      IF (PXY(I,K+2).LT.Ymin) Ymin=PXY(I,K+2)
	    END DO
	  END DO

!
!    Set data for drawing
!
	ML=ML+1   ! Main plate
	N=11
	NP=0
	WRITE(20,*)N,NP
	DO I=1,N
	  WRITE(20,*)PXY(I,1),PXY(I,2)
	END DO

	IF (MT.GE.1) THEN  ! Crossbars
	  DO I=1,MT
	    IK=11+(I-1)*2
	    DO J=1,2
	      ML=ML+1
	      N=2
	      NP=0
	      WRITE(20,*)N,NP
            WRITE(20,*)PXY(IK+J,1),PXY(IK+J,2)
	      WRITE(20,*)PXY(IK+J,3),PXY(IK+J,4)
	    END DO
	  END DO
	END IF

	DO I=1,10             ! Distributions
	  IF (I.LT.10) THEN
	    ML=ML+1
	    N=3
	    NP=12
          WRITE(20,*)N,NP
          WRITE(20,*)PXY(I,1),PXY(I,2)
	    WRITE(20,*)PXY(I,3),PXY(I,4)
	    WRITE(20,*)PXY(I+1,3),PXY(I+1,4)
	  ELSE
	    ML=ML+1
	    N=4
	    NP=12
          WRITE(20,*)N,NP
          WRITE(20,*)PXY(I,1),PXY(I,2)
	    WRITE(20,*)PXY(I,3),PXY(I,4)
	    WRITE(20,*)PXY(I+1,3),PXY(I+1,4)
	    WRITE(20,*)PXY(I+1,1),PXY(I+1,2)
	  END IF
	END DO

	END DO ! Finish on all the panels
!
! Write the data for drawing
!
 100	OPEN(21,file='FIG.DAT')

	IF (LANGUE.EQ.1) WRITE(21,'(3A)')TITLE,' en ',Units
	IF (LANGUE.EQ.2) WRITE(21,'(3A)')TITLE,' in ',Units
	WRITE(21,*)Vmax,Xmin,Ymin,Xmax,Ymax,ML

	REWIND(20)

	DO I=1,ML
	  READ(20,*)N,NP
	  WRITE(21,*)N,NP
	  DO J=1,N
	    READ(20,*)X,Y
	    WRITE(21,*)X,Y
	  END DO
	END DO

      WRITE(FNAME,'(A)')'FIG.DAT'   ! cad FNAME = 'FIG.DAT'
	CLOSE(22)
	CLOSE(20)
	CLOSE(21)
	RETURN

 200  WRITE(FNAME,'(A)')'FINISH'   ! cad FNAME = 'FINISH'

 300  RETURN
      END
