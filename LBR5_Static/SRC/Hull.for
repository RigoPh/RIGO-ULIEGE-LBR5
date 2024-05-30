      SUBROUTINE HULL(ITERA,NETO,SY)
C     ********************

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION TEXT(15)

      COMMON/USHULL/	DEPTH,DB,UHOGM,USAGM,
     *				SYSD,SySB1,SySB,SySSup,SySSdw,
     *                IULT,IRESTR,
     *				ND, ID(150),	!extension neto
     *				NB1,IB1(150),	!extension neto
     *				NB, IB(150),	!extension neto
     *				NS, IS(150),	!extension neto
     *				KD,KB1,KB,KSup,KSdw

      DIMENSION  SY(NETO)

      COMMON/OPTI/ IOPTI

c23456789012345678901234567890123456789012345678901234567890123456789012
C ******************************************************************
C     SUBROUTINE HULL
C     ================

C     Read of the data for the subroutine USHULL
C
c     --------------------------------------------------------------
c
C     Created   :  18-2-97   (Dr. Ph. RIGO)
C
C     Modified  :  3-3-97 : Yield stress of stiffeners introduced as dat
C
C     Last modification : 25-5-99
C
C *******************************************************************
C  .    DEPARTMENT OF NAVAL ARCHITECTURE, ANAST                     .
C  .    UNIVERSITY OF LIEGE                                         .
C  .    6 quai Banning, 4000 Liege, BELGIUM                         .
C  .    (TEL) +32 4 366 9225  (FAX) +32 4 366 9133                  .
C  .    (e-mail) Ph.Rigo@ULg.ac.be                                  .
C *******************************************************************
c IRESTR (0 = NON;1 = OUI)
c     La resistance ultime est elle utilisée comme restrictions lors
c     de l'optimisation de la structure.
c     Si OUI, il s'agit de 2 restrictions d'ensemble relatives à toute
c     la structure et pas aux panneaux en particulier.
c     Les bornes des restrictions sont : USAGM et UHOGM
c USAGM= Moment ultime maximum en SAGGING en N.m
c UHOGM= Moment ultime maximum en HOGGING en N.m
C    UHOGM (>0) et USAGM (<0)  en N.m


      READ(55,101,END=900)TEXT

      READ(55,*,END=900) IRESTR

      IF(IRESTR.EQ.1) THEN
         READ(55,*,END=900) UHOGM,USAGM
       IF(IOPTI.GE.1.AND.ITERA.EQ.0) THEN		!13.12.05
           WRITE(666,*)
           WRITE(666,*)' RESTRICTIONS D''ENSEMBLE.'
           WRITE(666,*)' -------------------------'
         WRITE(666,*)' Ultimate strength of ship girder'
         WRITE(666,*)' Mult(hogging>0) > ',UHOGM,' = MAx Hogging Moment'
         WRITE(666,*)' Mult(sagging<0) < ',USAGM,' = MAx Sagging Moment'
       ENDIF
        ENDIF

      READ(55,*,END=900) IULT

c     IULT = 1,2,3, ...
c       Ce réfère à la méthode utilisée pour déterminer la résistance ul
c         de la poutre navire
c         Si IRESTR= 1, alors  IULT _ 0 car il faut qu'une méthode de ca

C     IULT = 0 : Ultimate Strength is not considered (no data)
C     IULT = 1 : PAIK  method (based on the Caldwell approach)
C     IULT = 2 : PROCOL (based on the Smith's Progressive Collapse Analy
C     IULT = 3 :
C     IULT = 4 :
C
c23456789012345678901234567890123456789012345678901234567890123456789012

      IF(IULT.NE.0) WRITE(*,'(A/)')'SUBROUTINE HULL'

c     IF(IOPTI.GE.1) THEN
c         IW=666
c       ELSE
          IW=66
c       ENDIF

C 1.0 IULT=0 : PAS DE CALCUL
C     =======================
      IF(IULT.EQ.0) THEN
        IF(IRESTR.EQ.1) THEN
            WRITE(*,*)
            WRITE(*,*)'IULT=0  n''est compatible avec IRESTR=1'
            WRITE(*,*)'***************************************'
            WRITE(*,*)'Changez IULT et définisez une méthode de calcul'
            WRITE(*,*)
            WRITE(29,*)															!bug
            WRITE(29,*)'IULT=0  n''est compatible avec IRESTR=1'					!bug
            WRITE(29,*)'***************************************'					!bug
            WRITE(29,*)'Changez IULT et définisez une méthode de calcul'			!bug
            WRITE(29,*)															!bug

            PAUSE 'OK'
            STOP
          ELSE
            RETURN
          ENDIF

C 2.0 IULT=1 : PAIK/CALWELL APPROACH
C     ==============================
        ELSEIF(IULT.EQ.1) THEN

C     Ship sizes
C     ----------
C  .  DEPTH = DEPTH OF THE VESSEL          (m.)
C  .  DB    = DEPTH OF DOUBLE-BOTTOM       (m.)
C
      READ(55,*,END=900) DEPTH,DB

C   Modeling on the ship components (deck, bottoms, side) based on the N
C   --------------------------------------------------------------------

C   ND = Number of Panels included in the UPPER DECK   component
C   NB1= Number of Panels included in the INNER BOTTOM component (si dou
C   NB = Number of Panels included in the OUTER BOTTOM component
C   NS = Number of Panels included in the TWO   SIDES  component

C   ID (i=1,ND)  = List of Panels included in the UPPER DECK   component
C   IB1(i=1,NB1) = List of Panels included in the INNER BOTTOM component
C   IB (i=1,NB)  = List of Panels included in the OUTER BOTTOM component
C   IS (i=1,NS)  = List of Panels included in the TWO   SIDES  component

C   NUMBER of a Panel to use to evaluate the Ultimate Strength of a comp
C   KD   = nø of a Panel for calculation of the UPPER DECK   Strength
C   KB1  = nø of a Panel for calculation of the INNER BOTTOM Strength
C   KB   = nø of a Panel for calculation of the OUTER BOTTOM Strength
C   KSup = nø of a Panel for calculation of the UPPER PART of the SIDE P
C   KSdw = nø of a Panel for calculation of the LOWER PART of the SIDE P

C   YIELD STRESS in the STIFFENERS
C   SY(nel) = Yield Stress in the panels
C   SySD    = yield stress in the stiffeners of the deck elements
C   SySB1   = yield stress in the stiffeners of the inner bottom element
C   SySB    = yield stress in the stiffeners of the outer bottom element
C   SySSup  = yield stress in the stiffeners of the upper side plate ele
C   SySSdw  = yield stress in the stiffeners of the lower side plate ele

C Rem : - Il faut modéliser la structure complète (et pas une demi)
C       - Si seule la moitié de la structures a été modéliésée pour le cas
c         alors il faut répéter 2 fois les éléments de façon à générer l'ensemble complet
C       - Il faut donner les panneaux correspondants aux 2 côtés (side plate)

      READ(55,*,END=900) ND, (ID(i), i=1,ND)
      READ(55,*,END=900) NB1,(IB1(i),i=1,NB1)
      READ(55,*,END=900) NB, (IB(i), i=1,NB)
      READ(55,*,END=900) NS, (IS(i), i=1,NS)

      READ(55,*,END=900) KD,  SySD
      READ(55,*,END=900) KB1, SySB1
      READ(55,*,END=900) KB,  SySB
      READ(55,*,END=900) KSup,SySSup
      READ(55,*,END=900) KSdw,SySSdw

        IF(SYSD.LE.1.)  SYSD  =SY(KD)
        IF(SySB.LE.1.)  SySB  =SY(KB)
        IF(SySSup.LE.1.)SySSup=SY(KSup)
        IF(SySSdw.LE.1.)SySSdw=SY(KSdw)
        IF(KB1.NE.0)  THEN
          IF(SySB1.LE.1.) SySB1 =SY(KB1)
        ENDIF

c23456789012345678901234567890123456789012345678901234567890123456789012
      IF((ITERA.EQ.0.AND.IOPTI.GE.2).OR.(IOPTI.LE.1)) THEN		!13.12.05
        WRITE(IW,100) TEXT
        WRITE(IW,*)'Evaluation of the Ultimate Strength of ship girder'
        WRITE(IW,*)'   using the PAIK Algorithm (Caldwell Method)'
        WRITE(IW,'(50(1H=)/)')
        Write(IW,'(A,F6.3,A,F6.3,A/)')
     *         ' Depth=',DEPTH,'  High of double bottom=',DB,' (m.)'
        Write(IW,'(A,4(1x,I2),A1)')
     * 'Number of panels included in each components(',ND,NS,NB,NB1,')'
        Write(IW,'(A,5(20(1x,I2)/))') 'UPPER DECK  :',(ID(i),i=1,ND)
        Write(IW,'(A,5(20(1x,I2)/))') 'TWO   SIDES :',(IS(i),i=1,NS)
        Write(IW,'(A,5(20(1x,I2)/))') 'OUTER BOTTOM:',(IB(i),i=1,NB)
         IF(KB1.NE.0)  THEN
         Write(IW,'(A,5(20(1x,I2)/))')'INNER BOTTOM:',(IB1(i),i=1,NB1)
         ENDIF
        Write(IW,*)
        Write(IW,*) 'Panels used for the Ult. Str. Evaluation of long.',
     *            ' Comp. Stiff. Plates'
        Write(IW,'(A,1x,I2)') 'UPPER DECK   : ',KD
        Write(IW,'(A,E11.4,A,E11.4)') '   Sy Plate =',Sy(KD),
     *                                  ' Sy Stiff.=',SySD
        Write(IW,'(A,1x,I2)') 'SIDE PLATE UP: ',KSup
        Write(IW,'(A,E11.4,A,E11.4)') '   Sy Plate =',Sy(KSup),
     *                                  ' Sy Stiff.=',SySSup
        Write(IW,'(A,1x,I2)') 'SIDE PLATE DW: ',KSdw
        Write(IW,'(A,E11.4,A,E11.4)') '   Sy Plate =',Sy(KSdw),
     *                                  ' Sy Stiff.=',SySSdw
        Write(IW,'(A,1x,I2)') 'OUTER BOTTOM : ',KB
        Write(IW,'(A,E11.4,A,E11.4)') '   Sy Plate =',Sy(KB),
     *                                  ' Sy Stiff.=',SySB
          IF(KB1.NE.0)  THEN
          Write(IW,'(A,1x,I2)') 'INNER BOTTOM : ',KB1
          Write(IW,'(A,E11.4,A,E11.4)') '   Sy Plate =',Sy(KB1),
     *                                    ' Sy Stiff.=',SySB1
          ENDIF
        Write(IW,*)
        ENDIF

C 3.0 IULT>1 : Other APPROACH
C     ===========================
      ELSE
        WRITE(*,*)
        WRITE(*,*) 'This method is not available : Iult=',Iult
        WRITE(*,'(45(1H=)/)')
        WRITE(IW,*)
        WRITE(IW,*) 'This method is not available : Iult=',Iult
        WRITE(IW,'(45(1H=)/)')
 
        WRITE(29,*)																!bug
	  WRITE(29,*) 'subroutine HULL'												!bug
        WRITE(29,*) 'This method is not available : Iult=',Iult					!bug
        WRITE(29,'(45(1H=)/)')													!bug
 
          PAUSE'OK?'
          STOP
      ENDIF

      RETURN

c23456789012345678901234567890123456789012345678901234567890123456789012

  900 WRITE(*,*) 'Error à la lecture dans Subr. HULL'
	WRITE(29,*) 'Error à la lecture dans Subr. HULL'							!bug
      PAUSE 'STOP'
      STOP

  101 FORMAT(15A4)
  100 FORMAT(//' DONNEES RELATIVES à M ultime (hull girder)'/
     *         T2,50(1H*)/15A4/)

      END
