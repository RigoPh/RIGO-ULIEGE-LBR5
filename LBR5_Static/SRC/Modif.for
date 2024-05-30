      SUBROUTINE MODIF(NETO,NOH,NOEUD,MCOMP,NNO,NC,NSIGN,NCN,
     *                 NCONDI)									!flexion d'axe vert.

C**********************************************************************
C
C     SUBROUTINE MODIF
C
C           ! NCN = vecteur de travail
c
c    modif: juin 99
c           Juin 2000 (corrections)
C**********************************************************************

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION NOEUD(NETO,2),NOH(NETO,10),
     *          MCOMP(2*NETO,2),NNO(NETO+1,2),NCN(NETO+1),NSIGN(NETO)
                        
      CHARACTER *40 A(16),AA(16),TEXT

      COMMON/LANGUE/LANGUE

      COMMON/NOEUD/NTN

c             1234567890123456789012345678901234567890
      DATA A/' JONCTION DE PANNEAUX    ',             ! 1
     *       ' EXTREMITE LIBRE  MY=NY=NXY=RY=0',		! 2
     *       ' APPUI SIMPLE     W=0 ET MY=NY=NXY=0',	! 2
     *       ' APPUI SIMPLE   W=V=0 ET MY=NXY=0',		! 3
     *       ' APPUI SIMPLE   W=U=0 ET MY=NY=0',		! 4
     *       ' APPUI SIMPLE W=U=V=0 ET MY=0',		    ! 5
     *       ' ENCASTREMENT W=U=V=DW/DY=0',		    ! 6
     *       ' AXE DE SYMETRIE NXY=RY=V=DW/DY=0',		! 7
     *       ' APPUI SIMPLE    V=MY=NXY=RY=0',     	! 8                                                                       
     *       ' APPUI SIMPLE    U=MY=NY=RY=0',    		! 9                                                                     
     *       ' APPUI SIMPLE    U=V=0 ET MY=RY=0',  	! 10                                                                       
     *       ' AXE DE SYMETRIE W=DW/DY=NXY=NY=0',  	! 11                                                                   
     *       ' DOUBLE SYMETRIE V=W=DW/DY=NXY=0',  	! 13                                                                     
     *       '     ',                                                                    
     *       '     ',                                                                    
     *       '     '/      
      DATA AA/' JUNCTION OF PANELS      ',			! 1
     *       ' FREE EXTREMITY  MY=NY=NXY=RY=0',		! 1
     *       ' PINNED END   W=0 ET MY=NY=NXY=0',		! 2
     *       ' PINNED END   W=V=0 ET MY=NXY=0',		! 3
     *       ' PINNED END   W=U=0 ET MY=NY=0',		! 4
     *       ' PINNED END   W=U=V=0 ET MY=0',			! 5
     *       ' CLAMPED END  W=U=V=DW/DY=0',		    ! 6
     *       ' SYMMETRY AXIS(loc.OZ)NXY=RY=V=DW/DY=0',! 7
     *       ' PINNED END       V=MY=NXY=RY=0',     	! 8                                                                       
     *       ' PINNED END       U=MY=NY=RY=0',    	! 9                                                                     
     *       ' PINNED END       U=V=0 ET MY=RY=0',  	! 10                                                                       
     *       ' SYMMETRY AXIS(loc.OY)W=DW/DY=NXY=NY=0',! 11                                                                   
     *       ' DOUBLE SYMMETRY  V=W=DW/DY=NXY=0',  	! 12                                                                     
     *       '     ',                                                                    
     *       '     ',                                                                    
     *       '     '/      
      IF(LANGUE.NE.1) THEN
        DO I=1,13
	    A(I)=AA(I)
	  ENDDO
	ENDIF
C
      IF(NETO.NE.1) GOTO 51
      NC=0
      NOEUD(1,1)=1
      NOEUD(1,2)=2
      NTN=2
      GOTO 801
C
C CETTE PARTIE DU PROGRAMME ENGENDRE AUTOMATIQUEMENT LES NOEUDS
C -------------------------------------------------------------
   51 KK=1
      DO 110 II=1,NETO
        NOEUD(II,1)=0
        NOEUD(II,2)=0
  110 CONTINUE
      DO 120 I=1,NETO
        IF(I.EQ.NETO) GOTO 135
        IF (NOH(I,1).EQ.0) GOTO 121
        K=I+1
        DO 130 L=K,NETO
          IF (NOH(L,1).NE.NOH(I,1))  GOTO 130
          IF (NOEUD(L,2).EQ.0)  NOEUD(L,2)=KK
  130   CONTINUE
  135   DO 140 LL=1,10
          LI=NOH(I,LL)
          IF(LI.EQ.0) GOTO 121
          IF (NOEUD(LI,1).EQ.0)  NOEUD(LI,1)=KK
  140   CONTINUE
  121   IF (NOEUD(I,2).NE.0) GOTO 120
        NOEUD(I,2)=KK
        KK=KK+1
  120 CONTINUE

      DO 150 JJ=1,NETO
        IF(NOEUD(JJ,1).NE.0) GOTO 150
        NOEUD(JJ,1)=KK
        KK=KK+1
  150 CONTINUE
      NTN=KK-1
C
C  ELIMINATION  DANS NOH DES COEFFICIENTS DONNANT UN SURPLUS
C  D'EQUATIONS DE COMPATIBILITE
C  ---------------------------------------------------------
      NN=NETO-1
      DO 25 I=1,NN
        K=I+1
        DO 35 L=K,NETO
          IF((NOH(L,1).EQ.NOH(I,1)).AND.(NOH(L,2).NE.0)) GOTO 41
          GOTO 35
   41     DO 45 J=2,10
            NOH(L,J)=0
   45     CONTINUE
   35   CONTINUE
   25 CONTINUE

C  INITIALISATION A ZERO DE MCOMP
C  ------------------------------
      DO 50 I=1,2*NETO
        MCOMP(I,1)=0
        MCOMP(I,2)=0
   50 CONTINUE
C
C  ECRITURE DES RELATIONS DE COMPATIBILITE DANS MCOMP
C  --------------------------------------------------
      MM=1
      DO 200 I=1,NETO
        DO 32 J=1,10
          IF(NOH(I,J).EQ.0) GOTO 200
          MCOMP(MM,1)=I
          MCOMP(MM,2)=NOH(I,J)
          MM=MM+1
   32   CONTINUE
  200 CONTINUE
      NC=MM-1 ! NC = Nbre d'équations de compatabilité

C  INITALISATION A ZERO DE NNO
C  ---------------------------
  801 DO 800 I=1,NETO
        NNO(I,1)=0
        NNO(I,2)=0
  800 CONTINUE
C
C  ECRITURE DES CONDITIONS AUX LIMITES DANS NNO
C  --------------------------------------------
      READ(55,*) TEXT   ! Lecture du titre
      READ(55,*) NCONDI

      IF(NCONDI.EQ.0) GOTO 910
      DO 900 I=1,NCONDI
  900 READ(55,*) (NNO(I,J),J=1,2)
  910 CONTINUE

      IF(NETO.EQ.1) GOTO 930
      DO 700 I=1,NETO
        IF(NNO(I,1).EQ.0)GOTO 920
        NN=NNO(I,1)
        ND=NOEUD(NN,1)
        KK=0
        DO 710 J=1,NETO
          IF((NOEUD(J,1).EQ.ND).OR.(NOEUD(J,2).EQ.ND)) KK=KK+1
  710   CONTINUE
        NNO(I,1)=ND
        IF(KK.NE.1) NNO(I,1)=NOEUD(NN,2)
  700 CONTINUE

  920 DO 925 I=1,NETO+1
        NCN(I)=0
  925 CONTINUE

      DO 940 I=1,NTN
        MM=NNO(I,1)
        IF(MM.EQ.0) GOTO 945
        NCN(MM)=NNO(I,2)
  940 CONTINUE

  945 DO 950 J=1,NTN
        NNO(J,1)=J
        NNO(J,2)=NCN(J)
  950 CONTINUE
  930 CONTINUE
      
C     IMPRESSIONS
c     ---------------
	IF(LANGUE==1) WRITE(66,210)  ! Cond. de bord
	IF(LANGUE==2) WRITE(66,310)  ! Cond. de bord
      DO 931 I=1,NTN
        IX=NNO(I,2)+1
c       WRITE(66,214)(NNO(I,J),J=1,2),(A(J),J=IX,IX+3)
        WRITE(66,214)(NNO(I,J),J=1,2),A(IX)
  931 CONTINUE

      IF(LANGUE==1) WRITE(66,211) ! N° des noeuds
      IF(LANGUE==2) WRITE(66,311) ! N° des noeuds
      DO 867 I=1,NETO
        WRITE(66,216)I,(NOEUD(I,J),J=1,2)
  867 CONTINUE

      IF(NC.NE.0) THEN
        IF(LANGUE==1) WRITE(66,212) ! Rel. de compatibilité
        IF(LANGUE==2) WRITE(66,312) ! Rel. de compatibilité
        DO 876 I=1,NC
          WRITE(66,216)I,(MCOMP(I,J),J=1,2)
  876   CONTINUE
      ENDIF
  
C     Test de vérification des donnees
c     --------------------------------
      I1=NETO*8
	I2=4*(NTN+NC)
	IF(I1.NE.I2) THEN
        WRITE(*,*)  'Le nombre de relation de compatibilté est errone.'
        WRITE(*,*)  'The number of Compatibility Equation is wrong'
        WRITE(*,*)  'STOP'
        WRITE(66,*) 'Subr. MODIF:'
        WRITE(66,*) 'Le nombre de relation de compatibilté est errone.'
        WRITE(66,*) 'Il doit y avoir une erreur dans les donnees.'
        WRITE(66,*) 'Vérifier les NOH de chaque panneau.'
        WRITE(66,*) 'NOH= liste des panneaux qui suivent le panneau',
     *              ' concerne'
        
	  WRITE(29,*) 'Subr. MODIF:'												!bug
        WRITE(29,*) 'Le nombre de relation de compatibilté est errone.'			!bug	
        WRITE(29,*) 'Il doit y avoir une erreur dans les donnees.'				!bug
        WRITE(29,*) 'Vérifier les NOH de chaque panneau.'							!bug
        WRITE(29,*) 'NOH= liste des panneaux qui suivent le panneau',				!bug
     *              ' concerne'													!bug
	
        STOP
	ENDIF


      RETURN

  210 FORMAT('DESCRIPTION DE L''ASSEMBLAGE DES PANNEAUX'/
     * 41(1H*)/' NUMERO DU NOEUD',T35,'TYPE DE CONDITIONS DE BORD')
  211 FORMAT(/' NUMERO DU PANNEAU',T30,'NOEUD au DEPART',T60,
     *        'NOEUD d''ARRIVEE')
  212 FORMAT(/' RELATION DE COMPATIBILITE',T30,'ENTRE LE PANNEAU ',
     *'ARRIVANT',T60,'ET LE PANNEAU PARTANT'/T12,'NUMERO')

  310 FORMAT('PRESENTATION OF THE STRUCTURE''S MESH MODEL'/41(1H*)/
     *  '  NODE NUMBER',T35,'TYPE OF BOUNDARY CONDITION (Y=0 or Y=H)')
  311 FORMAT(/' PANEL NUMBER',T30,'NODE at DEPARTURE (Y=0)',
     *        T60,'NODE at ARRIVAL (Y=H)')
  312 FORMAT(/' COMPATIBILITY RELATION',T30,'between AN ARRIVING ',
     *'PANEL',T60,'and A DEPARTING PANEL'/T12,'NUMBER')

  214 FORMAT(T5,I7,T35,I4,A40)
  216 FORMAT(T5,I7,T35,I7,T65,I7)

      END
