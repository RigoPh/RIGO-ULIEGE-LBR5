      SUBROUTINE POLRT(XCOF,COF,M,ROOTR,ROOTI,IER)
      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION XCOF(9),COF(9),ROOTR(8),ROOTI(8)
C***********************************************************************
C
C     SUBROUTINE POLRT
C     ****************
C     SUBROUTINE DE RESOLUTION DE L'EQUATION DU 8 EME ORDRE DE LA PLAQUE
C     NON CHARGEE,RAIDIE,PAR ITERATIONS SUCCESIVES.
C
C     Données   : XCOF coef de l'équation polynomiale d'ordre M
C	            M    ordre de l'équation (M=8)
C	Solutions : ROOTR  parties réelles
C	            ROOTR  parties imaginaires
C		        IER    Indice de validité
C
C	Vect de travail : COF
C
C      Modif: 17-3-94				Création : Thèse Ph. Rigo                                                                 
C***********************************************************************
C      VERIFICATIONS
C      _____________
      IER=0
c     Vérification si le terme en X**8 n'est pas nul (si M=8)
      IF(XCOF(M+1).EQ.0) THEN
      IER=4
      RETURN
      ENDIF
      
c     Vérification si M n'est pas nul (M=0) c.à.d equation de degré 0    
      IF(M.LE.0) THEN
      IER=1
      RETURN
      ENDIF

c     Vérification si on a M < 36  c.à.d  équation de degré inférieur à 35    
      IF((M-36).GT.0)THEN
      IER=2
      RETURN
      ENDIF

C***********************************************************************

c     Recherche du coef. EQU maximum et division par ce coef. max
      Z=XCOF(1)
      NP=M+1
      DO 180 I=2,NP
      IF(DABS(Z)-DABS(XCOF(I)))181,180,180
  181 Z=XCOF(I)
  180 CONTINUE
      DO 182 I=1,NP
  182 XCOF(I)=XCOF(I)/Z

c     Classement des coef dans COF par ordre décroissant
      DO 40 L=1,M+1
        MT=M-L+2
   40 COF(MT)=XCOF(L)

C***********************************************************************
C IN  = Nbre de fois que l'on change de pt de départ (XO=-10.0*YO ,YO=-10.0*X)
C ICT = Nbre d'itération avec le même pt de départ (XO + i YO)

      IFIT=0
      N=M
      NX=M
      NXX=M+1
      N2=1

C     Point de départ (XO + i YO)
   45 XO=.00500101
      YO=0.01000101
      IN=0

C     Changement de Point de départ (XO + i YO)
   50 X=XO
      XO=-10.0*YO
      YO=-10.0*X
      X=XO
      Y=YO
      ICT=0
      IN=IN+1
      GO TO 60

   55 IFIT=1
      XPR=X
      YPR=Y
C     Début d'une nouvelle itération
   60 UX=0.0
      UY=0.0
      V=0.0
      YT=0.0
      XT=1.0
      U=COF(N+1)
      IF(U) 65,130,65

C     Calcul de la Fct (U + i V) et et sa Dérivée (UX + i UY) au pt. (X+iY)
C     _____________________________________________________________________
C     (XT,YT) et (XT2,YT2) = nbre complexe = (X,Y)**I
   65 DO 70 I=1,N
      L=N-I+1
      XT2=X*XT-Y*YT
      YT2=X*YT+Y*XT
      U=U+COF(L)*XT2
      V=V+COF(L)*YT2
      FI=I
      UX=UX+FI*XT*COF(L)
      UY=UY-FI*YT*COF(L)
      XT=XT2
  70  YT=YT2

C     calcul du nouveau pt (X+DX , Y+DY)
C ---------------------------------------
      SUMSQ=UX*UX+UY*UY
      IF(SUMSQ) 75,110,75
   75 DX=(V*UY-U*UX)/SUMSQ
      DY=-(U*UY+V*UX)/SUMSQ
      X=X+DX
      Y=Y+DY
   78 IF(DABS(DY)+DABS(DX)-1.0D-10)100,80,80
   80 ICT=ICT+1
C     vers nouvelle iteration (si ICT<500) 
      IF(ICT-500) 60,85,85   
   85 IF(IFIT) 115,90,115
C     vers nouveau pt de départ (max 4 fois)
   90 IF(IN-5) 50,95,95     
   95 IER=3
      RETURN

  100 DO 105 L=1,NXX
        MT=M-L+2
        TEMP=XCOF(MT)
        XCOF(MT)=COF(L)
        COF(L)=TEMP
  105 CONTINUE

      ITEMP=N
      N=NX
      NX=ITEMP
      IF(IFIT) 120,55,120
  110 IF(IFIT) 115,50,115
  115 X=XPR
      Y=YPR
  120 IFIT=0
C     Si Y trop petit, solution réelle (X,0) sinon solution complexe (X,Y)
  122 IF(DABS(Y/X)-1.0D-8) 135,125,125
  125 ALPHA=X+X
      SUMSQ=X*X+Y*Y
      N=N-2
      IF(N) 155,155,140

  130 X=0.0
      NX=NX-1
      NXX=NXX-1
  135 Y=0.0
      SUMSQ=0.0
      ALPHA=X
      N=N-1
      IF(N) 155,155,140

  140 COF(2)=COF(2)+ALPHA*COF(1)
  145 DO 150 L=2,N
        COF(L+1)=COF(L+1)+ALPHA*COF(L)-SUMSQ*COF(L-1)
  150 CONTINUE

  155 ROOTI(N2)=Y
      ROOTR(N2)=X
      N2=N2+1

      IF(SUMSQ) 160,165,160
  160 Y=-Y
      SUMSQ=0.0
      GO TO 155
C     Retour au début
  165 IF(N.GT.0) GOTO 45

      RETURN
      END
