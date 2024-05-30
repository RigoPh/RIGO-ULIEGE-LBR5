      SUBROUTINE LOC(ARG,M,PHIL,A,D,DISH,TETA,Q,IJK,LAMB,WIDTH,
     *               IMPR,MS,IS,CHA,NPTS,BKSE,NSOL)

      IMPLICIT REAL *8(A-H,O-Z)
      REAL*8 LAMB
      DIMENSION ARG(8),A(464),D(1102),DISH(1102),SI(4),CO(4),
     *          EX(4),Y(21),CHA(100,3,NSOL),NPTS(NSOL)
      COMMON /PY/PI,SI,CO,EX
C***********************************************************************
C     SUBROUTINE LOC (CHA2 et CHA3)
C     +++++++++++++++++++++++++++++
C     SUBROUTINE D'INTEGRATION DE LA CHARGE DE PRESSION QUI VARIE
C     SELON LA LONGUEUR DE LA STRUCTURE
C     IL S'AGIT D'UNE PRESSION AGISSANT NORNALEMENT AU PANNEAU ET QUI 
C     VARIE LINEAIREMENT LE LONG DE L'AXE Y  C.A.D. CHI (Y=0) ET CHF (Y=PHIL).                                           
C     CHI=CHA(L,2,IS)  et   CHF=CHA(L,3,IS)   avec IS =1,NSOL
C                                                                       
C     LA CHARGE VARIE SELON OX, PAS PAR PAS (NPTS = NBRE DE PAS).       
C
C
C     LES RESULTATS SONT PLACES DANS D (puis additionner à DISH),
C     LES DONNEES SONT CONTENUES  DANS A (C.A.D. DISC).
C     LE VECTEUR D EST UN VECTEUR DE TRAVAIL TEMPORAIRE.
C
C                                                                       
C     Modif: 14-2-96                        Création : Version Bateau (1990)
C******************************************************************************
c      IF(IMPR.NE.0) WRITE(9,*) ' ENTREE DANS LOC  '              
      
      Y(1)=0.
      IPT=NPTS(IS)
      DW=WIDTH/FLOAT(IPT)
      DO 155 I=1,IPT
 155  Y(I+1)=Y(I)+DW

      QPHIL=Q*PHIL*PI/180.
      C=-2.*BKSE*0.981/(PI*IJK)

      DO 156 LL=1,IPT
      CHI=CHA(LL,2,IS)
      CHF=CHA(LL,3,IS)
      
c      IF(IMPR.NE.0) WRITE(9,*) 'Intervalle ',LL,' CHI et CHF= ',CHI,CHF

      DO 11 I=1,M
      AL=ARG(2*I-1)
      BE=ARG(2*I)
      U1=AL*AL+BE*BE
      R1=-AL*AL+BE*BE
      S1=2.*AL*BE
C
      P1=CHF-CHI
      P2=P1/QPHIL
      P3=P2/U1
      P2=P2*Q

      Z1=1.-EX(I)*CO(I)
      Z2=EX(I)*SI(I)

      DO 3 L=1,29
      IF((L.GT.(3+MS)).AND.(L.LE.23)) GOTO 3
      K=(L-1)*38
      JK=K+8*I-2
      J=(L-1)*16+4*I-3

      AA=A(J)
      BB=A(J+1)
      T1=( AL*AA+BE*BB)/U1
      T2=(-BE*AA+AL*BB)/U1
      T3=( R1*AA-S1*BB)*P3/U1
      T4=( S1*AA+R1*BB)*P3/U1

      GOTO(2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2),L

c     Termes U,W, Xo,Zo,  My et Ny (c.à.d. 2,3, 4à23, 24,25)
  1   TEMP=2.*(T1*Z1-T2*Z2)
      D(K+1)=D(K+1)+CHI*TEMP
      D(K+2)=D(K+2)+TEMP*P2
      D(JK+1)=-CHI*T1-T3
      D(JK+2)=-CHI*T2-T4
      D(JK+3)=-CHF*T1+T3
      D(JK+4)=-CHF*T2+T4
      D(JK+5)= CHI*T1-T3
      D(JK+6)= CHI*T2-T4
      D(JK+7)= CHF*T1+T3
      D(JK+8)= CHF*T2+T4
      GOTO 3

c     Termes V,Ry,Nyx,Nxy,Wø  (c.à.d. 1, 26, 27, 28 et 29)
 2    D(K+1)=D(K+1)+2.*(T3*Z1-T4*Z2)
      D(JK+1)=-CHI*T1-T3
      D(JK+2)=-CHI*T2-T4
      D(JK+3)= CHF*T1-T3
      D(JK+4)= CHF*T2-T4
      D(JK+5)=-CHI*T1+T3
      D(JK+6)=-CHI*T2+T4
      D(JK+7)= CHF*T1+T3
      D(JK+8)= CHF*T2+T4
  3   CONTINUE
 11   CONTINUE

c       WRITE(66,*) ' DISH DANS BLOC'
c       CALL PIMPR(D,4,M)

      E=C* ( DCOS(LAMB*Y(LL+1))-DCOS(LAMB*Y(LL)) )

      DO 20 I=1,1102
      DISH(I)=DISH(I)+E*D(I)
 20   D(I)=0.
156   CONTINUE

c      IF(IMPR.NE.0) WRITE(9,*) ' SORTIE DE BLOC  TAPER ENTER '
c      IF(IMPR.NE.0) PAUSE
      RETURN
      END
