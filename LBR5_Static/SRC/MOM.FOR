      SUBROUTINE MOM(ARG,M,A,B,C,D,PHIL,DISB,DL,DISH,Q,MS)
      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION ARG(8),DISB(720),DL(1710),DISH(1710),SI(4),CO(4),EX(4)	!!!aout04 (pas nécessaire ces modifs mais c'est par cohérence vis-à-vis de Locy,...)
      COMMON /PY/PI,SI,CO,EX
C***********************************************************************
C
C     SUBROUTINE  MOM
c     ----------------
C     SUBROUTINE D'INTEGRATION DES SOLLICITATIONS DE BORD
C     A SAVOIR DES FORCES  DANS LE PLAN DE LA COQUE (DISB)
C
C     LES SOLLICITATIONS SONT DE LA FORME A*(Y**3)+B(Y**2)+C*Y+D
C
C     LES DONNEES (sollicitations) SONT CONTENUES DANS  A,B,C,D
C        et dans DISB (cas de charge unitaire)
C
C     LES RESULTATS SONT CONTENUS dans DL puis additionnés à DISH 
C        (DL = vect. de travail) 
C
C     Modif: 20-3-96 (adaptation pour LBR5)        Création : Version Bateau 1990
C            29-3-96 (SIMPLIFICATION)
C	    
C*******************************************************************************
      DO 11 I=1,M
      AL=ARG(2*I-1)
      BE=ARG(2*I)

      U1=AL*AL+BE*BE
      U2=U1*U1
      U3=U2*U1
      U4=U2*U2
      P1=Q*PHIL*PI/180.
      P2=P1*P1
      P3=P2*P1
      COS1=EX(I)*CO(I)
      SIN1=EX(I)*SI(I)

      R1=-AL*AL+BE*BE
      S1=2.*AL*BE
      R2=(-AL*AL+3.*BE*BE)*2.*AL
      S2=(-3.*AL*AL+BE*BE)*2.*BE
      R3=6.*(AL**4+BE**4-6.*((AL*BE)**2))
      S3=-12.*S1*R1

      ALU=AL/U1
      BEU=BE/U1
      R1U=R1/U2
      S1U=S1/U2
      R2U=R2/U3
      S2U=S2/U3
      R3U=R3/U4
      S3U=S3/U4

      DO 3 L=1,38									!!!aout04
      IF((L.GT.(3+MS)).AND.(L.LE.23)) GOTO 3
      K=(L-1)*38+1
      JK=K+8*I-2
      J=(L-1)*16+4*I-3

      AA=DISB(J)
      BB=DISB(J+1)
      T1U= ALU*AA+BEU*BB
      T2U=-BEU*AA+ALU*BB
      T3U= R1U*AA-S1U*BB
      T4U= S1U*AA+R1U*BB
      T5U= R2U*AA+S2U*BB
      T6U=-S2U*AA+R2U*BB
      T7U= R3U*AA+S3U*BB
      T8U=-S3U*AA+R3U*BB

      GOTO(2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,1,2	!!!aout04
     *,2,2,1,1,1,2,2),L													!!!aout04

c     Termes U,W, Xo,Zo,  My et Ny (c.à.d. 2,3, 4à23, 24,25)
C     -----------------------------------------------------
  1   TEMP1=2.*( T1U*(1-COS1)-SIN1*T2U)
      TEMP2=2.*(-T5U*(1-COS1)+SIN1*T6U)
      DL(K)  =DL(K)  +TEMP1*D     +TEMP2*B
      DL(K+1)=DL(K+1)+TEMP1*C*Q   +TEMP2*A*Q*3.
      DL(K+2)=DL(K+2)+TEMP1*B*Q*Q
      DL(K+3)=DL(K+3)+TEMP1*A*Q*Q*Q

      DL(JK  )=-D*T1U-T3U*C+T5U*B+T7U*A
      DL(JK+1)=-D*T2U-T4U*C+T6U*B+T8U*A
      DL(JK+4)= D*T1U-T3U*C-T5U*B+T7U*A
      DL(JK+5)= D*T2U-T4U*C-T6U*B+T8U*A

      DL(JK+2)=-DL(JK+4)
     * + P1*(-T1U*C+T3U*2.*B+T5U*3.*A)
     * + P2*(-T1U*B+T3U*3.*A)
     * + P3*(-T1U*A)
      DL(JK+3)=-DL(JK+5)
     * + P1*(-T2U*C+T4U*2.*B+T6U*3.*A)
     * + P2*(-T2U*B+T4U*3.*A)
     * + P3*(-T2U*A)
      DL(JK+6)=-DL(JK)
     * + P1*( T1U*C+T3U*2.*B-T5U*3.*A)
     * + P2*( T1U*B+T3U*3.*A)
     * + P3*( T1U*A)
      DL(JK+7)=-DL(JK+1)
     * + P1*( T2U*C+T4U*2.*B-T6U*3.*A)
     * + P2*( T2U*B+T4U*3.*A)
     * + P3*( T2U*A)
      GOTO 3

c     Termes V,Ry,Nyx,Nxy,Wø  (c.à.d. 1, 26, 27, 28 et 29)
C     -----------------------------------------------------
  2   TEMP1=2.*( T3U*(1-COS1)-SIN1*T4U)
      TEMP2=2.*(-T7U*(1-COS1)+SIN1*T8U)
      DL(K)  =DL(K)  +TEMP1*C       +TEMP2*A
      DL(K+1)=DL(K+1)+TEMP1*B*Q*2.
      DL(K+2)=DL(K+2)+TEMP1*A*Q*Q*3.

      DL(JK  )=-D*T1U-T3U*C+T5U*B+T7U*A
      DL(JK+1)=-D*T2U-T4U*C+T6U*B+T8U*A
      DL(JK+4)=-D*T1U+T3U*C+T5U*B-T7U*A
      DL(JK+5)=-D*T2U+T4U*C+T6U*B-T8U*A

      DL(JK+2)=-DL(JK+4)
     *  - P1*(-T1U*C+T3U*2.*B+T5U*3.*A)
     *  - P2*(-T1U*B+T3U*3.*A)
     *  - P3*(-T1U*A)
      DL(JK+3)=-DL(JK+5)
     *  - P1*(-T2U*C+T4U*2.*B+T6U*3.*A)
     *  - P2*(-T2U*B+T4U*3.*A)
     *  - P3*(-T2U*A)
      DL(JK+6)=-DL(JK  )
     *  + P1*( T1U*C+T3U*2.*B-T5U*3.*A)
     *  + P2*( T1U*B+T3U*3.*A)
     *  + P3*( T1U*A)
      DL(JK+7)=-DL(JK+1)
     *  + P1*( T2U*C+T4U*2.*B-T6U*3.*A)
     *  + P2*( T2U*B+T4U*3.*A)
     *  + P3*( T2U*A)
 
  3   CONTINUE
 11   CONTINUE
C
      DO 20 I=1,1710				!!!aout04
      DISH(I)=DISH(I)+DL(I)
 20   DL(I)=0.
      RETURN
      END
