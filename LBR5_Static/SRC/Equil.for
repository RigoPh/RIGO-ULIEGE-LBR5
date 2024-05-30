      SUBROUTINE EQUIL(MT,Z2,LAMB,CONST,XI,XF,IVARIA,PHIL,IJK,KSE,Q,
     *   NEL,ABTR,IND,AAA,BBB,CCC,DDD,AA1,BB1,CC1,DD1,DELTO,WIDTH,
     *   TETAQ,NETO)

      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 LAMB
      DIMENSION CONST(74),Z2(2295),ABTR(10),A(10),TETAQ(NETO)

      COMMON/PY/PI
      COMMON/PH/PHMAX
C***********************************************************************
C
C     SUBROUTINE EQUIL
C     SUBROUTINE DE VERIFICATION DE L'EQUILIBRE DU PANNEAU.
C     ON CONSIDERE L'EQUILIBRE SELON LES TROIS AXES OX,OY,OZ.
C
C    modif :17-8-94                              Cr�er : Th�se Ph. Rigo
C***********************************************************************
      S0=DSIN(PI*IJK/2.)
      TETA=TETAQ(NEL)
      PHX=0.
      FMO=0.
      FMV=0.

c     WRITE(66,*)LAMB,PHIL,Q,IJK,TETA
c     WRITE(66,*)'LAMB,PHIL,Q,IJK,TETA'

      SP1=VSIN(PHIL,0.D00)
      CP1=VCOS(PHIL,0.D00)
      SP2=VSIN(PHIL/2.,0.D00)
      CP2=VCOS(PHIL/2.,0.D00)
      C1=VCOS(TETA,0.D00)
      C2=VCOS(TETA,PHIL)
      C3=VCOS(TETA,PHIL/2.)
      C4=VCOS(2.*TETA,PHIL)
      S1=VSIN(TETA,0.D00)
      S2=VSIN(TETA,PHIL)
      S3=VSIN(TETA,PHIL/2.)
      S4=VSIN(2.*TETA,PHIL)
      QPHIL=Q*PHIL*PI/180.


C     EFFORTS DE BORD
C     ----------------
      IF(IND.EQ.0) GOTO 300
      F=AAA*(QPHIL**4)/4.+BBB*(QPHIL**3)/3.+CCC*(QPHIL**2)/2.+DDD*QPHIL
      C5=DCOS(IJK*PI*(WIDTH-2.*DELTO)/(2.*WIDTH))
      PHX=4.*WIDTH*F*C5/(IJK**2*PI*PI*DELTO)

      CST=8.*S0*C5/(IJK*PI*DELTO)
      IF(PHIL.LE.0.000001) THEN
      P=(AA1*(QPHIL**3)/4.+BB1*(QPHIL**2)/3.+CC1*QPHIL/2.+DD1)*QPHIL
      FMO=CST*P*C1
      FMV=CST*P*S1
      ELSE
      P1=AA1*(QPHIL**3)+BB1*(QPHIL**2)+CC1*QPHIL+DD1
      P2=(3.*AA1*(QPHIL**2)+2.*BB1*QPHIL+CC1)*Q
      P3=(6.*AA1*QPHIL+2.*BB1)*Q*Q
      P4=(Q**3)*6.*AA1
      FMO=(P1-P3)*S2+(P2-P4)*C2
     *              +S1*(2.*BB1*Q*Q-DD1)+C1*(6.*AA1*(Q**3)-CC1*Q)
      FMV=(P3-P1)*C2+(P2-P4)*S2
     *              -C1*(2.*BB1*Q*Q-DD1)+S1*(6.*AA1*(Q**3)-CC1*Q)
      FMO=FMO*CST*Q
      FMV=FMV*CST*Q
      ENDIF

C     PRESION HYDROSTATIQUE
C     ----------------------
  300 AA=Q*8.*9810./(LAMB*PI*IJK)*((-1)**KSE)

      IF(IVARIA.EQ.0)GOTO 301
C
C     IVARIA=1  LA PRESSION VARIE LINEAIREMENT LE LONG DE L'AXE Y (Subr.
C
      IF(PHIL.LE.1.E-05) GOTO 302
      T1=-2.*S3*SP2/(PHIL*PI/180.)+S2
      PHO=AA*(T1*(XF-XI)+XI*2.*C3*SP2)
      T1=2.*C3*SP2/(PHIL*PI/180.)-C2
      PHV=AA*(T1*(XF-XI)+XI*2.*S3*SP2)
      GOTO 304
  302 T1=(XF+XI)*(PHIL*PI/180.)/2.
      PHO=T1*C1*AA
      PHV=T1*S1*AA
      GOTO 304

C     IVARIA=0  LA PRESSION VARIE AVEC LA PROFONDEUR (Subr. HYDRO)

  301 IF(PHIL.LE.1.E-05) THEN
      PHO=(2.*XI+QPHIL*C1)*(PHIL*PI/180.)/2.
      PHV=PHO*S1*AA
      PHO=PHO*C1*AA
      ELSE
      T1=Q*(0.5*S4*SP1-2.*S1*C3*SP2)
      PHO=AA*(T1+XI*2.*C3*SP2)
      T1=Q*(PHIL*PI/(180.*2.)-0.5*SP1*C4-2.*S1*S3*SP2)
      PHV=AA*(T1+XI*2.*S3*SP2)
      ENDIF

C     Recherche de la composante maximale

  304 WRITE(66,100) PHO,FMO,PHV,FMV,PHX
      PH=DMAX1(DABS(PHO),DABS(PHV),DABS(PHX),DABS(FMO),DABS(FMV))
      IF(PH.GE.PHMAX) THEN
      PHMAX=PH
      ELSE
      PH=PHMAX
      ENDIF
c     WRITE(66,*) 'La composante maximale de r�f�rence est ',PH

C     EQUILIBRE SUIVANT L'AXE VERTICALE
C     -----------------------------------
      CALL PINTEG(PHIL,TETA,40,45,SOM,Z2,MT,Q,PI,ABTR,C1,C2,S1,S2,3,BOF)

c      WRITE(66,*)'SOM (Nxy et RX)=', SOM

c    Mxy(x=0,y=0)=Z2(2092),  Mxy(x=0,y=Yo)=Z2(2122)
C    Myx(x=0,y=0)=Z2(2143),  Myx(x=0,y=Yo)=Z2(2173)'

      TEMP=(Z2(1225)*C1-Z2(1276)*S1-Z2(1255)*C2+Z2(1306)*S2)
     *  *2./LAMB-2*((Z2(2092)+Z2(2143))*S1-(Z2(2122)+Z2(2173))*S2)


c      WRITE(66,*)'Z2(2092),Z2(2122),Z2(2143),Z2(2173)'
c      WRITE(66,*) Z2(2092),Z2(2122),Z2(2143),Z2(2173)
c      WRITE(66,*)'Z2(1225),Z2(1255),Z2(1276),Z2(1306)'
c      WRITE(66,*) Z2(1225),Z2(1255),Z2(1276),Z2(1306)
c      TEMP1=(Z2(1225)*C1-Z2(1276)*S1-Z2(1255)*C2+Z2(1306)*S2)*2./LAMB
c      TEMP2=-2*((Z2(2092)+Z2(2143))*S1-(Z2(2122)+Z2(2173))*S2)
c      WRITE(66,*)'TEMP1 (Ny,Ry),TEMP2(Mxy,Myx),TEMP  (TEMP1+TEMP2=TEMP)
c      WRITE(66,*)TEMP1,TEMP2,TEMP

      SOM=SOM*2.+TEMP

c      WRITE(66,*)'2*SOM+TEMP=', SOM

      IF(MT.EQ.0)GOTO 200
      DO 201 I=1,MT
      JI=6*(I-1)

C  LE DERNIER TERME DE A(I) SE RAPPORTE A D2U/D2Y, DERIVEE NON CONTINUE
C     V1= LAMB**2* (-LAMB*CONST(18+JI)*Z2(2*I+132)+CONST(16+JI)*
C    *              Z2(2*I+81))
C     V2= CONST(19+JI)*LAMB*Z2(2*I+1764)
C     V3=  CONST(20+JI)*(Z2(2*I+1509)*LAMB+Z2(2*I+1713))
C     V4=(CONST(8)+CONST(14)) *(Z2(2*I+1561)-Z2(2*I+1560))
C     V5=  CONST(20+JI)  *  Z2(2*I+1713)
C     WRITE(66,*) Z2(2*I+1713)
C     WRITE(66,*) V1,V2,V3,V4,V5

      A(I)= LAMB**2* (-LAMB*CONST(18+JI)*Z2(2*I+132)  +
     * CONST(16+JI)*Z2(2*I+81)  ) + CONST(19+JI)*LAMB*Z2(2*I+1764)
     * + (CONST(8)+CONST(14)) * (Z2(2*I+1561)-Z2(2*I+1560))
     * +  CONST(20+JI)*(Z2(2*I+1509)*LAMB+Z2(2*I+1713))
C     WRITE(66,*) A(I)
  201 SOM=SOM-2.*A(I)*DSIN((TETA+ABTR(I))*PI/180.)
C     WRITE(66,*) SOM

  200 SOM=SOM+PHV+FMV
      WRITE(66,101)SOM
      IF(DABS(PH).LT.0.01) GOTO 1
      PE=SOM/PH
      WRITE(66,105)PE
C
C     EQUILIBRE SUIVANT L'AXE OX
C     --------------------------
C
    1 CALL PINTEG(PHIL,TETA,39,0,SOM,Z2,MT,Q,PI,ABTR,C1,C2,S1,S2,2,VMAX)

c      WRITE(66,*) 'SOM (Nx) =',SOM

      SOM=(SOM+((Z2(1357)-Z2(1327))/LAMB))*S0+PHX

c      WRITE(66,*) ' S0 =',S0,'PHX=',PHX
c      WRITE(66,*) '(SOM + Nyx)*S0 + PHX =',SOM

      IF(MT.EQ.0)GOTO 5
      DO 34 I=1,MT
      JI=6*(I-1)

c      V1=-LAMB *CONST(15+JI)*Z2(2*I+81)
c      V2= LAMB*LAMB * CONST(16+JI)*Z2(2*I+132)
c      V3= CONST(17+JI)*Z2(2*I+1509)
c      V4= CONST(17+JI)*  Z2(2*I+1713)/LAMB
c      V5= CONST(17+JI)* (Z2(2*I+1713)+Z2(2*I+1561)-Z2(2*I+1560))/LAMB
c      WRITE(66,*) 'S0,Z2(2*I+1509),Z2(2*I+81),Z2(2*I+132)'
c      WRITE(66,*) S0,Z2(2*I+1509),Z2(2*I+81),Z2(2*I+132)
c      WRITE(66,*) 'Z2(2*I+1561),Z2(2*I+1560)'
c      WRITE(66,*) Z2(2*I+1561),Z2(2*I+1560)
c      WRITE(66,*) 'V1,V2,V3,V4,V5'
c      WRITE(66,*) V1,V2,V3,V4,V5
c      SAM1= V1+V2+V3
c      WRITE(66,*)' SAM1=V1+V2+V3=',SAM1

      SAM= ( LAMB *(-CONST(15+JI)*Z2(2*I+81)+LAMB*CONST(16+JI)
     *    *Z2(2*I+132))   + CONST(17+JI)* Z2(2*I+1509)
     *                    + CONST(17+JI)* Z2(2*I+1713)/LAMB  )*S0

c     CONST(17+JI)*(Z2(2*I+1713)+Z2(2*I+1561)-Z2(2*I+1560))/LAMB  )*S0
c      WRITE(66,*)'SAM=',SAM

      SOM=SOM+SAM

c      WRITE(66,*)'SOM+SAM=',SOM

  34  CONTINUE
   5  WRITE(66,102)SOM
      PH2=DMAX1(VMAX,PH)
      IF(PH2.LT.0.01) GOTO 2
c      WRITE(66,*) 'La composante maximale de r�f�rence est ',PH2
      PE=SOM/PH2
      WRITE(66,105)PE

C     EQUILIBRE SUIVANT L'AXE HORIZONTAL
C     -----------------------------------
C
  2   CALL PINTEG(PHIL,TETA,45,40,SOM,Z2,MT,Q,PI,ABTR,C1,C2,S1,S2,1,BOF)

c      WRITE(66,*)'SOM (Nxy et Rx)=', SOM

      TEMP= 2.*((Z2(2092)+Z2(2143))*C1-(Z2(2122)+Z2(2173))*C2)
     * +(2./LAMB)*(Z2(1276)*C1+Z2(1225)*S1-Z2(1306)*C2-Z2(1255)*S2)

c      TEMP1=2.*((Z2(2092)+Z2(2143))*C1-(Z2(2122)+Z2(2173))*C2)
c      TEMP2=(2./LAMB)*(Z2(1276)*C1+Z2(1225)*S1-Z2(1306)*C2-Z2(1255)*S2)
c      WRITE(66,*)'TEMP1,TEMP2,TEMP  (TEMP1+TEMP2=TEMP)'
c      WRITE(66,*)TEMP1,TEMP2,TEMP

      SOM=(SOM*2.)+TEMP
c      WRITE(66,*) '2*SOM+TEM=',SOM

      SOM=SOM-PHO-FMO
c      WRITE(66,*) 'SOM-PHO-FMO=',SOM

      IF(MT.EQ.0)GOTO 6
      DO 33 I=1,MT
      JI=6*(I-1)
      SAM=2.*A(I)*DCOS((TETA+ABTR(I))*PI/180.)
   33 SOM=SOM+SAM
    6 WRITE(66,103)SOM

      IF(DABS(PH).LT.0.01) GOTO 3
      PE=SOM/PH
      WRITE(66,105)PE
    3 CONTINUE

  100 FORMAT( /,2X,' EQUILIBRE SUIVANT LES TROIS AXES'///
     *1X,'VALEUR DE LA CHARGE HYDRAULIQUE HORIZONTAL POUR CE TERME'
     *,T65,E14.7,2X,2HN.//
     *1X,'COMPOSANTE HORIZONTALE DU AUX MOMENTS DE BORD POUR CE TERME'
     *,T65,E14.7,2X,2HN.//1X,'VALEUR DE LA CHARGE ',
     *'HYDRAULIQUE VERTICALE POUR CE TERME',T65,E14.7,2X,'N.'//1X,
     *'COMPOSANTE VERTICALE DU AUX MOMENTS DE BORD POUR CE TERME'
     *,T65,E14.7,2X,2HN.//1X,'VALEUR DE LA CHARGE ',
     *'REPARTIE DE BORD POUR CE TERME',T65,E14.7,2X,'N.'//)
  101 FORMAT(2X,'AXE VERT  :RESULTANTE DES EFFORTS = ERREUR ABSOLUE'/T36
     *,' =',E14.7,2X,7HNEWTON.)
  105 FORMAT(9X,'ERREUR RELATIVE PAR RAPPORT A LA COMPOSANTE MAXIMALE',
     * ' DE LA PH'/T36,' =',E14.7/)
  102 FORMAT(2X,'AXE  OX   :RESULTANTE DES EFFORTS = ERREUR ABSOLUE'/T36
     *,' =',E14.7,2X,7HNEWTON.)
  103 FORMAT(2X,'AXE HORIZ :RESULTANTE DES EFFORTS = ERREUR ABSOLUE'/T36
     *,' =',E14.7,2X,7HNEWTON.)

      RETURN
      END