      SUBROUTINE MANQ(LAM,ETA,C,D,K,Q,ITYPE)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION D(1710),C(74),LAM(8)
      REAL*8 LAM
C***********************************************************************
C
C     SUBROUTINE MANQ
C     SUBROUTINE DE CALCUL DES COEFFICIENTS NON ENCORE CALCULES
C     PRECEDEMMENT  NX,NXY,MX,MXY,MYX,QX,RX
C     27-1-94
C***********************************************************************
      DO 10 I=1,K
	IF (ITYPE.NE.5) THEN			
      D(38*K+I)=-(C(1)+C(9))*LAM(1)*D(K+I)+C(1)*ETA*(D(29*K+I)
     *         +D(2*K+I)/Q) + LAM(2)*C(10)*D(2*K+I)
      D(39*K+I)=D(26*K+I)
c       WRITE(66,*) ETA
      D(40*K+I)=-LAM(2)*(C(2)+C(12))*D(2*K+I)
     *         +C(2)*ETA*D(34*K+I)+C(10)*LAM(1)*D(K+I)
c       WRITE(66,*) ETA,ETA
      D(41*K+I)=(C(2)*(1.-ETA)+C(13))*LAM(1)*D(31*K+I)
     *          +C(14)*LAM(1)*D(I)
      D(42*K+I)=(C(2)*(1.-ETA)+C(7))*LAM(1)*D(31*K+I)+C(8)*D(30*K+I)
      D(43*K+I)=(C(2)+C(7))*LAM(1)*D(34*K+I)
     *         -LAM(3)*(C(2)+C(12))*D(2*K+I)+C(8)*D(33*K+I)
     *         +LAM(2)*C(10)*D(1*K+I)
      D(44*K+I)=D(43*K+I)+(C(2)*(1.-ETA)+C(13))*LAM(1)*D(34*K+I)
     *         + C(14)*LAM(1)*D(29*K+I)
 	ELSE
      D(38*K+I)=-C(9)*LAM(1)*D(K+I)			!Nx
     *         + LAM(2)*C(10)*D(2*K+I)	
      D(39*K+I)=D(26*K+I)						!Nxy
      D(40*K+I)=-LAM(2)*C(12)*D(2*K+I)		!Mx
     *         +C(10)*LAM(1)*D(K+I)			
      D(41*K+I)= C(13)*LAM(1)*D(31*K+I)		!Mxy
      D(42*K+I)=C(7)*LAM(1)*D(31*K+I)			!Myx
      D(43*K+I)=C(7)*LAM(1)*D(34*K+I)			!Qx
     *         -LAM(3)*C(12)*D(2*K+I)
     *         +LAM(2)*C(10)*D(1*K+I)
      D(44*K+I)=D(43*K+I)+C(13)*LAM(1)*D(34*K+I)	!Rx
	ENDIF
   10 CONTINUE
      RETURN
      END
