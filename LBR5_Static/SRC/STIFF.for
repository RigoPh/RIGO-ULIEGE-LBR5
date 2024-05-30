      SUBROUTINE STIFF(IPRINT,NETO,NEL,E,ETA,SIGY,WIDTH,Q,PHIL,
     *                 PLOC,XI,XF,CHAMAX,dCHAMAX,KSE,NSOL,IS,			!!!aout04
     *                 DELTA,EPSA,HXR,DXR,WXR,TXR,EPSR,ENTR,KSR,
     *                 NVAR,NXIT,
     *                 Plat,Fl,
     *                 SIG1,SIG2,SIG3,SIG4,Tau,Flr,Wpl1,Wpl2,         !avril2003				    
     *                 dSIG1,dSIG2,dSIG3,dSIG4,dTau,dFlr,dWpl1,dWpl2, !avril2003
     *                 IQ50,IOPTI,NTOT,IMPR)						!15.10.05		

      IMPLICIT REAL*8(A-HXR,O-Z)

	REAL *8 IX,IX2   ! variables de travail

      COMMON /PY/ PI
c      COMMON /OPTI/IOPTI,NTOT
      DIMENSION NVAR(NETO),NXIT(9,NETO)
      DIMENSION XI(NSOL),XF(NSOL),CHAMAX(5,NSOL),dCHAMAX(5,NSOL,9),	!!!aout04
     *          dPLAT(9),dPlat2(9)									!!!aout04

      DIMENSION dSIG1(9),dSIG2(9),dSIG3(9),dSIG4(9),dTau(9),dFlr(9), !avril2003
     *          dWpl1(9),dWpl2(9)  ! vecteur de travail   
    
C*******************************************************************************
C     SUBROUTINE STIFF (FLEXION LOCALE DES RAIDISSEURS) 
C     =================
C     Cette subrourine est appellée dans BO2.
C                                                                    
C  INPUTS:E,ETA,SIGY,WIDTH,Q,PHIL, 
C         DELTA, EPSA et HXR,DXR,WXR,TXR,EPSR  + ENTR, KSR
C         avec ENTR : entredistance réelle entre raidisseurs = DELTA(raid)
C                     (cfr valeur du fichier de donnees)
C              EPSR : Largeur effective associée à 1 raid (= EPSA/nbre de raid)
c                     Soit la vrai varaible de coneption
C                                                                       
C  OUTPUTS:
C
C  Créer: le 15-11-2000  par  Ph. Rigo
C
C  Modifications: 
C     - 15 avril 2003 : Modif en vue de la combinaison des contraintes (F. Bair)
C                               
C  Dernière modif : 20-5-2003	     	
C                                                                       
C*************************************************************************
C*************************************************************************
C*************************************************************************
c
C               ^ Y et Sigma Y
C               I
C               *****************************
C               *                           *  b=EPSR (Var. de conception)
C               *                           *  b=ENTR (pour STIFF.)
C               *                           *
C               ***************************** --------> X et Sigma X
c               <----------- a = EPSA ------>
c
c      EPSA  	    = a   (m)
c      ENTR,EPSR	= b   (m)
c      SIGY		= Limite Elastique (N/m2), (Ex, Re=240 N/mm2)
C      Plat = Max(XI,XF,CHA,...)
c ------------------------------------------------------------------------
c          
c      dS   = dSdXI = d(S)/d(XI)    XI=1,9
c      dS(K)= d(S)/d(XI=K) 
C   
c    dZg,dV1,dV2,dZ1,dZ2,dZ3,dZa,dIx,dS  ! variable de travail
c    dSIG1(XI),dSIG2(XI,dTau(XI),dFlr(XI),dWpl(XI)
c
c	XI = 1  DELTA
c	XI = 5  EPSA
c	XI = 6  HXR
c	XI = 7  DXR
c	XI = 8  WXR
c	XI = 9  EPSR (vrai variable et non pas ENTR)
c =============================================================
 
C     Recherche de la pression max et de son signe (sens)
c     ---------------------------------------------------
C     Plat positif si oriente dans le sens de l'axe z du repere local du panneau
c          Plat>0 si KSE = 1  et XI>0
c                 si KSE = 2  et XI<0
c          Plat<0 si KSE = 1  et XI<0
c                 si KSE = 2  et XI>0
c     KSR = 1  Raid. du cote des z<0  (cas de reference)
c          si XPlat >0 : semelle comprimee et borde tendu
c          si XPlat <0 : semelle tendue    et borde comprime
c     KSR = 2  Raid. du cote des z>0
c          si XPlat >0 : semelle tendue    et borde comprime  
c          si XPlat <0 : semelle comprimee et borde tendu

	CALL ANNULD(dPLAT,9)							!!!aout04
      Plat =DMAX1(CHAMAX(1,IS),CHAMAX(2,IS),CHAMAX(3,IS),   ! Plat en N/m2   
     *            CHAMAX(4,IS),CHAMAX(5,IS)) *9810.0        ! CHAMAX en m d'eau
	INDMAX=1										!!!aout04
	DO I=2,5										!!!aout04
	  IF (CHAMAX(I,IS).GT.CHAMAX(I-1,IS)) INDMAX=I	!!!aout04
	ENDDO											!!!aout04
      Plat2=DMIN1(CHAMAX(1,IS),CHAMAX(2,IS),CHAMAX(3,IS),   !   
     *            CHAMAX(4,IS),CHAMAX(5,IS)) *9810.0        ! 
	INDMIN=1										!!!aout04
	DO I=2,5										!!!aout04
	  IF (CHAMAX(I,IS).LT.CHAMAX(I-1,IS)) INDMIN=I	!!!aout04
	ENDDO											!!!aout04
	
	IF(DABS(Plat).LE.DABS(Plat2)) THEN				!!!aout04
	  Plat=Plat2									!!!aout04
	  IND=INDMIN									!!!aout04
	ELSE											!!!aout04
	  IND=INDMAX									!!!aout04
	ENDIF											!!!aout04

	IF(DABS(Plat).LE.DABS(PLOC*9810)) THEN			!!!aout04
	  Plat=PLOC*9810								!!!aout04
	ELSE											!!!aout04
	  DO K=1,9										!!!aout04
		dPlat(K)=dCHAMAX(IND,IS,K)*9810.0			!!!aout04
	  ENDDO											!!!aout04
	ENDIF											!!!aout04

	PLAT2= DABS(Plat) ! pour le calcul des Tau et de la fleche
	SIGNE=1.										!!!aout04
	IF (Plat.lt.0) SIGNE=-1.						!!!aout04
	DO K=1,9										!!!aout04
	  dPlat2(K)=SIGNE*dPlat(K)						!!!aout04
	ENDDO											!!!aout04
	  

c     IF(KSR.EQ.2) Plat= Plat  !si KSR=1 (-> Plat >0 : semelle comprimee et borde tendu)
cc	IF(KSR.EQ.2) Plat=-Plat  !si KSR=2 (-> Plat <0 : semelle comprimee et borde tendu)  !avril2003

C     Détermination du signe des contraintes (positif en traction)                        !avril2003
      SIGNE=1.										!!!aout04
	IF (Plat.lt.0) THEN
	   IF (KSE.eq.KSR) THEN							!!!aout04
	     Plat = - Plat								!!!aout04
		 SIGNE=-1.									!!!aout04
	   ENDIF										!!!aout04
	ELSE
	   IF (KSE.ne.KSR) THEN							!!!aout04
		 Plat = - Plat								!!!aout04
		 SIGNE=-1.									!!!aout04
	   ENDIF										!!!aout04
	ENDIF

	DO K=1,9										!!!aout04
		dPlat(K)=SIGNE*dPlat(K)						!!!aout04
	ENDDO

c------Impressions des données-----------------------------------------------
	IF(IMPR.GE.-1) THEN						!15.10.05
	WRITE (66,*)'Subr STIFF (flexion locale raid.)'
	WRITE (66,*)'*********************************'
	WRITE (66,*)'  DELTA(m)  H(web)   D(web)    B(Flange)  T(Flange)'
	WRITE (66,'(5F9.4)')  DELTA,HXR,DXR,WXR,TXR
	WRITE (66,*)'  EPSA(m)  EPSR    ENTR(m)     Plat(N/m2)    E(N/m2)'
	WRITE (66,'(3F9.4,2E14.7)') EPSA,EPSR,ENTR,Plat,E
	ENDIF								!15.10.05

c  ===========================================================
c !! Le repère local du raid. est situé à la junction JAB
c    -----------------------------------------------------
	DELTA2=DELTA*DELTA
	EPSA2 =EPSA *EPSA
	HXR2  =HXR  *HXR

	AREA= (ENTR*DELTA +DXR*HXR +WXR*TXR)			    ! surface totale non reduite
	STAT= -ENTR*DELTA2+DXR*HXR2+WXR*TXR*(2.0*HXR+TXR)	! 2*Moment statique
	Zg= STAT/(2*AREA)  ! position axe neutre
	Z1=  Zg+DELTA/2.0
	Z2= -Zg+  HXR/2.0
	Z3= -Zg+HXR+TXR/2.0
	za=(-Zg+HXR)/2.0

	Ix= ENTR*DELTA*(Z1*Z1) + DXR*HXR*(Z2*Z2) + WXR*TXR*(Z3*Z3)
     *	 + (ENTR*(DELTA**3)+DXR*(HXR**3)+WXR*(TXR**3))/12
	S= (HXR-Zg)**2*DXR/2 + WXR*TXR*Z3 ! moment statique partie sup. par rapport axe neutre 
                                        ! (pour calcul Tau)
c	v1= Zg+DELTA		! excentrement borde (fibre extreme) !avril2003
      v1 = Zg + DELTA/2                                        !avril2003
	v2=-(HXR-Zg+TXR)	! excentrement partie sup semelle
	v3 = Zg             ! jonction âme-bordage               !avril2003
	v4 = Zg - HXR       ! jonction âme-semelle               !avril2003

c	Calcul contraintes et fleche (Fl) du raidisseur
c     ------------------------------------------------
	SIG1 = Plat*ENTR*EPSA2*v1 /(10.0*Ix)        ! M=Pl**2/10 soit intermediare entre 1/8 et 1/12
	SIG2 = Plat*ENTR*EPSA2*v2 /(10.0*Ix)
	SIG3 = Plat*ENTR*EPSA2*v3 /(10.0*Ix)      !avril2003
	SIG4 = Plat*ENTR*EPSA2*v4 /(10.0*Ix)      !avril2003
	Tau  = Plat2*ENTR*EPSA *S /(2.0*DXR*Ix)
	Fl   = 5.*Plat2*ENTR*(EPSA**4)/(384.0*E*Ix)  ! 5/384 c-à-d extrémites libres
	Flr  = FL/EPSA                               ! Fl/portée = flèche relative
   
c	Calcul de la fleche (Wpl) de la plaque non raidie, encastrée entre 2 raidisseurs
c     ---------------------------------------------------------------------------------
C     Wpl1= 1/384 P ENTR**4/EI  (borne sup. car effets des 2 autres bords négligés)
C     Cette déformation correspond à une charge uniforme. 
C     Toutes les mailles se déforment de façon identique (idem bord encasreés)
      Wpl1=Plat2*(ENTR**4)/(384.0*E*DELTA2*DELTA/12.0)

c	Calcul de la fleche (Wpl) de la plaque non raidie, appuyée entre 2 raidisseurs
c     ---------------------------------------------------------------------------------
C     Wpl2= 5/384 P ENTR**4/EI (borne sup. car effets des 2 autres bords négligés)
C     Cette déformation correspond à une charge localisée. 
C     Seule cette maille se déforme (idem poutre sur appuis multiples avec 1 seule travée chargée)
      Wpl2=5.0*Wpl1   ! borne sup. car assimilé à 1 poutre sur 2 appuis

C     IMPRESSIONS
c     -----------
	IF(IMPR.GE.-1) THEN						!15.10.05
	  write(66,9) ' Area (AREA)           ',AREA
	  write(66,9) ' Moment statique (STAT)',STAT
 	  write(66,9) ' Gravity center        ',Zg
c	  write(66,9) ' Z1 ', Z1
c	  write(66,9) ' Z2 ', Z2
c	  write(66,9) ' Z3 ', Z3
c	  write(66,9) ' ZA ', ZA
	  write(66,9) ' Static moment(S)        ', S         !avril2003
	  write(66,9) ' Inertia moment          ', Ix        !avril2003
	  write(66,9) ' v1 (milieu bordé z=0)   ', v1        !avril2003
	  write(66,9) ' v2 (partie sup semelle) ', v2        !avril2003
	  write(66,9) ' v3 (jonction âme-bordé) ', v3        !avril2003
	  write(66,9) ' v4 (jonction âme-sem)   ', v4        !avril2003

	  write(66,*)  
	  write(66,*) ' Resultat Flexion Locale Raid. et de la Maille '
	  write(66,*) ' --------------------------------------------- '
	  write(66,9) ' Sigma 1 (milieu bordé)  ', SIG1       !avril2003
	  write(66,9) ' Sigma 2 (semelle)       ', SIG2
	  write(66,9) ' Sigma 3 (JAB)           ', SIG3       !avril2003
	  write(66,9) ' Sigma 4 (JAS)           ', SIG4       !avril2003
	  write(66,9) ' Shear Stress (Max)      ', Tau
	  write(66,9) ' Flèche Raid.(5/384) (m) ', Fl
	  write(66,9) ' Flèche Relative (f/EPSA)', Flr
	  write(66,*)  
	  write(66,*) ' Flèche Maille (valeur absolue)' 
	  write(66,9) ' - charge uniforme  (bords encastrés)',Wpl1
	  write(66,9) ' - charge localisée (bords articulés)',Wpl2
	  write(66,*)

	ENDIF					!15.10.05
c	  write(67,'(I3,3(1x,E11.4),3x,4(E11.4,2x))')
c     *                NEL,Plat,SIG1,SIG2,TAU,Fl,Wpl1,Wpl2

C     CALCUL DES SENSIBILITES
c     ===========================
      IF(IOPTI.EQ.0) RETURN
      IF(IQ50.EQ.0)  RETURN ! restriction pas selectionnées

	NBRXI = NVAR(NEL)
      
	IF(IPRINT.EQ.1) THEN
	 WRITE(66,*) ' =*=*=*=*=*=*=*=*=*=*=*=*=*=* '
	 WRITE(66,*) '  Sensibilities = d(f)/d(XI)  '
	 WRITE(66,*) ' =*=*=*=*=*=*=*=*=*=*=*=*=*=* '
	ENDIF

	AREA2=AREA*AREA
	IX2  =IX*IX

      DO 1 J=1,NBRXI
	K=NXIT(J,NEL)

	IF(K.EQ.1) THEN

c	XI= DELTA 
c     --------
	dZg= -(2.0*DELTA*ENTR*AREA+ENTR*STAT)/(2.0*AREA2)
	dV1=  dZg + 1.0/2   !avril2003
	dV2=  dZg
	dV3=  dZg           !avril2003
	dV4=  dZg           !avril2003
	dZ1=  dZg + 0.5
	dZ2= -dZg
	dZ3= -dZg
	dZa= -dZg/2.0
	dIx= ENTR*((Z1*Z1)+2.0*DELTA*Z1*dZ1)
     *		  + (ENTR*DELTA2)/4.+2*DXR*HXR*Z2*dZ2
     *		  + 2.*WXR*TXR*Z3*dZ3
	dS= DXR*(-dZg*za+(HXR-Zg)*dZa) + WXR*TXR*dZ3

	IF (PLAT.NE.0) THEN														!!!aout04
	dSIG1(K) = (Plat*ENTR*EPSA2/(10.*IX2))*(Ix*dV1-v1*dIx) 
     *           + dPlat(K)*SIG1/Plat											!!!aout04
	dSIG2(K) = (Plat*ENTR*EPSA2/(10.*IX2))*(Ix*dV2-v2*dIx)
     *		   + dPlat(K)*SIG2/Plat											!!!aout04
	dSIG3(K) = (Plat*ENTR*EPSA2/(10.*IX2))*(Ix*dV3-v3*dIx)    !avril2003
     *		   + dPlat(K)*SIG3/Plat											!!!aout04
	dSIG4(K) = (Plat*ENTR*EPSA2/(10.*IX2))*(Ix*dV4-v4*dIx)    !avril2003
     *		   + dPlat(K)*SIG4/Plat											!!!aout04
	dTau(K)  = (Plat2*ENTR*EPSA/(2.*DXR*IX2))*(Ix*dS-S*dIx)
     *           + dPlat2(K)*Tau/Plat2										!!!aout04
c	dFl(K) =-5.0*(Plat2*ENTR*(EPSA**4)/(384.*E*IX2))*dIx
	dFlr(K)=-5.0*(Plat2*ENTR*(EPSA**3)/(384.*E*IX2))*dIx
     *           + dPlat2(K)*Flr/Plat2										!!!aout04
      dWpl1(K)=-3.*Wpl1/DELTA + dPlat2(K)*Wpl1/Plat2							!!!aout04
      dWpl2(K)= 5.*dWpl1(K)
	ENDIF																	!!!aout04

	IF(IPRINT.EQ.1) THEN
	 WRITE(66,'(/A)') ' XI = DELTA '
       WRITE(66,10) dSIG1(K),dSIG2(K),dSIG3(K),dSIG4(K),dTau(K),dFlr(K),   !avril2003
     *             dWpl1(K)                                                !avril2003
      ENDIF

	if (nel.eq.2) then
	continue
	endif

	ELSE IF(K.EQ.5) THEN

c	XI= EPSA 
c     --------
	IF (PLAT.NE.0) THEN												!!!aout04
	dSIG1(K) = Plat*ENTR*EPSA*v1/( 5.0*Ix) + dPlat(K)*SIG1/Plat		!!!aout04
	dSIG2(K) = Plat*ENTR*EPSA*v2/( 5.0*Ix) + dPlat(K)*SIG2/Plat		!!!aout04
	dSIG3(K) = Plat*ENTR*EPSA*v3/( 5.0*Ix) + dPlat(K)*SIG3/Plat		!!!aout04         !avril2003
	dSIG4(K) = Plat*ENTR*EPSA*v4/( 5.0*Ix) + dPlat(K)*SIG4/Plat		!!!aout04         !avril2003
	dTau(K)  = Plat2*ENTR*S      /( 2.0*DXR*Ix) + dPlat2(K)*Tau/Plat2 !!!aout04
c	dFl(K)    = 5.0*Plat2*ENTR*(EPSA**3) /( 96.0*E*Ix)
	dFlr(K)   = 5.0*Plat2*ENTR*(EPSA**2) /(128.0*E*Ix)
     *			 + dPlat2(K)*Flr/Plat2								!!!aout04
      dWpl1(K)= 0. + dPlat2(K)*Wpl1/Plat2								!!!aout04
      dWpl2(K)= 5.*dWpl1(K)											!!!aout04
	ENDIF															!!!aout04

	IF(IPRINT.EQ.1) THEN
	 WRITE(66,'(/A)') ' XI = EPSA '
       WRITE(66,10) dSIG1(K),dSIG2(K),dSIG3(K),dSIG4(K),dTau(K),dFlr(K),   !avril2003
     *             dWpl1(K)                                                !avril2003
      ENDIF

	ELSE IF(K.EQ.6) THEN

c	XI= HXR 
c     --------
	dZg= (2.0*(DXR*HXR+WXR*TXR)*AREA-DXR*STAT)/(2.0*AREA2)
	dV1=       dZg
	dV2=-1.0 + dZg
      dV3=       dZg    !avril2003
      dV4=-1.0 + dZg    !avril2003
	dZ1=       dZg
	dZ2= 0.5 - dZg
	dZ3= 1.0 - dZg
	dZa=(1.0 - dZg) /2.0
	dIx= DXR*((Z2**2)+2.0*HXR*Z2*dZ2)
     *		+ (DXR*HXR2)/4.0 + 2.0*ENTR*DELTA*Z1*dZ1
     *		+ 2.*WXR*TXR*Z3*dZ3
	dS= DXR*za + HXR*DXR*dZa-DXR*za*dZg - Zg*DXR*dZa+WXR*TXR*dZ3

	IF (PLAT.NE.0) THEN												!!!aout04
	dSIG1(K)= (Plat*ENTR*EPSA2/(10.*IX2))*(Ix*dV1-v1*dIx)
     *           + dPlat(K)*SIG1/Plat									!!!aout04
	dSIG2(K)= (Plat*ENTR*EPSA2/(10.*IX2))*(Ix*dV2-v2*dIx)
     *           + dPlat(K)*SIG2/Plat									!!!aout04
	dSIG3(K)= (Plat*ENTR*EPSA2/(10.*IX2))*(Ix*dV3-v3*dIx)     !avril2003
     *           + dPlat(K)*SIG3/Plat									!!!aout04
	dSIG4(K)= (Plat*ENTR*EPSA2/(10.*IX2))*(Ix*dV4-v4*dIx)     !avril2003
     *           + dPlat(K)*SIG4/Plat									!!!aout04
	dTau(K) = (Plat2*ENTR*EPSA/(2.*DXR*IX2))*(Ix*dS-S*dIx)
     *           + dPlat2(K)*Tau/Plat2								!!!aout04
c	dFl(K) = -5.0*(Plat2*ENTR*(EPSA**4)/(384.*E*IX2))*dIx
	dFlr(K)= -5.0*(Plat2*ENTR*(EPSA**3)/(384.*E*IX2))*dIx
     *           + dPlat2(K)*Flr/Plat2								!!!aout04

      dWpl1(K)= 0. + dPlat2(K)*Wpl1/Plat2								!!!aout04
      dWpl2(K)= 5.*dWpl1(K)											!!!aout04
	ENDIF															!!!aout04

	IF(IPRINT.EQ.1) THEN
	 WRITE(66,'(/A)') ' XI = HXR '
       WRITE(66,10) dSIG1(K),dSIG2(K),dSIG3(K),dSIG4(K),dTau(K),dFlr(K),   !avril2003
     *             dWpl1(K)                                                !avril2003
      ENDIF

	ELSE IF(K.EQ.7) THEN

c	XI= DXR 
c     --------
	dZg= (HXR2*AREA-HXR*STAT)/(2*AREA2)
	dV1=  dZg
	dV2=  dZg
	dV3=  dZg    !avril2003
	dV4=  dZg    !avril2003
	dZ1=  dZg
	dZ2= -dZg
	dZ3= -dZg
	dZa= -dZg/2
	dIx= HXR*((Z2**2)+2.*DXR*Z2*dZ2)
     *		  + (HXR**3)/12. + 2.*ENTR*DELTA*Z1*dZ1
     *		  + 2.*WXR*TXR*Z3*dZ3
	dS= (HXR-Zg)*za+DXR*(-dZg*za + (HXR-Zg)*dZa) + WXR*TXR*dZ3

	IF (PLAT.NE.0) THEN												!!!aout04
	dSIG1(K)= (Plat*ENTR*EPSA2/(10.*IX2))*(Ix*dV1-v1*dIx)
     *		   + dPlat(K)*SIG1/Plat									!!!aout04
	dSIG2(K)= (Plat*ENTR*EPSA2/(10.*IX2))*(Ix*dV2-v2*dIx)
     *		   + dPlat(K)*SIG2/Plat									!!!aout04
	dSIG3(K)= (Plat*ENTR*EPSA2/(10.*IX2))*(Ix*dV3-v3*dIx)    !avril2003
     *		   + dPlat(K)*SIG3/Plat									!!!aout04
	dSIG4(K)= (Plat*ENTR*EPSA2/(10.*IX2))*(Ix*dV4-v4*dIx)	 !avril2003
     *		   + dPlat(K)*SIG4/Plat									!!!aout04
	dTau(K) = (Plat2*ENTR*EPSA/(2.*(DXR**2)*IX2))*
     *                 (Ix*DXR*dS-S*(DXR*dIx+Ix))
     *		   + dPlat2(K)*Tau/Plat2								!!!aout04
c	dFl(K) = -5.0*(Plat2*ENTR*(EPSA**4)/(384.*E*IX2))*dIx
	dFlr(K)= -5.0*(Plat2*ENTR*(EPSA**3)/(384.*E*IX2))*dIx
     *		   + dPlat2(K)*Flr/Plat2								!!!aout04
      
	dWpl1(K)= 0. + dPlat2(K)*Wpl1/Plat2 							!!!aout04
      dWpl2(K)= 5.*dWpl1(K)											!!!aout04
	ENDIF															!!!aout04

	IF(IPRINT.EQ.1) THEN
 	 WRITE(66,'(/A)') ' XI = DXR '
       WRITE(66,10) dSIG1(K),dSIG2(K),dSIG3(K),dSIG4(K),dTau(K),dFlr(K),   !avril2003
     *             dWpl1(K)                                                !avril2003
      ENDIF

	ELSE IF(K.EQ.8) THEN

c	XI= WXR 
c     --------
	dZg= (TXR*(2.0*HXR+TXR)*AREA-TXR*STAT)/(2.0*AREA2)
	dV1=  dZg
	dV2=  dZg
	dV3=  dZg   !avril2003
	dV4=  dZg   !avril2003
	dZ1=  dZg
	dZ2= -dZg
	dZ3= -dZg
	dZa= -dZg/2
	dIx= TXR*((Z3**2)+2.0*WXR*Z3*dZ3) + (TXR**3)/12.0 
     *		+ 2.0*DXR*HXR*Z2*dZ2 + 2.0*ENTR*DELTA*Z1*dZ1
	dS =  DXR*(-dZg*za+(HXR-Zg)*dZa)+TXR*Z3+WXR*TXR*dZ3

	IF (PLAT.NE.0) THEN												!!!aout04
	dSIG1(K) = (Plat*ENTR*EPSA2/(10.*IX2))*(Ix*dV1-v1*dIx)
     *		   + dPlat(K)*SIG1/Plat									!!!aout04
	dSIG2(K) = (Plat*ENTR*EPSA2/(10.*IX2))*(Ix*dV2-v2*dIx)
     *		   + dPlat(K)*SIG2/Plat									!!!aout04
	dSIG3(K) = (Plat*ENTR*EPSA2/(10.*IX2))*(Ix*dV3-v3*dIx)   !avril2003
     *		   + dPlat(K)*SIG3/Plat									!!!aout04
	dSIG4(K) = (Plat*ENTR*EPSA2/(10.*IX2))*(Ix*dV4-v4*dIx)   !avril2003
     *		   + dPlat(K)*SIG4/Plat									!!!aout04
	dTau(K) =  (Plat2*ENTR*EPSA/(2.0*DXR*IX2))*(Ix*dS-S*dIx)
     *		   + dPlat2(K)*Tau/Plat2								!!!aout04
c	dFl(K)   = -5.0*(Plat2*ENTR*(EPSA**4)/(384.0*E*IX2))*dIx
	dFlr(K)  = -5.0*(Plat2*ENTR*(EPSA**3)/(384.0*E*IX2))*dIx
     *		   + dPlat2(K)*Flr/Plat2								!!!aout04

      dWpl1(K)= 0. + dPlat2(K)*Wpl1/Plat2								!!!aout04
      dWpl2(K)= 5.*dWpl1(K)											!!!aout04
	ENDIF															!!!aout04

	IF(IPRINT.EQ.1) THEN
	 WRITE(66,'(/A)') ' XI = WXR '
       WRITE(66,10) dSIG1(K),dSIG2(K),dSIG3(K),dSIG4(K),dTau(K),dFlr(K),   !avril2003
     *             dWpl1(K)                                                !avril2003
      ENDIF

	ELSE IF(K.EQ.9) THEN

c	XI= EPSR 
c     --------
	dZg= -(DELTA2*AREA+DELTA*STAT)/(2*AREA2)  != d(ZG)/d(XI)
	dV1=  dZg
	dV2=  dZg
	dV3=  dZg    !avril2003
	dV4=  dZg    !avril2003
	dZ1=  dZg
	dZ2= -dZg
	dZ3= -dZg
	dZa= -dZg/2.0
	dIx= DELTA*((Z1**2)+2.0*ENTR*Z1*dZ1) + (DELTA**3)/12.
     *	     + 2.0*DXR*HXR*Z2*dZ2 + 2.0*WXR*TXR*Z3*dZ3
	dS  = DXR*(-dZg*za + (HXR-Zg)*dZa) + WXR*TXR*dZ3

	IF (PLAT.NE.0) THEN												!!!aout04
	dSIG1(K)= (Plat*EPSA2/(10.0*Ix))*(v1+ENTR*(Ix*dV1-v1*dIx)/Ix)
     *		   + dPlat(K)*SIG1/Plat									!!!aout04
	dSIG2(K)= (Plat*EPSA2/(10.0*Ix))*(v2+ENTR*(Ix*dV2-v2*dIx)/Ix)
     *		   + dPlat(K)*SIG2/Plat									!!!aout04
	dSIG3(K)= (Plat*EPSA2/(10.0*Ix))*(v3+ENTR*(Ix*dV3-v3*dIx)/Ix)   !avril2003
     *		   + dPlat(K)*SIG3/Plat									!!!aout04
	dSIG4(K)= (Plat*EPSA2/(10.0*Ix))*(v4+ENTR*(Ix*dV4-v4*dIx)/Ix)   !avril2003
     *		   + dPlat(K)*SIG4/Plat									!!!aout04
	dTau(K) = (Plat2*EPSA/(2*DXR*Ix))*(S+ENTR*(Ix*dS-S*dIx)/Ix)
     *		   + dPlat2(K)*Tau/Plat2								!!!aout04
c	dFl(K) =5.0*(Plat2*(EPSA**4)/(384.*E*IX2))*(Ix-ENTR*dIx)
	dFlr(K)=5.0*(Plat2*(EPSA**3)/(384.*E*IX2))*(Ix-ENTR*dIx)
     *		   + dPlat2(K)*Flr/Plat2								!!!aout04

      dWpl1(K)= 4.0* Wpl1/ENTR + dPlat2(K)*Wpl1/Plat2					!!!aout04
      dWpl2(K)= 5.0*dWpl1(K)
	ENDIF															!!!aout04

c	!!!!  DESIGN VARIABLE= ENTR et pas EPSR'
c	WRITE(66,'(/A)') ' XI = ENTR '
c     WRITE(66,10) dSIG1(K),dSIG2(K),dTau(K),dFl(K)

      !Prise en compte de  d(ENTR)/d(EPSR) = (ENTR/EPSR)**2 
	  Temp=(ENTR/EPSR)**2
	  dSIG1(K) = dSIG1(K) * Temp
	  dSIG2(K) = dSIG2(K) * Temp
	  dTau(K)  = dTau(K)  * Temp
	  dFlr(K)  = dFlr(K)  * Temp
	  dWpl1(K) = dWpl1(K) * Temp
	  dWpl2(K) = dWpl2(K) * Temp

	IF(IPRINT.EQ.1) THEN
	 WRITE(66,'(/A)') ' XI = EPSR '
       WRITE(66,10) dSIG1(K),dSIG2(K),dSIG3(K),dSIG4(K),dTau(K),dFlr(K),   !avril2003
     *             dWpl1(K)                                                !avril2003
      ENDIF

      ELSE

c	XI= 2,3,4 (HYA, DYA et WYA)
c     ---------------------------
	dSIG1(K) = 0.0
	dSIG2(K) = 0.0
	dSIG3(K) = 0.0   !avril2003
	dSIG4(K) = 0.0   !avril2003
	dTau (K) = 0.0
	dFlr (K) = 0.0
      dWpl1(K) = 0.0
      dWpl2(K) = 0.0

	IF(IPRINT.EQ.1) THEN
	 WRITE(66,'(/A,I1,A)') ' XI = ',K,' (Not used)'
       WRITE(66,10) dSIG1(K),dSIG2(K),dSIG3(K),dSIG4(K),dTau(K),dFlr(K),   !avril2003
     *             dWpl1(K)                                                !avril2003
      ENDIF


      ENDIF

  1   CONTINUE

      IF (EPSA.GE.WIDTH) THEN !avril2003
	SIG1 = 0                !avril2003
	SIG2 = 0                !avril2003
	SIG3 = 0                !avril2003
	SIG4 = 0                !avril2003
	TAU  = 0                !avril2003
	CALL ANNULD(dSIG1,NBRXI)!avril2003
	CALL ANNULD(dSIG2,NBRXI)!avril2003
	CALL ANNULD(dSIG3,NBRXI)!avril2003
	CALL ANNULD(dSIG4,NBRXI)!avril2003
	CALL ANNULD(dTAU,NBRXI) !avril2003
	ENDIF                   !avril2003   

C ------------------------------------

   9	FORMAT(A,' = ',E14.7,1x,A)
  10  FORMAT('   Sigma 1 (milieu bordé) ',E14.7/     !avril2003
     *       '   Sigma 2 (semelle)      ',E14.7/     !avril2003
     *       '   Sigma 3 (JAB)          ',E14.7/     !avril2003
     *       '   Sigma 4 (JAS)          ',E14.7/     !avril2003 
     *       '   Tau(ame - axe neutre)  ',E14.7/
     *       '   Deflection  Rel. Raid. ',E14.7/
     *       '   Deflection  Abs. Maille',E14.7)

C ------------------------------------	                                                                   
      RETURN                                                              
      END                                                                       
