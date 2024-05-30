
      SUBROUTINE CONTR(NETO,NEL,IPRINT,IM,IMG,ITERA,IS,
     *                 E,ETA,WIDTH,SIGY,CONST,EFFCOMB,EFF1,				!avril2003
     *       EFF,EFF2,CONC,Z2,IMPR,										!15.10.05
     *       DTPL,TPLD,TPLA,TPLR,DSIGPL,dDSIGPL,DSIGSTIF,dDSIGSTIF,		!r&d13
     *	   SIGX,dSIGX,													!r&d13
     *       LAMB,Q,DELTA,PHIL,DYA,TYA,WYA,HYA,EPSA,KSA,HXR,DXR,WXR,
     *       TXR,EPSR,ENTR,KSR,SMX,CO,SENS2,SENS3,SENS4,DC,
     *       NVAR,M1TABL,M2CONT,NXIT,IPTS,YPTS,IPTS2,IPTS3,
     *       LCONT,CJMAX,INV,NXI2,
     *       SIG1,SIG2,SIG3,SIG4,Tau,Flr,Wpl1,Wpl2,           !avril2003
     *       dSIG1,dSIG2,dSIG3,dSIG4,dTau,dFlr,dWpl1,dWpl2,   !avril2003
     *       SigPlaque,DSigPlaque)                            !avril2003

      IMPLICIT REAL*8(A-H,O-Z)

	DIMENSION EFF(9690),EFF2(4280),CONC(750),Z2(2295),
     *          CO(11)

      CALL CONTR2(NETO,NEL,IPRINT,IM,IMG,ITERA,IS,
     *            E,ETA,WIDTH,SIGY,CONST,EFFCOMB,EFF1,        !avril2003
     *   EFF(1),EFF(256),EFF(511),EFF(4846),EFF(1021),EFF(6121),      !V,U,W,SBORD,NX,SAJAS, etc.
     *   EFF(6376),EFF(7141),EFF(8926),EFF(9181),EFF(9436),           ! suite
     *   EFF2(766),EFF2(1786),CONC(651),CONC(701),CONC(601),Z2(1480), ! suite
     *   IMPR,DTPL,TPLD,TPLA,TPLR,DSIGPL,dDSIGPL,DSIGSTIF,dDSIGSTIF,	!15.10.05	!r&d13
     *   SIGX,dSIGX,													!r&d13
     *   LAMB,Q,DELTA,PHIL,DYA,TYA,WYA,HYA,EPSA,KSA,HXR,DXR,WXR,TXR,
     *   EPSR,ENTR,KSR,SMX,CO(1),CO(10),CO(11),						!SMX,DSPAIK,SPAIK,OMT
     *   SENS2,SENS3,SENS4,DC,                        
     *   NVAR,M1TABL,M2CONT,NXIT,IPTS,YPTS,IPTS2,IPTS3,
     *   LCONT,CJMAX,INV,NXI2,
     *   SIG1,SIG2,SIG3,SIG4,Tau,Flr,Wpl1,Wpl2,               !avril2003
     *   dSIG1,dSIG2,dSIG3,dSIG4,dTau,dFlr,dWpl1,dWpl2,       !avril2003
     *   SigPlaque,DSigPlaque)                                !avril2003

	RETURN
	END

C     ****************************************************
C     ****************************************************
C     ****************************************************


      SUBROUTINE CONTR2(NETO,NEL,IPRINT,IM,IMG,ITERA,IS,
     *                  E,ETA,WIDTH,SIGY,CONST,EFFCOMB,EFF,			!avril2003
     *      V,U,W,SBORD,NX,SAJAS,SAJAB,SASEM,SRJAS,SRJAB,SRSEM,
     *      SBORD1,SBORD2,STJAS,STJAB,STSEM,DV,IMPR,					!15.10.05
     *      DTPL,TPLD,TPLA,TPLR,DSIGPL,dDSIGPL,DSIGSTIF,dDSIGSTIF,	!r&d13
     *	  SIGX,dSIGX,												!r&d13
     *      LAMB,Q,DELTA,PHIL,DYA,TYA,WYA,HYA,EPSA,KSA,HXR,DXR,WXR,
     *      TXR,EPSR,ENTR,KSR,SMX,DSPAIK,SPAIK,OMT,
     *      SENS2,SENS3,SENS4,DC,                     
     *      NVAR,M1TABL,M2CONT,NXIT,IPTS,YPTS,IPTS2,IPTS3,
     *      LCONT,CJMAX,INV,NXI2,
     *      SIG1,SIG2,SIG3,SIG4,Tau,Flr,Wpl1,Wpl2,            !avril2003
     *      dSIG1,dSIG2,dSIG3,dSIG4,dTau,dFlr,dWpl1,dWpl2,    !avril2003
     *      SigPlaque,DSigPlaque)                             !avril2003
C     ****************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 LAMB,NX(51,5)
      CHARACTER *12 VNOM2

      COMMON/DIM1/Nmax,NEmax,NVmax,
     *            M1max,M1Tmax,M2max,M2Tmax,Mmax,NGmax,ISmax,IPTmax ! Dimensions max des vecteurs
      COMMON/OPTI/IOPTI,NTOT,M1TOT,M2TOT

      DIMENSION V(51,5),U(51,5),W(51,5),SBORD(51,5),SAJAS(51,5),
     *   SAJAB(51,5),SASEM(51,5),SRJAS(51,5),SRJAB(51,5),SRSEM(51,5),
     *   SBORD1(51,5),SBORD2(51,5),
     *   STJAS(10,5),STJAB(10,5),STSEM(10,5),
     *   DV(51),CONST(74),EFFCOMB(9690),EFF(9690)          !avril2003

      DIMENSION DC(9*NETO),SENS2(16,IPTmax,9,NETO),        !avril2003
     *          SENS3(21,IPTmax,9,NETO),SENS4(9,IPTmax,9,NETO),
     *          SMX(IPTmax),DSPAIK(9)

      DIMENSION dSIG1(9),dSIG2(9),dSIG3(9),dSIG4(9),dTau(9),dFlr(9),   !avril2003
     *          dWpl1(9),dWpl2(9)                                      ! vecteur de travail

      DIMENSION NVAR(NETO),M1TABL(NETO),M2CONT(NETO),
     *          NXIT(9,NETO),IPTS(NETO),YPTS(IPTmax,NETO),
     *          IPTS2(IPTmax,NETO),IPTS3(IPTmax,NETO),LCONT(2,M1Tmax)

      DIMENSION CJMAX(M1Tmax),INV(M1Tmax),NXI2(9,NETO)
	DIMENSION SigPlaque(IPTmax),DSigPlaque(IPTmax,9)    !avril2003

	DIMENSION dDSIGPL(9,NETO),dDSIGSTIF(9,NETO),dSIGX(9,NETO)	!r&d13

C **********************************************************************
C     SUBROUTINE CONTRaintes (c.�.d) "RESTRICTIONS"
C     ==============================================
C     Calcul des M restrictions. C(j) du pan. NEL , (j=1+IM,M+IM)  (max 7000 pour la struct.)
C     IM = Nbre de restrictions d�j� consid�r�es par les panneaux 1 � NEL-1.
C
C     Calcul des NBRXI d�riv�es des restrictions DC(i,j), i=1,NBRXI [NBRXI=NVAR(NEL)]
C     IPTS = Nbre de points de calcul des restrictions (par panneau) (max 10)
C     YPTS = Ordonn�es de ces IPTS points de calcul des restrictions (par panneau)
C	LCONT(1,j+IM) = N� de r�f�rence de la restriction j du panneau NEL
C                     Ex: 11 pour Sigma Comp Bordage
C	LCONT(2,j+IM) = N� du pt d'applic. de la restr. j du panneau NEL (sensibilit�)
C                   = 1 � 10
C	IPT2(i,NEL) = N� des IPT pts d'applic. pour le panneau NEL (1 � 31)
C	IPT2(LCONT(2,j+IM),NEL) = N� du pt d'applic. de la restr. j du panneau NEL (1 � 31).
C	IPT3(LCONT(2,j+IM),NEL) = N� de la Trav. correspondant au pt IPT (pour le panneau NEL)
C
C     Cr�er   :  29-04-94   (Dr. Ph. RIGO)
C
C     Modifier:  15-05-96   Restriction n� 4  (dmin - d < 0)
C                 5- 6-96   Restrictions g�om�triques
C                 4- 7-96   Restrictions n�5 (W relatif) et subr. SENS3
C                22- 7-96   Restrictions n�15 Sx(ULS) de PAIK et subr. SENS4
C                 6- 3-97   Prise en compte des diff�rences entre D(raid) et de EPSR	       
C                16-11-00   Restrictions 51 � 56: Flexion locale raid.	       
C                 4-02-02   Restrictions struct par cas de charge	    
C                  -07-03   Ajouts de restrictions combin�es, changement du num�ro de 
C                           quelques restrictions   
C
C    DERNIERE MODIFICATION : juillet 2003
C                
C ******************************************************************************

C   Les variables de conception  ; NBRXI = NVAR(NEL)
C   -------------------------------------------------
C       1       DELTA =  �paisseur du bordage
C       2       HYA   =  hauteur de l'�me des cadres   
C       3       DYA   =  �paisseur de l'�me des cadres   
C       4       WYA   =  largeur des semelles des cadres   
C       5       EPSA  =  entredistance entre cadres   
C       6       HXR   =  hauteur de l'�me des raidisseurs
C       7       DXR   =  �paisseur de l'�me des raidisseurs
C       8       WXR   =  largeur des semelles des raidisseurs
C       9       EPSR  =  entredistance entre raidisseurs

C  Restrictions:
C  -------------
C  IC(J) = le num�ro de r�f�rence des M restrictions struct. (J=1,M) du panneau en question. 

C  [IX(J),IY(J)] coordonn�es (x,y) du point o� la restriction est appliqu�e.
C    - IX est choisi par d�faut (mi-port�e ou appuis selon la fct SINUS ou COSINUS).
C    - IY est une donn�e.
C       IY1 = n� du pt d'applic. de la restriction =1�4  (pts d'applic. des restrictions)
C       IY2 = n� du pt d'applic. de la restriction =1�31 (pts de calcul des d�plac. et contr.)

C  D�riv�es :SENS4(J,I,K,IPAN) Valeurs cumulees.
C        avec      J=1,9   les fonctions (ex: pour W, J=1)
C				   W,W�,W��,W���,U,U�,V,V�,V��
C		           1 2  3   4    5 6  7 8  9 
C  			 I=1,IPTS les points de calcul (correspondant aux pts de calcul des restrictions et des sensibilites)
C  			 K=1,9    les variables de conception (si Ep. bord., K=1)
C              IPAN=1,NETO le panneau relatif � ces K variables de conception

C ****************************************************************
C 1.  RESTRICTIONS STRUCTURELLES  (boucle 1)
C ****************************************************************
C       N� de r�f�rence de la restriction = IC=LCONT(1,ICONT)
C   ----------------------------------------------------------
C     n� 1     V d�placement (en m)
C     n� 2     U d�placement (en m)
C     n� 3     W d�placement (en m)

C	n� 4	 Epaisseur minimale du bord� (en m): dmin - d < 0 [Restriction dite de "Hughes"] 

C	n� 5	 W(relatif) D�pl. relatif par rapport au bord de d�part (Y=0)  du panneau (en m)
C	n� 6	 W(relatif) D�pl. relatif par rapport au bord d'arriv�e (Y=Yo) du panneau (en m)
C	n� 7	 W(relatif) D�pl. relatif par rapport au d�pl. moyen des extr�mit�s du panneau (en m)

C	n� 11	 Sigma comp. (Sx,Sy) bordage (Z=0)      en x=L/2 (en N/m2)  SENS2(1,..
C	n� 12	 Sigma comp. (Sx,Sy) bordage (Z=0)      en x=L/2 (en N/m2)  SENS2(1,.. avec flex.loc.des raid.  !avril2003
C	n� 13	 Sigma comp. (Sx,Sy) bordage (Z=0)      en x=L/2 (en N/m2)  SENS2(1,.. idem + effet plaque      !avril2003

C	n� 15	 Sx < S(Ult. Strength)  Faxial(moyen)   en X=L/2  (Cfr Subr PAIK)

C	n� 16	 Sigma comp. (Txy)   bordage (Z=0)      en x=0			SENS2(2,..       !avril2003
C	n� 17	 Sigma comp. (Sx,Sy) bordage (Z=+d/2)   en x=L/2		SENS2(4,..       !avril2003
C	n� 18	 Sigma comp. (Sx,Sy) bordage (Z=-d/2)   en x=L/2		SENS2(3,..       !avril2003

C	n� 21	 Sigma comp. (T�me)   cadre      JAS/JAB en X=0  (en N/m2)	SENS2(5,..
C	n� 22	 Sigma comp. (Sy,Txy) cadre      JAS     en X=L/2		SENS2(6,..
C	n� 23	 idem cond. n�21 car Tau(JAS)=Tau(JAB)				    SENS2(7,..
C	n� 24	 Sigma comp. (Sy,Txy) cadre      JAB     en X=L/2		SENS2(8,..
C	n� 25	 Sigma comp. (Sy,Txy) cadre      Semelle en X=L/2		SENS2(9,..

C	n� 31	 Sigma comp. (T�me)   raidisseur JAB     en X=0			SENS2(11,.. !avril2003
C	n� 32	 Sigma comp. (Sx)     raidisseur Semelle en X=L/2		SENS2(12,.. !avril2003

C     n� 33    Sigma comp. (T�me)   raidisseur JAB     en X=0			SENS2(11,.. avec flex.loc.des raid.  !avril2003
C	n� 34	 Sigma comp. (Sx)     raidisseur Semelle en X=L/2		SENS2(12,.. avec flex.loc.des raid.  !avril2003
C     n� 35    Sigma comp. (Sx,T�me) raidisseur JAB    en X=L/2		SENS2(10,.. avec flex.loc.des raid.  !avril2003
C     n� 36    Sigma comp. (Sx,T�me) raidisseur JAS    en X=L/2		SENS2(16,.. avec flex.loc.des raid.  !avril2003

C	n� 41	 Sigma comp. (T�me)   traverse  JAS      en X=0  (en N/m2)	SENS2(13,..
C	n� 42	 Sigma comp. (T�me)   traverse  JAB      en X=0			SENS2(14,..
C	n� 43	 Sigma comp. (Sx)     traverse  Semelle  en X=L/2		SENS2(15,..

C     Flexion locale du raidisseur:
C	n� 51	 Fl�che relative Raid: Extr. Encastr�es : F/L=1*Pl^3/384EI < ratio (1/200, ...
C	n� 52	 Fl�che relative Raid: Appuis simples   : F/L=5*Pl^3/384EI < ratio (1/200, ..
C	n� 54	 Contrainte Max semelle:  (M=Pl^2/10) ; Sig < S(adm)
C	n� 55	 Contrainte Max borde:    (M=Pl^2/10) ; Sig < S(adm)
C	n� 56	 Contrainte Max cisaillement dans �me : 1.73*Tau < T(adm);(Tau � l'axe neutre et M=0)
C	n� 57	 Fl�che Abs. maille: Bords Encastr�s : W=1*PL^4/384EI < Wmax (m)
C	n� 58	 Fl�che Abs. maille: Bords articul�s : W=5*PL^4/384EI < Wmax (m)

C Rem: Dans les donnees, les restrictions ne doivent pas �tre classees par ordre  !!!
C **********************************************************************************

	M=M1TABL(NEL)  ! Nbre de restrictions du cas de charge IS pour le panneau NEL
				
      IF((ITERA.EQ.0).AND.(M.NE.0)) THEN		!13.12.05
	  WRITE(666,'(A,I2)') 'PANNEAU n� ', NEL
	  WRITE(666,'(A)')    '-----------------'
        WRITE(666,284) M,(YPTS(I,NEL),I=1,IPTS(NEL))   ! YPTS (Ordonn�es des pts)
      ENDIF
	
	
c	 WRITE(66,*) ' NTOT=',NTOT	
c 	 WRITE(66,*) ' SENSIBILITES combinees (d�pl.) dans Subr CONTR '
c       WRITE(66,*) ' ********************************************** '
c       DO 181 IPAN=1,NETO
c 	 NBRXI=NVAR(IPAN)
c       DO 181 KK=1,NBRXI
c       K=NXIT(KK,IPAN)
c       WRITE(66,*) ' Variable de conception n�',K,' du panneau n�',IPAN
c       WRITE(66,*) ' --------------------------------------------------'
c       WRITE(66,*) ' SENS4  (SENS1 cumule)'
c       DO 190 JJ=1,9
c  190 WRITE(66,'(5(E14.7,1X))') (SENS4(JJ,I,K,IPAN),I=1,IPTS(NEL))
c       WRITE(66,*) ' SENS2'
c       DO 191 JJ=1,15
c  191 WRITE(66,'(5(E14.7,1X))') (SENS2(JJ,I,K,IPAN),I=1,IPTS(NEL))
c  181 CONTINUE


C     BOUCLE PRINCIPALE SUR LES "M" RESTRICTIONS DU PANNEAU "NEL"
C     ===========================================================
      DO 1 ICONT=IM+1,IM+M

        IC =LCONT(1,ICONT)
        IY1=LCONT(2,ICONT)
        IY2=IPTS2(IY1,NEL)
        IT =IPTS3(IY1,NEL)

        INVV=INV(ICONT)
        CM=CJMAX(ICONT)*INVV

c       WRITE(67,*) 'REST ICONT=',ICONT
c       WRITE(67,*) 'IC NEL,IM,IY1,IY2,IT =',IC,NEL,IM,IY1,IY2,IT 
c       WRITE(67,*) ' CJMAX =',CM, 'INVV=',INVV
c       PAUSE 'C0'

      IF(IC.EQ.1) THEN
C     RESTRICTIONS n� 1 :  D�form�e V � mi-port�e (x=L/2 m)
C     -----------------------------------------------------------
C     D�form�e V(IY,IX) ; IY=IY2 et IX=5 pour X=L/2
        VNOM2='D�pl. V; L/2'
        IX=5
        C=V(IY2,IX)*INVV

C     D�riv�e de V � mi-port�e = SENS4(7,IY,KK,IPAN) et IY = IY1
        J=7
        CALL CSENS(NETO,INVV,NVAR,NXIT,IY1,J,SENS4,DC)

      ELSE IF(IC.EQ.2) THEN
C     RESTRICTIONS n� 2 :  D�form�e U aux extr. (x=0 m)
C     -----------------------------------------------------------
C     D�form�e U(IY,IX); IY=IY2 et IX=1 pour X=0
        VNOM2='D�pl. U; X=0'
        IX=1
        C=U(IY2,IX)*INVV

C     D�riv�e de U aux extr. = SENS4(5,IY,KK,IPAN) et IY = IY1
        J=5
      CALL CSENS(NETO,INVV,NVAR,NXIT,IY1,J,SENS4,DC)

      ELSE IF(IC.EQ.3) THEN
C     RESTRICTIONS n� 3 :  D�form�e W � mi-port�e  (x=L/2 m)
C     -----------------------------------------------------------
C     D�form�e W(IY,IX);  IY=IY2  et IX=5 pour X=L/2
        VNOM2='D�pl. W; L/2'
        IX=5
        C=W(IY2,IX)*INVV

C     D�riv�e de W � mi-port�e = SENS4(1,IY,KK,IPAN) et IY = IY1
        J=1
        CALL CSENS(NETO,INVV,NVAR,NXIT,IY1,J,SENS4,DC)

      ELSE IF(IC.EQ.4) THEN
C     RESTRICTIONS n� 4 : Epaisseur minimale du bord� (Hughes) : dmin - d < 0
C     ------------------------------------------------------------------
        VNOM2='dmin - d < 0'
        C=0.
        CM=0.

        CALL ANNULD (DC,NTOT)

c       WRITE(*,*) 'Restr de Hughes (subr Contr), C et CM= ',C,CM
c       WRITE(*,*) 'VNOM2 =',VNOM2
c       WRITE(*,*) 'NXI2(1,NEL) =',NXI2(1,NEL)
c       WRITE(*,*) 'NXI2(5,NEL) =',NXI2(5,NEL)
c       WRITE(*,*) 'NXI2(9,NEL) =',NXI2(9,NEL)
c       PAUSE'C1a'

c       WRITE(*,*) 'Restr de Hughes (subr Contr), C et CM= ',C,CM
c       WRITE(*,*) 'DC =',(DC(JH),JH=1,9)
c       PAUSE'C1b'

        IF(NXI2(1,NEL).NE.0)THEN
C         Delta = d est une variable de conception (XI=1)
          C=DTPL
          DC(NXI2(1,NEL))=TPLD
        ENDIF
        IF(NXI2(5,NEL).NE.0)THEN
C         EPSA est une variable de conception (XI=5)
          C=DTPL
          DC(NXI2(5,NEL))=TPLA
        ENDIF
        IF(NXI2(9,NEL).NE.0)THEN
C         EPSR est une variable de conception (XI=9)
          C=DTPL
          DC(NXI2(9,NEL))=TPLR
        ENDIF
c       WRITE(*,*) 'Restr de Hughes (subr Contr), C et CM= ',C,CM
c       WRITE(*,*) 'DC =',(DC(JH),JH=1,9)
c       PAUSE'C2'

      ELSE IF((IC.GE.5).AND.(IC.LE.7)) THEN
C     RESTRICTIONS n� 5,6,7 :  D�form�e RELATIVE de W   (x=L/2 m)
C     ---------------------------------------------------------------
C     D�form�e W(IY,IX);  IY=IY2  et IX=5 pour X=L/2
        VNOM2='Depl W relat'
        IX=5
        IF(IC.EQ.5) C=W(IY2,IX) - W(1,IX)
        IF(IC.EQ.6) C=W(IY2,IX) - W(31,IX)
        IF(IC.EQ.7) C=W(IY2,IX) - (W(1,IX)+W(31,IX))/2.
        C=C*INVV

C       D�riv�e de W � mi-port�e = SENS4(1,IY,KK,IPAN) et IY = IY1
        J=1
        CALL CSENS3(NETO,INVV,NVAR,NXIT,IY1,J,IPTS(NEL),IC,SENS4,DC)

      ELSE IF(IC.EQ.10) THEN
C	RESTRICTIONS n� 10: PLASTIFICATION DU BORDAGE en Z=0 (X=L/2)		!r&d13
C	------------------------------------------------------------
	  VNOM2='El.bord.;L/2'

	  CALL ANNULD(DC,NTOT)

	  C=SIGX
	  
	  L1=0
        DO IPAN=1,NETO
          NBRXI=NVAR(IPAN)
          DO K=1,NBRXI
            KK=NXIT(K,IPAN)
            L=L1+K
            DC(L)=dSIGX(KK,IPAN)
		ENDDO
          L1=L1+NBRXI
	  ENDDO

	ELSE IF(IC.EQ.11) THEN      
C     RESTRICTIONS n� 11: CONTRAINTE DE COMP. DANS LE BORDAGE en Z=0 (X=L/2)
C     ----------------------------------------------------------------------
C       Contrainte SBORD(IY,IX);    IY=IY2  et IX=5 pour X=L/2
        VNOM2='Sbor Z=0;L/2'
        IX=5
        C=SBORD(IY2,IX)*INVV

C       D�riv�e de SBORD � mi-port�e = SENS2(1,IY,KK,IPAN) et IY = IY1
        J=1
        CALL CSENS2(NETO,INVV,NVAR,NXIT,IY1,J,SENS2,DC)
	CONTINUE

      ELSE IF(IC.EQ.12) THEN    !avril2003 (pour tout le point 12!!!!!!!!!!!!!!!!!!!!!!!!!!!!)
C     RESTRICTIONS n� 12: CONTRAINTE DE COMP. DANS LE BORDAGE en Z=0 (X=L/2) 
C     ----------------------------------------------------------------------
C     (AVEC FLEXION LOCALE DES RAIDISSEURS)
C       Contrainte SBORD(IY,IX);    IY=IY2  et IX=5 pour X=L/2
        VNOM2='Sbord+STIFF '
	  IX=5
c        C=SBORD(IY2,IX)*INVV
        C = EFFCOMB(4845+4*51+IY2)*INVV

      L1=0

	DO IPAN=1,NETO
	  NBRXI=NVAR(IPAN)
	  IJ = 4*51 + IPTS2(IY1,IPAN)
c	  IF (IPAN.EQ.NEL) THEN			!!!aout04
	    DO K=1,NBRXI
	      KK=NXIT(K,IPAN)
	      L = L1 + K
		  IF (IPAN.EQ.NEL) THEN		!!!aout04
		    DC(L) = SENS3(1,IY1,KK,IPAN)  ! = d(SIGMAX)/dx
	        IF (EFF(IJ+4335)*SIG1.NE.0) THEN
	          DEN = EFF(IJ+4335)*SIG1
	          DC(L) = DC(L) + dSIG1(KK)*DEN/DABS(DEN)
		    ELSE
		      DC(L) = DC(L) + dSIG1(KK) 
	        ENDIF
	        DEN = EFFCOMB(IJ+4845)
	        IF(DABS(DEN).LE.(+1.E-10)) DEN = 1.E-10
	        DC(L)=0.5*INVV*((2.*EFFCOMB(IJ+4335)-EFFCOMB(IJ+4080)) 		! d(SIGMA COMP)
     *            *DC(L) + (2.*EFFCOMB(IJ+4080) - EFFCOMB(IJ+4335))
     *                 *SENS3(2,IY1,KK,IPAN)  )/DEN
	      ELSE									!!!aout04
			DC(L) = SENS2(1,IY1,KK,IPAN)*INVV	!!!aout04
	      ENDIF									!!!aout04
	    ENDDO
c	    GOTO 300		!!!aout04
c	  ENDIF				!!!aout04
	  L1 = L1 + NBRXI
	ENDDO
  300	CONTINUE
     	
	ELSE IF(IC.EQ.13) THEN    !avril2003 (pour tout le point 13!!!!!!!!!!!!!!!!!!!!!!!!!!!!)
C     RESTRICTIONS n� 13: CONTRAINTE DE COMP. DANS LE BORDAGE en Z=0 (X=L/2) 
C     ----------------------------------------------------------------------
C     (AVEC FLEXION LOCALE DES RAIDISSEURS et CONTRAINTE PLAQUE)
C       Contrainte SBORD(IY,IX);    IY=IY2  et IX=5 pour X=L/2
        VNOM2='S+STIFF+PLTB'
	  IX=5
c        C=SBORD(IY2,IX)*INVV
c        C = EFFCOMB(4845+4*51+IY2)*INVV
        C = SigPlaque(IY1)*INVV

      L1=0

	DO IPAN=1,NETO
	  NBRXI=NVAR(IPAN)
	  IF (IPAN.EQ.NEL) THEN
	    DO K=1,NBRXI
	      KK=NXIT(K,IPAN)
	      L = L1 + K
		  DC(L) = DSigPlaque(IY1,KK)*INVV
	    ENDDO
	    GOTO 301
	  ENDIF
	  L1 = L1 + NBRXI
	ENDDO
  301 CONTINUE
		
      ELSE IF(IC.EQ.14) THEN                                                     !avril2003
C	RESTRICTIONS n� 14: FLAMBEMENT DU BORDAGE en Z=0 (X=L/2)				   !r&d13
C	--------------------------------------------------------
	  VNOM2='Fl.bord.;L/2'
c	  CM=1.

c	  CALL ANNULD(DC,NTOT)

	  C=DSIGPL
	  
	  L1=0
        DO IPAN=1,NETO
          NBRXI=NVAR(IPAN)
          DO K=1,NBRXI
            KK=NXIT(K,IPAN)
            L=L1+K
            DC(L)=dDSIGPL(KK,IPAN)
		ENDDO
          L1=L1+NBRXI
	  ENDDO

	ELSE IF(IC.EQ.16) THEN      
C     RESTRICTIONS n� 16: CONTRAINTE DE COMP. DANS LE BORDAGE en Z=0 (X=0)       !avril2003
C     --------------------------------------------------------------------------
C       Contrainte SBORD(IY,IX);    IY=IY2  et IX=1 pour X=0
        VNOM2='Tbor Z=0 X=0'
	  IX=1
        C=SBORD(IY2,IX)*INVV

C       D�riv�e de SBORD � mi-port�e = SENS2(2,IY,KK,IPAN) et IY = IY1	Pour T bord J=2 
        J=2
        CALL CSENS2(NETO,INVV,NVAR,NXIT,IY1,J,SENS2,DC)

	ELSE IF(IC.EQ.17) THEN                                                     !avril2003
C     RESTRICTIONS n� 17: CONTRAINTE DE COMP. DANS LE BORDAGE en Z=+d/2 (X=L/2)  !avril2003
C     --------------------------------------------------------------------------
C       Contrainte SBORD1(IY,IX);    IY=IY2  et IX=5 pour X=L/2
        VNOM2='Sbord Z=+d/2'
	  IX=5
        C=SBORD1(IY2,IX)*INVV

C       D�riv�e de SBORD � mi-port�e = SENS2(4,IY,KK,IPAN) et IY = IY1	Pour S bord en Z=d/2
        J=4                                                             !   SENS2(4,.....
        CALL CSENS2(NETO,INVV,NVAR,NXIT,IY1,J,SENS2,DC)

	ELSE IF(IC.EQ.18) THEN                                                      !avril2003
C     RESTRICTIONS n� 18: CONTRAINTE DE COMP. DANS LE BORDAGE en Z=-d/2 (X=L/2)   !avril2003
C     --------------------------------------------------------------------------
C       Contrainte SBORD2(IY,IX);    IY=IY2  et IX=5 pour X=L/2
        VNOM2='Sbord Z=-d/2'
	  IX=5
        C=SBORD2(IY2,IX)*INVV

C       D�riv�e de SBORD � mi-port�e = SENS2(3,IY,KK,IPAN) et IY = IY1	Pour S bord en Z=-d/2 
        J=3                                                             ! SENS2(3,.....
        CALL CSENS2(NETO,INVV,NVAR,NXIT,IY1,J,SENS2,DC)

	ELSE IF(IC.EQ.15) THEN
C     RESISTANCE ULTIME of longitudinally compresses stiffened panels
c     - cfr article de PAIK dans Ship Research).
C	- restriction n� 15  Sx < S(Ult. Strength)  ; Faxial moyen  en X=L/2 
C     ---------------------------------------------------------------------
	
c       Cas d'une coque cylindrique	   
        IF(PHIL.GE.1.) THEN     ! COQUE/PLAQUE
	    WRITE(666,196)
	    WRITE(9  ,196)
		WRITE(29 ,196)															!bug
	  ENDIF										

        VNOM2='Sx/S(ULS)<Sc'
        CM=DABS(CM)         ! borne; C(x) < CM (=Cmax)

	  CALL ANNULD(DC,9*NETO)

C     SMX    = valeur  du SX axial moyen (calcule dans Subr RESUL)
C     d(SMX)/dx = SENS3(21,IPTmax,9,NETO)
C     SPAIK  = r�sistance ultime selon la formule de J. PAIK
C     DSPAIK = d�riv�e de la r�sistance ultime SPAIK par rapport aux XI

        CALL PAIK(IPRINT,NETO,NEL,E,ETA,SIGY,WIDTH,LAMB,Q,DELTA,
     *            PHIL,EPSA,HXR,DXR,WXR,TXR,EPSR,ENTR,KSR,
     *            NVAR,NXIT,
     *            DSPAIK,SPAIK,OMT)

        SMOYEN=SMX(IY1)
        C=-SMOYEN/SPAIK

c        IF(ITERA.NE.1)  THEN
c          WRITE(666,285) C,CM,SMOYEN,SPAIK
c 285      FORMAT(5x,' (-Sig.Mean/Sig Ult.)=',F7.4,' ?<<? ',F6.4,
c     *         ', Sx Mean=', E11.4,', Sigma Ult.=',E11.4,' N/m2')
c	   ENDIF

c       WRITE(666,*)' ** Ds CONTR    IY =',IY1
c       WRITE(666,*)'   SMX= ',(SMX(IV),IV=1,4)
c       WRITE(666,*)'   SMOYEN= ',SMOYEN
c       WRITE(666,*)'   DSPAIK= ',DSPAIK

C     D�riv�e de "-SMOYEN/SPAIK" � mi-port�e
C     .......................................
C      Avec SPAIK > 0  et NX < 0 en compression.
C      Si NX>0 la restriction n'a plus de sens et est automatiquement v�rifi�e.

        CALL CSENS4(NETO,NVAR,NXIT,IY1,NEL,DSPAIK,SENS3,
     *              SMOYEN,SPAIK,DC)

c       WRITE(666,*)  'LES DERIVEES DC (Restrictions 15, PAIK)'
c       WRITE(666,100) ICONT,IC,C,(DC(I),I=1,NTOT)
c       WRITE(666,*)

      ELSE IF(IC.EQ.21) THEN
C     RESTRICTIONS n� 21: CONTRAINTE DE COMP. CADRE JAS aux appuis (X=0)
C     ------------------------------------------------------------------
C       Contrainte SAJAS(IY,IX);    IY=IY2  et IX=1 pour X=0
        VNOM2='Tcad JAS x=0'
        IX=1
        C=SAJAS(IY2,IX)*INVV

C       D�riv�e de SBORD � mi-port�e = SENS2(5,IY,KK,IPAN) et IY = IY1
        J=5
        CALL CSENS2(NETO,INVV,NVAR,NXIT,IY1,J,SENS2,DC)

      ELSE IF(IC.EQ.22) THEN
C     RESTRICTIONS n� 22: CONTRAINTE DE COMP. CADRE JAS � mi-port�e (X=L/2)
C     --------------------------------------------------------------------------
C       Contrainte SAJAS(IY,IX);    IY=IY2  et IX=5 pour X=0
        VNOM2='Scad JAS L/2'
	  IX=5
        C=SAJAS(IY2,IX)*INVV

C       D�riv�e de SBORD � mi-port�e = SENS2(6,IY,KK,IPAN) et IY = IY1	Pour Sig Cadre JAS
        J=6                 ! SENS2(6,.....
        CALL CSENS2(NETO,INVV,NVAR,NXIT,IY1,J,SENS2,DC)

	ELSE IF(IC.EQ.23) THEN
C     RESTRICTIONS n� 23: CONTRAINTE DE COMP. CADRE JAB aux appuis (X=0)
C     --------------------------------------------------------------------------
C       Contrainte SAJAB(IY,IX);    IY=IY2  et IX=1 pour X=0
        VNOM2='Tcad JAB x=0'
	  IX=1
        C=SAJAB(IY2,IX)*INVV

C       D�riv�e de SBORD � mi-port�e = SENS2(7,IY,KK,IPAN) et IY = IY1	Pour Sig Cadre JAB
        J=7  ! SENS2(7,.....
        CALL CSENS2(NETO,INVV,NVAR,NXIT,IY1,J,SENS2,DC)

	ELSE IF(IC.EQ.24) THEN
C     RESTRICTIONS n� 24: CONTRAINTE DE COMP. CADRE JAB � mi-port�e (X=L/2)
C     --------------------------------------------------------------------------
C       Contrainte SAJAB(IY,IX);    IY=IY2  et IX=5 pour X=0
        VNOM2='Scad JAB L/2'
	  IX=5
        C=SAJAB(IY2,IX)*INVV

C       D�riv�e de SBORD � mi-port�e = SENS2(8,IY,KK,IPAN) et IY = IY1	Pour Sig Cadre JAB
        J=8   ! SENS2(8,.....
        CALL CSENS2(NETO,INVV,NVAR,NXIT,IY1,J,SENS2,DC)

	ELSE IF(IC.EQ.25) THEN
C     RESTRICTIONS n� 25: CONTRAINTE DE COMP. CADRE SEM  (X=L/2)
C     --------------------------------------------------------------------------
C       Contrainte SASEM(IY,IX);    IY=IY2  et IX=5 pour X=L/2
        VNOM2='Scad Sem L/2'
	  IX=5
        C=SASEM(IY2,IX)*INVV

C       D�riv�e de SBORD � mi-port�e = SENS2(9,IY,KK,IPAN) et IY = IY1	Pour Sig Cadre SEM
        J=9 ! SENS2(9,.....
        CALL CSENS2(NETO,INVV,NVAR,NXIT,IY1,J,SENS2,DC)

	ELSE IF(IC.EQ.31) THEN                                               !avril2003
C     RESTRICTIONS n� 31: CONTRAINTE DE COMP. RAID JAB aux appuis (X=0)    !avril2003
C     -----------------------------------------------------------------
C       Contrainte SRJAB(IY,IX);    IY=IY2  et IX=1 pour X=0
        VNOM2='Trai JAB x=0'
	  IX=1
        C=SRJAB(IY2,IX)*INVV

C       D�riv�e de SBORD � mi-port�e = SENS2(11,IY,KK,IPAN) et IY = IY1	Pour Sig RAID JAB
        J=11  ! SENS2(11,.....
        CALL CSENS2(NETO,INVV,NVAR,NXIT,IY1,J,SENS2,DC)

	ELSE IF(IC.EQ.32) THEN                                         !avril2003
C     RESTRICTIONS n� 32: CONTRAINTE DE COMP. RAID SEM  (X=L/2)      !avril2003
C     ---------------------------------------------------------
C       Contrainte SRSEM(IY,IX);    IY=IY2  et IX=5 pour X=L/2
        VNOM2='Srai Sem L/2'
	  IX=5
        C=SRSEM(IY2,IX)*INVV

C       D�riv�e de SBORD � mi-port�e = SENS2(12,IY,KK,IPAN) et IY = IY1	Pour Sig RAID SEM
        J=12  !  SENS2(12,.....
        CALL CSENS2(NETO,INVV,NVAR,NXIT,IY1,J,SENS2,DC)

      ELSE IF(IC.EQ.33) THEN            !avril2003 (pour tout le point 33!!!!!!!!!!!!!!!!!!!!!!!!!!!!)
C     RESTRICTIONS n� 33: CONTRAINTE DE COMP. RAID JAB aux appuis (X=0) AVEC FLEXION LOCALE DES RAIDISSEURS
C     -----------------------------------------------------------------------------------------------------
C       Contrainte SRJAB(IY,IX);    IY=IY2  et IX=1 pour X=0
        VNOM2='TJABx=0+STIFF'
	  IX=1
c        C=SRJAB(IY2,IX)*INVV
	  C = EFFCOMB(9180+IY2)*INVV

      L1=0
	DO IPAN=1,NETO
	  NBRXI=NVAR(IPAN)
	  IJ = IPTS2(IY1,IPAN)
c	  IF (IPAN.EQ.NEL) THEN		!!!aout04
	    DO K=1,NBRXI
	      KK=NXIT(K,IPAN)
	      L = L1 + K
		  IF (IPAN.EQ.NEL) THEN	!!!aout04  
	        DEN = EFFCOMB(IJ+8415)                                     ! pour Tau JAB
	        IF(DABS(DEN).LE.(+1.E-10)) DEN = 1.E-10
              DC(L) = (SENS2(11,IY1,KK,IPAN) 
     *                      + 1.732051*dTau(KK)*DEN/DABS(DEN))*INVV
		  ELSE									!!!aout04
	        DC(L) = SENS2(11,IY1,KK,IPAN)*INVV	!!!aout04
	      ENDIF									!!!aout04
	    ENDDO
c	    GOTO 302	!!!aout04
c	  ENDIF			!!!aout04
	  L1 = L1 + NBRXI
	ENDDO
  302	CONTINUE

      ELSE IF(IC.EQ.34) THEN               !avril2003 (pour tout le point 34!!!!!!!!!!!!!!!!!!!!!!!!!!!!)
C     RESTRICTIONS n� 34: CONTRAINTE DE COMP. RAID SEM  (X=L/2) AVEC FLEXION LOCALE DES RAIDISSEURS
C     ---------------------------------------------------------------------------------------------
C       Contrainte SRSEM(IY,IX);    IY=IY2  et IX=5 pour X=L/2
        VNOM2='SemL/2+STIFF'
	  IX=5
C	  C=SRSEM(IY2,IX)*INVV
	  C = EFFCOMB(9435+4*51+IY2)*INVV

      L1=0
	DO IPAN=1,NETO
	  NBRXI=NVAR(IPAN)
	  IJ=4*51 + IPTS2(IY1,IPAN)
c	  IF (IPAN.EQ.NEL) THEN		!!!aout04
	    DO K=1,NBRXI
	      KK=NXIT(K,IPAN)
	      L = L1 + K
		  IF (IPAN.EQ.NEL) THEN		!!!aout04
	        DEN = EFF(IJ+7905)  ! Sig X sem raid (de RESUL)
	        IF(DABS(DEN).LE.(+1.E-10)) DEN = 1.E-10
	        DC(L) = SENS2(12,IY1,KK,IPAN)*DABS(DEN)/DEN
	        IF (EFF(IJ+7905)*SIG2.NE.0) THEN
 	          DEN = EFF(IJ+7905)*SIG2
	          DC(L) = DC(L) + dSIG2(KK)*DEN/DABS(DEN) ! SIGMAX SEM
	        ELSE
                DC(L) = DC(L) + dSIG2(KK)
	        ENDIF
	        DEN = EFFCOMB(IJ+7905)
              IF(DABS(DEN).LE.(+1.E-10)) DEN = 1.E-10
              DC(L) = DC(L)*DEN/DABS(DEN)*INVV
	      ELSE									!!!aout04
	        DC(L)= SENS2(12,IY1,KK,IPAN)*INVV	!!!aout04
	      ENDIF									!!!aout04
	    ENDDO
c	    GOTO 303	!!!aout04
c	  ENDIF			!!!aout04
        L1 = L1 + NBRXI
	ENDDO
  303	CONTINUE

      ELSE IF(IC.EQ.35) THEN               !avril2003 (pour tout le point 35!!!!!!!!!!!!!!!!!!!!!!!!!!!!)
C     RESTRICTIONS n� 35: CONTRAINTE DE COMP. RAID JAB  (X=L/2) AVEC FLEXION LOCALE DES RAIDISSEURS
C     ---------------------------------------------------------------------------------------------
C       Contrainte SRJAB(IY,IX);    IY=IY2  et IX=5 pour X=L/2
        VNOM2='JABL/2+STIFF'
	  IX=5
c	  C=SRJAB(IY2,IX)*INVV
        C = EFFCOMB(9180+4*51+IY2)*INVV

      L1=0
	DO IPAN=1,NETO
	  NBRXI=NVAR(IPAN)
	  IJ=4*51 + IPTS2(IY1,IPAN)
c	  IF (IPAN.EQ.NEL) THEN		!!!aout04
	    DO K=1,NBRXI
	      KK=NXIT(K,IPAN)
	      L = L1 + K
		  IF (IPAN.EQ.NEL) THEN	!!!aout04
			dSIG3(KK) = dSIG3(KK)*10./12.             ! pour avoir PL�/12 (et non PL�/10)
			DEN=EFF(IJ+7650) ! = Sx jab raid
			IF(DABS(DEN).LE.(+1.E-10)) DEN=1.E-10
			DC(L) = SENS2(10,IY1,KK,IPAN)/DEN*DABS(DEN) ! = d(Sx jab raid)
			IF (EFF(IJ+7650)*SIG3.NE.0) THEN
		      DEN = EFF(IJ+7650)*SIG3
			  DC(L) = DC(L) + dSIG3(KK)*DEN/DABS(DEN)
			ELSE
			  DC(L) = DC(L) + dSIG3(KK) 
			ENDIF ! ici, DC(L) = d(Sx(RESUL)+Sx(STIFF) � jab raid), c'est donc d(EFFCOMB(IJ+7650))
			DC(L) = (EFFCOMB(IJ+7650)*DC(L)
     *                    + 3.*TAU*dTAU(KK))/C*INVV ! = d(SIGMA COMP JAB) 
		  ELSE									!!!aout04
			DC(L) = SENS2(10,IY1,KK,IPAN)*INVV	!!!aout04
	      ENDIF									!!!aout04
	    ENDDO
c	    GOTO 304		!!!aout04
c	  ENDIF				!!!aout04
	  L1 = L1 + NBRXI
	ENDDO
  304	CONTINUE

      ELSE IF(IC.EQ.36) THEN               !avril2003 (pour tout le point 36!!!!!!!!!!!!!!!!!!!!!!!!!!!!)
C     RESTRICTIONS n� 36: CONTRAINTE DE COMP. RAID JAS  (X=L/2) AVEC FLEXION LOCALE DES RAIDISSEURS
C     ---------------------------------------------------------------------------------------------
C       Contrainte SRJAS(IY,IX);    IY=IY2  et IX=5 pour X=L/2
        VNOM2='JASL/2+STIFF'
	  IX=5
c	  C=SRJAS(IY2,IX)*INVV
        C = EFFCOMB(8925+4*51+IY2)*INVV

      L1=0

	DO IPAN=1,NETO
	  NBRXI=NVAR(IPAN)
	  IJ=4*51 + IPTS2(IY1,IPAN)
c	  IF (IPAN.EQ.NEL) THEN		!!!aout04
	    DO K=1,NBRXI
	      KK=NXIT(K,IPAN)
		  L = L1 + K
		  IF (IPAN.EQ.NEL) THEN	!!!aout04
	        DEN=EFF(IJ+7395) ! = Sx jas raid
		    IF(DABS(DEN).LE.(+1.E-10)) DEN=1.E-10
              DC(L)= SENS2(16,IY1,KK,IPAN)/DEN*DABS(DEN)      ! d(Sx JAS raid)
		    IF (EFF(IJ+7395)*SIG4.NE.0) THEN
		      DEN = EFF(IJ+7395)*SIG4
		      DC(L) = DC(L) + dSIG4(KK)*DEN/DABS(DEN)
		    ELSE
		      DC(L) = DC(L) + dSIG4(KK)
		    ENDIF
	        DEN=EFFCOMB(IJ+7395) ! = Sx jas raid
	        IF(DABS(DEN).LE.(+1.E-10)) DEN=1.E-10
		    DC(L) = DC(L)*DEN/DABS(DEN)*INVV            ! d(SIGMA COMP JAS)
	      ELSE		!!!aout04
	        DC(L)= SENS2(16,IY1,KK,IPAN)*INVV	!!!aout04
		  ENDIF		!!!aout04
	    ENDDO
c	    GOTO 305	!!!aout04
c	  ENDIF			!!!aout04
	  L1 = L1 + NBRXI
	ENDDO
  305	CONTINUE

      ELSE IF(IC.EQ.37) THEN                                                     
C	RESTRICTIONS n� 37: FLAMBEMENT DU RAIDISSEUR (X=L/2)				!r&d13
C	----------------------------------------------------
	  VNOM2='Fl.raid.;L/2'
c	  CM=1.

c	  CALL ANNULD(DC,NTOT)

	  C=DSIGSTIF
	  
	  L1=0
        DO IPAN=1,NETO
          NBRXI=NVAR(IPAN)
          DO K=1,NBRXI
            KK=NXIT(K,IPAN)
            L=L1+K
            DC(L)=dDSIGSTIF(KK,IPAN)
		ENDDO
          L1=L1+NBRXI
	  ENDDO

	ELSE IF(IC.EQ.41) THEN
      IF(IT.EQ.0) GOTO 10
C     RESTRICTIONS n� 41: CONTRAINTE DE COMP. TRAV JAS aux appuis (X=0)
C     --------------------------------------------------------------------------
C       Contrainte SRJAS(IY,IX);    IT= N� de trav.  et IX=1 pour X=0
        VNOM2='Ttra JAS x=0'
	  IX=1
        IF(IT.GT.0) C=STJAS(IT,IX)*INVV

C       D�riv�e de SBORD � mi-port�e = SENS2(13,IY,KK,IPAN) et IY = IY1	Pour Sig TRAV JAS
        J=13 ! SENS2(13,.....
        CALL CSENS2(NETO,INVV,NVAR,NXIT,IY1,J,SENS2,DC)

	ELSE IF(IC.EQ.42) THEN
      IF(IT.EQ.0) GOTO 10
C     RESTRICTIONS n� 42: CONTRAINTE DE COMP. TRAV JAB aux appuis (X=0)
C     --------------------------------------------------------------------------
C       Contrainte SRJAB(IY,IX);    IT= N� de trav.  et IX=1 pour X=0
        VNOM2='Ttra JAB x=0'
	  IX=1
        IF(IT.GT.0) C=STJAB(IT,IX)*INVV

C       D�riv�e de SBORD � mi-port�e = SENS2(14,IY,KK,IPAN) et IY = IY1	Pour Sig TRAV JAB
        J=14 ! SENS2(14,.....
        CALL CSENS2(NETO,INVV,NVAR,NXIT,IY1,J,SENS2,DC)

	ELSE IF(IC.EQ.43) THEN
      IF(IT.EQ.0) GOTO 10
C     RESTRICTIONS n� 43: CONTRAINTE DE COMP. TRAV SEM  (X=L/2)
C     --------------------------------------------------------------------------
C       Contrainte SRSEM(IY,IX);    IT= N� de trav.  et IX=5 pour X=L/2
        VNOM2='Stra Sem L/2'
	  IX=5
        IF(IT.GT.0) C=STSEM(IT,IX)*INVV

C       D�riv�e de SBORD � mi-port�e = SENS2(15,IY,KK,IPAN) et IY = IY1	Pour Sig TRAV SEM
        J=15  ! SENS2(15,.....
        CALL CSENS2(NETO,INVV,NVAR,NXIT,IY1,J,SENS2,DC)

	ELSE IF(IC.EQ.51) THEN
c     RESTRICTIONS n� 51: Fleche Rel. Raid (Extr. encastrees): F/L=1*Pl^3/384EI < ratio (1/200, ...
C     ----------------------------------------------------------------------------------------------
        VNOM2='Raid Loc FL1'
        C=FLR/5.
	  CALL ANNULD(DC,9*NETO)
        L1=0
        DO IPAN=1,NETO
          NBRXI=NVAR(IPAN)
          IF(IPAN.EQ.NEL) THEN
            DO K=1,NBRXI
               KK=NXIT(K,NEL)
               L=L1+K
               DC(L)=DFLR(KK) /5.  
            ENDDO
	      GOTO 308
	    ENDIF
          L1=L1+NBRXI
	  ENDDO
  308   CONTINUE

	ELSE IF(IC.EQ.52) THEN
c     RESTRICTIONS n� 52:  Fleche Rel. Raid (Extr. appuy�es): F/L=5*Pl^3/384EI < ratio (1/200, ...
C     ----------------------------------------------------------------------------------------------
        VNOM2='Raid Loc FL5'
        C=FLR
	  CALL ANNULD(DC,9*NETO)
        L1=0
        DO IPAN=1,NETO
          NBRXI=NVAR(IPAN)
          IF(IPAN.EQ.NEL) THEN
            DO K=1,NBRXI
               KK=NXIT(K,NEL)
               L=L1+K
               DC(L)=DFLR(KK)   
            ENDDO
	      GOTO 309
	    ENDIF
          L1=L1+NBRXI
	  ENDDO
  309   CONTINUE

	ELSE IF(IC.EQ.54) THEN
c     RESTRICTIONS n� 54: Flexion locale raidisseur: Contr dans semelle
C     ------------------------------------------------------------------
        VNOM2='Raid Loc Sem'   ! (M=Pl^2/10) ; Sig < S(adm)
	  INVV=1
	  IF(SIG2.LE.0.0) INVV=-1
        C=SIG2*INVV
	  CALL ANNULD(DC,9*NETO)
        L1=0
        DO IPAN=1,NETO
          NBRXI=NVAR(IPAN)
          IF(IPAN.EQ.NEL) THEN
            DO K=1,NBRXI
               KK=NXIT(K,NEL)
               L=L1+K
               DC(L)=DSIG2(KK) * INVV  
            ENDDO
	      GOTO 310
	    ENDIF
          L1=L1+NBRXI
	  ENDDO
  310   CONTINUE
        INVV=1

	ELSE IF(IC.EQ.55) THEN
c     RESTRICTIONS n� 55: Flexion locale raidisseur: Contr dans bord�
C     --------------------------------------------------------------------
        VNOM2='Raid Loc Bor'   ! (M=Pl^2/10) ; Sig < S(adm)
	  INVV=1
	  IF(SIG1.LE.0.0) INVV=-1
        C=SIG1*INVV
	  CALL ANNULD(DC,9*NETO)
        L1=0
        DO IPAN=1,NETO
          NBRXI=NVAR(IPAN)
          IF(IPAN.EQ.NEL) THEN
            DO K=1,NBRXI
               KK=NXIT(K,NEL)
               L=L1+K
               DC(L)=DSIG1(KK) * INVV    
            ENDDO
	      GOTO 311
	    ENDIF
          L1=L1+NBRXI
	  ENDDO
  311   CONTINUE
        INVV=1

	ELSE IF(IC.EQ.56) THEN
c     RESTRICTIONS n� 56:  Contrainte Max cisaillement ame (flexion local Raid) 
C     --------------------------------------------------------------------------
        VNOM2='Raid Loc Tau' ! 1.73*Tau < T(adm) ;(Tau a l'axe neutre et M=0)
        C=TAU
	  CALL ANNULD(DC,9*NETO)
        L1=0
        DO IPAN=1,NETO
          NBRXI=NVAR(IPAN)
          IF(IPAN.EQ.NEL) THEN
            DO K=1,NBRXI
               KK=NXIT(K,NEL)
               L=L1+K
               DC(L)=Dtau(KK)   
            ENDDO
	      GOTO 312
	    ENDIF
          L1=L1+NBRXI
	  ENDDO
  312   CONTINUE


	ELSE IF(IC.EQ.57) THEN
c     RESTRICTIONS n� 57: Fleche Abs. Maille (Extr. encastrees): 
c               F/L=1*Pl^3/384EI < ratio (1/200, ...
C     -----------------------------------------------------------------------------
        VNOM2='W Maille UNI'
        C=Wpl1
	  CALL ANNULD(DC,9*NETO)
        L1=0
        DO IPAN=1,NETO
          NBRXI=NVAR(IPAN)
          IF(IPAN.EQ.NEL) THEN
            DO K=1,NBRXI
               KK=NXIT(K,NEL)
               L=L1+K
               DC(L)=dWpl1(KK)   
            ENDDO
	      GOTO 313
	    ENDIF
          L1=L1+NBRXI
	  ENDDO
  313   CONTINUE


	ELSE IF(IC.EQ.58) THEN
c     RESTRICTIONS n� 58: Fleche Abs. Maille (Appuis simples):
c                   F/L=1*Pl^3/384EI < ratio (1/200, ...
C     ----------------------------------------------------------------
        VNOM2='W Maille LOC'
        C=Wpl2
	  CALL ANNULD(DC,9*NETO)
        L1=0
        DO IPAN=1,NETO
          NBRXI=NVAR(IPAN)
          IF(IPAN.EQ.NEL) THEN
            DO K=1,NBRXI
               KK=NXIT(K,NEL)
               L=L1+K
               DC(L)=dWpl2(KK)   
            ENDDO
	      GOTO 314
	    ENDIF
          L1=L1+NBRXI
	  ENDDO
  314   CONTINUE


C     -----------------------------------------------------------
C     -----------------------------------------------------------

	ELSE
        WRITE(*,*)' La restriction n�',IC,' n''existe pas !!!'
        WRITE(*,*)' Dans panneau n�',NEL,' !!!'
        WRITE(*,*)' Veuillez svp corriger les donn�es.'

        WRITE(29,*)' La restriction n�',IC,' n''existe pas !!!'					!bug
        WRITE(29,*)' Dans panneau n�',NEL,' !!!'									!bug
        WRITE(29,*)' Veuillez svp corriger les donn�es.'							!bug

	  PAUSE 'STOP'
	  STOP
	END IF

C ***********************************************************************   
C     SAUVETAGE SUR DISQUE,  File 204 
C     -----------------------
	WRITE(303) C,CM,VNOM2			!extension neto
	WRITE(303) (DC(I),I=1,NTOT)		!extension neto

c      WRITE (66,*) 'DC dans Contr apr�s write 204: IC=',IC
c      WRITE (66,*) 'C,CM,VNOM2 =', C,'<',CM,VNOM2
c      WRITE (66,'(9(E10.3,1X))') (DC(I),I=1,NTOT)

C     Impressions
C     ------------
C     IR Compteur  global de toutes les restrictions (pour la structure compl�te)
      IR=ICONT+IMG+M2TOT     !IS = load case

c     IF(IPRINT.GE.1) WRITE(*,100) IR,IC,C,(DC(I),I=1,NTOT)
      IF(ITERA.EQ.0) THEN				!13.12.05
	IF (IMPR.GE.-2) THEN					!15.10.05
        IF(INVV.EQ.-1) THEN
          WRITE(666,427) IR,CJMAX(ICONT),IC,VNOM2,IY1,IY2
        ELSE
          WRITE(666,428) IR,CJMAX(ICONT),IC,VNOM2,IY1,IY2
        ENDIF
c       WRITE(666,100) IR,IC,C,(DC(I),I=1,NTOT)  !  mode DEBUG
	ENDIF								!15.10.05
      ENDIF

c     IF(IPRINT.GE.1) PAUSE 'pause'

C **********************************************************************

C     Fin de la boucle principale sur le Nbre de restrictions
   1  CONTINUE

      IM=IM+M   ! increment du compteur des restr structurelles

      RETURN

  10  WRITE(*,*) 'Dans Subr. Contr : IT=0 . IMPOSSIBLE -> ERROR'
      WRITE(*,*) 'Positions de calcul des restrictions relatives'
      WRITE(*,*) 'aux traverses sont erron�s (subr. CONTR)'
      WRITE(666,*) 'Dans Subr. Contr : IT=0 . IMPOSSIBLE -> ERROR'
      WRITE(666,*) 'Positions de calcul des restrictions relatives'
      WRITE(666,*) 'aux traverses sont erron�s (subr. CONTR)'

      WRITE(29,*) 'Dans Subr. Contr : IT=0 . IMPOSSIBLE -> ERROR'						!bug
      WRITE(29,*) 'Positions de calcul des restrictions relatives'					!bug
      WRITE(29,*) 'aux traverses sont erron�s (subr. CONTR)'							!bug

	PAUSE 'STOP'
	STOP
	
C     ----------------------------------------------------------------- 

  100 FORMAT(' Rest. N�',I4,' de type ',I2,'; C=',E14.7/'DC=',
     *        (T4,6(E11.4,1X)))
  196 FORMAT(/'!!! Le calcul de Sig Ultime (PAIK)',
     *       ' n''est pas pr�vu pour une coque cylindrique !!!'/
     *       ' mais il est acceptable en 1�re approximation.'/)
  284 FORMAT('Nombre de restrictions structurelles locales = ',I3/
     *       'aux pts YPT=',5(E11.5,1X),' (deg)')
  427 FORMAT('C(',I4,') > ',E11.5,' de type n�',I2,' (',A12,')',
     *       ' au pt YPT n�',I2,' et au pt de calcul n�',I2)
  428 FORMAT('C(',I4,') < ',E11.5,' de type n�',I2,' (',A12,')',
     *       ' au pt YPT n�',I2,' et au pt de calcul n�',I2)
      END

************************************************************************
C **********************************************************************

      SUBROUTINE CSENS(NETO,INVV,NVAR,NXIT,IY,J,SENS4,DC)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION NVAR(NETO),NXIT(9,NETO),SENS4(9,IPTmax,9,NETO),
     *          DC(9*NETO)

      COMMON/DIM1/Nmax,NEmax,NVmax,
     *            M1max,M1Tmax,M2max,M2Tmax,Mmax,NGmax,ISmax,IPTmax ! Dimensions max des vecteurs

      L1=0

      DO 306 IPAN=1,NETO
        NBRXI=NVAR(IPAN)
        DO 305 K=1,NBRXI
                KK=NXIT(K,IPAN)
                L=L1+K
                DC(L)=SENS4(J,IY,KK,IPAN)*INVV
  305   CONTINUE
        L1=L1+NBRXI
  306 CONTINUE

c     WRITE (66,*)'Ds CSENS :pour J=',J,'  DC ='
c     WRITE (66,'(9(E10.3,1X))') (DC(I),I=1,27)

      RETURN
      END

************************************************************************

      SUBROUTINE CSENS2(NETO,INVV,NVAR,NXIT,IY,J,SENS2,DC)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION NVAR(NETO),NXIT(9,NETO),SENS2(16,IPTmax,9,NETO),    !avril2003
     *          DC(9*NETO)

      COMMON/DIM1/Nmax,NEmax,NVmax,
     *            M1max,M1Tmax,M2max,M2Tmax,Mmax,NGmax,ISmax,IPTmax ! Dimensions max des vecteurs

      L1=0

      DO 306 IPAN=1,NETO
        NBRXI=NVAR(IPAN)
        DO 305 K=1,NBRXI
                KK=NXIT(K,IPAN)
                L=L1+K
                DC(L)=SENS2(J,IY,KK,IPAN)*INVV
  305   CONTINUE
        L1=L1+NBRXI
  306 CONTINUE

c      WRITE (66,*)'Ds CSENS2 :pour J=',J,'  DC ='
c      WRITE (66,'(9(E10.3,1X))') (DC(I),I=1,27)

      RETURN
      END

************************************************************************

      SUBROUTINE CSENS3(NETO,INVV,NVAR,NXIT,IY,J,IPT,IC,SENS4,DC)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION NVAR(NETO),NXIT(9,NETO),SENS4(9,IPTmax,9,NETO),
     *          DC(9*NETO)

      COMMON/DIM1/Nmax,NEmax,NVmax,
     *            M1max,M1Tmax,M2max,M2Tmax,Mmax,NGmax,ISmax,IPTmax ! Dimensions max des vecteurs

      L1=0

      DO 306 IPAN=1,NETO
        NBRXI=NVAR(IPAN)
        DO 305 K=1,NBRXI
            KK=NXIT(K,IPAN)
            L=L1+K
            IF(IC.EQ.5) DC(L)=SENS4(J,IY,KK,IPAN)-SENS4(J,1,KK,IPAN)
            IF(IC.EQ.6) DC(L)=SENS4(J,IY,KK,IPAN)-SENS4(J,IPT,KK,IPAN)
            IF(IC.EQ.7) DC(L)=SENS4(J,IY,KK,IPAN) -
     *                     (SENS4(J,1,KK,IPAN)+SENS4(J,IPT,KK,IPAN))/2.
            DC(L)=DC(L)*INVV
  305   CONTINUE
        L1=L1+NBRXI
  306 CONTINUE


c      WRITE (66,*)'Ds CSENS3 :pour J=',J,'  DC ='
c      WRITE (66,'(9(E10.3,1X))') (DC(I),I=1,27)

      RETURN
      END

************************************************************************

      SUBROUTINE CSENS4(NETO,NVAR,NXIT,IY,NEL,DSPAIK,SENS3,
     *                  SMOYEN,SPAIK,DC)

C       C(x)    = -  SMOYEN  / S(ULS de PAIK)     (avec SMOYEN= NX/sect  et S(ULS de PAIK) = SPAIK
C     d C(x)/dXI= - (SPAIK* d[SMOYEN]/dXI - SMOYEN * d[SPAIK]/dXI ) / (S**2)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION NVAR(NETO),NXIT(9,NETO),DSPAIK(9),DC(9*NETO),
     *          SENS3(21,IPTmax,9,NETO)

      COMMON/DIM1/Nmax,NEmax,NVmax,
     *            M1max,M1Tmax,M2max,M2Tmax,Mmax,NGmax,ISmax,IPTmax ! Dimensions max des vecteurs

      L1=0
      SP2=SPAIK*SPAIK

c      WRITE(666,*)'== SUBR SENS4'
c      WRITE(666,'(9(E11.4,1X))') (DC(I),I=1,27)
c      WRITE(666,*)' SMOYEN=',SMOYEN
c      WRITE(666,*)' SPAIK= ',SPAIK
c      WRITE(666,*)'DSPAIK=',DSPAIK

      DO 306 IPAN=1,NETO
        NBRXI=NVAR(IPAN)

        DO 305 K=1,NBRXI
            KK=NXIT(K,IPAN)
            L=L1+K
c           WRITE(666,*)'IPAN et NEL=',IPAN,NEL
c           WRITE(666,*)'DSPAIK(KK)=',DSPAIK(KK)
c           WRITE(666,*)'DSENS3(21,IY,KK,IPAN)=',SENS3(21,IY,KK,IPAN)
c           WRITE(666,*)'DSPAIK(KK) =',DSPAIK(KK)
c           WRITE(666,*)'SMOYEN=',SMOYEN
c           WRITE(666,*)'DC(L)=',DC(L),' pour L=',L
            DC(L)= SPAIK * SENS3(21,IY,KK,IPAN) ! 1er partie
c		       = SPAIK * D(Smoyen)/dx !       
c           WRITE(666,*)'DC(L)=',DC(L),' pour L=',L
 
            IF(IPAN==NEL) THEN
              DC(L)= DC(L) - SMOYEN * DSPAIK(KK)  ! 2eme partie
		                 ! SMOYEN * d[SPAIK]/dXI
            ENDIF
            DC(L)=-DC(L)/SP2
c           WRITE(666,*)'DC(L)=',DC(L),' pour L=',L
  305   CONTINUE
        L1=L1+NBRXI
  306 CONTINUE

c      WRITE (666,*)'Ds CSENS4 , DC ='
c      WRITE (666,'(9(E10.3,1X))') (DC(I),I=1,27)
      RETURN
      END

************************************************************************
