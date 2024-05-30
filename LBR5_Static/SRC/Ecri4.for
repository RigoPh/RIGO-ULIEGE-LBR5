
      SUBROUTINE ECRI4(DIS,NEL,EFF,CONC,NE,INDAIG,INDRAI,Q,NS,IMPR,
     *			ISECT,VNyMAX,VMyMAX,SyMAX,SyMIN,VM1,VM2)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION DIS(5),EFF(9690),CONC(750)
	CHARACTER *50 A(37),B(15),C(37),D(15),E(32),F(32)
C	CHARACTER *7 SECTION
      COMMON/ECR/U(51),MT
      COMMON/PY/PI
      COMMON/LANGUE/LANGUE
C
C***********************************************************************
C
C     SUBROUTINE ECRIT(FORME COMPLETE)
C     SUBROUTINE DE MISE EN FORME DES RESULTATS POUR ECRITURE SUR
C     UN LISTING.
C
C    Modif :13- 3-1995                           Créer : Thèse Ph. Rigo;
C            1-11-2000
c            4-12-2002  22 variables classées dans SOL2 (valeurs extrêmes)
C
C***********************************************************************
c             12345678901234567890123456789012345678901234567890
      DATA A/'V (m) - DEPLACEMENT TRANSVERSAL selon OY (Z=0)',		! 1
     *       'U (m) - DEPLACEMENT LONGITUDINAL selon OX (Z=0)',
     *       'W (m) - FLECHE, Deplacement selon OZ (Z=0)',			! 3
     *       'dW/dY - ROTATION autour de l axe OX ',
     *       'Nx (N/m) - Effort Normal OX (+ Participation Raid)',	! 5
     *       'Ny (N/m) - Effort Transv OY (+ Participation Cadr)',
     *       'Nxy (N/m) - Effort Cisaillement (+ Part renforts)',
     *       'Nyx (N/m) - Effort Cisaillement (+ Part renforts)',
     *       'Mx (N.m/m)- Moment autour de OY (+ Particip Raid)',		! 9
     *       'My (N.m/m)- Moment autour de OX (+ Particip Cadr)',
     *       'Mxy (N.m/m) - Moment de Torsion',
     *       'Myx (N.m/m) - Moment de Torsion',
     *       'Qx (N/m) - Reaction sur bord OY (avec Part. Raid)',		! 13
     *       'Qy (N/m) - Reaction sur bord OY (avec Part. Cadre)',
     *       'Ry (N/m) - Kirchoff Reaction sur OX (= Qy + ...)',
     *       'Rx (N/m) - Kirchoff Reaction sur OY (= Qx + ...)',		! 16
c     *       'Sy (N/mm2) - Contr. Trans. selon OX, Plaque (Z=0)',     ! 
c     *       'Sx (N/mm2) - Contr. Long.  selon OY, Plaque (Z=0)',
c     *       'Txy (N/mm2) - Contr. Cisaillement, Plaque (Z=0)',
c     *       'Scomp (N/mm2) - Contr. de Von Mises, Plaque(Z=0)',		
     *       'Sy Ep.+ - Contr. Jonct.supérieur, épontille',    ! 17  20.02.04
     *       'Sy Ep.- - Contr. Jonct inférieure, épontille',	! 18   20.02.04
     *       'Tyz Ep. - Tau (shear)Jonct AME-SEM,épont',		! 19	20.02.04
     *       'Tyz Ep. (a.n.) - Tau (shear) axe neutre,épont',			! 20	20.02.04
     *       'Sc VM.Ep.+ - Von Mises, J. AME-SEM sup, epontille',		! 21	20.02.04
     *       'Sc VM.Ep.- - Von Mises, J. AME-SEM inf, epontille',		    ! 22  20.02.04
     *       'Sc VM.Ep. (a.n.) - Von Mises, J. axe neutre,epont',		    ! 23	20.02.04
     *       'Sy Ep.+ (SEM)- Contrainte sup SEMELLE, epontille',		    ! 24	20.02.04
     *       'Sy Ep.- (SEM)- Contrainte inf SEMELLE, epontille',		        ! 25    20.02.04
     *   	   'Txy Ep. (SEM)- Tau (shear) dans SEMELLE epont',		! 26	20.02.04
     *       'Sc VM.Ep.+ (SEM)- Von Mises, sup SEMELLE, epont',		    ! 27	20.02.04
     *       'Sc VM.Ep.- (SEM)- Von Mises, inf SEMELLE, epont',	         	! 28   20.02.04
     *       'Sx Raid (JAS)- Contr. Jonction AME-SEM, RAID',			! 29
     *       'Sx Raid (JAB)- Contr. Jonction AME-epontille, RAID',		! 30
     *       'Sx Raid (SEM)- Contrainte dans SEMELLE, RAID',			! 31
     *       'Txz Raid (JAS)- Tau (shear), Jonct. AME-SEM',			! 32
     *       'Txz Raid (JAB)- Tau (shear), Jonct. AME-epontille',			! 33
     *       'Tyx Raid (SEM)- Tau (shear) dans SEMELLE RAID',			! 34
     *       'Sc Raid (JAS)- Von Mises, Jonct AME-SEM,RAID',			! 35
     *       'Sc Raid (JAB)- Von Mises, Jonct AME-epontille, RAID',		! 36
     *       'Sc Raid (SEM)- Von Mises, dans SEMELLE, RAID'/  		! 37
c             12345678901234567890123456789012345678901234567890

      DATA B/	'NX  CONCENTRE',
     *		'NXY CONCENTRE  = NYX CONCENTRE',
     *		'MX  CONCENTRE',
     *		'MXY CONCENTRE',
     *		'QX  CONCENTRE',
     *		'RX  CONCENTRE',
     *		'SIGMA X SEMELLE    TRAVERSE',
     *		'SIGMA X  JAS       TRAVERSE',
     *		'SIGMA X  JAB       TRAVERSE',
     *		'TAU YX MAX SEMELLE TRAVERSE',
     *		'TAU XZ AME JAS     TRAVERSE',
     *        'TAU XZ AME JAB     TRAVERSE',
     *		'SIGMA Von-Mises SEMELLE TRAVERSE',
     *		'SIGMA Von-Mises JAS  TRAVERSE',
     *		'SIGMA Von-Mises JAB  TRAVERSE'/

      DATA C/'V (m) - TRANSV. DISPL. along OY (Z=0)',					! 1
     *       'U (m) - LONG. DISPL. along OX (Z=0)',
     *       'W (m) - LATERAL DEFLECTION along OZ(Z=0)',				! 3
     *       'dW/dY - ROTATION around OX ',
     *       'Nx (N/m) - Long. Force along OX(with Stiff. Effect)',	! 5
     *       'Ny (N/m) - Transv Force along OY(with Frame Effect)',
     *       'Nxy (N/m) - Inplane Shear Force (with Memb. Effect)',
     *       'Nyx (N/m) - Inplane Shear Force (with Memb. Effect)',
     *       'Mx (N.m/m)- Moment around OY (with Stiff. Effect)',		! 9
     *       'My (N.m/m)- Moment around OX (with Frame Effect)',
     *       'Mxy (N.m/m) - Twisting Moment',
     *       'Myx (N.m/m) - Twisting Moment',
     *       'Qx (N/m) - Reaction along OY(with Stiff. Effect)',		! 13
     *       'Qy (N/m) - Reaction along OX(with Stiff. Effect)',
     *       'Ry (N/m) - Kirchoff Reaction along OX (= Qy + ...)',
     *       'Rx (N/m) - Kirchoff Reaction along OY (= Qx + ...)',	! 16
c     *       'Sy (N/mm2) - Transv. Stress in Plate (Z=0)',
c     *       'Sx (N/mm2) - Longit. Stress in Plate (Z=0)',
c     *       'Txy (N/mm2) - Inplane Shear Stress in Plate (Z=0)',
c     *       'Scomp (N/mm2) - Von-Mises in Plate (Z=0)',				! 20
     *       'Sy Pillar (JWF)- Stress, Junct. top, pillar',		! 21
     *       'Sy Pillar (JWP)- Stress, Junct. bottom, pillar',		! 22
     *       'Tyz Pillar (JWF)- Shear Stress, Junct. Web-Flange',		! 23
     *       'Tyz Pillar - Shear Stress, at the neutral axis',		! 24
     *       'Sc Pillar (JWF)- Von-Mises at top J. Web-Flange,pillar',		! 25
     *       'Sc Pillar (JWF)- Von-Mises at bottom J. Web-Flange',		! 26
     *       'Sc Pillar - Von-Mises at the neutral axis,pillar',		! 26
     *       'Sy Pillar (FLANGE)- Stress in top Flange, pillar',			! 27
     *       'Sy Pillar (FLANGE)- Stress in bottom Flange, pillar',			! 27
     *       'Txy Pillar (FLANGE)- Shear Stress in Flange pillar',		! 28
     *       'Sc Pillar (FLANGE)- Von-Mises in top Flange, pillar',			! 29
     *       'Sc Pillar (FLANGE)- Von-Mises in bottom Flange, pillar',			! 29
     *       'Sx Stiff (JWF)- Stress, Junct. Web-Flange, STIFF',		! 30
     *       'Sx Stiff (JWP)- Stress, Junct. Web-Pillar, STIFF',		! 31
     *       'Sx Stiff (FLANGE)- Stress in Flange, STIFF',			! 32
     *       'Txz Stiff (JWF)- Shear Stress, Junct. Web-Flange',		! 33
     *       'Txz Stiff (JWP)- Shear Stress, Junct. Web-Pillar',		! 34
     *       'Tyx Stiff (FLANGE)- Shear Stress in Flange STIFF',		! 35
     *       'Sc Stiff (JWF)- Von-Mises at J. Web-Flange,STIFF',		! 36
     *       'Sc Stiff (JWP)- Von-Mises at J. Web-Pillar, STIFF',		! 37
     *       'Sc Stiff (FLANGE)- Von-Mises in Flange, STIFF'/			! 38
c             12345678901234567890123456789012345678901234567890
      DATA D/	'NX  , Local force',
     *		'NXY = NYX , Local force',
     *		'MX  , Local force',
     *		'MXY , Local force',
     *		'QX  , Local force',
     *		'RX  , Local force',
     *		'SIGMA X  FLANGE  , Stress in Girder',
     *		'SIGMA X  JWF     , Stress in Girder',
     *		'SIGMA X  JWP     , Stress in Girder',
     *		'TAU YX FLANGE  , Stress in Girder',
     *		'TAU XZ WEB JWF , Stress in Girder',
     *        'TAU XZ WEB JWP , Stress in Girder',
     *		'SIGMA COMP FLANGE , Von-Mises Stress in Girder',
     *		'SIGMA COMP WEB JWF, Von-Mises Stress in Girder',
     *		'SIGMA COMP WEB JWP, Von-Mises Stress in Girder'/
	DATA E/'V (m) - DEPLACEMENT TRANSVERSAL selon OY (Z=0)',		! 1
     *       'U (m) - DEPLACEMENT LONGITUDINAL selon OX (Z=0)',
     *       'W (m) - FLECHE, Deplacement selon OZ (Z=0)',			! 3
     *       'dW/dY - ROTATION autour de l axe OX ',
     *       'Nx (N/m) - Effort Normal OX (+ Participation Raid)',	! 5
     *       'Ny (N/m) - Effort Transv OY (+ Participation Cadr)',
     *       'Nxy (N/m) - Effort Cisaillement (+ Part renforts)',
     *       'Nyx (N/m) - Effort Cisaillement (+ Part renforts)',
     *       'Mx (N.m/m)- Moment autour de OY (+ Particip Raid)',		! 9
     *       'My (N.m/m)- Moment autour de OX (+ Particip Cadr)',
     *       'Mxy (N.m/m) - Moment de Torsion',
     *       'Myx (N.m/m) - Moment de Torsion',
     *       'Qx (N/m) - Reaction sur bord OY (avec Part. Raid)',		! 13
     *       'Qy (N/m) - Reaction sur bord OY (avec Part. Cadre)',
     *       'Ry (N/m) - Kirchoff Reaction sur OX (= Qy + ...)',
     *       'Rx (N/m) - Kirchoff Reaction sur OY (= Qx + ...)',		! 16
c     *       'Sy (N/mm2) - Contr. Trans. selon OX, Plaque (Z=0)',
c     *       'Sx (N/mm2) - Contr. Long.  selon OY, Plaque (Z=0)',		
c     *       'Txy (N/mm2) - Contr. Cisaillement, Plaque (Z=0)',
c     *       'Scomp (N/mm2) - Contr. de Von Mises, Plaque(Z=0)',		
     *       'Sy Ep.+ - Contr.supérieure, épontille',		    ! 17  20.02.04
     *       'Sy Ep.- - Contr. inférieure, épontille',		! 18   20.02.04
     *       'Tyz Ep. - Tau (shear)SOMMET,épont',		! 19	20.02.04
     *       'Tyz Ep. (a.n.)- Tau (shear) axe neutre,épont',	    ! 20	20.02.04
     *       'Sc VM.Ep.+ - Von Mises, sup, epontille',		    ! 21	20.02.04
     *       'Sc VM.Ep.- - Von Mises, inf, epontille',		    ! 22	20.02.04
     *       'Sc VM.Ep. (a.n.)- Von Mises, axe neutre,épontille',		    ! 23	20.02.04
c     *       'Sy Cadre (SEM)- Contrainte dans SEMELLE, CADRE',		!
c     *       'Txy Cadre (SEM)- Tau (shear) dans SEMELLE CADRE',		!
c     *       'Sc Cadre (SEM)- Von Mises, dans SEMELLE, CADRE',		!
     *       'Sx Raid (JAS)- Contr. Jonction AME-SEM, RAID',			! 24
     *       'Sx Raid (JAE)- Contr. Jonction AME-Epon, RAID',		! 25
     *       'Sx Raid (SEM)- Contrainte dans SEMELLE, RAID',			! 26
     *       'Txz Raid (JAS)- Tau (shear), Jonct. AME-SEM',			! 27
     *       'Txz Raid (JAE)- Tau (shear), Jonct. AME-Epon',			! 28
     *       'Tyx Raid (SEM)- Tau (shear) dans SEMELLE RAID',			! 29
     *       'Sc Raid (JAS)- Von Mises, Jonct AME-SEM,RAID',			! 30
     *       'Sc Raid (JAE)- Von Mises, Jonct AME-Epon, RAID',		! 31
     *       'Sc Raid (SEM)- Von Mises, dans SEMELLE, RAID'/  		! 32
c             12345678901234567890123456789012345678901234567890

	DATA F/'V (m) - TRANSV. DISPL. along OY (Z=0)',					! 1
     *       'U (m) - LONG. DISPL. along OX (Z=0)',
     *       'W (m) - LATERAL DEFLECTION along OZ(Z=0)',				! 3
     *       'dW/dY - ROTATION around OX ',
     *       'Nx (N/m) - Long. Force along OX(with Stiff. Effect)',	! 5
     *       'Ny (N/m) - Transv Force along OY(with Frame Effect)',
     *       'Nxy (N/m) - Inplane Shear Force (with Memb. Effect)',
     *       'Nyx (N/m) - Inplane Shear Force (with Memb. Effect)',
     *       'Mx (N.m/m)- Moment around OY (with Stiff. Effect)',		! 9
     *       'My (N.m/m)- Moment around OX (with Frame Effect)',
     *       'Mxy (N.m/m) - Twisting Moment',
     *       'Myx (N.m/m) - Twisting Moment',
     *       'Qx (N/m)- Reaction along OY(with Stiff. Effect)',		! 13
     *       'Qy (N/m)- Reaction along OX(with Stiff. Effect)',
     *       'Ry (N/m)- Kirchoff Reaction along OX (= Qy + ...)',
     *       'Rx (N/m)- Kirchoff Reaction along OY (= Qx + ...)',	    ! 16
c     *       'Sy (N/mm2) - Transv. Stress in Plate (Z=0)',
c     *       'Sx (N/mm2) - Longit. Stress in Plate (Z=0)',
c     *       'Txy (N/mm2) - Inplane Shear Stress in Plate (Z=0)',
c     *       'Scomp (N/mm2) - Von-Mises in Plate (Z=0)',				
     *       'Sy Pillar - Stress, top of section, pillar',		! 17
     *       'Sy Pillar - Stress, bottom of section, pillar',		! 18
     *       'Tyz Pillar - Shear Stress, top of section',		! 19
     *       'Tyz Pillar- Shear Stress, section''s neutral axis',		! 20
     *       'Sc Pillar- Von-Mises at top J. Web-Flange,pillar',		! 21
     *       'Sc Pillar- Von-Mises at bottom J.Web-Flange',		! 22
     *       'Sc Pillar- Von-Mises at neutral axis,pillar',		! 23
c     *       'Sy Frame (FLANGE)- Stress in Flange, FRAME',			
c     *       'Txy Frame (FLANGE)- Shear Stress in Flange FRAME',		
c     *       'Sc Frame (FLANGE)- Von-Mises in Flange, FRAME',			
     *       'Sx Stiff (JWF)- Stress, Junct. Web-Flange, STIFF',		! 24
     *       'Sx Stiff (JWP)- Stress, Junct. Web-Pillar, STIFF',		! 25
     *       'Sx Stiff (FLANGE)- Stress in Flange, STIFF',			! 26
     *       'Txz Stiff (JWF)- Shear Stress, Junct. Web-Flange',		! 27
     *       'Txz Stiff (JWP)- Shear Stress, Junct. Web-Pillar',		! 28
     *       'Tyx Stiff (FLANGE)- Shear Stress in Flange STIFF',		! 29
     *       'Sc Stiff (JWF)- Von-Mises at J.Web-Flange,STIFF',		! 30
     *       'Sc Stiff (JWP)- Von-Mises at J.Web-Pillar, STIFF',		! 31
     *       'Sc Stiff (FLANGE)- Von-Mises in Flange, STIFF'/			! 32
c             12345678901234567890123456789012345678901234567890
C
C     L = indice relatif à la variables étudiées (L=1 à 38) dans EFF 
c					pour épontilles en Double T
C     L=1       V               L=2     U                L=3    W
C       4       dW/dy             5     Nx                 6    Ny
C       7       Nxy               8     Nyx                9    Mx
C      10       My               11     Mxy               12    Myx
C      13       Qx               14     Qy                15    Ry
C      16       Rx

C      17     Sig Y JAS (sup)	  18   Sig Y JAB (inf)		19    Tau JAS
C      20     Tau a.n.		  21   Sig COMP JAS (sup)	22    Sig COMP (inf)
C      23     Sig COMP a.n.     24   Sig Y sem (sup)      25    Sig Y sem (inf)
C	 26		Tau sem			  27	Sig COMP sem (sup)	28	  Sig COMP sem (inf)	

C      29    Sig X JAS (raid)    30   Sig X JAB (raid)     31    Sig X Sem (raid)
C      32    Tau   JAS (raid)    33   Tau JAB (raid)       34    Tau sem (raid)
C      35    Sig COMP JAS (raid) 36   Sig COMP JAB (raid)  37							    Sig COMP (raid)

      IF(LANGUE.NE.1) THEN
        DO I=1,37
	    A(I)=C(I)  
	  ENDDO
        DO I=1,32
	    E(I)=F(I)  
	  ENDDO
	  DO I=1,15  
	    B(I)=D(I)
	  ENDDO
	ENDIF

      KH=2*MT+31
      DO 1 I=1,KH
  1   U(I)=U(I)*Q*PI/180.
      WRITE(66,150) NEL
      WRITE(66,*) 'EPONTILLE'												!12.05.04
	L=0

	IF(ISECT.EQ.3)THEN
      DO 157 KR=1,37   !variables eff
        L=L+1
        IF((IMPR.LE.-1).AND.(L.EQ.5)) L=17
	  IF(L.EQ.2) L=3
	  IF(L.EQ.5) L=6
	  IF(L.EQ.7) L=9
	  IF(L.EQ.11) L=14
	  IF(L.EQ.16) L=17
        IF(L.EQ.38) GOTO 201
        IF((L.EQ.16).AND.(INDAIG.EQ.0)) L=29
        IF((L.EQ.29).AND.(INDRAI.EQ.0)) GOTO 201
        WRITE(66,50) A(L)

  50    FORMAT(/T15,A50/T15,50(1H+))
        WRITE(66,156) DIS
C
C     NS = 1 si PHIL positif (sens normal) ; NS=-1 si PHIL négatif
C
        IF(NS.EQ.1) THEN
          DO 163 I=1,KH
              JM=(L-1)*255+I
              IJ=JM+204
  163     WRITE(66,158) U(I),(EFF(K),K=JM,IJ,51)
	    IF (L.EQ.10)THEN
	    VM1= -EFF(2500)
	    VM2= -EFF(2530)
          ENDIF
        ELSE
          IF(L.GE.16) GOTO 2
          GOTO(3,2,2,3,2,2,3,3,2,2,3,3,2,3,3),L
   3          JM=(L-1)*255+1
              IJ=L*255
              DO 4 KK=JM,IJ
   4          EFF(KK)=-EFF(KK)
   2      DO 159 I=1,31				
              JM=(L-1)*255+32-I
              IJ=JM+204
  159     WRITE(66,158) U(I),(EFF(K),K=JM,IJ,51)
          DO 161 I=32,KH
              JM=(L-1)*255+(KH+32-I)
              IJ=JM+204
              UTRAV=U(31)-U(KH+32-I)
  161     WRITE(66,158) UTRAV,(EFF(K),K=JM,IJ,51)
        ENDIF
     
      CALL EXTRE4(L,KH,EFF,A,ISECT,VNyMAX,VMyMAX,SyMAX,SyMIN)	
  157 CONTINUE

	ELSE
	DO 155 KR=1,32   !variables eff
        L=L+1

        IF((IMPR.LE.-1).AND.(L.EQ.5)) L=17
	  IF(L.EQ.2) L=3
	  IF(L.EQ.5) L=6
	  IF(L.EQ.7) L=10
	  IF(L.EQ.11) L=14
	  IF(L.EQ.16) L=17
        IF(L.EQ.33) GOTO 201
        IF((L.EQ.16).AND.(INDAIG.EQ.0)) L=24
        IF((L.EQ.24).AND.(INDRAI.EQ.0)) GOTO 201
        WRITE(66,51) E(L)

  51    FORMAT(/T15,A50/T15,50(1H+))
        WRITE(66,156) DIS
C
C     NS = 1 si PHIL positif (sens normal) ; NS=-1 si PHIL négatif
C
        IF(NS.EQ.1) THEN
          DO 165 I=1,KH
              JM=(L-1)*255+I
              IJ=JM+204
  165     WRITE(66,158) U(I),(EFF(K),K=JM,IJ,51)
      	    IF (L.EQ.10)THEN
	    VM1= -EFF(2500)			!M1 moment à extrémité sup pour FLAM
	    VM2= -EFF(2530)			!M2 moment à extrémité inf pour FLAM
	endif
	  ELSE
          IF(L.GE.16) GOTO 6
          GOTO(5,6,6,5,6,6,5,5,6,6,5,5,6,5,5),L
   5          JM=(L-1)*255+1
              IJ=L*255
              DO 41 KK=JM,IJ
  41          EFF(KK)=-EFF(KK)
  6      DO 1599 I=1,31                                
              JM=(L-1)*255+32-I
              IJ=JM+204
 1599     WRITE(66,158) U(I),(EFF(K),K=JM,IJ,51)
          DO 1619 I=26,KH
              JM=(L-1)*255+(KH+32-I)
              IJ=JM+204
              UTRAV=U(31)-U(KH+32-I)
 1619     WRITE(66,158) UTRAV,(EFF(K),K=JM,IJ,51)
        ENDIF
      
      CALL EXTRE4(L,KH,EFF,E,ISECT,VNyMAX,VMyMAX,SyMAX,SyMIN)
	
  155 CONTINUE

	ENDIF
  201 WRITE(2218,*) '99'

C
      IF(MT.EQ.0) GOTO 200
      IF(LANGUE==1) THEN
        WRITE(66,301)
	ELSE
        WRITE(66,302)
	ENDIF
      DO 160 L=1,15
      IF((IMPR.LE.-1).AND.(L.LE.6)) GOTO 160
         WRITE(66,50) B(L)
         WRITE(66,156) DIS
         IF(NS.EQ.1) THEN
         DO 164 I=1,MT
            JM=(L-1)*50+I
            IJ=JM+40
 164     WRITE(66,158) U(2*I+30),(CONC(K),K=JM,IJ,10)
         ELSE
         DO 162 I=1,MT
            JM=(L-1)*50+MT-I+1
            IJ=JM+40
            UTRAV=U(31)-U(KH+1-2*I)
  162    WRITE(66,158) UTRAV,(CONC(K),K=JM,IJ,10)
         ENDIF
  160 CONTINUE
C
  150 FORMAT(T3,'PANNEAU-PANEL No ',I2,/T3,18(1H+))
  156 FORMAT(/10X,5(4X,'X=',F6.3,' m')/)
  158 FORMAT(' Y=',F6.3,'m',5(1X,E13.6))
  301 FORMAT(//T17,'CALCUL RELATIF AUX TRAVERSES DES EFFETS ',
     *             'CONCENTRES '/T15,53(1H+)//)
  302 FORMAT(//T17,'STRESSES IN THE LONGITUDINAL GIRDERS'/
     *         T15,35(1H+)//)

  200 RETURN
      END

c ------------------------------------------------------------
      SUBROUTINE EXTRE4(L,KH,EFF,A,ISECT,VNyMAX,VMyMAX,SyMAX,SyMIN)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION EFF(9690)
	CHARACTER *50 A(37)
      COMMON/PY/PI
      COMMON/LANGUE/LANGUE
	INTEGER*2 ISECT
C
C***********************************************************************
C
C     SUBROUTINE EXTRE4
C     ++++++++++++++++++
C     Recherche les valeurs extrêmes (max et min)
C
C     Modif : Nov 2000                                    Créer : 13-3-95
C
C***********************************************************************

      VMIN=0.
      VMAX=0.
      IXMIN=0
      IXMAX=0
      IYMIN=0
      IYMAX=0
      I1=(L-1)*255

      DO 1 KX=1,5
      I2=I1+(KX-1)*51
        DO 2 KY=1,KH
        K1=I2+KY
        IF(VMIN.GT.EFF(K1)) THEN
                 VMIN=EFF(K1)
                IXMIN=KX
                IYMIN=KY
        ENDIF
        IF(VMAX.LT.EFF(K1)) THEN
                VMAX=EFF(K1)
                IXMAX=KX
                IYMAX=KY
        ENDIF
   2    CONTINUE
   1  CONTINUE
C	enregistrement de données pour routine FLAM
	IF(L.EQ.6) THEN
	 VNyMAX=-VMIN
	ELSEIF(L.EQ.10) THEN
	 VMyMAX= -VMIN
	ELSEIF (L.EQ.17) THEN
	SyMAX=-VMIN
	ELSEIF (L.EQ.18) THEN
	SyMIN =-VMIN
	ENDIF

      IF(LANGUE==1) WRITE(66,102) VMIN,IXMIN,IYMIN,VMAX,IXMAX,IYMAX
      IF(LANGUE==2) WRITE(66,100) VMIN,IXMIN,IYMIN,VMAX,IXMAX,IYMAX

C     Sauvetage pour le module d'analyse des extrêmes (subr. ANALYS3)
      IF(L.LE.3)  GOTO 101				
	IF(ISECT.EQ.3) THEN
       IF((L.GE.17).AND.(L.LE.23)) GOTO 101
      IF(L.EQ.27) GOTO 101
      IF(L.EQ.28) GOTO 101
	ELSE
       IF((L.GE.17).AND.(L.LE.23)) GOTO 101
	ENDIF
      GOTO 999
  101 CONTINUE
      WRITE(2218,50) L,A(L)
      WRITE(2218,*) VMIN,IXMIN,IYMIN,VMAX,IXMAX,IYMAX
	

c      WRITE(*,*)' L et titre'
c      WRITE(*,50) L,(A(J),J=3*L-2,3*L)
c      WRITE(*,*) 'VMIN,IXMIN,IYMIN,VMAX,IXMAX,IYMAX'
c      WRITE(*,*) VMIN,IXMIN,IYMIN,VMAX,IXMAX,IYMAX
c      PAUSE 'ECRI'

  999 RETURN

  50  FORMAT(I2,T15,50A)
  100 FORMAT(/' Min. Value =',E11.4,'   in (X,Y)=(',I2,' , ',I2,')'/
     *        ' Max. Value =',E11.4,'   in (X,Y)=(',I2,' , ',I2,')')
  102 FORMAT(/' Valeur min =',E11.4,'   en (X,Y)=(',I2,' , ',I2,')'/
     *        ' Valeur max =',E11.4,'   en (X,Y)=(',I2,' , ',I2,')')
      RETURN
      END

c ------------------------------------------------------------
