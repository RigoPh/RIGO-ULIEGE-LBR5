      SUBROUTINE MDR2(NETO,NE,A,DZSN,A8,B1,ZSN,PHILN,IMPR,NVAR,NXIT,
     *                IPTS,NSOL,
     *                ANGL,NOEUD,MCOMP,NNO,NC,NSIGN)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(NE,NE),DZSN(NE,9*NETO,NSOL),A8(NE,8),B1(NE,NSOL),
     *          ZSN(NE,NSOL),PHILN(NETO),
     *          NVAR(NETO),NXIT(9,30),NXI(9),IPTS(NETO)

c     POUR le COMMOM OPTI2 (IPT = 4 et NSOL=10 a changer manuellement)
      DIMENSION SA(13,4,9), SB(13,4,9) ,SC(13,4,9), SD(13,4,9),
     *         SAA(13,4,9),SBB(13,4,9),SCC(13,4,9),SDD(13,4,9),
     *          SH(13,4,9,10),  !SH(13,IPT,9,NSOL)
     *          DEFX(13*4*8)    !DEFX = DEFA + ... DEFDD

      COMMON/PY/    PI
      COMMON        AUX(400),ZB(360)
      COMMON/OPTI2/ DEFX,SH,SA,SB,SC,SD,SAA,SBB,SCC,SDD  
C***********************************************************************
C
C     SUBROUTINE MDR2 (MODULE OPTIMISATION)
C     ****************
C     Calcul des dérivées des EQU par la méthode des pseudo-charges
C
C INPUT:
C     A     = Matrice inverse du système  : [A] * (X) =(B)
C     ZSN   = Vecteur solution du système : [A] * (X) =(B)
C     A8,B1 = Matrice et vecteur de travail
C     AUX,ZB= Vecteurs de travail;[AUX]*(X)=(ZB) ; voir subr MATR
C OUTPUT:
C     DZSN= Matrice contenant les dérivées de ZSN par les variables de conception.
C           Chaque colonne correspond à une dérivée (c.à.d. à une variable)
C                 -1
C        DZSN =  A   * ( dB/dx - dA/dx * ZSN) = A * (B1 - A8 * ZSN)
C
C***********************************************************************
C     Modifié : 28-06-95 			                   Créer: 19-8-94
C         	   4-12-95 (pour NSOL cas de charge)                                                              
C         	  3-3-2001 (pour JLPH termes de la series de Fourier)                                                              
C                                                                       
C***********************************************************************
C     IVAR = Compteur des variables de conception (=1,NTOT)
C     NTOT = Nbre total de variables de conception

      CALL ANNULD(AUX,160)
      CALL ANNULD(ZB,16*NSOL)

C  Boucle 1 : Boucle sur les variables de conception (NTOT variables de conception) 
C             Pour chaque panneau (IPAN=1,NETO), il y a NBRXI vatiables (K=1,NBRXI)

C     But : Création de la matrice dérivée de A(NE,NE)   (Ax=B)
      
      IVAR=0

      DO 1 NEL=1,NETO
          READ(NEL+400) SH,SA,SB,SC,SD,SAA,SBB,SCC,SDD	!extension neto
          NBRXI=NVAR(NEL)
          IPT=IPTS(NEL)
          DO 18 L=1,NBRXI
  18      NXI(L)=NXIT(L,NEL)

c      WRITE(66,*) 'RELECTURE DES SA,SB, ...'
c      DO 55 I=1,5
c      DO 55 KK=1,NBRXI
c      K=NXI(KK)
c      DO 44 J=1,8
c      WRITE(66,*) J,K,SA(J,I,K),SB(J,I,K) ,SC(J,I,K) ,SD(J,I,K)
c      WRITE(66,*)    SAA(J,I,K),SBB(J,I,K),SCC(J,I,K),SDD(J,I,K)
c      WRITE(66,*)    SH(J,I,K,1)
c   44 CONTINUE
c   55 WRITE(66,*)

      DO 1 KK=1,NBRXI
          IVAR=IVAR+1
          IT=8*NE
          CALL ANNULD(A8,IT)
          CALL ANNULD(B1,NE*NSOL)
      
C  Ordre des var. dans SENS (U=1 V=2 Wø=3 Ny=4 Nyx=5 My=6  Ry=7   W =8) 
C  Ordre des var. dans AUX  (W=1 U=3 My=5 Ny=7 Nyx=9 Ry=11 V =13  Wø=15) 
      
      K=NXI(KK)
   
      CALL MATR( 1,8,  1,K,NSOL)           ! W(0)
      CALL MATR( 2,8,IPT,K,NSOL)           ! W(Yo)
      CALL MATR( 3,1,  1,K,NSOL)           ! U(0)
      CALL MATR( 4,1,IPT,K,NSOL)           ! U(Yo)
      CALL MATR( 5,6,  1,K,NSOL)           ! My(0)
      CALL MATR( 6,6,IPT,K,NSOL)           ! My(Yo)
      CALL MATR( 7,4,  1,K,NSOL)           ! Ny(0)
      CALL MATR( 8,4,IPT,K,NSOL)           ! Ny(Yo)
      CALL MATR( 9,5,  1,K,NSOL)           ! Nyx(0)
      CALL MATR(10,5,IPT,K,NSOL)           ! Nyx(Yo)
      CALL MATR(11,7,  1,K,NSOL)           ! Ry(0)
      CALL MATR(12,7,IPT,K,NSOL)           ! Ry(Yo)
      CALL MATR(13,2,  1,K,NSOL)           ! V(0)
      CALL MATR(14,2,IPT,K,NSOL)           ! V(Yo)
      CALL MATR(15,3,  1,K,NSOL)           ! Wø(0)
      CALL MATR(16,3,IPT,K,NSOL)           ! Wø(Yo)

      IF(IMPR.EQ.1) THEN
      WRITE(66,*) 
      WRITE(66,*) 'PANNEAU nø',NEL,' Variable nø',KK
      WRITE(66,*) '+++++++++++++++++++++++++++++++++'
      WRITE(66,*) 
      WRITE(66,*) '(dA/dx) et (dB/dx) avant Subr. ANGLE'
      DO 100 I=1,16
  100 WRITE(66,2)(AUX((II-1)*20+I),II=1,8),(ZB(I+(IS-1)*16),IS=1,NSOL)
      ENDIF

C     CHANGEMENT DE COORDONNEES (Repère local -> repère global)
c     =========================================================
      IF(NETO.EQ.1) GOTO 7

      CALL ANGLE(NEL,DABS(PHILN(NEL)),NSOL,		!!!aout04
     *           ANGL,NOEUD,NNO,NSIGN,NETO)
C     *******************************

      IF(IMPR.EQ.1) THEN
      WRITE(66,*) 
      WRITE(66,*) '(dA/dx) et (dB/dx) après Subr ANGLE - PANNEAU nø',NEL
      WRITE(66,*) '----------------------------------------------------'
      DO 101 I=1,16
  101 WRITE(66,2) (AUX((II-1)*20+I),II=1,8),(ZB(I+(IS-1)*16),IS=1,NSOL)
      ENDIF

  7   CONTINUE
  
C     CREATION DE LA MATRICE A8 (dA/dx) ET DU VECTEUR B1 (dB/dx) du syst. (A*X=B).
c     ===========================================================================

c      IF(IMPR.EQ.1) THEN
c      WRITE(66,*) '(dA/dx) et (dB/dx) avant RANG2'
c      DO 103 J=1,NE                                                     
c      WRITE(66,*)'Lign. ',J,'   Term ind =',(B1(J,IS),IS=1,NSOL)                       
c 103  WRITE(66,6)(A8(J,I),I=1,8) 
c      ENDIF

      CALL RANG2(NEL,NE,A8,B1,NETO,NSOL,
     *          NOEUD,MCOMP,NNO,NC,NSIGN)
C     ***********************************

      IF(IMPR.EQ.1) THEN
      WRITE(66,*) '(dA/dx) et (dB/dx) après RANG2'
      DO 102 J=1,NE
      WRITE(66,*)'Lign. ',J,'   Term ind =',(B1(J,IS),IS=1,NSOL)
 102  WRITE(66,6)(A8(J,I),I=1,8)
      ENDIF


C     Calcul de [DZSN](n) = [dB/dx](n) - [dA/dx](n,8) * ZSN(8)
c     ========================================================
C DZSN(n) = [DZSN](n,ivar,IS)     pour chaque variable XI et chaque IS
C B1 	  = [dB/dx](n,IS)         car 1 colonne chaque cas de charge IS
C A8      = [dA/dx](n,8)
C ZSN     = Vect. Solution(n,IS)  car 1 colonne chaque cas de charge IS 

      NEL2=8*(NEL-1)
      DO 10 IS=1,NSOL
      DO 10 I=1,NE
      	DZSN(I,IVAR,IS)=B1(I,IS)
      	DO 11 L=1,8
      	  DZSN(I,IVAR,IS)=DZSN(I,IVAR,IS) - A8(I,L) * ZSN(L+NEL2,IS)
   11     CONTINUE
   10 CONTINUE

      IF(IMPR.EQ.1) THEN
      DO 19 IS=1,NSOL
        WRITE(66,*) 'Cas de charge nø',IS !fff
        WRITE(66,*) 'ZSN  pour panneau ',NEL
        WRITE(66,6) (ZSN(I,IS),I=1+NEL2,8+NEL2)                        
        WRITE(66,'(8E14.7)') (ZSN(I,IS),I=1+NEL2,8+NEL2)  ! Plus de decimales                 
        WRITE(66,*) 'dB/dx - dA/dx*ZSN pour la variable no ',IVAR
        WRITE(66,6) (DZSN(I,IVAR,IS),I=1,NE) 
   19 CONTINUE
      ENDIF

   1  CONTINUE
c     ****************************************************************
C     Fin de la boucle principale. La matrice DZSN est constituée.
c     ****************************************************************


C     Calcul du produit de matrice inverse A(NE,NE) par DZSN(NE,NTOT,NSOL)
c     ====================================================================
C     (le résultat est placé dans DZSN, B1 servant de vecteur de travail)
      IF(IMPR.EQ.1) WRITE(66,17) 

c      IF(IMPR.EQ.1) THEN
c      WRITE(66,*)' La matrice inverse est :'
c      DO 553 J=1,NE                                                     
c      WRITE(66,*)'Ligne nø',J
c 553  WRITE(66,'(8E11.4)')(A(J,I),I=1,NE)
c      ENDIF

      NTOT=IVAR
      DO 14 IS=1,NSOL
      DO 14 IVAR=1,NTOT
         CALL ANNULD(B1,NE*NSOL)
      	 DO 12  I=1,NE
      		DO 9 J=1,NE
      		   B1(I,IS)=B1(I,IS)+A(I,J)*DZSN(J,IVAR,IS)
    9 		CONTINUE
   12 	 CONTINUE
      	 DO 13 J=1,NE
      		DZSN(J,IVAR,IS)=B1(J,IS)
   13 	 CONTINUE
      IF(IMPR.EQ.1) THEN
        WRITE(66,*)'CAS DE CHARGE Nø',IS
        WRITE(66,*)'---------------------'
        WRITE(66,*)'VARIABLE XI nø',IVAR 
        DO 104 NEL=1,NETO
      	J1=8*(NEL-1)
          WRITE(66,*)'DZSN relatif au panneau n', NEL
          WRITE(66,6) (DZSN(J,IVAR,IS),J=J1+1,J1+8)
  104   CONTINUE
      ENDIF 
   14 CONTINUE

C     Sauvetage des DZSN  (relecture dans BO2)
C     ===================
      DO NEL=1,NETO
     	J1=8*(NEL-1)
		DO  IS=1,NSOL
		DO  J=J1+1,J1+8
 			WRITE(700+NEL) (DZSN(J,IVAR,IS),IVAR=1,NTOT) !DZSN(8*NETO,9*NETO,NSOL)
		END DO
		END DO
      END DO
     

    2 FORMAT(4(1X,E11.4)/4(1X,E11.4)/5(1X,E11.4)/)
    6 FORMAT(8E11.4)
   17 FORMAT('Dérivées de ZSN pour chaque variables XI'/40(1H-))
      RETURN
      END

C***********************************************************************
C***********************************************************************

      SUBROUTINE MATR(IAUX,IVAR,IPT,K,NSOL)
C     *********************************      
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION SA(13,4,9),SB(13,4,9),SC(13,4,9),SD(13,4,9),
     *          SH(13,4,9,10),
     *         SAA(13,4,9),SBB(13,4,9),SCC(13,4,9),SDD(13,4,9),
     *         DEFX(13*4*8) ! DEFX = DEFA + ... DEFDD

      COMMON AUX(400),ZB(360)
      COMMON/OPTI2/ DEFX,SH,SA,SB,SC,SD,SAA,SBB,SCC,SDD
C***********************************************************************
C
C     SUBROUTINE MATR (MODULE OPTIMISATION cfr Subr. MDR2)
C     ****************
C     TRANSFER DES DERIVEES (SENC,SENCC, ect.) dans AUX et ZB
C
C     Modifié : 1-12-95 			             Créer: 19-8-94
C 
C***********************************************************************
      AUX(IAUX    )= SC(IVAR,IPT,K)
      AUX(IAUX+ 20)=SCC(IVAR,IPT,K)
      AUX(IAUX+ 40)= SD(IVAR,IPT,K)
      AUX(IAUX+ 60)=SDD(IVAR,IPT,K)
      AUX(IAUX+ 80)= SA(IVAR,IPT,K)
      AUX(IAUX+100)=SAA(IVAR,IPT,K)
      AUX(IAUX+120)= SB(IVAR,IPT,K)
      AUX(IAUX+140)=SBB(IVAR,IPT,K)
      
      DO 1 IS=1,NSOL
       IAUX2=IAUX+(IS-1)*16
       ZB(IAUX2)= -SH(IVAR,IPT,K,IS)
   1  CONTINUE

      RETURN
      END
