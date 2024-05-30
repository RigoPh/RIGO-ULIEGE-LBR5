      SUBROUTINE MDR(NEL,NETO,NE,A,B,BLOKA,KLI,ZSN,SOLT,PHIL,NVSMB,
     *               IJK,JLMAX,IMPR,NSOL,
     *               ANGL,NOEUD,MCOMP,NNO,NC,NSIGN)
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER *2 KLI

c     DIMENSION A(NE,NE),B(NE,NE+1),BLOKA(NE,NE),KLI(NE,2),
      DIMENSION A(NE,NE),B(NE,NVSMB),BLOKA(NE,NE),KLI(NE,2),  ! 16 june 2000
     *          ZSN(NE,NSOL),SOLT(NE,NE)

      COMMON AUX(400),ZB(360)
C***********************************************************************
C
C     SUBROUTINE MDR                                                
C     ================
C     SUBROUTINE D'INITIALISATION DE LA MATRICE DE JONCTION. ELLE ORGANI-
C     SE EN BLOCS DE DONNEES PAR APPEL A LA SUBROUTINE RANG. LORSQUE LA 
C     MATRICE EST COMPLETE ELLE LA TRANSMET POUR RESOLUTION DANS LA     
C     SUBROUTINE RECIP POUR EN CALCULER SON INVERSE   

C     Modifié : 27-11-95  MODULE OPTIMISATION       Créer: Thèse de Ph. Rigo
C               29-03-99  Nbre de terme non limité
C                                                                       
C***********************************************************************
      IF(NEL.EQ.1) THEN
        DO 51 J=1,NE
        DO 50 I=1,NVSMB
   50   B(J,I)=0.
        DO 51 I=1,NE
   51   A(I,J)=0.
      ENDIF

c      WRITE(67,*) 'Avant Subr.ANGLE,  NEL=',NEL
c      DO 4008 I=1,16
c 4008 WRITE(67,4001) (AUX((II-1)*20+I),II=1,8),
c     *               (ZB(I+(II-1)*16),II=1,NSOL) 
c 4001 FORMAT(4(1X,E11.4)/4(1X,E11.4)/9(1X,E11.4)/)

      IF(NETO.EQ.1) GOTO 7

C     CHANGEMENT DE COORDONNEES (Repère local -> repère global)
c     =========================================================

      CALL ANGLE(NEL,PHIL,NVSMB,
     *           ANGL,NOEUD,NNO,NSIGN,NETO)

c      WRITE(67,*) 'Après Subr.ANGLE (NEL=',NEL
c      DO 4009 I=1,16
c 4009 WRITE(67,4002) I,(AUX((II-1)*20+I),II=1,8),
c     *                 (ZB(I+(II-1)*16),II=1,NSOL)
c 4002 FORMAT(I2,4(1X,E11.4)/4(1X,E11.4)/10(1X,E11.4)/)



C     CREATION DES MATRICES A ET B :(A*X=B).
c     ============================================

   7  CALL RANG(NEL,NE,A,B,NVSMB,NETO,NSOL,
     *          NOEUD,MCOMP,NNO,NC,NSIGN)

c      WRITE(67,*) 'Après RANG (NEL=',NEL
c      WRITE(67,*)' La matrice B des termes indépendants :'
c      DO 551 J=1,NE
c      WRITE(67,401)(A(J,I),I=1,NE)
c      WRITE(67,*)'J=',J,' ',(B(J,IS),IS=1,NSOL)
c  551 CONTINUE


      IF(NEL.NE.NETO) RETURN


C     DETERMINATION DE L'INVERSE DE LA MATRICE A.
c     ============================================
C     BLOKA et KLI sont des matrices de travail
C     Input  : A est la matrice à inverser
C     Output : A est la matrice inversée 

      IF(IMPR.EQ.1) THEN
        WRITE(66,*)' La matrice A à inverser est :'
        DO 554 J=1,NE                                                     
          WRITE(66,*)'Ligne nø ',J
          WRITE(66,401)(A(J,I),I=1,NE)
 554    CONTINUE                       
        WRITE(66,*)
        WRITE(66,*)' La matrice B des termes indépendants :'
        DO 555 J=1,NE                                                     
           WRITE(66,*)'J=',J,' ',(B(J,IS),IS=1,NSOL)
 555    CONTINUE                       
      ENDIF

      CALL RECIP(NE,A,BLOKA,KLI(1,1),KLI(1,2))

c      WRITE(66,*)
c      WRITE(66,*)' La matrice B des termes indépendants :'
c      DO 556 J=1,NE                                                     
c        WRITE(66,*)'Ligne nø ',J
c        WRITE(66,401)(B(J,IS),IS=1,NSOL)
c 556  CONTINUE                       


      IF(IMPR.EQ.1) THEN
        WRITE(66,*)' La matrice inverse est :'
        DO 553 J=1,NE                                                     
          WRITE(66,*)'Ligne nø ',J
          WRITE(66,401)(A(J,I),I=1,NE)
 553    CONTINUE
      ENDIF
                                        
C     Calcul des vecteurs solutions (ZSN et SOLT)
c     ============================================
      
c      DO 2001 IS=1,NSOL
c      IS=1
C      DO 2001 I=1,NE
c        I=1
c        ZSN(I,IS)=0.
c	   WRITE(66,*)
c	   WRITE(66,*)' DEBUG'
c	   WRITE(66,*) 'ZSN(I,IS)=',ZSN(I,IS)
c	   WRITE(66,*) 'ds boucle 2002'

c        DO 2003 J=1,NE
c	     WRITE(66,*) 'J=',J,'A(I,J) et B(J,IS)=',A(I,J),B(J,IS)
c          ZSN(I,IS)=ZSN(I,IS)+A(I,J)*B(J,IS)
c	     WRITE(66,*) 'ZSN(I,IS)=',ZSN(I,IS)
c 2003   CONTINUE
c 2001 CONTINUE

      DO 2001 IS=1,NSOL
      DO 2001 I=1,NE
        ZSN(I,IS)=0.
        DO 2002 J=1,NE
          ZSN(I,IS)=ZSN(I,IS)+A(I,J)*B(J,IS)
 2002   CONTINUE
 2001 CONTINUE

      IF(IMPR.EQ.1) THEN
      WRITE(66,*)' Le vecteur solution ZSN est :'
      DO 2005 IS=1,NSOL
        WRITE(66,*) 'Cas de charge nø ',IS
        WRITE(66,401) (ZSN(I,IS),I=1,NE)				
 2005 CONTINUE
      ENDIF

c      DO 121 K=1,(NVSMB-1)
c      K1=K+1
c        DO 2003 I=1,NE
c          DO 2004 J=1,NE
c 2004     SOLT(I,K)=SOLT(I,K)+A(I,J)*B(J,K1)
c 2003   CONTINUE
c      WRITE(66,*)' Le vecteur solution SOLT est:terme n°=',IJK
c      WRITE(66,401) (SOLT(I,K),I=1,NE)
c 121  CONTINUE

C     Sauvetage des vecteurs solutions (ZSN et SOLT)
c     ==============================================
      DO 2006 J=1,NETO
        DO  IS=1,NSOL
	     I1=(J-1)*8+1
           WRITE(700+J) (ZSN(I,IS),I=I1,I1+7)
        END DO
 2006 CONTINUE
      
C     Vérification de la solution (ZSN)
c     ============================================
c      WRITE(66,43)
c      DO 40 I=1,NE
c      AZ=0.
c      DO 41 J=1,NE
c   41 AZ=AZ+ A(I,J)*ZSN(J,1)
c      AZZ=AZ-B(I,1)
c   40 WRITE(66,42)I,AZ,B(I,1),AZZ
c   42 FORMAT(22X,I3,3(4X,E11.4))
c   43 FORMAT(//32X,' VERIFICATION DE L''INVERSION ',/32X,29(1H=)/22X,
c     *  'LIGNE',5X,'EQUATION',5X,'TERME INDEP',6X,'DIFFERANCE',/22X,
c     *   5(1H=),5X,8(1H=),5X,11(1H=),6X,10(1H=)/)

  401 FORMAT(8E17.10)
      RETURN
      END
