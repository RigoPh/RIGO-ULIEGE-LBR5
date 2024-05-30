      SUBROUTINE SYSTEM(N,NS,X,NA)                                      PSY00010

      IMPLICIT REAL *8(A-H,O-Z)                                         PSY00020
      DIMENSION D(20),IPOS(20),X(NS*N)                                  PSY00030

      COMMON A(400),C(360)                                              PSY00040

C***********************************************************************PSY00050
C    SYSTEM est utilisé par LINEA mais aussi par BATEAU !!!!!!!!!!!                                                                    
C***********************************************************************PSY00050
C                                                                       
C     SUBROUTINE SYSTEM (N,NS,X,NA) + COMMON A,C                        
C     SUBROUTINE DE RESOLUTION D'UN SYSTEME DE N EQUATIONS A N INCONNUES 
C     PAR LA METHODE DES PIVOTS.                                        

C     Soit le systeme [A] X = C 
C	 A la matrice,  C le vect indep. et X les inconnues (solutions)
C	 C peut contenir NS vecteurs indépendants

C     N  = Nbr d'inconnues  (taille du système)
C     NA = DIMENSION DE LA MATRICE [A] QUE L'ON FORME HORS DU VECTEUR 
C          SITUE DANS LE COMMON (NA=20)        
C     NS = NBRE DE VECTEURS INDEPENDANTS (ICI "NS" MAX VAUT 340/20=17)    

C     Dimensions nécessaires : A(NA,NA);X(NA*NS);C(NA*NS)    		
C     NB: Les vecteurs D et IPOS sont des vecteurs de travail.

C     Modif: 21-8-94                         Création : Thèse de Ph. Rigo
C            Avril 99 (LBR5.1)
C***********************************************************************PSY00160
      DO 10 K=1,N                                                       PSY00170
      IPOS(K)=0                                                         PSY00180
   10 D(K)=A(K)                                                         PSY00190
      CALL PMAXAR(D,1,N,UMAX,IP,NA)                                     PSY00200
      A(1)=UMAX                                                         PSY00210
      IPOS(1)=IP                                                        PSY00220
      D(IP)=D(1)                                                        PSY00230
      DO 15 I=2,N                                                       PSY00240
   15 A(I)=D(I)/A(1)                                                    PSY00250
      DO 100 I=2,N                                                      PSY00260
      DO 20 J=1,N                                                       PSY00270
      IA=J+(I-1)*NA                                                     PSY00280
   20 D(J)=A(IA)                                                        PSY00290
      JAUX=I-1                                                          PSY00300
      DO 50 K=1,JAUX                                                    PSY00310
      IP=IPOS(K)                                                        PSY00320
      IA=K+(I-1)*NA                                                     PSY00330
      A(IA)=D(IP)                                                       PSY00340
      D(IP)=D(K)                                                        PSY00350
      JAUXP=K+1                                                         PSY00360
      DO 50 KL=JAUXP,N                                                  PSY00370
      IAL=KL+(K-1)*NA                                                   PSY00380
   50 D(KL)=D(KL)-A(IAL)*A(IA)                                          PSY00390
      CALL PMAXAR(D,I,N,UMAX,IP,NA)                                        PSY00400
      II=I+(I-1)*NA                                                     PSY00410
      A(II)=UMAX                                                        PSY00420
      IPOS(I)=IP                                                        PSY00430
      D(IP)=D(I)                                                        PSY00440
      IX=I+1                                                            PSY00450
      IF(IX-N) 31,31,100                                                PSY00460
   31 DO 60 KM=IX,N                                                     PSY00470
      IAM=KM+(I-1)*NA                                                   PSY00480
   60 A(IAM)=D(KM)/A(II)                                                PSY00490
  100 CONTINUE                                                          PSY00500
      DO 505 IJ=1,NS                                                    PSY00510
      DO 110 I=1,N                                                      PSY00520
      IP=I+(IJ-1)*NA                                                    PSY00530
  110 D(I)=C(IP)                                                        PSY00540
      NM=N-1                                                            PSY00550
      DO 200 I=1,NM                                                     PSY00560
      IP=IPOS(I)                                                        PSY00570
      IR=I+(IJ-1)*NA                                                    PSY00580
      C(IR)=D(IP)                                                       PSY00590
      D(IP)=D(I)                                                        PSY00600
      IJK=I+1                                                           PSY00610
      DO 200 J=IJK,N                                                    PSY00620
      JI=J+(I-1)*NA                                                     PSY00630
  200 D(J)=D(J)-A(JI)*C(IR)                                             PSY00640
      NN=N+(N-1)*NA                                                     PSY00650
      NX=N+(IJ-1)*NA                                                    PSY00660
      X(NX)=D(N)/A(NN)                                                  PSY00670
      NAUX=N-1                                                          PSY00680
      DO 500 I=1,NAUX                                                   PSY00690
      IZ=N-I                                                            PSY00700
      IZP=IZ+1                                                          PSY00710
      IZC=IZ+(IJ-1)*NA                                                  PSY00720
      D(IZ)=C(IZC)                                                      PSY00730
      DO 501 J=IZP,N                                                    PSY00740
      IZJ=IZ+(J-1)*NA                                                   PSY00750
      IZX=J+(IJ-1)*NA                                                   PSY00760
  501 D(IZ)=D(IZ)-A(IZJ)*X(IZX)                                         PSY00770
      IZI=IZ+(IZ-1)*NA                                                  PSY00780
      IZX=IZ+(IJ-1)*NA                                                  PSY00790
  500 X(IZX)=D(IZ)/A(IZI)                                               PSY00800
  505 CONTINUE                                                          PSY00810
   51 RETURN                                                            PSY00820
      END                                                               PSY00830


      SUBROUTINE PMAXAR(X,IB,IE,UMAX,IS,NA)                             PSY00840
      IMPLICIT REAL *8(A-H,O-Z)                                         PSY00850
      DIMENSION X(NA)                                                   PSY00860
C***********************************************************************PSY00870
C                                                                       PSY00880
C     SUBROUTINE MAXAR                                                  PSY00890
C     SUBROUTINE DE RECHERCHE D'UNE VALEUR MAXIMUM EN VALEUR ABSOLUE    PSY00900
C     DANS UN GROUPE DE VALEURS DONNEES.                                PSY00910
C                                                                       PSY00920
C***********************************************************************PSY00930
      DUMAX=X(IB)                                                       PSY00940
      IS=IB                                                             PSY00950

      IF(IB-IE) 15,20,20                                                PSY00960

   15 IBP=IB+1                                                          PSY00970
      DO 10 I=IBP,IE                                                    PSY00980
        IF(DABS(DUMAX)-DABS(X(I))) 11,10,10                               PSY00990
   11   DUMAX=X(I)                                                        PSY01000
        IS=I                                                              PSY01010
   10 CONTINUE                                                          PSY01020

   20 UMAX=DUMAX                                                        PSY01030

      RETURN                                                            PSY01040
      END                                                               PSY01050
