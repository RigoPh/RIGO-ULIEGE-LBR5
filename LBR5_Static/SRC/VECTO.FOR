      SUBROUTINE VECTO(A,C,S,I,K,MT,L,SY,CC,SS,ZB)

      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION A(464),ZB(2*MT,17)

C***********************************************************************
C                                                                       
C     SUBROUTINE VECTOR (effet des traverses)
C     ------------------------------------------
C     SUBROUTINE DE CREATION DU VECTEUR ZB REPRESENTANT LES TERMES
C     INDEPENDANTS DU SYSTEME de calcul des inconnues hyperstatiques
C     des traverses (Xo et Zo).
C
C     ZB correspond aux 8 CAS DE CHARGE agissant sur les
C     bords du panneau (c.à.d DISA, DISB, DISC et DISD en Y=0 et Y=PHIL) 
C                                                                       
C     modifié: le 21-6-95 (Ph. Rigo)                        Créer : Thèse        
C***********************************************************************
C  avec I nø de la traverse
C       L nø du cas de sollicitation (2 à 9)

      KH=4*K+45+32*(I-1)
      ZB(I,L)=ZB(I,L)+(A(KH)*C+A(KH+1)*S+A(KH+2)*CC+A(KH+3)*SS)*SY
      J=I+MT
      ZB(J,L)=ZB(J,L)+(A(KH+16)*C+A(KH+17)*S+A(KH+18)*CC+A(KH+19)*SS)*SY
      RETURN
      END
