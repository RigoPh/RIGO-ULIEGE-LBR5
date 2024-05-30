      SUBROUTINE PLIMIT(A,B,C,S,CC,SS,I,K,MT,L,N,ALI)
      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION A(464),B(464),ALI(360)
C***********************************************************************
C
C     SUBROUTINE LIMIT   (effet des traverses)
C     ------------------
C     LA SUBROUTINE CALCULE les 8 GRANDEURS UTILES '(u,v,w,wø,My,Ny,Ry et Nxy)
C     SUR LES BORDS DU PANNEAUX SOUS L'ACTION DES EFFETS CONCENTRES unitaires 
C     de type XODY ET ZODY agissant au NIVEAU DES TRAVERSES.
C
C***********************************************************************
      IJ=L*20+I
      KH=4*K+(N-1)*16-3
      ALI(IJ)=ALI(IJ)+B(KH)*C+B(KH+1)*S+B(KH+2)*CC+B(KH+3)*SS
      J=IJ+MT
      ALI(J)=ALI(J)+A(KH)*C+A(KH+1)*S+A(KH+2)*CC+A(KH+3)*SS
      RETURN
      END
