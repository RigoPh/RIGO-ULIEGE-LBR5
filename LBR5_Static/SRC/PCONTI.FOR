
      SUBROUTINE PCONTI(A,B,C,D,E,K,I,S,X)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(720),B(720),C(720),D(720),E(8),S(1710)
C***********************************************************************
C
C     SUBROUTINE CONTI
C     LA SUBROUTINE AJOUTE AUX COEFFICIENTS DISH LES COEFFICIENTS DUS
C     AUX CHARGES UNITAIRES DE BORDS DU PANNEAU MULTIPLIEES PAR LES
C     INCONNUES HYPERSTATIQUES DES CHARGES UNITAIRES DE BORDS.
C     L'ENSEMBLE EST PLACE DANS DISH
C
C***********************************************************************
      KH=(I-1)*38+8*K-1
      IJ=(I-1)*16+4*K-3
      S(KH)=S(KH)+E(1)*C(IJ)+E(3)*D(IJ)+E(5)*A(IJ)+E(7)*B(IJ)
      S(KH+1)=S(KH+1)+E(1)*C(IJ+1)+E(3)*D(IJ+1)
     *               +E(5)*A(IJ+1)+E(7)*B(IJ+1)
      S(KH+2)=S(KH+2)+(E(2)*C(IJ)-E(4)*D(IJ)-E(6)*A(IJ)+E(8)*B(IJ))*X
      S(KH+3)=S(KH+3)+(E(2)*C(IJ+1)-E(4)*D(IJ+1)-E(6)*A(IJ+1)
     *                                          +E(8)*B(IJ+1))*X
      S(KH+4)=S(KH+4)+E(1)*C(IJ+2)+E(3)*D(IJ+2)+E(5)*A(IJ+2)
     *               +E(7)*B(IJ+2)
      S(KH+5)=S(KH+5)+E(1)*C(IJ+3)+E(3)*D(IJ+3)
     *               +E(5)*A(IJ+3)+E(7)*B(IJ+3)
      S(KH+6)=S(KH+6)+X*(E(2)*C(IJ+2)-E(4)*D(IJ+2)
     *                  -E(6)*A(IJ+2)+E(8)*B(IJ+2))
      S(KH+7)=S(KH+7)+X*(E(2)*C(IJ+3)-E(4)*D(IJ+3)
     *                  -E(6)*A(IJ+3)+E(8)*B(IJ+3))
      RETURN
      END
