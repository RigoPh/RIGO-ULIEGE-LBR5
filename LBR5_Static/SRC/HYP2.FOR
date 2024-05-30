      SUBROUTINE HYP2(D,MT,ZB,TM,TN,TP,TQ,TMM,TNN,TPP,TQQ,KH,I,K)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION D(1710),ZB(2*MT,17)

C***********************************************************************
C    CALCULE LES TERMES INDEPENDANTS DU SYSTEME EN VUE DE DETERMINER
C    L'EFFET DES TRAVERSES SUR LA COQUE.
C    ET CELA POUR LA CHARGE EXTERIEURE (D=DISH)
C                                                                       
C     modifié: le 21-6-95 (Ph. Rigo)                        Créer : Thèse        
C***********************************************************************
      ZB(I,K)=ZB(I,K)+D(KH)*TM+D(KH+1)*TN+D(KH+2)*TP+D(KH+3)*TQ
     *    +D(KH+4)*TMM+D(KH+5)*TNN+D(KH+6)*TPP+D(KH+7)*TQQ
      J=I+MT
      ZB(J,K)=ZB(J,K)+D(KH+38)*TM+D(KH+39)*TN+D(KH+40)*TP+D(KH+41)*TQ
     *       +D(KH+42)*TMM+D(KH+43)*TNN+D(KH+44)*TPP+D(KH+45)*TQQ
      RETURN
      END
