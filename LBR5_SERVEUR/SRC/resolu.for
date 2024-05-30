      subroutine resolu(ms,nbrequ,aux,zb)

      use param_section
!      use param_cout
!      use param_opti_local

      implicit double precision(a-h,o-z)
      dimension aux(ms,ms),zb(ms,nbrequ)

c***********************************************************************
c
c     subroutine resolu (remplacement de la sub. system)
c     ================
c     subroutine de résolution du système de calcul des traverses.
c           (aux) * hyp = zb
c     soit aux la matrice ; aux(2*mt,2*mt)
c          zb  le vecteur indépendant (nbrequ cas) ; zb(2*mt,nbrequ)
c          hyp le vecteur solution (nbrequ cas) ; hyp(360=(8+nsolmax)*2*mt_max)
c
c     modifié : 21-6-95 (module optimisation)              créer: 21-6-95
c                                                                      
c***********************************************************************
c     determination de l'inverse de la matrice aux.
c     ============================================
c     bloka et kli sont des matrices de travail
c     input  : aux est la matrice à inverser
c     output : aux est la matrice inversée


      call recip(ms,ms,aux)

                                        
c     calcul des vecteurs solutions (hyp)
c     ============================================
      do 121 k=1,nbrequ
        k1=(k-1)*20
        do 2003 i=1,ms
          ik=i+k1
          do 2004 j=1,ms
 2004     hyp(ik)=hyp(ik)+aux(i,j)*zb(j,k)
 2003   continue
 121  continue

  401 format(i2,1x,8(e11.4,1x))                  

      return
      end
