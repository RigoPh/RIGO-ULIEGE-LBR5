      subroutine pedge(c1,s1,c2,s2,c3,s3,c4,s4,k,lr,is,da,db,dc,dd,
     *               ald,alz,aux,mt_max)
      implicit double precision (a-h,o-z)
      dimension aux(2*mt_max*2*mt_max),da(720),db(720),dc(720),dd(720)
c***********************************************************************
c
c     subroutine edge
c     subroutine de calcul des effets de bords sous l'ensemble des 8
c     forces aux coupures en x=0 et x=h,coefficients des inconnues
c     hyperstatiques de la matrice de jonction.la plaque est consideree
c     comme lisse.
c
c***********************************************************************
      ka=4*k+16*is-19
      aux(lr)=aux(lr)+dc(ka)*c1+dc(ka+1)*s1
     *                     +dc(ka+2)*c3+dc(ka+3)*s3
      aux(lr+20)=aux(lr+20)+(dc(ka)*c2+dc(ka+1)*s2
     *                     +dc(ka+2)*c4+dc(ka+3)*s4)*ald
      aux(lr+40)=aux(lr+40)+dd(ka)*c1+dd(ka+1)*s1
     *                     +dd(ka+2)*c3+dd(ka+3)*s3
      aux(lr+60)=aux(lr+60)+(dd(ka)*c2+dd(ka+1)*s2
     *                     +dd(ka+2)*c4+dd(ka+3)*s4)*alz
      aux(lr+80)=aux(lr+80)+da(ka)*c1+da(ka+1)*s1
     *                     +da(ka+2)*c3+da(ka+3)*s3
      aux(lr+100)=aux(lr+100)+(da(ka)*c2+da(ka+1)*s2
     *                       +da(ka+2)*c4+da(ka+3)*s4)*alz
      aux(lr+120)=aux(lr+120)+db(ka)*c1+db(ka+1)*s1
     *                       +db(ka+2)*c3+db(ka+3)*s3
      aux(lr+140)=aux(lr+140)+(db(ka)*c2+db(ka+1)*s2
     *                       +db(ka+2)*c4+db(ka+3)*s4)*ald
      return
      end
