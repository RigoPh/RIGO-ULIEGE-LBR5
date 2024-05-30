      subroutine linea(m,coeff,dcoeff,j1,j2,j3,j4,x,sx,k,mt,ip,
     *                 isym,impr,q2pi,dpx,px,darg,nxi,nbrxi,
     *                 j,jk,iopti,sinu,cosi,expt)
      implicit real *8(a-h,o-z)
      dimension coeff(32,41),sx(360),ip(4),x(720),k(4),dcoeff(32,33,9),
     *          dpx(8,9),px(8),darg(8,9),nxi(9),
     *          j(4),jk(4)
      dimension aux(400),aux2(400),z(360)
      dimension sinu(4),cosi(4),expt(4)

c***********************************************************************
c     subroutine linea
c     ================
c     subroutine de calcul des effets d'une charge lineaire en x=0.
c     soit les 4 cas de base (coque non raidie et infinie)
c     les donnees se trouvent dans coeff et les resultats se trouvent
c     dans x (disc, disb, disa et disd).

c     dpx contient les dérivées des solutions px
c          c.à.d. a,b,c,d correspondant à 1 cas de sollicitation)

c     modif: 12-6-95                                     créer : 8-2-94
c            13-4-99 (lbr5.1)
c***********************************************************************
      do 111 i=1,720
  111 x(i)=0.

      j(1)=j1
      j(2)=j2
      j(3)=j3
      j(4)=j4

      z(1)=0.
      z(2)=0.
      z(3)=0.
      z(4)=-5000.

      do 11 i=1,4
      aux(i)=coeff(ip(1),j(i))-expt(k(1))*(coeff(ip(1),j(i))*cosi(k(1))
     *                                    -coeff(ip(2),j(i))*sinu(k(1)))
      aux(i+20)=coeff(ip(2),j(i))-expt(k(2))*(coeff(ip(2),j(i))*
     *       cosi(k(2))+coeff(ip(1),j(i))*sinu(k(2)))
      aux(i+40)=coeff(ip(3),j(i))-expt(k(3))*(coeff(ip(3),j(i))*
     *       cosi(k(3))-coeff(ip(4),j(i))*sinu(k(3)))
      aux(i+60)=coeff(ip(4),j(i))-expt(k(4))*(coeff(ip(4),j(i))*
     *       cosi(k(4))+coeff(ip(3),j(i))*sinu(k(4)))
      if(impr.eq.0) goto 11
      write(66,'(i3,4e12.5)') i,aux(i),aux(20+i),aux(40+i),aux(60+i)
   11 continue

      do 13 i=1,400
   13 aux2(i)=aux(i)

      call system(aux,z,4,1,sx,20)
      
      if(impr.eq.0) goto 112
      write(66,*) 'terme indép.'
      write(66,'(4e12.5)') (sx(ik),ik=1,4)
      write(66,*)  ' '
  112 continue

      go to(99,17,18,19),m
   18 px(1)=sx(1)
      px(2)=0.
      px(3)=sx(2)
      px(4)=0.
      px(5)=sx(3)
      px(6)=sx(4)
      go to 21
   19 do 20 ik=1,4
        px(2*ik)=0.
        px(2*ik-1)=sx(ik)
   20 continue
      go to 21
   17 do 22 ik=1,4
        px(ik)=sx(ik)
   22 continue
   21 continue

c     isym = 1  : si cas de charge symétrique   (disc et disb)
c     isym = -1 : si cas de charge antimétrique (disa et disd)
      call fillx(coeff,px,x, -3, 2,m,isym)
      call fillx(coeff,px,x, 13, 1,m,isym)
      call fillx(coeff,px,x, 29,16,m,isym)
      call fillx(coeff,px,x,365, 8,m,isym)
      call fillx(coeff,px,x,381, 4,m,isym)
      call fillx(coeff,px,x,397,14,m,isym)
      call fillx(coeff,px,x,413, 6,m,isym)
      call fillx(coeff,px,x,429,12,m,isym)
      call fillx(coeff,px,x,445, 3,m,isym)

      if(mt.eq.0) goto 10
      do 2 ik=1,mt
        jj=2*(ik-1)
        ijt1=17+jj
        ijt2=18+jj
        ji1=45+32*(ik-1)
        ji2=61+32*(ik-1)
        call fillx(coeff,px,x,ji1,ijt1,m,isym)
        call fillx(coeff,px,x,ji2,ijt2,m,isym)
    2 continue
   10 continue

c     deuxieme partie (calcul des pseudo-charges et résolution)
c     ----------------
      if(iopti.eq.0) goto 99

      do 50 ik=1,4
c     1 2   3   4  5   6   7  8  9  10  11  12 13 14 15 16
c     u v dw/dy ny nx nyx nxy my mx myx mxy qy qx ry rx  w
c     1 2   3   4      5      6                   7      8
c     u v dw/dy ny    nyx     my                  ry     w
      jk(ik)=j(ik)
      if(j(ik).eq.6)  jk(ik)=5
      if(j(ik).eq.8)  jk(ik)=6
      if(j(ik).eq.14) jk(ik)=7
      if(j(ik).eq.16) jk(ik)=8
   50 continue

c     termes indépendants (pseudo charge)
      do 32 l=1,nbrxi
      kl=nxi(l)
      do 12 ik=1,4
      aux(ik)=    dcoeff(ip(1),jk(ik),kl) - expt(k(1)) *
     * (  cosi(k(1)) * ( q2pi* (-darg(2*k(1)-1,kl)*coeff(ip(1),j(ik))
     *                          -darg(2*k(1),kl)  *coeff(ip(2),j(ik)) )
     *                   + dcoeff(ip(1),jk(ik),kl) )
     *  + sinu(k(1)) * ( q2pi* (+darg(2*k(1)-1,kl)*coeff(ip(2),j(ik))
     *                          -darg(2*k(1),kl)  *coeff(ip(1),j(ik)) )
     *                   - dcoeff(ip(2),jk(ik),kl) )   )
      aux(ik+20)= dcoeff(ip(2),jk(ik),kl) - expt(k(2)) *
     * (  cosi(k(2)) * ( q2pi* (-darg(2*k(2)-1,kl)*coeff(ip(2),j(ik))
     *                          +darg(2*k(2),kl)  *coeff(ip(1),j(ik)) )
     *                   + dcoeff(ip(2),jk(ik),kl) )
     *  + sinu(k(2)) * ( q2pi* (-darg(2*k(2)-1,kl)*coeff(ip(1),j(ik))
     *                          -darg(2*k(2),kl)  *coeff(ip(2),j(ik)) )
     *                   + dcoeff(ip(1),jk(ik),kl) )   )
      aux(ik+40)= dcoeff(ip(3),jk(ik),kl) - expt(k(3)) *
     * (  cosi(k(3)) * ( q2pi* (-darg(2*k(3)-1,kl)*coeff(ip(3),j(ik))
     *                          -darg(2*k(3),kl)  *coeff(ip(4),j(ik)) )
     *                   + dcoeff(ip(1),jk(ik),kl) )
     *  + sinu(k(3)) * ( q2pi* (+darg(2*k(3)-1,kl)*coeff(ip(4),j(ik))
     *                          -darg(2*k(3),kl)  *coeff(ip(3),j(ik)) )
     *                   - dcoeff(ip(4),jk(ik),kl) )   )
      aux(ik+60)= dcoeff(ip(4),jk(ik),kl) - expt(k(4)) *
     * (  cosi(k(4)) * ( q2pi* (-darg(2*k(4)-1,kl)*coeff(ip(4),j(ik))
     *                          +darg(2*k(4),kl)  *coeff(ip(3),j(ik)) )
     *                   + dcoeff(ip(4),jk(ik),kl) )
     *  + sinu(k(4)) * ( q2pi* (-darg(2*k(4)-1,kl)*coeff(ip(3),j(ik))
     *                          -darg(2*k(4),kl)  *coeff(ip(4),j(ik)) )
     *                   + dcoeff(ip(3),jk(ik),kl) )   )
   12 continue

      kk=(l-1)*20
      do 31 ii=1,4
        z(ii+kk)=-( aux(ii)*sx(1)+aux(ii+20)*sx(2)+aux(ii+40)*sx(3)
     *                           +aux(ii+60)*sx(4) )
   31 continue
   32 continue

c     corps de la matrice (idem cas normal)
      do 14 ik=1,400
   14 aux(ik)=aux2(ik)

      call system(aux,z,4,9,sx,20)

      do  46 l=1,nbrxi
        kl=nxi(l)
        ll=(l-1)*20
        go to(99,40,41,42),m
   41   dpx(1,kl)=sx(ll+1)
        dpx(2,kl)=0.
        dpx(3,kl)=sx(ll+2)
        dpx(4,kl)=0.
        dpx(5,kl)=sx(ll+3)
        dpx(6,kl)=sx(ll+4)
        go to 45
   42   do 43 ik=1,4
          dpx(2*ik,kl)=0.
          dpx(2*ik-1,kl)=sx(ll+ik)
   43   continue
        go to 45
   40   do 44 ik=1,4
          dpx(ik,kl)=sx(ll+ik)
   44   continue
   45   continue
        if(impr.ne.0) write(66,47) kl,(dpx(i,kl),i=1,8)
   46 continue

   47 format(' xi=',i2,8(1x,e14.7))

   99 return
      end

      subroutine fillx(coeff,px,x,k,l,m,isym)
      implicit real *8(a-h,o-z)
      dimension coeff(32,41),px(8),x(720)
c***********************************************************************
c
c     subroutine fillx
c     subroutine de remplissage des vecteurs solutions (disc, disca, etc
c     a partir des solutions des systemes d'equations. c'est une subrout
c     de rangement et de creation des coefficients.
c     modif: 8-2-94
c***********************************************************************
      do 24 i=1,m
      i2=i*2
      i8=i*8
      n=4*i+k
      x(n)  = coeff(i8-7,l)*px(i2-1)+coeff(i8-6,l)*px(i2)
      x(n+1)= coeff(i8-5,l)*px(i2-1)+coeff(i8-4,l)*px(i2)
      x(n+2)=(coeff(i8-3,l)*px(i2-1)+coeff(i8-2,l)*px(i2))*isym
   24 x(n+3)=(coeff(i8-1,l)*px(i2-1)+coeff(i8,l)  *px(i2))*isym
      return
      end
