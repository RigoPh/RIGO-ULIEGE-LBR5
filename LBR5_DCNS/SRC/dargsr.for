      subroutine dargsr(lam,dequ,q,const,a,b,c,d,f,h,g,x,w,z,bc,cw,bw,
     *        xw,dxy,wc,oxy,rxy,gh,eta,eta2,k,da,nxi,nbrxi,ss,tt,itype,
     *        const2)

c     avec k = le numéro de la variables de conception (k=1,9)

      implicit real*8(a-h,o-z)
      real*8 lam
      dimension const(74),lam(8),dequ(9),da(9),nxi(9)
      dimension const2(6,9)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c       la routine darg calcule les dérivées des coéfficients (equ) de l
c     du 8ème ordre.
c
c  modif: 14-5-95                                  créer : 25-1-94
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       nxi(i) = les numeros des variables selectionnees ( i=1,nbrxi )                                                       880
c          bordage      1   epaisseur du bordage
c          aiguilles    2   hauteur de l'âme des aiguilles
c                       3   epaisseur de l'âme des aiguilles
c                       4   largeur de la semelle des aiguilles
c                       5   entredistance entre aiguilles
c          raidisseurs  6   hauteur de l'âme des raidisseurs
c                       7   epaisseur de l'âme des raidisseurs
c                       8   largeur de la semelle des raidisseur
c                       9   entredistance entre raidisseurs
c  input:
c      var. de concept. = epaisseur du bordage (k=1)
c      ---------------------------------------------
c        const(1)= d  -> const2(1,1)=d(d)/d(épaiss)
c        const(2)= k  -> const2(2,1)=d(k)/d(épaiss)
c        const(3)= hy -> const2(3,1)=d(hy)/d(épaiss)
c        const(4)= ry -> const2(4,1)=d(ry)/d(épaiss)
c        const(5)= hx -> const2(5,1)=d(hx)/d(épaiss)
c        const(6)= rx -> const2(6,1)=d(rx)/d(épaiss)

c      var. de concept. relative aux aig.(k=2,5) et aux raid.(k=6,9)
c      ---------------------------------------------------------------
c      constante      dérivée(k=2,5)   constante       dérivée(k=6,9)
c    1 const(3)= oy -> const2(1,k)     const( 9)= ox -> const2(1,k)
c    2 const(4)= hy -> const2(2,k)     const(10)= hx -> const2(1,k)
c    3 const(5)= sy -> const2(3,k)     const(11)= sx -> const2(1,k)
c    4 const(6)= ry -> const2(4,k)     const(12)= rx -> const2(1,k)
c    5 const(7)= ty -> const2(5,k)     const(13)= tx -> const2(1,k)
c    6 const(8)= ly -> const2(6,k)     const(14)= lx -> const2(1,k)

c     z= d*(1-eta)/2+(sx+sy)            eta2 = (1-eta)/2
c     a= d*(1+eta)/2+(sx+sy)            bc =(k+rx)*(d+ox)-(hx)**2
c     b= (k+rx)                         xw =(k+ry)*(d+oy)-(hy)**2
c     c= (d+ox)                         cw =(d+ox)*(d+oy)-(d*eta)**2
c     d= 2k+(tx+ty)                     bw =(k+rx)*(d+oy)+(k+ry)*(d+ox)
c     f= (lx+ly)                        dxy=2d*(1.-eta**2)+ox+oy
c     g= d*eta                          oxy=2d+(ox+oy)
c     h= d+2(sx+sy)                     rxy=2k+(rx+ry)
c     x= (k+ry)                         wc =hx*(d+oy)+hy*(d+ox)
c     w= (d+oy)                         gh =(d+ox)*(d+oy)-d*eta *(d+2(sx

c  output:
c     aetoil=da(1), betoil=da(2), cetoil=da(3), detoil=da(4), eetoil=da(
c     fetoil=da(6), getoil= 0     ietoil=da(7), jetoil=da(8), ketoil=da(

c      write(66,*) 'subr darg','  k=',k

	if(itype.ne.5) then													!février 2004
        goto(1,2,2,2,2,3,3,3,3),k
	else																!février 2004
	  goto (5,5,5,5,5),k												!février 2004
	endif																!février 2004
c       variable de conception nø1 : epaisseur du bordage
c     __________________________________________________
   1    c1=const2(1,1)
        c2=const2(2,1)

      da(1)=eta2*bc*const2(1,1) + z*( c*(const2(2,1)+const2(6,1))
     *                        +b*const2(1,1)-2.*const(10)*const2(5,1) )
      da(3)=( (-eta*f-const(10))*const(10) + (b*w+d*z) - eta*b*h )*
     *  const2(1,1) + c*((w+2.*z)*const2(2,1)+(b+eta2*d)*const2(1,1))
     *              - g*(h*const2(2,1)+b*const2(1,1))
     *              - (g*f+2.*const(10)*w)*const2(5,1) + gh*const2(6,1)
      da(6)=const2(1,1) * ( (1.+eta)*const(4)*const(10)
     *  + f*(const(4)+const(10)) + d*(oxy-eta*h-g) + eta2*bw )
     *  +2.*const2(2,1)*gh + z*(oxy*const2(2,1)+rxy*const2(1,1))
     *  +2.*a* (const(10)*const2(3,1)+const(4)*const2(5,1))
     *  +   f* (   c     *const2(3,1)+   w    *const2(5,1))
     *  +   z* (   c     *const2(4,1)+   w    *const2(6,1))
      da(8)=((-eta*f-const(4))*const(4)+(x*c+d*z)-eta*x*h )*const2(1,1)
     *              + w*((c+2.*z)*const2(2,1)+(x+eta2*d)*const2(1,1))
     *              - g*(h*const2(2,1)+x*const2(1,1))
     *              - (g*f+2.*const(4)*c)*const2(3,1) + gh*const2(4,1)
      da(9)=eta2*xw*const2(1,1) + z*( w*(const2(2,1)+const2(4,1))
     *                        +x*const2(1,1)-2.*const(4)*const2(3,1) )
      if(q.ge.(1.e5)) then
c         cas d'une plaque raidie  (rayon très grand)
          da(2)=0.d+00
          da(4)=0.d+00
          da(5)=0.d+00
          da(7)=0.d+00
        else
c         cas d'une coque raidie  (rayon <100.000 m)
          da(2)=2.*eta*const(10)*(z+const(1)*eta2)*const2(1,1)/q
     *        + 2.*g*z*const2(5,1)/q
          da(4)=const2(1,1)*(eta2*cw+z*dxy)/(q*q)
          da(5)=-( f*dxy+2.* (eta2*wc+z*(const(4)+const(10)) )  )
     *                                              * const2(1,1)/q
     *         - 2.*z* (w*const2(5,1)+c*const2(3,1)) /q
          da(7)= 2.*eta*const(4)*const2(1,1)*(z+const(1)*eta2) /q
     *         + 2.*g*z*const2(3,1)/q
      endif
      goto 4

c       variable de conception liées aux aiguilles : k=2,3,4,5
c     _____________________________________________________
   2    doy=const2(1,k)
        dhy=const2(2,k)
        dsy=const2(3,k)
        dry=const2(4,k)
        dty=const2(5,k)
        dly=const2(6,k)

        da(1)= dsy * bc
      da(3)=-g*(const(10)*dly+2.*b*dsy) - const(10)*const(10)*doy
     *        + c*(b*doy+d*dsy+z*dty)
      da(6)=2.*const(10)*(const(4)*dsy+a*dhy) + wc*dly
     *  + doy*(f*const(10)+d*c+z*b) + f*c*dhy + (c*w-g*h)*dty
     *  + (-2.*g*d+bw)*dsy + z*c*dry
      da(8)=-g*(f*dhy+const(4)*dly+h*dry+2.*x*dsy) - 2.*c*dhy*const(4)
     *     + doy*(x*c+d*z) + w*(c*dry+d*dsy+z*dty)
      da(9)=dsy*xw + z*(-2.*const(4)*dhy+w*dry+x*doy)
      if(q.ge.(1.e5)) then
c         cas d'une plaque raidie  (rayon très grand)
          da(2)=0.d+00
          da(4)=0.d+00
          da(5)=0.d+00
          da(7)=0.d+00
        else
c         cas d'une coque raidie  (rayon <10000.000 m)
          da(2)=2.*g* const(10)*dsy /q
          da(4)=(cw*dsy+z*c*doy)/(q*q)
          da(5)=-(cw*dly+f*c*doy+2.*(wc*dsy+z*(const(10)*doy+c*dhy)))/q
          da(7)= 2.*g* (z*dhy+const(4)*dsy) /q
      endif
      goto 4

c       variable de conception liées aux raidisseurs : k=6,7,8,9
c     _______________________________________________________
   3    dox=const2(1,k)
        dhx=const2(2,k)
        dsx=const2(3,k)
        drx=const2(4,k)
        dtx=const2(5,k)
        dlx=const2(6,k)

      da(1)=dsx*bc + z*(-2.*const(10)*dhx+c*drx+b*dox)
      da(3)=-g*(f*dhx+const(10)*dlx+h*drx+2.*b*dsx) -2.*w*dhx*const(10)
     *     + dox*(b*w+d*z) + c*(w*drx+d*dsx+z*dtx)
      da(6)=2.*const(4)*(const(10)*dsx+a*dhx) + wc*dlx
     *  + dox*(f*const(4)+d*w+z*x) + f*w*dhx + (c*w-g*h)*dtx
     *  + (-2.*g*d+bw)*dsx + z*w*drx
        da(8)=-g*(const(4)*dlx+2.*x*dsx) - const(4)*const(4)*dox
     *        + w*(x*dox+d*dsx+z*dtx)
      da(9)=dsx*xw
      if(q.ge.(1.e5)) then
c         cas d'une plaque raidie  (rayon très grand)
          da(2)=0.d+00
          da(4)=0.d+00
          da(5)=0.d+00
          da(7)=0.d+00
        else
c         cas d'une coque raidie  (rayon <10000.000 m)
          da(2)=2.*g* (z*dhx+const(10)*dsx) /q
          da(4)=(cw*dsx+z*w*dox)/(q*q)
          da(5)=-(cw*dlx+f*w*dox+2.*(wc*dsx+z*(const(4)*dox+w*dhx)) )/q
          da(7)= 2.*g*const(4)*dsx /q
      endif
      goto 4
c	 variable de conception liées aux épontilles : k=1,2,3,4,5			!février 2004
c	 -----------------------------------------------------------		!février 2004
c      constante      dérivée(k=1,5)   constante							!février 2004
c    1 const(3)= oy -> const2(1,k)     const( 9)= ox						!février 2004
c    2 const(4)= hy -> const2(2,k)     const(10)= hx						!février 2004
c    3 const(5)= sy -> const2(3,k)     const(11)= sx						!février 2004
c    4 const(6)= ry -> const2(4,k)     const(12)= rx						!février 2004
c    5 const(7)= ty -> const2(5,k)     const(13)= tx						!février 2004
c    6 const(8)= ly -> const2(6,k)     const(14)= lx						!février 2004

    5   if(itype.ne.5) goto 4												!février 2004
        doy=const2(1,k)													!février 2004
        dhy=const2(2,k)													!février 2004
        dsy=const2(3,k)													!février 2004
        dry=const2(4,k)													!février 2004
        dty=const2(5,k)													!février 2004
        dly=const2(6,k)													!février 2004

      da(1)= dsy *( const(12)*const(9)-const(10)**2)						!février 2004
      da(3)=- const(10)*const(10)*doy										!février 2004
     *        + const(9)*(const(12)*doy+tt*dsy+ss*dty)					!février 2004
      da(6)=2.*const(10)*ss*dhy											!février 2004
     *  + doy*(tt*const(9)+ss*const(12)) + const(9)*const(3)*dty			!février 2004
     *  + (const(12)*const(3)+const(6)*const(9)							!février 2004
     *  +2*const(4)*const(10))*dsy + ss*const(9)*dry						!février 2004
      da(8)=doy*(const(6)*const(9)+tt*ss)									!février 2004
     *   + const(3)*(const(9)*dry+tt*dsy+ss*dty)-2*const(4)*const(9)*dhy	!février 2004		
      da(9)=dsy*(const(6)*const(3)-const(4)*const(4))						!février 2004
     *	 + ss*(const(3)*dry+const(6)*doy-2*const(4)*dhy)				!février 2004

          da(2)=0.d+00													!février 2004
          da(4)=0.d+00													!février 2004
         da(5)=0.d+00														!février 2004
          da(7)=0.d+00													!février 2004

    4 continue
      call assemb(lam,dequ,da(1),da(2),da(3),da(4),da(5),da(6),da(7),
     *                    da(8),da(9))

      return
      end


