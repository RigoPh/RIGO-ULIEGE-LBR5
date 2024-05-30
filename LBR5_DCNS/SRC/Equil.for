      subroutine equil(mt,z2,lamb,xi,xf,ivaria,phil,ijk,kse,q,
     *                 nel,ind,aaa,bbb,ccc,ddd,aa1,bb1,cc1,dd1,delto,
     *                 width)
      use sharedvar
      implicit real*8(a-h,o-z)
      real*8 lamb
      dimension z2(2295),a1(10)

c***********************************************************************
c
c     subroutine equil
c     subroutine de verification de l'equilibre du panneau.
c     on considere l'equilibre selon les trois axes ox,oy,oz.
c
c    modif :17-8-94                              créer : thèse ph. rigo
c***********************************************************************

      s0=dsin(pi*ijk/2.)
      teta=tetaq(nel)
      phx=0.
      fmo=0.
      fmv=0.

      sp1=vsin(phil,0.d00)
      cp1=vcos(phil,0.d00)
      sp2=vsin(phil/2.,0.d00)
      cp2=vcos(phil/2.,0.d00)
      cc1=vcos(teta,0.d00)
      cc2=vcos(teta,phil)
      cc3=vcos(teta,phil/2.)
      c4=vcos(2.*teta,phil)
      s1=vsin(teta,0.d00)
      s2=vsin(teta,phil)
      s3=vsin(teta,phil/2.)
      s4=vsin(2.*teta,phil)
      qphil=q*phil*pi/180.


c     efforts de bord
c     ----------------
      if(ind.eq.0) goto 300
      f=aaa*(qphil**4)/4.+bbb*(qphil**3)/3.+ccc*(qphil**2)/2.+ddd*qphil
      c5=dcos(ijk*pi*(width-2.*delto)/(2.*width))
      phx=4.*width*f*c5/(ijk**2*pi*pi*delto)

      cst=8.*s0*c5/(ijk*pi*delto)
      if(phil.le.0.000001) then
      p=(aa1*(qphil**3)/4.+bb1*(qphil**2)/3.+cc1*qphil/2.+dd1)*qphil
      fmo=cst*p*cc1
      fmv=cst*p*s1
      else
      p1=aa1*(qphil**3)+bb1*(qphil**2)+cc1*qphil+dd1
      p2=(3.*aa1*(qphil**2)+2.*bb1*qphil+cc1)*q
      p3=(6.*aa1*qphil+2.*bb1)*q*q
      p4=(q**3)*6.*aa1
      fmo=(p1-p3)*s2+(p2-p4)*cc2
     *              +s1*(2.*bb1*q*q-dd1)+cc1*(6.*aa1*(q**3)-cc1*q)
      fmv=(p3-p1)*cc2+(p2-p4)*s2
     *              -cc1*(2.*bb1*q*q-dd1)+s1*(6.*aa1*(q**3)-cc1*q)
      fmo=fmo*cst*q
      fmv=fmv*cst*q
      endif

c     presion hydrostatique
c     ----------------------
  300 aa=q*8.*9810./(lamb*pi*ijk)*((-1)**kse)

      if(ivaria.eq.0)goto 301
c
c     ivaria=1  la pression varie lineairement le long de l'axe y (subr.
c
      if(phil.le.1.e-05) goto 302
      t1=-2.*s3*sp2/(phil*pi/180.)+s2
      pho=aa*(t1*(xf-xi)+xi*2.*cc3*sp2)
      t1=2.*cc3*sp2/(phil*pi/180.)-cc2
      phv=aa*(t1*(xf-xi)+xi*2.*s3*sp2)
      goto 304
  302 t1=(xf+xi)*(phil*pi/180.)/2.
      pho=t1*cc1*aa
      phv=t1*s1*aa
      goto 304

c     ivaria=0  la pression varie avec la profondeur (subr. hydro)

  301 if(phil.le.1.e-05) then
      pho=(2.*xi+qphil*cc1)*(phil*pi/180.)/2.
      phv=pho*s1*aa
      pho=pho*cc1*aa
      else
      t1=q*(0.5*s4*sp1-2.*s1*cc3*sp2)
      pho=aa*(t1+xi*2.*cc3*sp2)
      t1=q*(phil*pi/(180.*2.)-0.5*sp1*c4-2.*s1*s3*sp2)
      phv=aa*(t1+xi*2.*s3*sp2)
      endif

c     recherche de la composante maximale

  304 write(66,100) pho,fmo,phv,fmv,phx
      ph=dmax1(dabs(pho),dabs(phv),dabs(phx),dabs(fmo),dabs(fmv))
      if(ph.ge.phmax) then
      phmax=ph
      else
      ph=phmax
      endif

c     equilibre suivant l'axe verticale
c     -----------------------------------
      call pinteg(phil,teta,40,45,som,z2,mt,q,pi,abtr,cc1,cc2,
     *            s1,s2,3,bof)

      temp=(z2(1225)*cc1-z2(1276)*s1-z2(1255)*cc2+z2(1306)*s2)
     *  *2./lamb-2*((z2(2092)+z2(2143))*s1-(z2(2122)+z2(2173))*s2)
      som=som*2.+temp

      if(mt.eq.0)goto 200
      do 201 i=1,mt
      ji=6*(i-1)

      a1(i)= lamb**2* (-lamb*const(18+ji)*z2(2*i+132)  +
     * const(16+ji)*z2(2*i+81)  ) + const(19+ji)*lamb*z2(2*i+1764)
     * + (const(8)+const(14)) * (z2(2*i+1561)-z2(2*i+1560))
     * +  const(20+ji)*(z2(2*i+1509)*lamb+z2(2*i+1713))
  201 som=som-2.*a1(i)*dsin((teta+abtr(i))*pi/180.)

  200 som=som+phv+fmv
      write(66,101)som
      if(dabs(ph).lt.0.01) goto 1
      pe=som/ph
      write(66,105)pe
c
c     equilibre suivant l'axe ox
c     --------------------------
c
    1 call pinteg(phil,teta,39,0,som,z2,mt,q,pi,abtr,cc1,cc2,
     *            s1,s2,2,vmax)

      som=(som+((z2(1357)-z2(1327))/lamb))*s0+phx

      if(mt.eq.0)goto 5
      do 34 i=1,mt
      ji=6*(i-1)

      sam= ( lamb *(-const(15+ji)*z2(2*i+81)+lamb*const(16+ji)
     *    *z2(2*i+132))   + const(17+ji)* z2(2*i+1509)
     *                    + const(17+ji)* z2(2*i+1713)/lamb  )*s0

      som=som+sam

  34  continue
   5  write(66,102)som
      ph2=dmax1(vmax,ph)
      if(ph2.lt.0.01) goto 2
      pe=som/ph2
      write(66,105)pe

c     equilibre suivant l'axe horizontal
c     -----------------------------------
c
  2   call pinteg(phil,teta,45,40,som,z2,mt,q,pi,abtr,cc1,cc2,
     *            s1,s2,1,bof)
      temp= 2.*((z2(2092)+z2(2143))*cc1-(z2(2122)+z2(2173))*cc2)
     * +(2./lamb)*(z2(1276)*cc1+z2(1225)*s1-z2(1306)*cc2-z2(1255)*s2)
      som=(som*2.)+temp
      som=som-pho-fmo
      if(mt.eq.0)goto 6
      do 33 i=1,mt
      ji=6*(i-1)
      sam=2.*a1(i)*dcos((teta+abtr(i))*pi/180.)
   33 som=som+sam
    6 write(66,103)som

      if(dabs(ph).lt.0.01) goto 3
      pe=som/ph
      write(66,105)pe
    3 continue

  100 format( /,2x,' equilibre suivant les trois axes'///
     *1x,'valeur de la charge hydraulique horizontal pour ce terme'
     *,t65,e14.7,2x,2hn.//
     *1x,'composante horizontale du aux moments de bord pour ce terme'
     *,t65,e14.7,2x,2hn.//1x,'valeur de la charge ',
     *'hydraulique verticale pour ce terme',t65,e14.7,2x,'n.'//1x,
     *'composante verticale du aux moments de bord pour ce terme'
     *,t65,e14.7,2x,2hn.//1x,'valeur de la charge ',
     *'repartie de bord pour ce terme',t65,e14.7,2x,'n.'//)
  101 format(2x,'axe vert  :resultante des efforts = erreur absolue'/t36
     *,' =',e14.7,2x,7hnewton.)
  105 format(9x,'erreur relative par rapport a la composante maximale',
     * ' de la ph'/t36,' =',e14.7/)
  102 format(2x,'axe  ox   :resultante des efforts = erreur absolue'/t36
     *,' =',e14.7,2x,7hnewton.)
  103 format(2x,'axe horiz :resultante des efforts = erreur absolue'/t36
     *,' =',e14.7,2x,7hnewton.)

      return
      end
