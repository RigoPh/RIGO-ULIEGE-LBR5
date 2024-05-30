      subroutine ecri3(dis,nel,ne,ns,q,eff2,mt,uecr,langue)
      implicit real*8(a-h,o-z)
      dimension dis(5),eff2(2040),uecr(51)
	character*40 b(8),bb(8)

c***********************************************************************
c
c     subroutine ecri3 (eff2)
c     impression des contraintes dans le bordage au niveau des faces du borde
c     z=+delta/2 et z=-delta/2.

c  creer    :  7-9-93   ph. rigo
c  modifier :  8-5-95   mod. d'optimisation: subr. extrem2
c            15-12-2000 version anglaise

c***********************************************************************

      pi=2.d00*asin(1.d00)

      if(langue==1) then
      data b/'contr. transv.  sy  bordé (z=+d/2)',
     *       'contr. longit.  sx  bordé (z=+d/2)',
     *       'contr. cisaill. txy bordé (z=+d/2)',
     *       'contr. de von mises, bordé (z=+d/2)',
     *       'contr. transv.  sy  bordé (z=-d/2)',
     *       'contr. longit.  sx  bordé (z=-d/2)',
     *       'contr. cisaill. txy bordé (z=-d/2)',
     *       'contr. de von mises, bordé (z=-d/2)'/
	else
c             1234567890123456789012345678901234567890
      data bb/'transv. stress sy plate (z=+d/2)',
     *        'longit. stress sx plate (z=+d/2)',
     *        'shear stress, txy plate (z=+d/2)',
     *        'von mises stress, plate (z=+d/2)',
     *        'transv. stress sy plate (z=-d/2)',
     *        'longit. stress sx plate (z=-d/2)',
     *        'shear stress, txy plate (z=-d/2)',
     *        'von mises stress, plate (z=-d/2)'/
	endif


      if(langue==1) then
        write(66,150)nel
  150   format(/5x,'résultats complémentaires - panneau ',i3,/5x,
     *         45(1h*)/)
	else
        write(66,151)nel
  151   format(/5x,'additional results - panel ',i3,/5x,
     *          45(1h*)/)
        do i=1,8
	    b(i)=bb(i)
	  enddo
	endif

      kh=2*mt+31
      l=0

      do 157 kr=1,8
        l=l+1
        write(66,50) b(l)
  50    format(/t15,a41/t15,40(1h+))
        write(66,156) dis
        if(ns.eq.1) then
          do 163 i=1,kh
              jm=(l-1)*255+i
              ij=jm+204
  163     write(66,158) uecr(i),(eff2(k),k=jm,ij,51)
        else
          goto(2,2,3,2,2,2,3,2),l
   3          jm=(l-1)*255+1
              ij=l*255
              do 4 kk=jm,ij
   4          eff2(kk)=-eff2(kk)
   2      do 159 i=1,31
              jm=(l-1)*255+32-i
              ij=jm+204
  159     write(66,158) uecr(i),(eff2(k),k=jm,ij,51)
          do 161 i=32,kh
              jm=(l-1)*255+(kh+32-i)
              ij=jm+204
              utrav=uecr(31)-uecr(kh+32-i)
  161     write(66,158) utrav,(eff2(k),k=jm,ij,51)
        endif
      call extre2(l,kh,eff2,b,langue)
  157 continue

      write(304,*) '88'						!extension neto	!fev2007
c
  156 format(/10x,5(4x,'x=',f6.3,' m')/)
  158 format(' y=',f6.3,'m',5(1x,e13.6))
      return
      end

c***********************************************************************
c***********************************************************************

      subroutine extre2(l,kh,eff2,a,langue)
      implicit real*8(a-h,o-z)
      dimension eff2(2040)
	character *40 a(8)

c***********************************************************************
c
c     subroutine extre2
c     ++++++++++++++++++
c     recherche les valeurs extrêmes (max et min)
c
c     modif : nov 2000                                 créer :  5-5-95
c
c***********************************************************************

      vmin=0.
      vmax=0.
      ixmin=0
      ixmax=0
      iymin=0
      iymax=0
      i1=(l-1)*255

      do 1 kx=1,5
      i2=i1+(kx-1)*51
        do 2 ky=1,kh
        k1=i2+ky
        if(vmin.gt.eff2(k1)) then
                 vmin=eff2(k1)
                ixmin=kx
                iymin=ky
        endif
        if(vmax.lt.eff2(k1)) then
                vmax=eff2(k1)
                ixmax=kx
                iymax=ky
        endif
   2    continue
   1  continue

      if(langue==1) write(66,102) vmin,ixmin,iymin,vmax,ixmax,iymax
      if(langue==2) write(66,100) vmin,ixmin,iymin,vmax,ixmax,iymax

c     sauvetage pour le module d'analyse des extrêmes (subr. analys)
      if(l.eq.4) goto 101
      if(l.eq.8) goto 101
      goto 999
  101 continue
      write(304,50) l,a(l)								!extension neto	!fev2007
      write(304,*) vmin,ixmin,iymin,vmax,ixmax,iymax		!extension neto	!fev2007

  999 return

  50  format(i2,t15,40a)
  100 format(/' min. value =',e11.4,'   in (x,y)=(',i2,' , ',i2,')'/
     *        ' max. value =',e11.4,'   in (x,y)=(',i2,' , ',i2,')')
  102 format(/' valeur min =',e11.4,'   en (x,y)=(',i2,' , ',i2,')'/
     *        ' valeur max =',e11.4,'   en (x,y)=(',i2,' , ',i2,')')
      return
      end
