      subroutine ecri2(dis,nel,eff,conc,ne,indaig,indrai,ns,q,mt,
     *                 uecr,langue)
      implicit real*8(a-h,o-z)
      dimension dis(5),eff(9690),conc(750),uecr(51)
	character*50 a(3),b(3)
      data a/'v (m) - deplacement transversal selon oy (z=0)',		! 1
     *       'u (m) - deplacement longitudinal selon ox (z=0)',
     *       'w (m) - fleche, deplacement selon oz (z=0)'/			! 3
      data b/'v (m) - transv. displ. along oy (z=0)',				! 1
     *       'u (m) - long. displ. along ox (z=0)',
     *       'w (m) - lateral deflection along oz(z=0)'/			! 3

c***********************************************************************
c     20-1-94
c
c     subroutine ecrit(forme réduite)
c     subroutine de mise en forme des resultats pour ecriture dans un fichier
c
c     dernière modif: nov. 2000
c***********************************************************************

      pi=2.d00*asin(1.d00)
      if(langue.ne.1) then
        do i=1,3
	    a(i)=b(i)
	  enddo
	endif
c
      kh=2*mt+31
      do 1 i=1,kh
  1   uecr(i)=uecr(i)*q*pi/180.
      write(66,150)nel
      l=0
      do 157 l=1,3
      write(66,50) a(l)
  50  format(/t15,a50/t15,40(1h+))
      write(66,156) dis
        if(ns.eq.1) then
          do 163 i=1,kh
              jm=(l-1)*255+i
              ij=jm+204
  163     write(66,158) uecr(i),(eff(k),k=jm,ij,51)
        else
          do 159 i=1,31
              jm=(l-1)*255+31-i
              ij=jm+204
  159     write(66,158) uecr(31-i),(eff(k),k=jm,ij,51)
          do 161 i=32,kh
              jm=(l-1)*255+(kh+32-i)
              ij=jm+204
  161     write(66,158) uecr(kh+32-i),(eff(k),k=jm,ij,51)
        endif
	call extre5(l,kh,eff,a)			!sept06		!15.10.05
  201 write(304,*) '99'				!extension neto	!fev2007		!extension neto  !15.10.05	
  157 continue

  150 format(t3,'panneau-panel no ',i2,/t3,18(1h+))
  156 format(/10x,5(4x,'x=',f6.3,' m')/)
  158 format(' y=',f6.3,'m',5(1x,e13.6))

      return
      end
c ------------------------------------------------------------		!15.10.05
      subroutine extre5(l,kh,eff,a) 
      implicit real*8(a-h,o-z)
      dimension eff(9690)
	character *50 a(3)
      common/py/pi
      common/langue/langue
c
c***********************************************************************
c
c     subroutine extre5
c     ++++++++++++++++++
c     recherche les valeurs extrêmes (max et min)
c
c     modif : nov 2000                                    créer : 13-3-95
c
c***********************************************************************

      vmin=0.
      vmax=0.
      ixmin=0
      ixmax=0
      iymin=0
      iymax=0
      i1=(l-1)*255
	vmin=eff(i1+1) !avril2003   sinon, on ne détecte pas le min s'il est positif
      vmax=eff(i1+1) !avril2003   sinon, on ne détecte pas le max s'il est négatif
      
      do 1 kx=1,5
      i2=i1+(kx-1)*51
        do 2 ky=1,kh
        k1=i2+ky
        if(vmin.ge.eff(k1)) then    !avril2003
                 vmin=eff(k1)
                ixmin=kx
                iymin=ky
        endif
        if(vmax.le.eff(k1)) then     !avril2003 il vaut mieux mettre le plutôt que lt
                vmax=eff(k1)
                ixmax=kx
                iymax=ky
        endif
   2    continue
   1  continue


      if(langue==1) write(66,102) vmin,ixmin,iymin,vmax,ixmax,iymax
      if(langue==2) write(66,100) vmin,ixmin,iymin,vmax,ixmax,iymax


c     sauvetage pour le module d'analyse des extrêmes (subr. analys)
      write(304,50) l,a(l)								!extension neto	!fev2007
      write(304,*) vmin,ixmin,iymin,vmax,ixmax,iymax		!extension neto	!fev2007

c      write(*,*)' l et titre'
c      write(*,50) l,(a(j),j=3*l-2,3*l)
c      write(*,*) 'vmin,ixmin,iymin,vmax,ixmax,iymax'
c      write(*,*) vmin,ixmin,iymin,vmax,ixmax,iymax
c      pause 'ecri'

  999 return

  50  format(i2,t15,50a)
  100 format(/' min. value =',e11.4,'   in (x,y)=(',i2,' , ',i2,')'/
     *        ' max. value =',e11.4,'   in (x,y)=(',i2,' , ',i2,')')
  102 format(/' valeur min =',e11.4,'   en (x,y)=(',i2,' , ',i2,')'/
     *        ' valeur max =',e11.4,'   en (x,y)=(',i2,' , ',i2,')')
  110 format(/' min. value =',e11.4,'   in (x,y)=(',i2,' , ',i2,') with   !avril2003
     *local flexion of girders'/                                          !avril2003
     *        ' max. value =',e11.4,'   in (x,y)=(',i2,' , ',i2,') with   !avril2003
     *local flexion of girders')                                        !avril2003
  112 format(/' valeur min =',e11.4,'   en (x,y)=(',i2,' , ',i2,') avec   !avril2003
     *flexion locale des raidisseurs'/                                    !avril2003
     *        ' valeur max =',e11.4,'   en (x,y)=(',i2,' , ',i2,') avec   !avril2003
     *flexion locale des raidisseurs')                                  !avril2003
	return
      end

c ------------------------------------------------------------

