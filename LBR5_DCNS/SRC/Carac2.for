      subroutine carac2(e,eta,a,b,c,d,v,f,g,h,o,p,q,z,sh,fam,
     *                  ksr,ksa,spec,po,phil,rayon,w,
     *                  delt,delt2,impr2,const2)
      implicit real *8(a-h,o-z)
      dimension z(74),fam(6),const2(6,9)
      pi=2.d00*acos(0.)
c***********************************************************************
c
c     subroutine caract2
c     ==================
c     calcul des coefficients de raideur du panneau
c      pour la couche complementaire de raidisseurs et de cadres
c
c       c=delta,  w=width

c       a=epsa2  b=epsr2
c       d=hya2,  v=dya2,      f=wya2,  g=tya2,  (cadres)
c       h=hxr2,  o=dxr2,      p=wxr2,  q=txr2,  (raidisseurs)

c   solution vecteur const = z  et const2 (derivee de const)
c
c     modif: 8-06-95                              creation :21-8-95
c           20-11-2000 : calcul de delt2
c***********************************************************************
c      const(1)= d       const( 2)= k
c      const(3)= oy      const( 9)= ox
c      const(4)= hy      const(10)= hx
c      const(5)= sy      const(11)= sx
c      const(6)= ry      const(12)= rx
c      const(7)= ty      const(13)= tx
c      const(8)= ly      const(14)= lx

      kksa=(-1)**ksa
      kksr=(-1)**ksr

c     cadres
      if(a.ge.0.00001) then
      temp=e*(d*v+f*g)/a
      const2(3,1)=const2(3,1)+kksa*temp/2.
      z(3)=z(3)+temp
      z(4)=z(4)+e*(d*v*(c/2.+d/2.)+f*g*(c/2.+d+g/2.))*kksa/a
      const2(4,1)=dabs(z(4))
      temp=sh*5.*f*g/(6.*a)
      z(5)=z(5)+temp*fam(1)
      z(6)=z(6)+e*(v*d**3/12.+v*d*(c/2.+d/2.)**2+f*g**3/12.+
     *                   f*g*(c/2.+d+g/2.)**2)/a
      z(7)=z(7)+sh*(d*v**3+f*g**3)/(3.*a)
      z(8)=z(8)-(c/2.+d+g/2.)*temp*fam(2)*kksa
      endif

c     raidisseurs
      if(b.ge.0.00001) then
      temp=e*(h*o+p*q)/b
      const2(5,1)=const2(5,1)+kksr*temp/2.
      z(9)=z(9)+temp
      z(10)=z(10)+e*(h*o*(c/2.+h/2.)+p*q*(c/2.+h+q/2.))/b*kksr
      const2(6,1)=dabs(z(10))
      temp=sh*5.*p*q/(6.*b)
      z(11)=z(11)+temp*fam(3)
      z(12)=z(12)+e*(o*h**3/12.+o*h*(c/2.+h/2.)**2+p*q**3/12.+
     *                    p*q*(c/2.+h+q/2.)**2)/b
      z(13)=z(13)+sh*(h*o**3+p*q**3)/(3.*b)
      z(14)=z(14)-(c/2.+h+q/2.)*temp*fam(4)*kksr
      endif

c     calcul des poids du panneau
      specc=spec*w
      if(a.ge.0.00001) then
        paig2=((d*v+f*g)/a) * (rayon+kksa*d/2.)*phil*(pi/180.) *specc
      else
        paig2=0.0
      endif
      if(b.ge.0.00001) then
        praid2=((h*o+p*q)/b) * rayon*phil*(pi/180.) *specc
      else
        praid2=0.0
      endif
      potel=po*w+paig2+praid2
      po=potel/w

c     impressions de const
	if (impr2.ge.-1) then			!15.10.05		!sept2006
        write(66,15)									!sept2006
        write(66,11)(z(i),i=3,14)						!sept2006
	endif						!15.10.05			!sept2006
c
c    calcul de l'epaisseur moyenne compte tenu des raid. complementaires

      if(b.ge.0.00001) delt=delt + (h*o+p*q)/b

c    calcul de l'epaisseur moyenne compte tenu des raid. et cadres complementaires

      if(a.ge.0.00001) delt2= delt2 + (d*v+f*g)/a
      if(b.ge.0.00001) delt2= delt2 + (h*o+p*q)/b

      return
   11 format(6(1x,e11.4))
   12 format(//' poids hors de l''eau (non dejauge)'/
     *         ' pour les nervures complementaires'/
     *         '   poids des cadres      = ',e14.7, 'n.'/
     *         '   poids des raidisseurs = ',e14.7, 'n.'//
     *         ' poids total (avec nervure second)  = ',e14.7, 'n.'/)
   15 format(/'coefficients de raideur du panneau',/
     * '(avec la seconde couche de nervures: aig. et raid)'/
     * t10,'omyr hyr syr ryr tyr lyr'/t10,'omxr hxr sxr rxr txr lxr '/)
      end
