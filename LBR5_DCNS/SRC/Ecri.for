      subroutine ecri(dis,nel,eff,effcomb,conc,ne,indaig,indrai,ns,q,
     *                impr,vmaxsig,vmaxsigc,mt,uecr,langue)
      implicit real*8(a-h,o-z)
      dimension dis(5),eff(9690),effcomb(9690),conc(750),uecr(51)
	character*50 a(38),b(15),c(38),d(15)

c***********************************************************************
c
c     subroutine ecrit(forme complete)
c     subroutine de mise en forme des resultats pour ecriture sur
c     un listing.
c
c    modif :13- 3-1995                           créer : thèse ph. rigo;
c            1-11-2000
c            4-12-2002  22 variables classées dans sol2 (valeurs extrêmes)
c
c***********************************************************************

      pi=2.d00*asin(1.d00)

      data a/'v (m) - deplacement transversal selon oy (z=0)',		! 1
     *       'u (m) - deplacement longitudinal selon ox (z=0)',
     *       'w (m) - fleche, deplacement selon oz (z=0)',			! 3
     *       'dw/dy - rotation autour de l axe ox ',
     *       'nx (n/m) - effort normal ox (+ participation raid)',	! 5
     *       'ny (n/m) - effort transv oy (+ participation cadr)',
     *       'nxy (n/m) - effort cisaillement (+ part renforts)',
     *       'nyx (n/m) - effort cisaillement (+ part renforts)',
     *       'mx (n.m/m)- moment autour de oy (+ particip raid)',		! 9
     *       'my (n.m/m)- moment autour de ox (+ particip cadr)',
     *       'mxy (n.m/m) - moment de torsion',
     *       'myx (n.m/m) - moment de torsion',
     *       'qx (n/m) - reaction sur bord oy (avec part. raid)',		! 13
     *       'qy (n/m) - reaction sur bord oy (avec part. cadre)',
     *       'ry (n/m) - kirchoff reaction sur ox (= qy + ...)',
     *       'rx (n/m) - kirchoff reaction sur oy (= qx + ...)',		! 16
     *       'sy (n/m2) - contr. trans. selon ox, plaque (z=0)',
     *       'sx (n/m2) - contr. long.  selon oy, plaque (z=0)',
     *       'txy (n/m2) - contr. cisaillement, plaque (z=0)',
     *       'scomp (n/m2) - contr. de von mises, plaque(z=0)',		! 20
     *       'sy cadre (jas)- contr. jonct. ame-sem, cadre',		    ! 21
     *       'sy cadre (jab)- contr. jonct. ame-borde, cadre',		! 22
     *       'tyz cadre (jas)- tau (shear)jonct ame-sem,cadre',		! 23
     *       'tyz cadre (jab)- tau (shear)jonct ame-borde,cadre',	    ! 24
     *       'sc cadre (jas)- von mises, j. ame-sem, cadre',		    ! 25
     *       'sc cadre (jab)- von mises, j. ame-borde,cadre',		    ! 26
     *       'sy cadre (sem)- contrainte dans semelle, cadre',		! 27
     *       'txy cadre (sem)- tau (shear) dans semelle cadre',		! 28
     *       'sc cadre (sem)- von mises, dans semelle, cadre',		! 29
     *       'sx raid (jas)- contr. jonction ame-sem, raid',			! 30
     *       'sx raid (jab)- contr. jonction ame-borde, raid',		! 31
     *       'sx raid (sem)- contrainte dans semelle, raid',			! 32
     *       'txz raid (jas)- tau (shear), jonct. ame-sem',			! 33
     *       'txz raid (jab)- tau (shear), jonct. ame-borde',			! 34
     *       'tyx raid (sem)- tau (shear) dans semelle raid',			! 35
     *       'sc raid (jas)- von mises, jonct ame-sem,raid',			! 36
     *       'sc raid (jab)- von mises, jonct ame-borde, raid',		! 37
     *       'sc raid (sem)- von mises, dans semelle, raid'/  		! 38

      data b/	'nx  concentre',
     *		'nxy concentre  = nyx concentre',
     *		'mx  concentre',
     *		'mxy concentre',
     *		'qx  concentre',
     *		'rx  concentre',
     *		'sigma x semelle    traverse',
     *		'sigma x  jas       traverse',
     *		'sigma x  jab       traverse',
     *		'tau yx max semelle traverse',
     *		'tau xz ame jas     traverse',
     *        'tau xz ame jab     traverse',
     *		'sigma von-mises semelle traverse',
     *		'sigma von-mises jas  traverse',
     *		'sigma von-mises jab  traverse'/

      data c/'v (m) - transv. displ. along oy (z=0)',					! 1
     *       'u (m) - long. displ. along ox (z=0)',
     *       'w (m) - lateral deflection along oz(z=0)',				! 3
     *       'dw/dy - rotation around ox ',
     *       'nx (n/m) - long. force along ox(with stiff. effect)',	! 5
     *       'ny (n/m) - transv force along oy(with frame effect)',
     *       'nxy (n/m) - inplane shear force (with memb. effect)',
     *       'nyx (n/m) - inplane shear force (with memb. effect)',
     *       'mx (n.m/m)- moment around oy (with stiff. effect)',		! 9
     *       'my (n.m/m)- moment around ox (with frame effect)',
     *       'mxy (n.m/m) - twisting moment',
     *       'myx (n.m/m) - twisting moment',
     *       'qx (n/m) - reaction along oy(with stiff. effect)',		! 13
     *       'qy (n/m) - reaction along ox(with stiff. effect)',
     *       'ry (n/m) - kirchoff reaction along ox (= qy + ...)',
     *       'rx (n/m) - kirchoff reaction along oy (= qx + ...)',	! 16
     *       'sy (n/m2) - transv. stress in plate (z=0)',
     *       'sx (n/m2) - longit. stress in plate (z=0)',
     *       'txy (n/m2) - inplane shear stress in plate (z=0)',
     *       'scomp (n/m2) - von-mises in plate (z=0)',				! 20
     *       'sy frame (jwf)- stress, junct. web-flange, frame',		! 21
     *       'sy frame (jwp)- stress, junct. web-plate, frame',		! 22
     *       'tyz frame (jwf)- shear stress, junct. web-flange',		! 23
     *       'tyz frame (jwp)- shear stress, junct. web-plate',		! 24
     *       'sc frame (jwf)- von-mises at j. web-flange,frame',		! 25
     *       'sc frame (jwp)- von-mises at j. web-plate,frame',		! 26
     *       'sy frame (flange)- stress in flange, frame',			! 27
     *       'txy frame (flange)- shear stress in flange frame',		! 28
     *       'sc frame (flange)- von-mises in flange, frame',			! 29
     *       'sx stiff (jwf)- stress, junct. web-flange, stiff',		! 30
     *       'sx stiff (jwp)- stress, junct. web-plate, stiff',		! 31
     *       'sx stiff (flange)- stress in flange, stiff',			! 32
     *       'txz stiff (jwf)- shear stress, junct. web-flange',		! 33
     *       'txz stiff (jwp)- shear stress, junct. web-plate',		! 34
     *       'tyx stiff (flange)- shear stress in flange stiff',		! 35
     *       'sc stiff (jwf)- von-mises at j. web-flange,stiff',		! 36
     *       'sc stiff (jwp)- von-mises at j. web-plate, stiff',		! 37
     *       'sc stiff (flange)- von-mises in flange, stiff'/			! 38

      data d/	'nx  , local force',
     *		'nxy = nyx , local force',
     *		'mx  , local force',
     *		'mxy , local force',
     *		'qx  , local force',
     *		'rx  , local force',
     *		'sigma x  flange  , stress in girder',
     *		'sigma x  jwf     , stress in girder',
     *		'sigma x  jwp     , stress in girder',
     *		'tau yx flange  , stress in girder',
     *		'tau xz web jwf , stress in girder',
     *        'tau xz web jwp , stress in girder',
     *		'sigma comp flange , von-mises stress in girder',
     *		'sigma comp web jwf, von-mises stress in girder',
     *		'sigma comp web jwp, von-mises stress in girder'/
c
c     l = indice relatif à la variables étudiées (l=1 à 38) dans eff
c     l=1       v               l=2     u                l=3    w
c       4       dw/dy             5     nx                 6    ny
c       7       nxy               8     nyx                9    mx
c      10       my               11     mxy               12    myx
c      13       qx               14     qy                15    ry
c      16       rx

c      17       sig y bordage    18     sig x bordage      19    tau xy bordage
c      20       sig comp bordage

c      21     sig y jas (cadre)  22   sig y jab (cadre)    23    tau jas (cadre)
c      24     tau jab (cadre)    25   sig comp jas (cadre) 26    sig comp (cadre)
c      27     sig y sem (cadre)  28   tau sem (cadre)      29    sig comp (cadre)

c      30    sig x jas (raid)    31   sig x jab (raid)     32    sig x sem (raid)
c      33    tau   jas (raid)    34   tau jab (raid)       35    tau sem (raid)
c      36    sig comp jas (raid) 37   sig comp jab (raid)  38    sig comp (raid)

      if(langue.ne.1) then
        do i=1,38
	    a(i)=c(i)
	  enddo
        do i=1,15
	    b(i)=d(i)
	  enddo
	endif
c
      kh=2*mt+31
      do 1 i=1,kh
  1   uecr(i)=uecr(i)*q*pi/180.
      write(66,150) nel
      l=0

      do 157 kr=1,38
        l=l+1
        if((impr.le.-1).and.(l.eq.5)) l=17
        if(l.eq.39) goto 201
        if((l.eq.21).and.(indaig.eq.0)) l=30
        if((l.eq.30).and.(indrai.eq.0)) goto 201
        write(66,50) a(l)
  50    format(/t15,a50/t15,50(1h+))
        write(66,156) dis
c
c     ns = 1 si phil positif (sens normal) ; ns=-1 si phil négatif
c
        if(ns.eq.1) then
          do 163 i=1,kh
              jm=(l-1)*255+i
              ij=jm+204
  163     write(66,158) uecr(i),(eff(k),k=jm,ij,51)
        else
          if(l.ge.20) goto 2
          goto(3,2,2,3,2,2,3,3,2,2,3,3,2,3,3,2,2,2,3),l
   3          jm=(l-1)*255+1
              ij=l*255
              do 4 kk=jm,ij
   4          eff(kk)=-eff(kk)
   2      do 159 i=1,31
              jm=(l-1)*255+32-i
              ij=jm+204
  159     write(66,158) uecr(i),(eff(k),k=jm,ij,51)
          do 161 i=32,kh
              jm=(l-1)*255+(kh+32-i)
              ij=jm+204
              utrav=uecr(31)-uecr(kh+32-i)
  161     write(66,158) utrav,(eff(k),k=jm,ij,51)
        endif
      call extre1(l,kh,eff,effcomb,a,vmaxsig,vmaxsigc,langue)  !avril2003
  157 continue
  201 write(304,*) '99'		!extension neto	!fev2007
c
      if(mt.eq.0) goto 200
      if(langue==1) then
        write(66,301)
	else
        write(66,302)
	endif
      do 160 l=1,15
      if((impr.le.-1).and.(l.le.6)) goto 160
         write(66,50) b(l)
         write(66,156) dis
         if(ns.eq.1) then
         do 164 i=1,mt
            jm=(l-1)*50+i
            ij=jm+40
 164     write(66,158) uecr(2*i+30),(conc(k),k=jm,ij,10)
         else
         do 162 i=1,mt
            jm=(l-1)*50+mt-i+1
            ij=jm+40
            utrav=uecr(31)-uecr(kh+1-2*i)
  162    write(66,158) utrav,(conc(k),k=jm,ij,10)
         endif
  160 continue
c
  150 format(t3,'panneau-panel no ',i2,/t3,18(1h+))
  156 format(/10x,5(4x,'x=',f6.3,' m')/)
  158 format(' y=',f6.3,'m',5(1x,e13.6))
  301 format(//t17,'calcul relatif aux traverses des effets ',
     *             'concentres '/t15,53(1h+)//)
  302 format(//t17,'stresses in the longitudinal girders'/
     *         t15,35(1h+)//)

  200 return
      end

c ------------------------------------------------------------
      subroutine extre1(l,kh,eff,effcomb,a,vmaxsig,vmaxsigc,langue)
      implicit real*8(a-h,o-z)
      dimension eff(9690),effcomb(9690)
	character *50 a(38)

c***********************************************************************
c
c     subroutine extre1
c     ++++++++++++++++++
c     recherche les valeurs extrêmes (max et min)
c
c     modif : nov 2000                                    créer : 13-3-95
c
c***********************************************************************

      vmin=0.
	vminc=0.  !avril2003
      vmax=0.
	vmaxc=0.  !avril2003
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
        if(vmin.ge.eff(k1)) then     !avril2003
                 vmin=eff(k1)
	           vminc=effcomb(k1)   !avril2003
                ixmin=kx
                iymin=ky
        endif
        if(vmax.le.eff(k1)) then     !avril2003 il vaut mieux mettre le plutôt que lt
                vmax=eff(k1)
	          vmaxc=effcomb(k1)    !avril2003
                ixmax=kx
                iymax=ky
        endif
   2    continue
   1  continue

      if (l.eq.18) then  !avril2003
	 if (dabs(vmin).gt.dabs(vmax)) then  !avril2003
	   vmaxsig  = vmin                   !avril2003
	   vmaxsigc = vminc                  !avril2003
	 else                                !avril2003
	   vmaxsig  = vmax                   !avril2003
	   vmaxsigc = vmaxc                  !avril2003
	 endif                               !avril2003
	endif                                !avril2003

      if(langue==1) write(66,102) vmin,ixmin,iymin,vmax,ixmax,iymax
      if(langue==2) write(66,100) vmin,ixmin,iymin,vmax,ixmax,iymax

      if (l.eq.18.or.l.eq.20.or.l.eq.30.or.l.eq.31.or.l.eq.32.or.l.eq.34 !avril2003
     *.or.l.eq.36.or.l.eq.37.or.l.eq.38) then                            !avril2003
	 if (langue==1) write(66,112) vminc,ixmin,iymin,vmaxc,ixmax,iymax  !avril2003
	 if (langue==2) write(66,110) vminc,ixmin,iymin,vmaxc,ixmax,iymax  !avril2003
	endif

c     sauvetage pour le module d'analyse des extrêmes (subr. analys)
      if(l.le.3)  goto 101
      if((l.ge.17).and.(l.le.26)) goto 101
      if(l.eq.29) goto 101
      if(l.ge.36) goto 101
      goto 999
  101 continue
      write(304,50) l,a(l)								!extension neto	!fev2007
      write(304,*) vmin,ixmin,iymin,vmax,ixmax,iymax		!extension neto	!fev2007

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
