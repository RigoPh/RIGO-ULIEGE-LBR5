      subroutine ecri4(dis,nel,eff,conc,ne,indaig,indrai,q,ns,impr,
     *		     isect1,vnymax,vmymax,symax,symin,vm1,vm2,
     *                 mt,uecr,langue)
      implicit real*8(a-h,o-z)
      dimension dis(5),eff(9690),conc(750),uecr(51)
	character*50 a(37),b(15),c(37),d(15),e(32),f(32)

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
     *       'sy ep.+ - contr. jonct.supérieur, épontille',    ! 17  20.02.04
     *       'sy ep.- - contr. jonct inférieure, épontille',	! 18   20.02.04
     *       'tyz ep. - tau (shear)jonct ame-sem,épont',		! 19	20.02.04
     *       'tyz ep. (a.n.) - tau (shear) axe neutre,épont',			! 20	20.02.04
     *       'sc vm.ep.+ - von mises, j. ame-sem sup, epontille',		! 21	20.02.04
     *       'sc vm.ep.- - von mises, j. ame-sem inf, epontille',		    ! 22  20.02.04
     *       'sc vm.ep. (a.n.) - von mises, j. axe neutre,epont',		    ! 23	20.02.04
     *       'sy ep.+ (sem)- contrainte sup semelle, epontille',		    ! 24	20.02.04
     *       'sy ep.- (sem)- contrainte inf semelle, epontille',		        ! 25    20.02.04
     *   	   'txy ep. (sem)- tau (shear) dans semelle epont',		! 26	20.02.04
     *       'sc vm.ep.+ (sem)- von mises, sup semelle, epont',		    ! 27	20.02.04
     *       'sc vm.ep.- (sem)- von mises, inf semelle, epont',	         	! 28   20.02.04
     *       'sx raid (jas)- contr. jonction ame-sem, raid',			! 29
     *       'sx raid (jab)- contr. jonction ame-epontille, raid',		! 30
     *       'sx raid (sem)- contrainte dans semelle, raid',			! 31
     *       'txz raid (jas)- tau (shear), jonct. ame-sem',			! 32
     *       'txz raid (jab)- tau (shear), jonct. ame-epontille',			! 33
     *       'tyx raid (sem)- tau (shear) dans semelle raid',			! 34
     *       'sc raid (jas)- von mises, jonct ame-sem,raid',			! 35
     *       'sc raid (jab)- von mises, jonct ame-epontille, raid',		! 36
     *       'sc raid (sem)- von mises, dans semelle, raid'/  		! 37

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
     *       'sy pillar (jwf)- stress, junct. top, pillar',		! 21
     *       'sy pillar (jwp)- stress, junct. bottom, pillar',		! 22
     *       'tyz pillar (jwf)- shear stress, junct. web-flange',		! 23
     *       'tyz pillar - shear stress, at the neutral axis',		! 24
     *       'sc pillar (jwf)- von-mises at top j. web-flange,pillar',		! 25
     *       'sc pillar (jwf)- von-mises at bottom j. web-flange',		! 26
     *       'sc pillar - von-mises at the neutral axis,pillar',		! 26
     *       'sy pillar (flange)- stress in top flange, pillar',			! 27
     *       'sy pillar (flange)- stress in bottom flange, pillar',			! 27
     *       'txy pillar (flange)- shear stress in flange pillar',		! 28
     *       'sc pillar (flange)- von-mises in top flange, pillar',			! 29
     *       'sc pillar (flange)- von-mises in bottom flange, pillar',			! 29
     *       'sx stiff (jwf)- stress, junct. web-flange, stiff',		! 30
     *       'sx stiff (jwp)- stress, junct. web-pillar, stiff',		! 31
     *       'sx stiff (flange)- stress in flange, stiff',			! 32
     *       'txz stiff (jwf)- shear stress, junct. web-flange',		! 33
     *       'txz stiff (jwp)- shear stress, junct. web-pillar',		! 34
     *       'tyx stiff (flange)- shear stress in flange stiff',		! 35
     *       'sc stiff (jwf)- von-mises at j. web-flange,stiff',		! 36
     *       'sc stiff (jwp)- von-mises at j. web-pillar, stiff',		! 37
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
	data e/'v (m) - deplacement transversal selon oy (z=0)',		! 1
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
     *       'sy ep.+ - contr.supérieure, épontille',		    ! 17  20.02.04
     *       'sy ep.- - contr. inférieure, épontille',		! 18   20.02.04
     *       'tyz ep. - tau (shear)sommet,épont',		! 19	20.02.04
     *       'tyz ep. (a.n.)- tau (shear) axe neutre,épont',	    ! 20	20.02.04
     *       'sc vm.ep.+ - von mises, sup, epontille',		    ! 21	20.02.04
     *       'sc vm.ep.- - von mises, inf, epontille',		    ! 22	20.02.04
     *       'sc vm.ep. (a.n.)- von mises, axe neutre,épontille',		    ! 23	20.02.04
     *       'sx raid (jas)- contr. jonction ame-sem, raid',			! 24
     *       'sx raid (jae)- contr. jonction ame-epon, raid',		! 25
     *       'sx raid (sem)- contrainte dans semelle, raid',			! 26
     *       'txz raid (jas)- tau (shear), jonct. ame-sem',			! 27
     *       'txz raid (jae)- tau (shear), jonct. ame-epon',			! 28
     *       'tyx raid (sem)- tau (shear) dans semelle raid',			! 29
     *       'sc raid (jas)- von mises, jonct ame-sem,raid',			! 30
     *       'sc raid (jae)- von mises, jonct ame-epon, raid',		! 31
     *       'sc raid (sem)- von mises, dans semelle, raid'/  		! 32

	data f/'v (m) - transv. displ. along oy (z=0)',					! 1
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
     *       'qx (n/m)- reaction along oy(with stiff. effect)',		! 13
     *       'qy (n/m)- reaction along ox(with stiff. effect)',
     *       'ry (n/m)- kirchoff reaction along ox (= qy + ...)',
     *       'rx (n/m)- kirchoff reaction along oy (= qx + ...)',	    ! 16
     *       'sy pillar - stress, top of section, pillar',		! 17
     *       'sy pillar - stress, bottom of section, pillar',		! 18
     *       'tyz pillar - shear stress, top of section',		! 19
     *       'tyz pillar- shear stress, section''s neutral axis',		! 20
     *       'sc pillar- von-mises at top j. web-flange,pillar',		! 21
     *       'sc pillar- von-mises at bottom j.web-flange',		! 22
     *       'sc pillar- von-mises at neutral axis,pillar',		! 23
     *       'sx stiff (jwf)- stress, junct. web-flange, stiff',		! 24
     *       'sx stiff (jwp)- stress, junct. web-pillar, stiff',		! 25
     *       'sx stiff (flange)- stress in flange, stiff',			! 26
     *       'txz stiff (jwf)- shear stress, junct. web-flange',		! 27
     *       'txz stiff (jwp)- shear stress, junct. web-pillar',		! 28
     *       'tyx stiff (flange)- shear stress in flange stiff',		! 29
     *       'sc stiff (jwf)- von-mises at j.web-flange,stiff',		! 30
     *       'sc stiff (jwp)- von-mises at j.web-pillar, stiff',		! 31
     *       'sc stiff (flange)- von-mises in flange, stiff'/			! 32
c
c     l = indice relatif à la variables étudiées (l=1 à 38) dans eff 
c					pour épontilles en double t
c     l=1       v               l=2     u                l=3    w
c       4       dw/dy             5     nx                 6    ny
c       7       nxy               8     nyx                9    mx
c      10       my               11     mxy               12    myx
c      13       qx               14     qy                15    ry
c      16       rx

c      17     sig y jas (sup)	  18   sig y jab (inf)		19    tau jas
c      20     tau a.n.		  21   sig comp jas (sup)	22    sig comp (inf)
c      23     sig comp a.n.     24   sig y sem (sup)      25    sig y sem (inf)
c	 26		tau sem			  27	sig comp sem (sup)	28	  sig comp sem (inf)	

c      29    sig x jas (raid)    30   sig x jab (raid)     31    sig x sem (raid)
c      32    tau   jas (raid)    33   tau jab (raid)       34    tau sem (raid)
c      35    sig comp jas (raid) 36   sig comp jab (raid)  37							    sig comp (raid)

      if(langue.ne.1) then
        do i=1,37
	    a(i)=c(i)  
	  enddo
        do i=1,32
	    e(i)=f(i)  
	  enddo
	  do i=1,15  
	    b(i)=d(i)
	  enddo
	endif

      kh=2*mt+31
      do 1 i=1,kh
  1   uecr(i)=uecr(i)*q*pi/180.
      write(66,150) nel
	write(66,*) 'EPONTILLE'	
	l=0

	if(isect1.eq.3)then
      do 157 kr=1,37   !variables eff
        l=l+1
        if((impr.le.-1).and.(l.eq.5)) l=17
	  if(l.eq.2) l=3
	  if(l.eq.5) l=6
	  if(l.eq.7) l=9
	  if(l.eq.11) l=14
	  if(l.eq.16) l=17
        if(l.eq.38) goto 201
        if((l.eq.16).and.(indaig.eq.0)) l=29
        if((l.eq.29).and.(indrai.eq.0)) goto 201
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
	    if (l.eq.10)then
	    vm1= -eff(2500)
	    vm2= -eff(2530)
          endif
        else
          if(l.ge.16) goto 2
          goto(3,2,2,3,2,2,3,3,2,2,3,3,2,3,3),l
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
     
      call extre4(l,kh,eff,a,isect1,vnymax,vmymax,symax,symin,langue)	
  157 continue

	else
	do 155 kr=1,32   !variables eff
        l=l+1

        if((impr.le.-1).and.(l.eq.5)) l=17
	  if(l.eq.2) l=3
	  if(l.eq.5) l=6
	  if(l.eq.7) l=10
	  if(l.eq.11) l=14
	  if(l.eq.16) l=17
        if(l.eq.33) goto 201
        if((l.eq.16).and.(indaig.eq.0)) l=24
        if((l.eq.24).and.(indrai.eq.0)) goto 201
        write(66,51) e(l)

  51    format(/t15,a50/t15,50(1h+))
        write(66,156) dis
c
c     ns = 1 si phil positif (sens normal) ; ns=-1 si phil négatif
c
        if(ns.eq.1) then
          do 165 i=1,kh
              jm=(l-1)*255+i
              ij=jm+204
  165     write(66,158) uecr(i),(eff(k),k=jm,ij,51)
      	    if (l.eq.10)then
	    vm1= -eff(2500)			!m1 moment à extrémité sup pour flam
	    vm2= -eff(2530)			!m2 moment à extrémité inf pour flam
	endif
	  else
          if(l.ge.16) goto 6
          goto(5,6,6,5,6,6,5,5,6,6,5,5,6,5,5),l
   5          jm=(l-1)*255+1
              ij=l*255
              do 41 kk=jm,ij
  41          eff(kk)=-eff(kk)
  6      do 1599 i=1,31                                
              jm=(l-1)*255+32-i
              ij=jm+204
 1599     write(66,158) uecr(i),(eff(k),k=jm,ij,51)
          do 1619 i=26,kh
              jm=(l-1)*255+(kh+32-i)
              ij=jm+204
              utrav=uecr(31)-uecr(kh+32-i)
 1619     write(66,158) utrav,(eff(k),k=jm,ij,51)
        endif
      
      call extre4(l,kh,eff,e,isect1,vnymax,vmymax,symax,symin,langue)
	
  155 continue

	endif
  201 write(2218,*) '99'

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

      subroutine extre4(l,kh,eff,a,isect1,vnymax,vmymax,symax,symin,
     *                  langue)
      implicit real*8(a-h,o-z)
      dimension eff(9690)
	character*50 a(37)

c***********************************************************************
c
c     subroutine extre4
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

      do 1 kx=1,5
      i2=i1+(kx-1)*51
        do 2 ky=1,kh
        k1=i2+ky
        if(vmin.gt.eff(k1)) then
                 vmin=eff(k1)
                ixmin=kx
                iymin=ky
        endif
        if(vmax.lt.eff(k1)) then
                vmax=eff(k1)
                ixmax=kx
                iymax=ky
        endif
   2    continue
   1  continue
c	enregistrement de données pour routine flam
	if(l.eq.6) then
	 vnymax=-vmin
	elseif(l.eq.10) then
	 vmymax= -vmin
	elseif (l.eq.17) then
	symax=-vmin
	elseif (l.eq.18) then
	symin =-vmin
	endif

      if(langue==1) write(66,102) vmin,ixmin,iymin,vmax,ixmax,iymax
      if(langue==2) write(66,100) vmin,ixmin,iymin,vmax,ixmax,iymax

c     sauvetage pour le module d'analyse des extrêmes (subr. analys3)
      if(l.le.3)  goto 101				
	if(isect1.eq.3) then
       if((l.ge.17).and.(l.le.23)) goto 101
      if(l.eq.27) goto 101
      if(l.eq.28) goto 101
	else
       if((l.ge.17).and.(l.le.23)) goto 101
	endif
      goto 999
  101 continue
      write(2218,50) l,a(l)
      write(2218,*) vmin,ixmin,iymin,vmax,ixmax,iymax

  999 return

  50  format(i2,t15,50a)
  100 format(/' min. value =',e11.4,'   in (x,y)=(',i2,' , ',i2,')'/
     *        ' max. value =',e11.4,'   in (x,y)=(',i2,' , ',i2,')')
  102 format(/' valeur min =',e11.4,'   en (x,y)=(',i2,' , ',i2,')'/
     *        ' valeur max =',e11.4,'   en (x,y)=(',i2,' , ',i2,')')
      return
      end

c ------------------------------------------------------------
