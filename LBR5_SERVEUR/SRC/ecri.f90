subroutine ecri(dis,nel,eff,effcomb,conc,indaig,indrai,ns,q,  &
                impr,vmaxsig,vmaxsigc,mt,uecr)

use param_section,DIS_=>DIS,UECR_=>UECR,A_=>A,B_=>B,IMPR_=>IMPR,    &
INDAIG_=>INDAIG,INDRAI_=>INDRAI,NS_=>NS

implicit double precision(a-h,o-z)
dimension dis(5),eff(9690),effcomb(9690),conc(750),uecr(51)
character*50 a(38),b(15),c(38),d(15)

!-------------------------------------------------------------------------
!
!     subroutine ecrit(forme complete)
!     subroutine de mise en forme des resultats pour ecriture sur
!     un listing.
!
!    modif :13- 3-1995                           créer : thèse ph. rigo;
!            1-11-2000
!            4-12-2002  22 variables classées dans sol2 (valeurs extrêmes)
!
!-------------------------------------------------------------------------

 pi=2.d00*asin(1.d00)

data a/'v (m) - deplacement transversal selon oy (z=0)',		   &
       'u (m) - deplacement longitudinal selon ox (z=0)',      &
       'w (m) - fleche, deplacement selon oz (z=0)',			     &
       'dw/dy - rotation autour de l axe ox ',                 &
       'nx (n/m) - effort normal ox (+ participation raid)',	 &
       'ny (n/m) - effort transv oy (+ participation cadr)',   &
       'nxy (n/m) - effort cisaillement (+ part renforts)',    &
       'nyx (n/m) - effort cisaillement (+ part renforts)',    &
       'mx (n.m/m)- moment autour de oy (+ particip raid)',		 &
       'my (n.m/m)- moment autour de ox (+ particip cadr)',    &
       'mxy (n.m/m) - moment de torsion',                      &
       'myx (n.m/m) - moment de torsion',                      &
       'qx (n/m) - reaction sur bord oy (avec part. raid)',		 &
       'qy (n/m) - reaction sur bord oy (avec part. cadre)',   &
       'ry (n/m) - kirchoff reaction sur ox (= qy + ...)',     &
       'rx (n/m) - kirchoff reaction sur oy (= qx + ...)',		 &
       'sy (n/m2) - contr. trans. selon ox, plaque (z=0)',     &
       'sx (n/m2) - contr. long.  selon oy, plaque (z=0)',     &
       'txy (n/m2) - contr. cisaillement, plaque (z=0)',       &
       'scomp (n/m2) - contr. de von mises, plaque(z=0)',		   &
       'sy cadre (jas)- contr. jonct. ame-sem, cadre',		     &
       'sy cadre (jab)- contr. jonct. ame-borde, cadre',		   &
       'tyz cadre (jas)- tau (shear)jonct ame-sem,cadre',		   &
       'tyz cadre (jab)- tau (shear)jonct ame-borde,cadre',	   &
       'sc cadre (jas)- von mises, j. ame-sem, cadre',		     &
       'sc cadre (jab)- von mises, j. ame-borde,cadre',		     &
       'sy cadre (sem)- contrainte dans semelle, cadre',		   &
       'txy cadre (sem)- tau (shear) dans semelle cadre',		   &
       'sc cadre (sem)- von mises, dans semelle, cadre',		   &
       'sx raid (jas)- contr. jonction ame-sem, raid',			   &
       'sx raid (jab)- contr. jonction ame-borde, raid',		   &
       'sx raid (sem)- contrainte dans semelle, raid',			   &
       'txz raid (jas)- tau (shear), jonct. ame-sem',			     &
       'txz raid (jab)- tau (shear), jonct. ame-borde',		     &
       'tyx raid (sem)- tau (shear) dans semelle raid',		     &
       'sc raid (jas)- von mises, jonct ame-sem,raid',		     &
       'sc raid (jab)- von mises, jonct ame-borde, raid',	     &
       'sc raid (sem)- von mises, dans semelle, raid'/  	

data b/'nx  concentre',                                        &
       'nxy concentre  = nyx concentre',                       &
       'mx  concentre',                                        &
       'mxy concentre',                                        &
       'qx  concentre',                                        &
       'rx  concentre',                                        &
       'sigma x semelle    traverse',                          &
       'sigma x  jas       traverse',                          &
       'sigma x  jab       traverse',                          &
       'tau yx max semelle traverse',                          &
       'tau xz ame jas     traverse',                          &
       'tau xz ame jab     traverse',                          &
       'sigma von-mises semelle traverse',                     &
       'sigma von-mises jas  traverse',                        &
       'sigma von-mises jab  traverse'/

data c/'v (m) - transv. displ. along oy (z=0)',					       &
       'u (m) - long. displ. along ox (z=0)',                  &
       'w (m) - lateral deflection along oz(z=0)',			       &
       'dw/dy - rotation around ox ',                          &
       'nx (n/m) - long. force along ox(with stiff. effect)',	 &
       'ny (n/m) - transv force along oy(with frame effect)',  &
       'nxy (n/m) - inplane shear force (with memb. effect)',  &
       'nyx (n/m) - inplane shear force (with memb. effect)',  &
       'mx (n.m/m)- moment around oy (with stiff. effect)',		 &
       'my (n.m/m)- moment around ox (with frame effect)',     &
       'mxy (n.m/m) - twisting moment',                        &
       'myx (n.m/m) - twisting moment',                        &
       'qx (n/m) - reaction along oy(with stiff. effect)',		 &
       'qy (n/m) - reaction along ox(with stiff. effect)',     &
       'ry (n/m) - kirchoff reaction along ox (= qy + ...)',   &
       'rx (n/m) - kirchoff reaction along oy (= qx + ...)',	 &
       'sy (n/m2) - transv. stress in plate (z=0)',            &
       'sx (n/m2) - longit. stress in plate (z=0)',            &
       'txy (n/m2) - inplane shear stress in plate (z=0)',     &
       'scomp (n/m2) - von-mises in plate (z=0)',				       &
       'sy frame (jwf)- stress, junct. web-flange, frame',	   &
       'sy frame (jwp)- stress, junct. web-plate, frame',		   &
       'tyz frame (jwf)- shear stress, junct. web-flange',	   &
       'tyz frame (jwp)- shear stress, junct. web-plate',		   &
       'sc frame (jwf)- von-mises at j. web-flange,frame',	   &
       'sc frame (jwp)- von-mises at j. web-plate,frame',		   &
       'sy frame (flange)- stress in flange, frame',			     &
       'txy frame (flange)- shear stress in flange frame',	   &
       'sc frame (flange)- von-mises in flange, frame',			   &
       'sx stiff (jwf)- stress, junct. web-flange, stiff',	   &
       'sx stiff (jwp)- stress, junct. web-plate, stiff',		   &
       'sx stiff (flange)- stress in flange, stiff',			     &
       'txz stiff (jwf)- shear stress, junct. web-flange',	   &
       'txz stiff (jwp)- shear stress, junct. web-plate',		   &
       'tyx stiff (flange)- shear stress in flange stiff',	   &
       'sc stiff (jwf)- von-mises at j. web-flange,stiff',	   &
       'sc stiff (jwp)- von-mises at j. web-plate, stiff',	   &
       'sc stiff (flange)- von-mises in flange, stiff'/			

data d/'nx  , local force',                                    &
		   'nxy = nyx , local force',                              &
     	 'mx  , local force',                                    &
     	 'mxy , local force',                                    &
     	 'qx  , local force',                                    &
     	 'rx  , local force',                                    &
     	 'sigma x  flange  , stress in girder',                  &
     	 'sigma x  jwf     , stress in girder',                  &
     	 'sigma x  jwp     , stress in girder',                  &
     	 'tau yx flange  , stress in girder',                    &
     	 'tau xz web jwf , stress in girder',                    &
       'tau xz web jwp , stress in girder',                    &
     	 'sigma comp flange , von-mises stress in girder',       &
     	 'sigma comp web jwf, von-mises stress in girder',       &
     	 'sigma comp web jwp, von-mises stress in girder'/
!      
!     l = indice relatif à la variables étudiées (l=1 à 38) dans eff
!       1       v                 2     u                      3    w
!       4       dw/dy             5     nx                     6    ny
!       7       nxy               8     nyx                    9    mx
!      10       my               11     mxy                   12    myx
!      13       qx               14     qy                    15    ry
!      16       rx
!
!      17       sig y bordage    18     sig x bordage         19    tau xy bordage
!      20       sig comp bordage                           
!                                                          
!      21     sig y jas (cadre)  22     sig y jab (cadre)     23    tau jas  (cadre)
!      24     tau jab (cadre)    25     sig comp jas (cadre)  26    sig comp (cadre)
!      27     sig y sem (cadre)  28     tau sem (cadre)       29    sig comp (cadre)
!                                                             
!      30    sig x jas (raid)    31     sig x jab (raid)      32    sig x sem (raid)
!      33    tau   jas (raid)    34     tau jab (raid)        35    tau sem   (raid)
!      36    sig comp jas (raid) 37     sig comp jab (raid)   38    sig comp  (raid)
                                                              
if(langue.ne.1) then
   do i=1,38                                             
      a(i)=c(i)
	 enddo
   do i=1,15
	    b(i)=d(i)
	 enddo
endif

    kh=2*mt+31
    do 1 i=1,kh
1   uecr(i)=uecr(i)*q*pi/180.
    write(iu_11(iboat),150) nel
    l=0

    do kr=1,38
       l=l+1
       if((impr.le.-1).and.(l.eq.5)) l=17
       if(l.eq.39) goto 201
       if((l.eq.21).and.(indaig.eq.0)) l=30
       if((l.eq.30).and.(indrai.eq.0)) goto 201
       write(iu_11(iboat),50) a(l)
50     format(/t15,a50/t15,50(1h+))
       write(iu_11(iboat),156) dis
!
!      ns = 1 si phil positif (sens normal) ; ns=-1 si phil négatif
!
       if(ns.eq.1) then
          do i=1,kh
              jm=(l-1)*255+i
              ij=jm+204
              write(iu_11(iboat),158) uecr(i),(eff(k),k=jm,ij,51)
          enddo
       else
          if(l.ge.20) goto 2
          goto(3,2,2,3,2,2,3,3,2,2,3,3,2,3,3,2,2,2,3),l

   3          jm=(l-1)*255+1
              ij=l*255
              do kk=jm,ij
                 eff(kk)=-eff(kk)
              enddo
   
   2          do i=1,31
                 jm=(l-1)*255+32-i
                 ij=jm+204
                 write(iu_11(iboat),158) uecr(i),(eff(k),k=jm,ij,51)
              enddo

              do i=32,kh
                 jm=(l-1)*255+(kh+32-i)
                 ij=jm+204
                 utrav=uecr(31)-uecr(kh+32-i)
                 write(iu_11(iboat),158) utrav,(eff(k),k=jm,ij,51)
              enddo
       endif
       call extre1(l,kh,eff,effcomb,a,vmaxsig,vmaxsigc) 
    enddo
201 write(iu_27(iboat),*) '99'		

    if(mt.eq.0) goto 200
    if(langue==1) then
       write(iu_11(iboat),301)
	  else
       write(iu_11(iboat),302)
	  endif
    do 160 l=1,15
       if((impr.le.-1).and.(l.le.6)) goto 160
       write(iu_11(iboat),50) b(l)
       write(iu_11(iboat),156) dis
       if(ns.eq.1) then
          do i=1,mt
             jm=(l-1)*50+i
             ij=jm+40
             write(iu_11(iboat),158) uecr(2*i+30),(conc(k),k=jm,ij,10)
          enddo
       else
          do i=1,mt
             jm=(l-1)*50+mt-i+1
             ij=jm+40
             utrav=uecr(31)-uecr(kh+1-2*i)
             write(iu_11(iboat),158) utrav,(conc(k),k=jm,ij,10)
          enddo
       endif
  160 continue

  150 format(t3,'panneau-panel no ',i3,/t3,18(1h+))
  156 format(/10x,5(4x,'x=',f8.3,' m')/)
  158 format(' y=',f6.3,'m',5(1x,e13.6))
  301 format(//t17,'calcul relatif aux traverses des effets ','concentres '/t15,53(1h+)//)
  302 format(//t17,'stresses in the longitudinal girders'/t15,35(1h+)//)

  200 return
      end

! ----------------------------------------------------------------------
subroutine extre1(l,kh,eff,effcomb,a,vmaxsig,vmaxsigc)

use param_section, ONLY : iu_11,iu_27,langue,iboat

implicit double precision(a-h,o-z)
dimension eff(9690),effcomb(9690)
character *50 a(38)

! ----------------------------------------------------------------------
!
!     subroutine extre1
!     ++++++++++++++++++
!     recherche les valeurs extrêmes (max et min)
!
!     modif : nov 2000                                    créé : 13-3-95
!
! ----------------------------------------------------------------------

vmin=0.
vminc=0.  
vmax=0.
vmaxc=0.  
ixmin=0
ixmax=0
iymin=0
iymax=0
i1=(l-1)*255
vmin=eff(i1+1)   ! sinon, on ne détecte pas le min s'il est positif
vmax=eff(i1+1)   ! sinon, on ne détecte pas le max s'il est négatif
      
do kx=1,5
   i2=i1+(kx-1)*51
   do ky=1,kh
      k1=i2+ky
      if(vmin.ge.eff(k1)) then 
         vmin=eff(k1)
         vminc=effcomb(k1)   
         ixmin=kx
         iymin=ky
      endif
      if(vmax.le.eff(k1)) then 
         vmax=eff(k1)
         vmaxc=effcomb(k1)    
         ixmax=kx
         iymax=ky
      endif
   enddo
enddo

if (l.eq.18) then  
	 if (dabs(vmin).gt.dabs(vmax)) then 
	    vmaxsig  = vmin                  
	    vmaxsigc = vminc                 
	 else                               
	    vmaxsig  = vmax                  
	    vmaxsigc = vmaxc                 
	 endif                              
endif                               

if(langue==1) write(iu_11(iboat),102) vmin,ixmin,iymin,vmax,ixmax,iymax
if(langue==2) write(iu_11(iboat),100) vmin,ixmin,iymin,vmax,ixmax,iymax

if (l.eq.18.or.l.eq.20.or.l.eq.30.or.l.eq.31.or.l.eq.32.or.l.eq.34  &
.or.l.eq.36.or.l.eq.37.or.l.eq.38) then                            
	 if (langue==1) write(iu_11(iboat),112) vminc,ixmin,iymin,vmaxc,ixmax,iymax  
	 if (langue==2) write(iu_11(iboat),110) vminc,ixmin,iymin,vmaxc,ixmax,iymax  
endif

! sauvetage pour le module d'analyse des extrêmes (subr. analys)

    if(l.le.3)  goto 101
    if((l.ge.17).and.(l.le.26)) goto 101
    if(l.eq.29) goto 101
    if(l.ge.36) goto 101
    goto 999
101 continue
    write(iu_27(iboat),50) l,a(l)								
    write(iu_27(iboat),*) vmin,ixmin,iymin,vmax,ixmax,iymax		

999 return

50  format(i2,t15,50a)
100 format(/' min. value =',e11.4,'   in (x,y)=(',i2,' , ',i2,')'/  &
            ' max. value =',e11.4,'   in (x,y)=(',i2,' , ',i2,')')
102 format(/' valeur min =',e11.4,'   en (x,y)=(',i2,' , ',i2,')'/  &
            ' valeur max =',e11.4,'   en (x,y)=(',i2,' , ',i2,')')
110 format(/' min. value =',e11.4,'   in (x,y)=(',i2,' , ',i2,') with  &   
            local flexion of girders'/                                 &
            ' max. value =',e11.4,'   in (x,y)=(',i2,' , ',i2,') with  &
            local flexion of girders')                                        
112 format(/' valeur min =',e11.4,'   en (x,y)=(',i2,' , ',i2,') avec  &
            flexion locale des raidisseurs'/                           &
            ' valeur max =',e11.4,'   en (x,y)=(',i2,' , ',i2,') avec  &
            flexion locale des raidisseurs')                                  
return
end


