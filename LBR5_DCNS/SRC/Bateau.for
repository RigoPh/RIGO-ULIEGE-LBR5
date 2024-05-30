      subroutine bateau(nsol,xneu,yneu)!,isymx,isymy)
      use sharedvar
      implicit real*8 (a-h,o-z)
      dimension sol(20),valf(11),zb(360),a1(8,neto),b1(400)

c*********************************************************************************
c
c    calcul de la repartition des forces de bord sur les extremites.
c    ces forces créent aux deux extrémités des moments (bm1,bm3) et (bm2,bm4)              !flexion d'axe vert.
c    qui correspondent à ceux induit par la proue et la poupe.
c
c    donnees : bm1,bm3,bm2,bm4                                                                                           !flexion d'axe vert.
c       mx: positif si le pont sup. est en traction et le fond en compression = hogging
c       my: positif si le vecteur moment est dirigé vers le bas                                   !flexion d'axe vert.
c
c       on ne peut pas considérer un moment autour d'un axe de symétrie!
c
c    resultat: a1(8,neto)
c              abcd(8) vect solution pour le sauvetage des résultats
c                      via  write(97) abcd
c    vecteur de travail sol(20)
c
c     créer   : lbr-4, const navale, 1990
c
c     modifiés:
c       - nbre de cas de charge :13-2-96 (nsol)
c       - delt : epaisseur moyenne (bordé + raid + trav)   5-2-97
c       - lbr5.1  mai 1999
c*********************************************************************************

      call annuld(zb,360)
      call annuld(sol,20)
      call annuld(a1,8*neto)
      call annuld(b1,400)
c
      ftmomyx=0.
      ftmomxx=0.
      forctx=0.
      ftmomxy=0.
      ftmomyy=0.
      forcty=0.                                                                   !flexion d'axe vert.

c
c ===main loop 10 forces de bord créées par un moment autour de x et autour de y===       !flexion d'axe vert.

      do 10 i=1,neto

      if(itype(i).eq.5) goto 10
      phi=-philn(i)
      q=qn(i)
      phi2=phi/3.
      phi3=2.*phi2
      sph1=vsin(phi2/2.,0.d00)
      sph2=vsin(phi3/2.,0.d00)
      cx1=vcos(tetas(i),phi2/2.)
      cx2=vcos(tetas(i),phi3/2.)
      cy1=vsin(tetas(i),phi2/2.)
      cy2=vsin(tetas(i),phi3/2.)                                          !flexion d'axe vert.
c
      zb(1)=delt(i) *( z(i,3)-yneu   )
      zb(2)=delt(i) *( z(i,3)-q*2.*cx1*sph1-yneu   )       !flexion d'axe vert.
      zb(3)=delt(i) *( z(i,3)-q*2.*cx2*sph2-yneu   )       !flexion d'axe vert.
      zb(4)=delt(i) *( z(i,4)-yneu   )
c
      qphi=phi*q*pi/180.
      qphi2=qphi*qphi
      qphi3=qphi2*qphi

c     a l'origine y=0
      b1(1)=0.
      b1(21)=0.
      b1(41)=0.
      b1(61)=1.
c     au point y=1/3 de phi
      b1(2)=qphi3/27.
      b1(22)=qphi2/9.
      b1(42)=qphi/3.
      b1(62)=1.
c     au point y=2/3 de phi
      b1(3)=8.*qphi3/27.
      b1(23)=4.*qphi2/9.
      b1(43)=2.*qphi/3.
      b1(63)=1.
c     au point y = phi
      b1(4)=qphi3
      b1(24)=qphi2
      b1(44)=qphi
      b1(64)=1.


      call system(b1,zb,4,1,sol,20)

c     * 'les coefficients a,b,c,d sont (forces de bord), pour un moment
c     * autour de x:',(sol(k),k=1,4)

      do 11 j=1,4
 11   a1(j,i)=sol(j)

      zb(1)=delt(i) *( z(i,1)-xneu   )                                   !flexion d'axe vert.
      zb(2)=delt(i) *( z(i,1)+q*2.*cy1*sph1-xneu   )              !flexion d'axe vert.
      zb(3)=delt(i) *( z(i,1)+q*2.*cy2*sph2-xneu   )              !flexion d'axe vert.
      zb(4)=delt(i) *( z(i,2)-xneu   )                                   !flexion d'axe vert.

c     a1 l'origine y=0
      b1(1)=0.
      b1(21)=0.
      b1(41)=0.
      b1(61)=1.
c     au point y=1/3 de phi
      b1(2)=qphi3/27.
      b1(22)=qphi2/9.
      b1(42)=qphi/3.
      b1(62)=1.
c     au point y=2/3 de phi
      b1(3)=8.*qphi3/27.
      b1(23)=4.*qphi2/9.
      b1(43)=2.*qphi/3.
      b1(63)=1.
c     au point y = phi
      b1(4)=qphi3
      b1(24)=qphi2
      b1(44)=qphi
      b1(64)=1.                                   !flexion d'axe vert.

c     * et zb(4)'

      call system(b1,zb,4,1,sol,20)

c     * 'les coefficients a,b,c,d sont (forces de bord), pour un moment
c     * autour de y:',(sol(k),k=1,4)

      do 12 j=1,4
 12     a1(j+4,i)=sol(j)                                                                      !flexion d'axe vert.


c     fmomyx moment autour de l'axe vert. y du aux forces de bord creees par un moment autour de x
c     fmomxx moment autour de l'axe horiz.x du aux forces de bord creees par un moment autour de x
c     forcx la somme selon ox des forces de bord creees par un moment autour de x(forctx la somme de forcx)
c     fmomyy moment autour de l'axe vert. y du aux forces de bord creees par un moment autour de y
c     fmomxy moment autour de l'axe horiz.x du aux forces de bord creees par un moment autour de y
c     forcy la somme selon ox des forces de bord creees par un moment autour de y(forcty la somme de forcy)

c     ftmomxx,ftmomyx les sommes des moments fmomxx et fmomyx
c     ftmomxy,ftmomyx les sommes des moments fmomxy et fmomyx                     !flexion d'axe vert.

c     les axes consideres etant ceux au centre de gravite.

      phil=dabs(philn(i))
      qph=q*phil*pi/180.
      cc1=vcos(tetas(i),0.d00)
      cc2=vcos(tetas(i),phil)
      cc3=vcos(tetas(i),phil/2.)
      s1=vsin(tetas(i),0.d00)
      s2=vsin(tetas(i),phil)
      s3=vsin(tetas(i),phil/2.)
      sph1=vsin(phil/2.,0.d00)

c     forces
c     *******
      forcx=( a1(1,i)*qph**3/4.+a1(2,i)*qph*qph/3.
     *       +a1(3,i)*qph/2.+a1(4,i))*qph
      forcy=( a1(5,i)*qph**3/4.+a1(6,i)*qph*qph/3.
     *       +a1(7,i)*qph/2.+a1(8,i))*qph

      if(isymx.eq.0.and.isymy.eq.0) then
        forctx=forctx+forcx
        forcty=forcty+forcy
      else
        if(isymx.eq.0.and.isymy.eq.1) forctx=forctx+2*forcx
        if(isymx.eq.1.and.isymy.eq.0) forcty=forcty+2*forcy
      endif                                                                                           !flexion d'axe vert.


c     moments
c     *******                                                                                                                                  bba01030
      if (phil.ge.1.e-05) then
        termx1=(a1(1,i)*qph**2+a1(2,i)*qph+a1(3,i)-q*q*6.*a1(1,i))  *qph
        termx2=(3.*a1(1,i)*qph+2.*a1(2,i)) *qph
        termx3=q* (-6.*a1(1,i)*q*q+a1(3,i))
        termx4=-2.*a1(2,i)*q*q+a1(4,i)
        termy1=(a1(5,i)*qph**2+a1(6,i)*qph+a1(7,i)-q*q*6.*a1(5,i))  *qph
        termy2=(3.*a1(5,i)*qph+2.*a1(6,i)) *qph
        termy3=q* (-6.*a1(5,i)*q*q+a1(7,i))
        termy4=-2.*a1(6,i)*q*q+a1(8,i)

        fmomyx=-forcx * (z(i,1)-xneu+q*cc1)
        fmomyx=fmomyx+q*q*  (s2*termx1+q*cc2*termx2-2.*sph1*s3*termx3+
     *                       2.*sph1 *cc3*termx4)
        fmomxx= - forcx * (z(i,3)-yneu+q*s1)
        fmomxx=fmomxx+q*q* (-cc2*termx1+q*s2*termx2+2.*sph1*s3*termx4+
     *                      2.*sph1*cc3*termx3)
        fmomyy=-forcy * (z(i,1)-xneu+q*cc1)
        fmomyy=fmomyy+q*q*  (s2*termy1+q*cc2*termy2-2.*sph1*s3*termy3+
     *                       2.*sph1 *cc3*termy4)
        fmomxy= - forcy * (z(i,3)-yneu+q*s1)
        fmomxy=fmomxy+q*q* (-cc2*termy1+q*s2*termy2+2.*sph1*s3*termy4+
     *                       2.*sph1*cc3*termy3)

      else
        tempx=qph*qph* (a1(1,i)*  qph**3 /5. + a1(2,i)  *qph**2 /4.
     *                 +a1(3,i)*  qph /3.    + 0.5 * a1(4,i) )
        tempy=qph*qph* (a1(5,i)*  qph**3 /5. + a1(6,i)  *qph**2 /4.
     *                  +a1(7,i)*  qph /3.    + 0.5 * a1(8,i) )

         fmomyx=-(z(i,1)-xneu) * forcx  -  tempx * s1
         fmomxx=-(z(i,3)-yneu) * forcx  +  tempx * cc1
         fmomyy=-(z(i,1)-xneu) * forcy  -  tempy * s1
         fmomxy=-(z(i,3)-yneu) * forcy  +  tempy * cc1
      endif

      if(isymx.eq.1.or.isymy.eq.1) then
        if(isymx.eq.1.and.isymy.eq.1) then
          ftmomxx=ftmomxx+4*fmomxx
          ftmomyy=ftmomyy+4*fmomyy
        else
          ftmomxx=ftmomxx+2*fmomxx
          ftmomyy=ftmomyy+2*fmomyy
        endif
      else
        ftmomxx=ftmomxx+fmomxx
        ftmomyx=ftmomyx+fmomyx
        ftmomyy=ftmomyy+fmomyy
        ftmomxy=ftmomxy+fmomxy
      endif

      valf(1)=a1(4,i)
      call pval(a1(1,i),a1(2,i),a1(3,i),a1(4,i),phil,q,0.1d+00,valf(2))
      call pval(a1(1,i),a1(2,i),a1(3,i),a1(4,i),phil,q,0.2d+00,valf(3))
      call pval(a1(1,i),a1(2,i),a1(3,i),a1(4,i),phil,q,0.3d+00,valf(4))
      call pval(a1(1,i),a1(2,i),a1(3,i),a1(4,i),phil,q,0.4d+00,valf(5))
      call pval(a1(1,i),a1(2,i),a1(3,i),a1(4,i),phil,q,0.5d+00,valf(6))
      call pval(a1(1,i),a1(2,i),a1(3,i),a1(4,i),phil,q,0.6d+00,valf(7))
      call pval(a1(1,i),a1(2,i),a1(3,i),a1(4,i),phil,q,0.7d+00,valf(8))
      call pval(a1(1,i),a1(2,i),a1(3,i),a1(4,i),phil,q,0.8d+00,valf(9))
      call pval(a1(1,i),a1(2,i),a1(3,i),a1(4,i),phil,q,0.9d+00,valf(10))
      call pval(a1(1,i),a1(2,i),a1(3,i),a1(4,i),phil,q,1.0d+00,valf(11))                     !flexion d'axe vert.

c     les impressions
c     ---------------

      valf(1)=a1(8,i)
      call pval(a1(5,i),a1(6,i),a1(7,i),a1(8,i),phil,q,0.1d+00,valf(2))
      call pval(a1(5,i),a1(6,i),a1(7,i),a1(8,i),phil,q,0.2d+00,valf(3))
      call pval(a1(5,i),a1(6,i),a1(7,i),a1(8,i),phil,q,0.3d+00,valf(4))
      call pval(a1(5,i),a1(6,i),a1(7,i),a1(8,i),phil,q,0.4d+00,valf(5))
      call pval(a1(5,i),a1(6,i),a1(7,i),a1(8,i),phil,q,0.5d+00,valf(6))
      call pval(a1(5,i),a1(6,i),a1(7,i),a1(8,i),phil,q,0.6d+00,valf(7))
      call pval(a1(5,i),a1(6,i),a1(7,i),a1(8,i),phil,q,0.7d+00,valf(8))
      call pval(a1(5,i),a1(6,i),a1(7,i),a1(8,i),phil,q,0.8d+00,valf(9))
      call pval(a1(5,i),a1(6,i),a1(7,i),a1(8,i),phil,q,0.9d+00,valf(10))
      call pval(a1(5,i),a1(6,i),a1(7,i),a1(8,i),phil,q,1.0d+00,valf(11))

 10   continue
c
c =====end of loop 10 ==================================================
c
	if(impr2.ge.-1) then			!15.10.05
        write(66,102)ftmomyx
        write(66,103)ftmomxx
        write(66,104)forctx
        write(66,105)ftmomyy
        write(66,106)ftmomxy
        write(66,107)forcty			
	endif

c   calcul des coéfficients pour chaque cas de charges et sauvetage des résultats
c   (pour subr. mom dans bo1) sur un fichier temporaire (file 97)
c    -------------------------------------------------

      do 15 is=1,nsol                                                                                                  !coefficient de participation

        b1(1)=ftmomxx
        b1(21)=ftmomxy
        b1(2)=ftmomyx
        b1(22)=ftmomyy
        zb(1)=bm1(is)
        zb(2)=bm3(is)
        call system(b1,zb,2,1,sol,20)
        coefx1=sol(1)
        coefy1=sol(2)                                          !flexion d'axe vert.

        b1(1)=ftmomxx
        b1(21)=ftmomxy
        b1(2)=ftmomyx
        b1(22)=ftmomyy
        zb(1)=bm2(is)
        zb(2)=bm4(is)
        call system(b1,zb,2,1,sol,20)
        coefx2=sol(1)
        coefy2=sol(2)                                          !flexion d'axe vert.

        do 15 i=1,neto
          if(itype(i).eq.5) goto 15  
		if(impr2.ge.-1) then			!sept2006                                                                                                !coefficient de participation
          write(66,*)
          write(66,'(a,i2)') 'panneau nø ',i
          write(66,'(a,a)')'cas de charge :  abcd(1à4) for mx1 & my1'	    !flexion d'axe vert.
     *									  ,'et abcd(5à8) for mx2 & my2'		!flexion d'axe vert.
         
	    endif			!15.10.05
		do j=1,4

            abcd(j)=a1(j,i)*coefx1+a1(j+4,i)*coefy1
            abcd(j+4)=a1(j,i)*coefx2+a1(j+4,i)*coefy2
          
		enddo                                                        !flexion d'axe vert.

          if(impr2.ge.-1) then			!15.10.05
		  write(66,'(t4,i2,a4,4(1x,e11.4),a4,4(1x,e11.4))')
     *          is,' : ',(abcd(k),k=1,4),'   ',(abcd(k),k=5,8)

          endif						!sept06
		
		write(97) abcd ! sauvetage sur file 97 des abcd (pour lecture dans bo1)

  15  continue

c     les formats
c    -------------------------------------------------
   99 format(t2,'panneau',i2/t2,10(1h-))
  100 format(/t2,'valeurs en 11 pts , y=0,...,y=q*phil',
     *' des forces de bord creees par un moment autour de x'/
     * t2,11(e10.3,1x)/
     * t2,'moment d''axe vert.y du a ces forces  =',e11.4,' n.m'/
     * t2,'moment d''axe horiz.x du a ces forces  =',e11.4,' n.m'/
     * t2,'force resultante selon ox ',e11.4,' n. '/)
  101 format(t2,'valeurs en 11 pts , y=0, ,  ...,y=q*phil',
     *' des forces de bord creees par un moment autour de y'/
     *t2,11(e10.3,1x)/
     *t2,'moment d''axe vert.y du a ces forces =',e11.4,' n.m'/
     *t2,'moment d''axe horiz.x du a ces forces  =',e11.4,' n.m'/
     *t2,'force resultante selon ox ',e11.4,' n. '/)
  102 format(t2,'moment general autour de l''axe vert.y pour tous les ',
     *'panneaux du aux forces de bord creees par un moment autour de x'/
     *t2,116(1h=)/t2,' moment myx total = ',e11.4,' n.m'/)
  103 format(t2,'moment general autour de l''axe horiz.x pour tous les 
     *','panneaux du aux forces de bord creees par un moment autour de x
     *'/t2,116(1h=)/t2,' moment mxx total = ',e11.4,' n.m'/)
  104 format(t2,'somme des forces selon ox pour tous les panneaux ',
     *'du aux forces de bord creees par un moment autour de x'/
     *t2,102(1h=)/t2,'force totale generale =',e11.4,' n.'/)
  105 format(t2,'moment general autour de l''axe vert.y pour tous les ',
     *'panneaux du aux forces de bord creees par un moment autour de y'/
     *t2,116(1h=)/t2,' moment myx total = ',e11.4,' n.m'/)
  106 format(t2,'moment general autour de l''axe horiz.x pour tous les 
     *','panneaux du aux forces de bord creees par un moment autour de y
     *'/t2,116(1h=)/t2,' moment mxx total = ',e11.4,' n.m'/)
  107 format(t2,'somme des forces selon ox pour tous les panneaux ',
     *'du aux forces de bord creees par un moment autour de y'/
     *t2,102(1h=)/t2,'force totale generale =',e11.4,' n.'/)                                   !flexion d'axe vert.

      return
      end
c
c **********************************************************************
c
      subroutine pval(a,b,c,d,ph,q,rap,val)
      implicit real*8(a-h,o-z)
      pi=2.d00*acos(0.)
      qph=q*ph*rap*pi/180.
      val=a*qph*qph*qph+b*qph*qph+c*qph+d
      return
      end
