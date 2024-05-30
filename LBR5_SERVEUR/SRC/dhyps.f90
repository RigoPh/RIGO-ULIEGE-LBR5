subroutine dhyps(phil,teta,q,m,mt,ms,aux,daux,dzb,ipan)




use param_section
!use param_opti_local


implicit double precision(a-h,o-z)
dimension aux(ms,ms),dzb(ms,8+nsolmax),daux(ms,ms)


!***********************************************************************
!     subroutine dhyp (optimisation)
!     ===============================
!     la subr. calcule les dérivées des effets des traverses (xo et zo)
!
!     rappel: hyp est la solution du système [ aux ] * hyp = zb
!                          (pour les "8+nsolmax" vecteurs indépendants, max
!
!                             -1
!   calculons: dhyp = [ aux ]     * (dzb - [ daux ] * hyp)
!              ============================================
!   avec:
!   aux(ms,ms) = inverse de la matrice aux.
!                 dans bo1 la mat. inverse est contenue dans aux (cfr. subr resolu
!   daux(ms,ms) = dérivée de la matrice aux par la variable xi=1,nvar(nel,iboat)
!   dzb (ms,13) = dérivée des "8+nsolmax" vecteurs zb par la variable xi=1,nbrxi
!   dhyp(i,j,k) = dérivée des vecteurs solutions de forces xo et zo (traverses)
!        si k  la variable de conception (1 à 9)
!           j  les "8+nsolmax" cas de charge (dish(is=1,nsol), disc en y=0 et en yo, disb, ...)
!           i  les dérivées de xo pour les mt traverses + idem avec zo
!
!     créer :   21-6-95       par ph. rigo
!     *******
!     modifier: le  5-07-95
!     ********* le 25-03-96 supprimer kse comme arg.
!               le 29-03-96 dzb1 : termes en y**2 et y**3
!               le  1-08-96 common/opti3/ ds subr. dhyp comme dans dzb1,
!               le  4-08-96 correction dans dinteq
!***********************************************************************
!  ordre des variables dans dvarh
!      u,v,w; ... de 1 à 13
!      (xo et zo) de (14 et 15) à (32 et 33) pour les traverses 1 à 10
!        call annuld(dhyp,9*20*18)

nel=ipan

dhyp(:,:,:)=0.d00

!     boucle sur les variables de conception
!     ========================================
do ivar=1,nvar(nel,iboat)
   k=nxit(ivar,nel,iboat)

	 if(impr.ge.1) then
      write(iu_11(iboat),*) 'soit la variable xi nø',k
      write(iu_11(iboat),*) '++++++++++++++++++++++++++++'
   endif

!   call annuld(daux,ms*ms)
!   call annuld(dzb,ms*(8+nsol))

daux(:,:)=0.d00
dzb (:,:)=0.d00

!   calcul de la dérivée de matrice aux = [ daux ]
!   -----------------------------------------------
   do lr=1,2
      call dinteq(darg,argq,q,disb,mt,m,lr,1,panneau(nel).abtr(:),daux,k,dvarb)
      call dinteq(darg,argq,q,disc,mt,m,lr,2,panneau(nel).abtr(:),daux,k,dvarc)
   enddo

   if(impr.ge.1) then
      write(iu_11(iboat),*) ' dérivée de aux (subr dhyp )'
      write(iu_11(iboat),*) ' ---------------------------'
      do i=1,ms
         write(iu_11(iboat),16) (daux(i,j),j=1,ms)
      enddo
   endif

!   calcul de la dérivée de zb (soit dzb)
!   ----------------------------------------
    do is=1,nsol
       if(is.gt.1) then
          is1=is+8
       else
          is1=1
       endif
       call dzb1(darg,phil,teta,argq,q,dish(1,is),mt,m,panneau(nel).abtr(:),dzb(1,is1),k,dvarh(1,1,1,is))
    enddo

    call dzb2(darg,phil,argq,q,disc,mt,m,panneau(nel).abtr(:),dzb,k,dvarc,2,+1)
    call dzb2(darg,phil,argq,q,disc,mt,m,panneau(nel).abtr(:),dzb,k,dvarc,3,+1)
    call dzb2(darg,phil,argq,q,disd,mt,m,panneau(nel).abtr(:),dzb,k,dvard,4,+1)
    call dzb2(darg,phil,argq,q,disd,mt,m,panneau(nel).abtr(:),dzb,k,dvard,5,-1)
    call dzb2(darg,phil,argq,q,disa,mt,m,panneau(nel).abtr(:),dzb,k,dvara,6,+1)
    call dzb2(darg,phil,argq,q,disa,mt,m,panneau(nel).abtr(:),dzb,k,dvara,7,-1)
    call dzb2(darg,phil,argq,q,disb,mt,m,panneau(nel).abtr(:),dzb,k,dvarb,8,+1)
    call dzb2(darg,phil,argq,q,disb,mt,m,panneau(nel).abtr(:),dzb,k,dvarb,9,+1)

    if(impr.ge.1) then
       write(iu_11(iboat),*) ' dérivée de zb (dish ) cas de charge nø 1'
       write(iu_11(iboat),*) ' dérivée de zb (disc (y=0) ,disc (y=yo))'
       write(iu_11(iboat),*) '           (idem avec disd, disa, disb )'
       write(iu_11(iboat),*) ' dérivée de zb (dish ) cas de charge nø2 à nsol'
       write(iu_11(iboat),*) ' -------------------------------------------------'

       do i=1,8+nsol
          write(iu_11(iboat),17) i,(dzb(j,i),j=1,ms)
       enddo
    endif

!   calcul de dhyp=  dzb - [ daux ] * hyp
!     --------------------------------------
!     dhyp(n,j,k) = [dhyp]  (ms,8+nsolmax,nbrxi)
!     dzb = [d(zb)/dx]         (ms,8+nsolmax) car 8+nsolmax cas de charge
!     daux = [d(aux)/dx]       (ms,ms)
!     hyp = vecteurs solutions (ms*(8+nsolmax)) car 8+nsolmax cas de charge

    do j=1,8+nsol
       do i=1,ms
          dhyp(i,j,k)=dzb(i,j)
          do l=1,ms
             jl=(j-1)*20+l
             dhyp(i,j,k)=dhyp(i,j,k) - daux(i,l) * hyp(jl)
          enddo
       enddo
    enddo

enddo

   !!write(9999,*)  'dhyp before='
   !!write(9999,*)   dhyp
   !!write(9999,*)  'aux before='
   !!write(9999,*)   aux
   !!write(9999,*)  'dzb before='
   !!write(9999,*)   dzb

!     multiplication par la matrice inverse (aux) (faire dhyp=aux* dhy
!     ------------------------------------------------------------------
!     (le résultat est placé dans dhyp et dzb sert de vecteur de travail
if(impr.ge.1) write(iu_11(iboat),18)

do ivar=1,nvar(nel,iboat)
   k=nxit(ivar,nel,iboat)
   do j=1,8+nsol
      call annuld(dzb,ms)
!	  dzb(:,1)=0.d00
      do i=1,ms
      do kh=1,ms
           dzb(i,1)=dzb(i,1)+aux(i,kh)*dhyp(kh,j,k)
      enddo
      enddo

      do kh=1,ms
         dhyp(kh,j,k)=dzb(kh,1)
      enddo
   enddo

   if(impr.ge.1) then
      write(iu_11(iboat),*)' '
      write(iu_11(iboat),*)'variable de conception nø',k
      do j=1,8+nsol
         write(iu_11(iboat),17) j,(dhyp(kh,j,k),kh=1,ms)
      enddo
   endif
enddo

return

16  format(6e11.4)
17  format(i2,6e11.4)
18  format(/'dérivées des hyp (xo et zo) pour chaque variable xi' /40(1h=))


end

!***********************************************************************
!***********************************************************************

      subroutine dinteq (darg,argq,q,disx,mt,m,lr,kc,abtr,daux,k,dvar)
      implicit double precision(a-h,o-z)
      dimension argq(8),disx(720),abtr(mt),daux(2*mt,2*mt), dvar(33,9,16)
      dimension darg(8,9)
!***********************************************************************
!     subroutine dinteq
!     ------------------
!     subroutine de remplissage des coefficients de la derivee de la
!     matrice des inconnues hyperstatiques des traverses
!
!     resultat : la matrice [ daux ].
!     ---------
!
!     modifié: le 4-8-96 (ph. rigo)                  créer : thèse
!***********************************************************************
!   avec:abtr = abtr(i),i=1,mt
!          disx = disb ou disc
!           kc  = indice sur les sollicitations: kc = 1  disb et kc = 2
!           lr  = indice sur les forces        : kc = 1  xo   et kc = 2
!            k  = nø de la variable de conception

pi=2.d00*acos(0.d00)
pi1=pi/180.d00
pi2=2.*pi

ki=(kc-1)*mt+1
kf=kc*mt
li=1+(lr-1)*mt
lf=lr*mt

do 14 j=ki,kf

      do 15 i=li,lf
      n=11+2*(i-(lr-1)*mt)+lr

      k9=j-ki+1
      l9=i-li+1
      z1=dabs(abtr(l9)-abtr(k9))*pi1
      z1q=z1*q
      z11=pi2-z1
      z11q=z11*q

      do 16 l=1,m
      l2=2*l
      l1=l2-1
      jj=4*(l-1)
      kh=jj+lr*16+32*(i-(lr-1)*mt)

      al1=argq(l1)*z1
      be1=argq(l2)*z1

      al3=argq(l1)*z11
      be3=argq(l2)*z11
      al1=expo(al1)
      al3=expo(al3)
      cos1=al1*dcos(be1)
      sin1=al1*dsin(be1)
      cos3=al3*dcos(be3)
      sin3=al3*dsin(be3)

      daux(i,j)= daux(i,j)                                                           &
                  - z1q * ( cos1* (darg(l1,k)*disx(kh+1)-darg(l2,k)*disx(kh+2) )     &
                           +sin1* (darg(l1,k)*disx(kh+2)+darg(l2,k)*disx(kh+1) ) )   &
                  - z11q* ( cos3* (darg(l1,k)*disx(kh+3)-darg(l2,k)*disx(kh+4) )     &
                           +sin3* (darg(l1,k)*disx(kh+4)+darg(l2,k)*disx(kh+3) ) )   &
                  + cos1 * dvar(n,k,jj+1)  + sin1 * dvar(n,k,jj+2)                   &
                  + cos3 * dvar(n,k,jj+3)  + sin3 * dvar(n,k,jj+4)                   


 16   continue

      daux(i,j)=-daux(i,j)/1.d04

 15   continue
 14   continue

return
end


!***********************************************************************
!     subroutine pour la charge extérieure (dish)
!     +++++++++++++++++++++++++++++++++++++++++++++

      subroutine dzb1(darg,phil,teta,argq,q,dish,mt,m,abtr,dzb,k,dvarh)
      implicit double precision(a-h,o-z)
      dimension argq(8),dish(1710),abtr(mt),dzb(2*mt),dvarh(33,9,38)
      dimension darg(8,9)

!***********************************************************************

pi=2.d00*acos(0.d00)
pi1=pi/180.
pi2=2.*pi
phil1=phil*pi1
teta1=teta*pi1

! boucle sur les traverses (mt)
! ------------------------------
do 3 i=1,mt

!  soit z1 la coordonnée du point de calcul par rapport à y=o

   z1=abtr(i)*pi1
   z1q=z1 * q
   z11=pi2-z1
   z11q=z11*q

!  soit z2 la coordonnée du point de calcul  par rapport à y=yo (phil

   z2=phil1-z1
   z2q=z2*q
   z22=pi2-z2
   z22q=z22*q

   if(dabs(teta).eq.180.) then
      cost=-dcos(z1)
      sint=-dsin(z1)
   else
      if(dabs(teta).eq.90.) then
         iss=1
         if(teta.eq.-90.) iss=-1
         cost=-dsin(z1)*iss
         sint= dcos(z1)*iss
      else
         cost=dcos(teta1+z1)
         sint=dsin(teta1+z1)
      endif
   endif

!  ii=1 pour xo et ii=2 pour zo
!  ------------------------------
   do 4 ii=1,2

      jj=2*(i-1)+ii
      j=13+jj
      ik=(ii-1)*mt+i

      dzb(ik)=dvarh(j,k,1)+dvarh(j,k,2)*z1                            &
                          +dvarh(j,k,3)*z1*z1+dvarh(j,k,4)*(z1**3)    &
                          +dvarh(j,k,5)*cost +dvarh(j,k,6)*sint

!     boucle sur m
!       --------------
      do 5 l=1,m
      l2=2*l
      l1=l2-1

      lk=(l-1)*8 + 6
      ij=38*(jj+2)+lk

      al1=argq(l1)*z1
      be1=argq(l2)*z1
      al2=argq(l1)*z2
      be2=argq(l2)*z2
      al3=argq(l1)*z11
      be3=argq(l2)*z11
      al4=argq(l1)*z22
      be4=argq(l2)*z22
      al1=expo(al1)
      al2=expo(al2)
      al3=expo(al3)
      al4=expo(al4)
      cos1=al1*dcos(be1)
      sin1=al1*dsin(be1)
      cos2=al2*dcos(be2)
      sin2=al2*dsin(be2)
      cos3=al3*dcos(be3)
      sin3=al3*dsin(be3)
      cos4=al4*dcos(be4)
      sin4=al4*dsin(be4)

      dzb(ik)=dzb(ik)                                                       &
      - z1q * ( cos1* (darg(l1,k)*dish(ij+1)-darg(l2,k)*dish(ij+2) )        &
               +sin1* (darg(l2,k)*dish(ij+1)+darg(l1,k)*dish(ij+2) ) )      &
      - z2q * ( cos2* (darg(l1,k)*dish(ij+3)-darg(l2,k)*dish(ij+4) )        &
               +sin2* (darg(l2,k)*dish(ij+3)+darg(l1,k)*dish(ij+4) ) )      &
      - z11q* ( cos3* (darg(l1,k)*dish(ij+5)-darg(l2,k)*dish(ij+6) )        &
               +sin3* (darg(l2,k)*dish(ij+5)+darg(l1,k)*dish(ij+6) ) )      &
      - z22q* ( cos4* (darg(l1,k)*dish(ij+7)-darg(l2,k)*dish(ij+8) )        &
               +sin4* (darg(l2,k)*dish(ij+7)+darg(l1,k)*dish(ij+8) ) )      &
      + cos1 * dvarh(j,k,lk+1)  + sin1 * dvarh(j,k,lk+2)                    &
      + cos2 * dvarh(j,k,lk+3)  + sin2 * dvarh(j,k,lk+4)                    &
      + cos3 * dvarh(j,k,lk+5)  + sin3 * dvarh(j,k,lk+6)                    &
      + cos4 * dvarh(j,k,lk+7)  + sin4 * dvarh(j,k,lk+8)

    5 continue
    4 continue
    3 continue
      return
      end

!***********************************************************************
!     subroutine pour les 8 charges de bords (disc, d, a, b) en y=0 et y
!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine dzb2(darg,phil,argq,q,disx,mt,m,abtr,dzb,k,dvarx,nbr,isign)
      implicit double precision(a-h,o-z)
      dimension argq(8),disx(720),abtr(mt),dzb(2*mt,9),dvarx(33,9,16)
      dimension darg(8,9)

!***********************************************************************

      pi=2.d00*acos(0.d00)
      pi1=pi/180.
      pi2=2.*pi

!     boucle sur les traverses (mt)
!       ------------------------------
      do 3 i=1,mt

!       write(iu_11(iboat),*)'traverse nø',i
!       write(iu_11(iboat),*)'=============='

        if((nbr.eq.2).or.(nbr.eq.4).or.(nbr.eq.6).or.(nbr.eq.8)) then
!     soit z1 la coordonnée du point de calcul par rapport à y=o
      z1=abtr(i)*pi1
      z1q=z1 * q
      z11=pi2-z1
      z11q=z11*q
      else
!     soit z1 la coordonnée du point de calcul  par rapport à y=yo (phil
      z1=(phil-abtr(i))*pi1
      z1q=z1*q
      z11=pi2-z1
      z11q=z11*q
      endif

!      write(iu_11(iboat),*)' z1 =',z1, ' z1q =',z1q
!      write(iu_11(iboat),*)' sign =',sign,' nbr =',nbr


!     ii=1 pour xo et ii=2 pour zo
!       ------------------------------
      do 4 ii=1,2

!       write(iu_11(iboat),*)'ii=1 pour xo et ii=2 pour zo : ii=',ii
!       write(iu_11(iboat),*)'------------------------------------'

      jj=2*(i-1)+ii
      j=13+jj
      ik=(ii-1)*mt+i

!       write(iu_11(iboat),*)'jj=',jj
!       write(iu_11(iboat),*)'j=',j
!       write(iu_11(iboat),*)'ik=',ik

!     boucle sur m
!       -------------
  do 5 l=1,m
      l2=2*l
      l1=l2-1

      lk=(l-1)*4
      ij=16*(jj+2)+lk

      al1=argq(l1)*z1
      be1=argq(l2)*z1
      al3=argq(l1)*z11
      be3=argq(l2)*z11
      al1=expo(al1)
      al3=expo(al3)
      cos1=al1*dcos(be1)
      sin1=al1*dsin(be1)
      cos3=al3*dcos(be3)
      sin3=al3*dsin(be3)

      dzb(ik,nbr)=dzb(ik,nbr)                                            &
      - z1q * ( cos1* (darg(l1,k)*disx(ij+1)-darg(l2,k)*disx(ij+2) )     &
               +sin1* (darg(l1,k)*disx(ij+2)+darg(l2,k)*disx(ij+1) ) )   &
      - z11q* ( cos3* (darg(l1,k)*disx(ij+3)-darg(l2,k)*disx(ij+4) )     &
               +sin3* (darg(l1,k)*disx(ij+4)+darg(l2,k)*disx(ij+3) ) )   &
      + cos1 * dvarx(j,k,lk+1)  + sin1 * dvarx(j,k,lk+2)                 &
      + cos3 * dvarx(j,k,lk+3)  + sin3 * dvarx(j,k,lk+4)


 5 continue

      dzb(ik,nbr)=dzb(ik,nbr) * isign

    4 continue
    3 continue


return
end
