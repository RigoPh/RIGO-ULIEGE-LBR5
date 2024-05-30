      subroutine dhyps(phil,teta,q,m,
     *           mt,ms,
     *           nbrxi,nxi,aux,daux,dzb,nsol)
      use sharedvar
      implicit real *8(a-h,o-z)
      dimension nxi(9)
      dimension aux(ms,ms),dzb(ms,8+ismax),daux(ms,ms)

c***********************************************************************
c     subroutine dhyp (optimisation)
c     ===============================
c     la subr. calcule les dérivées des effets des traverses (xo et zo)
c
c     rappel: hyp est la solution du système [ aux ] * hyp = zb
c                          (pour les "8+nsol" vecteurs indépendants, max
c
c                             -1
c   calculons: dhyp = [ aux ]     * (dzb - [ daux ] * hyp)
c              ============================================
c   avec:
c   aux(ms,ms) = inverse de la matrice aux.
c                 dans bo1 la mat. inverse est contenue dans aux (cfr. subr resolu
c   daux(ms,ms) = dérivée de la matrice aux par la variable xi=1,nbrxi
c   dzb (ms,13) = dérivée des "8+nsol" vecteurs zb par la variable xi=1,nbrxi
c   dhyp(i,j,k) = dérivée des vecteurs solutions de forces xo et zo (traverses)
c        si k  la variable de conception (1 à 9)
c           j  les "8+nsol" cas de charge (dish(is=1,nsol), disc en y=0 et en yo, disb, ...)
c           i  les dérivées de xo pour les mt traverses + idem avec zo
c
c     créer :   21-6-95       par ph. rigo
c     *******
c     modifier: le  5-07-95
c     ********* le 25-03-96 supprimer kse comme arg.
c               le 29-03-96 dzb1 : termes en y**2 et y**3
c               le  1-08-96 common/opti3/ ds subr. dhyp comme dans dzb1,
c               le  4-08-96 correction dans dinteq
c***********************************************************************
c  ordre des variables dans dvarh
c      u,v,w; ... de 1 à 13
c      (xo et zo) de (14 et 15) à (32 et 33) pour les traverses 1 à 10

c        call annuld(dhyp,9*20*18)
      do i=1,20
       do j=1,18
        do k=1,9
         dhyp(i,j,k)=0.d00
        enddo
       enddo
      enddo

c     boucle sur les variables de conception
c     ========================================
      do 1 ivar=1,nbrxi
        k=nxi(ivar)

      if(impr.ge.1) then
        write(66,*) 'soit la variable xi nø',k
      write(66,*) '++++++++++++++++++++++++++++'
        endif

        call annuld(daux,ms*ms)
        call annuld(dzb,ms*(8+nsol))

c     calcul de la dérivée de matrice aux = [ daux ]
c     -----------------------------------------------
      do 2 lr=1,2
      call dinteq(darg,argq,q,disb,mt,m,lr,1,abtr,daux,k,dvarb)
      call dinteq(darg,argq,q,disc,mt,m,lr,2,abtr,daux,k,dvarc)
    2 continue

        if(impr.ge.1) then
        write(66,*) ' dérivée de aux (subr dhyp )'
      write(66,*) ' ---------------------------'
      do 14 i=1,ms
      write(66,16) (daux(i,j),j=1,ms)
   14 continue
      endif

c     calcul de la dérivée de zb (soit dzb)
c     ----------------------------------------
      do 19 is=1,nsol
          if(is.gt.1) then
            is1=is+8
          else
            is1=1
          endif
        call dzb1(darg,phil,teta,argq,q,dish(1,is),mt,m,abtr,dzb(1,is1),
     *            k,
     *            dvarh(1,1,1,is))
   19 continue

      call dzb2(darg,phil,argq,q,disc,mt,m,abtr,dzb,k,dvarc,2,+1)
      call dzb2(darg,phil,argq,q,disc,mt,m,abtr,dzb,k,dvarc,3,+1)
      call dzb2(darg,phil,argq,q,disd,mt,m,abtr,dzb,k,dvard,4,+1)
      call dzb2(darg,phil,argq,q,disd,mt,m,abtr,dzb,k,dvard,5,-1)
      call dzb2(darg,phil,argq,q,disa,mt,m,abtr,dzb,k,dvara,6,+1)
      call dzb2(darg,phil,argq,q,disa,mt,m,abtr,dzb,k,dvara,7,-1)
      call dzb2(darg,phil,argq,q,disb,mt,m,abtr,dzb,k,dvarb,8,+1)
      call dzb2(darg,phil,argq,q,disb,mt,m,abtr,dzb,k,dvarb,9,+1)

        if(impr.ge.1) then
      write(66,*) ' dérivée de zb (dish ) cas de charge nø 1'
      write(66,*) ' dérivée de zb (disc (y=0) ,disc (y=yo))'
      write(66,*) '                     (idem avec disd, disa, disb )'
      write(66,*) ' dérivée de zb (dish ) cas de charge nø2 à nsol'
      write(66,*) ' -------------------------------------------------'
      do 15 i=1,8+nsol
      write(66,17) i,(dzb(j,i),j=1,ms)
   15 continue
      endif

c     calcul de dhyp=  dzb - [ daux ] * hyp
c     --------------------------------------
c     dhyp(n,j,k) = [dhyp]  (ms,8+nsol,nbrxi)
c     dzb = [d(zb)/dx]         (ms,8+nsol) car 8+nsol cas de charge
c     daux = [d(aux)/dx]       (ms,ms)
c     hyp = vecteurs solutions (ms*(8+nsol)) car 8+nsol cas de charge

      do 10 j=1,8+nsol
      do 10 i=1,ms
        dhyp(i,j,k)=dzb(i,j)
        do 11 l=1,ms
                jl=(j-1)*20+l
        dhyp(i,j,k)=dhyp(i,j,k) - daux(i,l) * hyp(jl)
   11   continue
   10 continue

    1 continue

c     multiplication par la matrice inverse (aux) (faire dhyp=aux* dhy
c     ------------------------------------------------------------------
c     (le résultat est placé dans dhyp et dzb sert de vecteur de travail
      if(impr.ge.1) write(66,18)

      do 7 ivar=1,nbrxi
        k=nxi(ivar)
        do 6 j=1,8+nsol
          call annuld(dzb,ms)
         do 8  i=1,ms
            do 9 kh=1,ms
                   dzb(i,1)=dzb(i,1)+aux(i,kh)*dhyp(kh,j,k)
    9       continue
    8    continue
         do 4 kh=1,ms
            dhyp(kh,j,k)=dzb(kh,1)
    4    continue
    6   continue

        if(impr.ge.1) then
        write(66,*)' '
        write(66,*)'variable de conception nø',k
        do 5 j=1,8+nsol
    5     write(66,17) j,(dhyp(kh,j,k),kh=1,ms)
        endif
    7 continue

      return
  16  format(6e11.4)
  17  format(i2,6e11.4)
  18  format(/'dérivées des hyp (xo et zo) pour chaque variable xi'
     *       /40(1h=))
      end

c***********************************************************************
c***********************************************************************

      subroutine dinteq (darg,argq,q,disx,mt,m,lr,kc,abtr,daux,k,dvar)
      implicit real *8(a-h,o-z)
      dimension argq(8),disx(360),abtr(mt),daux(2*mt,2*mt),
     *          dvar(33,9,16)
      dimension darg(8,9)
c***********************************************************************
c     subroutine dinteq
c     ------------------
c     subroutine de remplissage des coefficients de la derivee de la
c     matrice des inconnues hyperstatiques des traverses
c
c     resultat : la matrice [ daux ].
c     ---------
c
c     modifié: le 4-8-96 (ph. rigo)                  créer : thèse
c***********************************************************************
c   avec:abtr = abtr(i),i=1,mt
c          disx = disb ou disc
c           kc  = indice sur les sollicitations: kc = 1  disb et kc = 2
c           lr  = indice sur les forces        : kc = 1  xo   et kc = 2
c            k  = nø de la variable de conception

      pi=2.d00*acos(0.)
      pi1=pi/180.
      pi2=2.*pi

      ki=(kc-1)*mt+1
      kf=kc*mt
      li=1+(lr-1)*mt
      lf=lr*mt

c       write(66,*) 'lr=',lr,' et kc=',kc
c       write(66,*) '===================='
c       write(66,*)' argq',argq
c       write(66,*)' q',q

      do 14 j=ki,kf

c       write(66,*) 'j=',j
c       write(66,*) '------'

      do 15 i=li,lf
      n=11+2*(i-(lr-1)*mt)+lr

      k9=j-ki+1
      l9=i-li+1
      z1=dabs(abtr(l9)-abtr(k9))*pi1
      z1q=z1*q
      z11=pi2-z1
      z11q=z11*q

c       write(66,*) 'z1=',z1,' et z1q=',z1q

      do 16 l=1,m
      l2=2*l
      l1=l2-1
      jj=4*(l-1)
      kh=jj+lr*16+32*(i-(lr-1)*mt)

c       write(66,*)'m,l,l1,l2,jj,kh,n=',m,l,l1,l2,jj,kh,n

      al1=argq(l1)*z1
      be1=argq(l2)*z1
c       write(66,*)'al1,be1=',al1,be1

      al3=argq(l1)*z11
      be3=argq(l2)*z11
      al1=expo(al1)
      al3=expo(al3)
      cos1=al1*dcos(be1)
      sin1=al1*dsin(be1)
      cos3=al3*dcos(be3)
      sin3=al3*dsin(be3)
c       write(66,*)'al3,be3=',al3,be3

      daux(i,j)=daux(i,j)
     * - z1q * ( cos1* (darg(l1,k)*disx(kh+1)-darg(l2,k)*disx(kh+2) )
     *          +sin1* (darg(l1,k)*disx(kh+2)+darg(l2,k)*disx(kh+1) ) )
     * - z11q* ( cos3* (darg(l1,k)*disx(kh+3)-darg(l2,k)*disx(kh+4) )
     *          +sin3* (darg(l1,k)*disx(kh+4)+darg(l2,k)*disx(kh+3) ) )
     * + cos1 * dvar(n,k,jj+1)  + sin1 * dvar(n,k,jj+2)
     * + cos3 * dvar(n,k,jj+3)  + sin3 * dvar(n,k,jj+4)

c       write(66,*)'cos1,sin1=',cos1,sin1, ' cos3,sin3=',cos3,sin3
c       write(66,*)'darg(l1,k),darg(l2,k)=',darg(l1,k),darg(l2,k)
c       write(66,*)'disx(kh+1à4)=',disx(kh+1),disx(kh+2),disx(kh+3),
c     *                           disx(kh+4)
c       write(66,*)'dvar(n,k,jj+1 à 4)=',dvar(n,k,jj+1),dvar(n,k,jj+2),
c     *                                 dvar(n,k,jj+3),dvar(n,k,jj+4)
c       write(66,*)'daux(i,j)=',daux(i,j)

 16   continue

      daux(i,j)=-daux(i,j)/10000.

 15   continue
 14   continue

      return
      end


c***********************************************************************
c     subroutine pour la charge extérieure (dish)
c     +++++++++++++++++++++++++++++++++++++++++++++

      subroutine dzb1(darg,phil,teta,argq,q,dish,mt,m,abtr,dzb,k,dvarh)
      implicit real *8(a-h,o-z)
      dimension argq(8),dish(1710),abtr(mt),dzb(2*mt),dvarh(33,9,38)
      dimension darg(8,9)

c***********************************************************************

      pi=2.d00*acos(0.)
      pi1=pi/180.
      pi2=2.*pi
      phil1=phil*pi1
      teta1=teta*pi1

c     boucle sur les traverses (mt)
c       ------------------------------
      do 3 i=1,mt

c     soit z1 la coordonnée du point de calcul par rapport à y=o
      z1=abtr(i)*pi1
      z1q=z1 * q
      z11=pi2-z1
      z11q=z11*q
c     soit z2 la coordonnée du point de calcul  par rapport à y=yo (phil
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

c     ii=1 pour xo et ii=2 pour zo
c       ------------------------------
      do 4 ii=1,2

      jj=2*(i-1)+ii
      j=13+jj
      ik=(ii-1)*mt+i

      dzb(ik)=dvarh(j,k,1)+dvarh(j,k,2)*z1
     *                    +dvarh(j,k,3)*z1*z1+dvarh(j,k,4)*(z1**3)
     *                    +dvarh(j,k,5)*cost +dvarh(j,k,6)*sint

c     boucle sur m
c       --------------
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

      dzb(ik)=dzb(ik)
     * - z1q * ( cos1* (darg(l1,k)*dish(ij+1)-darg(l2,k)*dish(ij+2) )
     *          +sin1* (darg(l2,k)*dish(ij+1)+darg(l1,k)*dish(ij+2) ) )
     * - z2q * ( cos2* (darg(l1,k)*dish(ij+3)-darg(l2,k)*dish(ij+4) )
     *          +sin2* (darg(l2,k)*dish(ij+3)+darg(l1,k)*dish(ij+4) ) )
     * - z11q* ( cos3* (darg(l1,k)*dish(ij+5)-darg(l2,k)*dish(ij+6) )
     *          +sin3* (darg(l2,k)*dish(ij+5)+darg(l1,k)*dish(ij+6) ) )
     * - z22q* ( cos4* (darg(l1,k)*dish(ij+7)-darg(l2,k)*dish(ij+8) )
     *          +sin4* (darg(l2,k)*dish(ij+7)+darg(l1,k)*dish(ij+8) ) )
     * + cos1 * dvarh(j,k,lk+1)  + sin1 * dvarh(j,k,lk+2)
     * + cos2 * dvarh(j,k,lk+3)  + sin2 * dvarh(j,k,lk+4)
     * + cos3 * dvarh(j,k,lk+5)  + sin3 * dvarh(j,k,lk+6)
     * + cos4 * dvarh(j,k,lk+7)  + sin4 * dvarh(j,k,lk+8)

    5 continue
    4 continue
    3 continue
      return
      end

c***********************************************************************
c     subroutine pour les 8 charges de bords (disc, d, a, b) en y=0 et y
c     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine dzb2(darg,phil,argq,q,disx,mt,m,abtr,dzb,k,dvarx,nbr,
     *                isign)
      implicit real *8(a-h,o-z)
      dimension argq(8),disx(720),abtr(mt),dzb(2*mt,9),
     *          dvarx(33,9,16)
      dimension darg(8,9)

c***********************************************************************

      pi=2.d00*acos(0.)
      pi1=pi/180.
      pi2=2.*pi

c     boucle sur les traverses (mt)
c       ------------------------------
      do 3 i=1,mt

c       write(66,*)'traverse nø',i
c       write(66,*)'=============='

        if((nbr.eq.2).or.(nbr.eq.4).or.(nbr.eq.6).or.(nbr.eq.8)) then
c     soit z1 la coordonnée du point de calcul par rapport à y=o
      z1=abtr(i)*pi1
      z1q=z1 * q
      z11=pi2-z1
      z11q=z11*q
      else
c     soit z1 la coordonnée du point de calcul  par rapport à y=yo (phil
      z1=(phil-abtr(i))*pi1
      z1q=z1*q
      z11=pi2-z1
      z11q=z11*q
      endif

c      write(66,*)' z1 =',z1, ' z1q =',z1q
c      write(66,*)' sign =',sign,' nbr =',nbr


c     ii=1 pour xo et ii=2 pour zo
c       ------------------------------
      do 4 ii=1,2

c       write(66,*)'ii=1 pour xo et ii=2 pour zo : ii=',ii
c       write(66,*)'------------------------------------'

      jj=2*(i-1)+ii
      j=13+jj
      ik=(ii-1)*mt+i

c       write(66,*)'jj=',jj
c       write(66,*)'j=',j
c       write(66,*)'ik=',ik

c     boucle sur m
c       -------------
      do 5 l=1,m
      l2=2*l
      l1=l2-1

      lk=(l-1)*4
      ij=16*(jj+2)+lk

c       write(66,*)'lk=',lk
c       write(66,*)'ij=',ij

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

c      write(66,*)' cos1 =',cos1, ' sin1 =',sin1
c      write(66,*)' dzb(ik,nbr)=',dzb(ik,nbr)
c      write(66,*)' disx(ij+1)=',disx(ij+1),disx(ij+2)
c      write(66,*)'darg(l1,k) et (darg(l2,k)=',darg(l1,k),darg(l2,k)
c      write(66,*)'dvarx(j,k,lk+1)=',dvarx(j,k,lk+1),dvarx(j,k,lk+2)

      dzb(ik,nbr)=dzb(ik,nbr)
     * - z1q * ( cos1* (darg(l1,k)*disx(ij+1)-darg(l2,k)*disx(ij+2) )
     *          +sin1* (darg(l1,k)*disx(ij+2)+darg(l2,k)*disx(ij+1) ) )
     * - z11q* ( cos3* (darg(l1,k)*disx(ij+3)-darg(l2,k)*disx(ij+4) )
     *          +sin3* (darg(l1,k)*disx(ij+4)+darg(l2,k)*disx(ij+3) ) )
     * + cos1 * dvarx(j,k,lk+1)  + sin1 * dvarx(j,k,lk+2)
     * + cos3 * dvarx(j,k,lk+3)  + sin3 * dvarx(j,k,lk+4)

c      write(66,*)' dzb(ik,nbr)=',dzb(ik,nbr)

    5 continue

      dzb(ik,nbr)=dzb(ik,nbr) * isign

    4 continue
    3 continue


      return
      end

