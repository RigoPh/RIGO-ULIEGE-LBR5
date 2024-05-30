      subroutine allocatevar(ilangue,inputfile,neto_a,iana_a)		!r&d14	!fev2007
      use sharedvar
      implicit double precision (a-h,o-z)
      character*80 inputfile
      langue=ilangue
      don1=inputfile
      neto=neto_a
	iana=iana_a		!r&d14	!fev2007
c
c valeurs des variables de l'ancien common dim1 ...............................
      nmax   = neto	!neto   !nbr max de panneau											(aout2006)
      nemax  = nmax*8
      nvmax  = nmax*9 !le nbre  de variables de conception (max = 9*n)					(aout2006)
      m1max  =  20    !nbre max  rest. struct. par panneau (pour 1 cas de charge)
      m1tmax =20*nmax !nbre max  rest. struct. pour la struct. (pour un cas de charge)	(aout2006)
      m2max  =  20    !nbre max  rest. géom. par panneau,									(aout2006)
      m2tmax =20*nmax !nbre max  rest. géom. pour la struct								(aout2006)
      mmmax  =7000    !mm=somme(m1t)+m2t  < 8000											!extension neto	!fev2007
      ngmax  = nmax*9 !nbre max  rest. égalité (pour la struct.)...(aout2006)				!dec2006
      ismax  =  10    !nombre max de cas de charges retenus pour l'analyse (max=10).
                      !is est limité à 10 à cause de sensh(13,4,9,nsol=10) dans common opti2
                      ! et de dzsn (8*neto,9*neto,nsol)
      iptmax =   4    !nombre max de pts de calcul des sensibilités par panneau
c
c valeur de pi ................................................................
      pi=2.d00*asin(1.d00)
c
c allocation des tableaux et matrices .........................................
      allocate (modes(neto))
      allocate (blocka(nemax,nemax))
      allocate (solt(nemax,nemax))
      allocate (sens1(9,iptmax,9,nmax))
      allocate (sens2(16,iptmax,9,nmax))
      allocate (sens3(21,iptmax,9,nmax))
      allocate (sens4(9,iptmax,9,nmax))
      allocate (a(nemax,nemax))
      allocate (b(nemax,nemax+1))
      allocate (dzsn(nemax,9*nmax,ismax))
      allocate (cha(100,3,ismax))
      allocate (dcha(100,ismax,9))
      allocate (npt(ismax))
      allocate (lm2(ngmax))
      allocate (tetas(nmax))
      allocate (tetaq(nmax))
      allocate (delt(nmax))
      allocate (delt2(nmax))
      allocate (z(nmax,4))
      allocate (angle(nmax))
      allocate (noh(nmax,10))
      allocate (noeud(nmax,2))
      allocate (mcomp(2*nmax,2))
      allocate (nno(nmax+1,2))
      allocate (nsign(nmax))
      allocate (e(nmax))
      allocate (eta(nmax))
      allocate (sigy(nmax))
      allocate (coefk(nmax))						!r&d15
      allocate (spec(nmax))
      allocate (ploc(nmax))
      allocate (ypts(iptmax,nmax))
      allocate (nvar(nmax))
      allocate (m1cont(nmax))
      allocate (m2cont(nmax))
      allocate (nxit(9,nmax))
      allocate (ipts(nmax))
      allocate (ipts2(iptmax,nmax))
      allocate (ipts3(10,nmax))					!mt=10	!fev2007
      allocate (lcont(2,m1tmax))
      allocate (lcont4(ismax,nmax,m1max,2))
      allocate (nvarr(nmax))
      allocate (nxitr(9,nmax))
      allocate (isemr(nvmax))
      allocate (isema(nvmax))
      allocate (xicou(nvmax))
      allocate (ximin(nvmax))
      allocate (ximax(nvmax))
      allocate (ximin_dcns(nvmax))
      allocate (ximax_dcns(nvmax))
      allocate (cjmax(m1tmax))
      allocate (cjmax3(ismax,nmax,m1max))
      allocate (ega(ngmax))
      allocate (inv(m1tmax))
      allocate (inv3(ismax,nmax,m1max))
      allocate (mega(ngmax,4))
      allocate (nvs(nmax))
      allocate (nxi2(9,nmax))
      allocate (negal(nmax))
      allocate (kli(nemax,2))
      allocate (zsn(nemax,ismax))
      allocate (qn(nmax))
      allocate (philn(nmax))
      allocate (abc(nmax))
      allocate (asol(nmax))
      allocate (bsol(nmax))
      allocate (csol(nmax))
      allocate (dsol(nmax))
      allocate (amom(nmax))
      allocate (bmom(nmax))
      allocate (cmom(nmax))
      allocate (dmom(nmax))
      allocate (part(nmax))
      allocate (abcd(8))
      allocate (coef(nmax,8,10))		!sept2000	!allocate (coef(100,8,10))
      allocate (icha(200))
      allocate (itype(nmax))
      allocate (isect(nmax))
      allocate (tfa(nmax))
      allocate (tfr(nmax))
      allocate (m1tabl(nmax,ismax))
      allocate (izut(mmmax))
      allocate (zut(mmmax))
      allocate (corro(nmax,3))    !corrosion			!aout 2006
      allocate (corroel(3))       !corrosion			!aout 2006 
c	allocate (dw(9*nmax))		!restri poids		!septembre 2006	
	allocate (dixx(9))								!septembre 2006	

c variables ushull ............................................................
      allocate (dsud(9,nmax))
      allocate (dsub(9,nmax))
      allocate (dsub1(9,nmax))
      allocate (dsusu(9,nmax))
      allocate (dsusl(9,nmax))
      allocate (dh(9,nmax))
      allocate (dg(9,nmax))
      allocate (dad(9,nmax))
      allocate (dab(9,nmax))
      allocate (dab1(9,nmax))
      allocate (das(9,nmax))
      allocate (dult(9*nmax))
	allocate (id(nmax))				!dec2006
	allocate (ib1(nmax))			!dec2006
	allocate (ib(nmax))				!dec2006
	allocate (is9(nmax))			!dec2006
c
c variables sensibcout ........................................................
      allocate (vlarg(nmax))
      allocate (vepaiss(nmax))
      allocate (vhac(nmax))
      allocate (veac(nmax))
      allocate (vlsc(nmax))
      allocate (vesc(nmax))
      allocate (deltacsc(nmax))
      allocate (vhar(nmax))
      allocate (vear(nmax))
      allocate (vlsr(nmax))
      allocate (vesr(nmax))
      allocate (deltar(nmax))
      allocate (entrsc(nmax))
      allocate (philsc(nmax))
      allocate (qsc(nmax))
      allocate (coutpan(nmax))
      allocate (costpan(nmax))
      allocate (coutmat(nmax))
      allocate (dcorsc(nmax))
      allocate (dcorsc2(nmax))
      allocate (dcorsc3(nmax))
c      allocate (dfctsc(nmax))	!janv2007
      allocate (vdiffc(nmax))

c variables optis .............................................................
      allocate (derredress(nvmax))    !redressage
	allocate (xiopt(nvmax))			!extension neto !fev2007
	allocate (fiopt(nvmax))			!extension neto !fev2007
	allocate (cjopt(mmmax))			!extension neto !fev2007
	allocate (ic_rest(mmmax))		!extension neto !fev2007  !dad
	allocate (ratio_rest(mmmax))		!extension neto !fev2007  !dad
	allocate (cjmopt(mmmax))		!extension neto !fev2007
      allocate (cijopt(nvmax,mmmax))	!real*8 cijopt(800,7000) oct06
	allocate (dxopt(nvmax,nvmax))	!extension neto !fev2007
	allocate (z1opt(nvmax))			!extension neto !fev2007
	allocate (z2opt(nvmax))			!extension neto !fev2007
      allocate (z3opt(nvmax))			!extension neto !fev2007
	iconlin=8*nvmax + 3*mmmax + nvmax*mmmax + 2*max0(nvmax,mmmax) + 3	!extension neto !fev2007
	allocate (sopt(iconlin))		!extension neto !fev2007
	allocate (fimulti(nvmax))		!multi obj

c	variables inertia
	allocate (dyneutnet(9,nmax))		!r&d14	!fev2007
	allocate (dyneutgro(9,nmax))		!r&d14	!fev2007
	allocate (dinet(9,nmax))			!r&d14	!fev2007
	allocate (digro(9,nmax))			!r&d14	!fev2007
	allocate (dyneutplnet(9,nmax))		!r&d14	!fev2007
	allocate (dyneutplgro(9,nmax))		!r&d14	!fev2007
	allocate (diplnet(9,nmax))			!r&d14	!fev2007
	allocate (diplgro(9,nmax))			!r&d14	!fev2007
	allocate (dsignet(2,9,nmax))		!r&d14	!fev2007
	allocate (dsiggro(2,9,nmax))		!r&d14	!fev2007

c	variables shear
	allocate (taunet(nmax,3))			!r&d14	!fev2007
	allocate (taugro(nmax,3))			!r&d14	!fev2007
	allocate (dtaunet(nmax,3,nmax))		!r&d14	!fev2007
	allocate (dtaugro(nmax,3,nmax))		!r&d14	!fev2007

c	variables buckdcn_07
	allocate (ddbuckpl_07  (9,nmax))				!dcn07
	allocate (ddbuckstif_07(9,nmax))			    !dcn07
	allocate (dinpart_07   (9,nmax))				!dcn07

c	variables buckdcn_08
	allocate (ddbuckpl_08  (9,nmax))				!dcn08
	allocate (ddbuckstif_08(9,nmax))			    !dcn08
	allocate (dinpart_08   (9,nmax))				!dcn08


c	variables dcn08

	allocate (itype_transv(nmax))
	allocate (itype_longit(nmax))
      allocate (itype_transv_second(nmax))
      allocate (itype_longit_second(nmax))

      allocate (name_type_transv(nmax))
      allocate (name_type_transv_second(nmax))
	allocate (name_type_longit(nmax))
	allocate (name_type_longit_second(nmax))
      allocate (rend_panel(nmax))

c initialisation des tableaux et matrices .........................................
      do i=1,8
       abcd(i)=0.d00
      enddo
      do i=1,ngmax
       ega(i)=0.
       do j=1,4
        mega(i,j)=0
       enddo
      enddo
      do i=1,nmax
       noeud(i,1)=0
       noeud(i,2)=0
       ipts(i)=0
       nvs(i)=0
       negal(i)=0
       tetas(i)=0.d00
       tetaq(i)=0.d00
       do j=1,3             !aout 2006
	  corro(i,j)=0        !aout 2006
       enddo                !aout 2006
       do j=1,ismax
        m1tabl(i,j)=0
       enddo
       do j=1,iptmax
        ypts(j,i)=0.d00
        do k=1,16
         do l=1,9
          sens2(k,j,l,i)=0.
         enddo
        enddo
       enddo
       do j=1,iptmax
        ipts2(j,i)=0
        ipts3(j,i)=0
       enddo
       do j=1,9
        nxi2(j,i)=0
        nxit(j,i)=0
       enddo
      enddo
      do i=1,ismax
       npt(i)=0
      enddo
      do i=1,mmmax
       zut(i)=0.d00
       izut(i)=0
      enddo
      do i=1,ismax
       do j=1,nmax
        do k=1,m1max
         inv3(i,j,k)=0
         cjmax3(i,j,k)=0.d00
         do l=1,2
          lcont4(i,j,k,l)=0
         enddo
        enddo
       enddo
      enddo


	itype_transv(1:nmax)=0
      itype_longit(1:nmax)=0
	name_type_transv(1:nmax)=''
      name_type_longit(1:nmax)=''
	rend_panel(1:nmax)=0


      end
