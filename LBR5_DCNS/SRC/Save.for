      subroutine save(nel,indaig,indrai,ityp1)

c     ***********************************************************************
c     subroutine de sauvetage des données en vue des dessins
c     modifié : 8-8-94                     créer: thèse de ph. rigo
c               avril 99 (lbr5.1)
c                1-11-2000 (version anglaise)
c               25-02-2001 impression de tous les cas de charge.
c               15-12-2002 impression de 4 types de contr. dans les cadres (au lieu des raid.)
c			  04-02-2004 impression des contraintes des épontilles à la place de certaintes autres contraintes
c     ***********************************************************************

      implicit real *8 (a-h,o-z)
      dimension eff(9690),icode(19)
	dimension p(6275)			!vecteur de travail		!février 2004
      common/graf/ncas,icode
c     common/di/eff

      if(nel.ne.1) goto 20
	                                               
c     description  :   variables
c     ***********      *********
      ncas=10             !février 2004
      icode(1)=1          ! v
      icode(2)=2          ! u
      icode(3)=3          ! w
      icode(4)=5          ! nx
      icode(5)=6          ! ny
      icode(6)=7          ! nxy
      icode(7)=9          ! mx
      icode(8)=10         ! my
      icode(9)=15         ! ry
      icode(10)=16        ! rx
      icode(11)=17        ! sy
      icode(12)=18        ! sx
      icode(13)=19        ! txy
      icode(14)=20        ! comp
                          !
      icode(15)=24        ! tau cadre jab		tau épon a.n.
      icode(16)=21        ! sig cadre jas		sig épon jas
                          !
      icode(17)=25        ! sig comp jas (cadre)  le 15-12-2002
      icode(18)=26        ! sig comp jab (cadre)

  20  continue
      do 12 i=1,ncas
      do 12 j=1,5
       k1=(icode(i)-1)*255+1+(j-1)*51
       k2=k1+30
  12  write(43)(eff(k),k=k1,k2,3)

	if(ityp1.ne.5) then					!février 2004
      do 18 i=11,14						!février 2004
      do 18 j=1,5							!février 2004
      k1=(icode(i)-1)*255+1+(j-1)*51		!février 2004
      k2=k1+30							!février 2004
  18  write(43)(eff(k),k=k1,k2,3)			!février 2004
	goto 19								!février 2004
	else				! si épontille	!février 2004
 25	if(i.gt.14) goto 19					!février 2004
	l=i-10								!février 2004
 	goto (24,23,24,24),l				!février 2004
 24   do 22 j=1,5							!février 2004
	icode(11)=23				!sy an  !février 2004                  		
      icode(13)=20				!t an	!février 2004
	icode(14)=23				!sy an	!février 2004
      k1=(icode(i)-1)*255+1+(j-1)*51		!février 2004
      k2=k1+30							!février 2004
  22  write(43)(eff(k),k=k1,k2,3)			!février 2004
	 i=i+1								!février 2004
	goto 25								!février 2004
	
  23  do 21 j=1,5							!février 2004
      k1=(icode(12)-1)*255+1+(j-1)*51		!février 2004
      k2=k1+30							!février 2004
	p(k)=0.0000							!février 2004
  21  write(43)(p(k),k=k1,k2,3)			!février 2004
	i=i+1								!février 2004
	goto 25								!février 2004
	endif								!février 2004
c     if(indaig.eq.0)  goto 15
c     do 13 i=15,16
  19  continue							!février 2004
	if(indaig.eq.0) return ! le 15-12-2002
      do 13 i=15,18          ! le 15-12-2002
      do 13 j=1,5
 	if(ityp1.eq.5)then											!février 2004
	icode(15)=20                          ! tau épon jab		!février 2004
      icode(16)=17                          ! sig épon jas sup	!février 2004
	icode(17)=21						  ! sig comp jas sup	!février 2004
	icode(18)=23						  ! sig comp a.n.		!février 2004
	endif														!février 2004
      k1=(icode(i)-1)*255+1+(j-1)*51
       k2=k1+30
  13  write(43)(eff(k),k=k1,k2,3)

      return                ! le 15-12-2002

  15  continue
      if(indrai.eq.0) goto 16
      do 14 i=17,18
      do 14 j=1,5
 	if(ityp1.eq.5)then											!février 2004
	icode(17)=21						  ! sig comp jas sup	!février 2004
	icode(18)=23						  ! sig comp a.n.		!février 2004
	endif														!février 2004
       k1=(icode(i)-1)*255+1+(j-1)*51
       k2=k1+30
  14  write(43)(eff(k),k=k1,k2,3)

  16  return
      end
