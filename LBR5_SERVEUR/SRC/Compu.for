      subroutine compu(phil,fp,a,c,b,x,m,argq,mt,u,teta,z,l,sy,icas)
      implicit double precision(a-h,o-z)
      dimension a(1710),b(720),c(720),x(10),argq(8),fp(20),u(51),z(2295)

c***********************************************************************
c
c     subroutine compu
c     la subroutine calcule les valeurs aux 31+20 points de l'axe ox,
c     des resultats pour un effet considere.
c
c  données : a  = dish,  b=disb et c=disc
c            x  = position des 10 traverses (abtr)
c            fp = valeurs des forces xo et zo à appliquer au droit des
c                  traverses
c            u  = coordonnées des 51 pts de calcul selon oy
c	       l  = nø de la fonction calculée (u,v,w, ...)

c  output :  z  = vecteur contenu les valeurs de z2 (par blocs de 51)
c
c     boucle 10:effet general sur la plaque orthotrope.
c     boucle 600:effet des traverses sur la plaque orthotrope.
c     boucle 60:calcul au dessus et en dessous de l'axe de chaque
c     traverses (20 points de calcul au maximum).
c
c    modif : 16-6-95                        création : thèse ph. rigo
c***********************************************************************

      pi=2.d00*acos(0.d00)
      pi1=pi/180.d00
c      itest1=1

      do 10 i=1,icas
        y=u(i)
        y1=y*pi1
        il=(l-1)*51+i
        ij=38*(l-1)+1
            if(dabs(teta).ne.180.d00) goto 404
            cc=-dcos(y1)
            ss=-dsin(y1)
            goto 406
 404        if(dabs(teta).ne.90.) goto 405
            ii=1.
            if(teta.eq.-90.) ii=-1
            cc=-ii*dsin(y1)
            ss=ii*dcos(y1)
            goto 406
  405       cc=dcos((teta+y)*pi1)
            ss=dsin((teta+y)*pi1)
  406   z(il)=a(ij)+a(ij+1)*y1+a(ij+2)*(y1*y1)
     *             +a(ij+3)*(y1**3)+a(ij+4)*cc+a(ij+5)*ss
        do 5 k=1,m
           ij=38*(l-1)+8*k-1
c
c          calcul de z(il)
c           go to 200
		 al1=argq(2*k-1)*y1
           be1=argq(2*k)*y1
           al2=argq(2*k-1)*(phil-y)*pi1
           be2=argq(2*k)  *(phil-y)*pi1
           al3=argq(2*k-1)*(360.-y)*pi1
           be3=argq(2*k)  *(360.-y)*pi1
           al4=argq(2*k-1)*(360.-(phil-y))*pi1
           be4=argq(2*k)  *(360.-(phil-y))*pi1
           al1=expo(al1)
           al2=expo(al2)
           al3=expo(al3)
           al4=expo(al4)
           r=al1*(a(ij  )*dcos(be1)+a(ij+1)*dsin(be1))
     *      +al2*(a(ij+2)*dcos(be2)+a(ij+3)*dsin(be2))
     *      +al3*(a(ij+4)*dcos(be3)+a(ij+5)*dsin(be3))
     *      +al4*(a(ij+6)*dcos(be4)+a(ij+7)*dsin(be4))
c
   12      z(il)=z(il)+r
    5   continue
   10 continue
c
c
      if(mt.eq.0)goto 19
c      itest2=1
      do 600 i=1,icas
      y=u(i)
      il=(l-1)*51+i
      do 3 j=1,mt
      kh=j+mt
      do 16 k=1,m
      sign=1.
      jm=4*k+(l-1)*16-3
c
c   calcul de p,q,pp,qq
c      goto 201
	dely=dabs(x(j)-y)
      dely1=dely*pi1
      al1=argq(2*k-1)*dely1
      be1=argq(2*k)*dely1
      al2=argq(2*k-1)*(360.-dely)*pi1
      be2=argq(2*k)*(360.-dely)*pi1
      al1=expo(al1)
      al2=expo(al2)
      p=al1*dcos(be1)
      q=al1*dsin(be1)
      pp=al2*dcos(be2)
      qq=al2*dsin(be2)

  301 if((dabs((x(j)-y)/phil).lt.(0.00001)).or.(x(j).lt.y))goto 16
      if(sy)18,16,16
   18 sign=-1.
   16 z(il)=z(il)+(fp(j)*(b(jm)*p+b(jm+1)*q+b(jm+2)*pp+b(jm+3)*qq)
     *      +fp(kh)*(c(jm)*p+c(jm+1)*q+c(jm+2)*pp+c(jm+3)*qq))*sign
    3 continue
  600 continue
c
c
      if(icas.le.7) goto 19
c      itest1=2
c      itest2=2
      do 60 i=1,mt
      y=x(i)
      y1=y*pi1
      il=(l-1)*51+2*i+30
      ij=38*(l-1)+1
            if(dabs(teta).ne.180.d00) goto 504
            cc=-dcos(y1)
            ss=-dsin(y1)
            goto 506
  504       if(dabs(teta).ne.90.) goto 505
            ii=1.
            if(teta.eq.-90.) ii=-1
            cc=-ii*dsin(y1)
            ss= ii*dcos(y1)
            goto 506
  505       cc=dcos((teta+y)*pi1)
            ss=dsin((teta+y)*pi1)
  506 r=a(ij)+a(ij+1)*y1+a(ij+2)*(y1*y1)
     *       +a(ij+3)*(y1**3)+a(ij+4)*cc+a(ij+5)*ss
      z(il)=r
      z(il+1)=r
      do 61 k=1,m
      ij=38*(l-1)+8*k-1
c
c   calcul de z(il)
c      go to 200
	al1=argq(2*k-1)*y1
      be1=argq(2*k)*y1
      al2=argq(2*k-1)*(phil-y)*pi1
      be2=argq(2*k)  *(phil-y)*pi1
      al3=argq(2*k-1)*(360.-y)*pi1
      be3=argq(2*k)  *(360.-y)*pi1
      al4=argq(2*k-1)*(360.-(phil-y))*pi1
      be4=argq(2*k)  *(360.-(phil-y))*pi1
      al1=expo(al1)
      al2=expo(al2)
      al3=expo(al3)
      al4=expo(al4)
      r=al1*(a(ij  )*dcos(be1)+a(ij+1)*dsin(be1))
     *  +al2*(a(ij+2)*dcos(be2)+a(ij+3)*dsin(be2))
     *  +al3*(a(ij+4)*dcos(be3)+a(ij+5)*dsin(be3))
     *  +al4*(a(ij+6)*dcos(be4)+a(ij+7)*dsin(be4))

   14 z(il)=z(il)+r
   61 z(il+1)=z(il+1)+r


      do 21 j=1,mt
      kh=j+mt
      do 24 k=1,m
      sign=1.
      jm=4*k+(l-1)*16-3
c
c     calcul de p,q,pp,qq
c      goto 201
	dely=dabs(x(j)-y)
      dely1=dely*pi1
      al1=argq(2*k-1)*dely1
      be1=argq(2*k)*dely1
      al2=argq(2*k-1)*(360.-dely)*pi1
      be2=argq(2*k)*(360.-dely)*pi1
      al1=expo(al1)
      al2=expo(al2)
      p=al1*dcos(be1)
      q=al1*dsin(be1)
      pp=al2*dcos(be2)
      qq=al2*dsin(be2)
c
  302 r=fp(j)*(b(jm)*p+b(jm+1)*q+b(jm+2)*pp+b(jm+3)*qq)
     *      +fp(kh)*(c(jm)*p+c(jm+1)*q+c(jm+2)*pp+c(jm+3)*qq)
      if(sy)53,52,52
   53 if(dabs((x(j)-y)/phil).le.(0.01))goto 54
      if(x(j).lt.y)goto 52
      sign=-1.
   52 r=r*sign
      r1=r
      goto 58
   54 sign=-1
      r=r*sign
      r1=-r
   58 z(il)=z(il)+r
      z(il+1)=z(il+1)+r1
   24 continue
   21 continue
   60 continue
   19 continue
      return
	end

c  ******************************************************************
c     boucle 200 : calcul des exponentielles
c  ******************************************************************
c  200 al1=argq(2*k-1)*y1
c      be1=argq(2*k)*y1
c      al2=argq(2*k-1)*(phil-y)*pi1
c      be2=argq(2*k)  *(phil-y)*pi1
c      al3=argq(2*k-1)*(360.-y)*pi1
c      be3=argq(2*k)  *(360.-y)*pi1
c      al4=argq(2*k-1)*(360.-(phil-y))*pi1
c      be4=argq(2*k)  *(360.-(phil-y))*pi1
c      if((l.ne.30).and.(l.ne.25)) goto 6745
c      if(i.ge.10) goto 6745
c     write(6,*)phil,y,l
c     write(6,*)al1,al2,al3,al4
c     write(6,*)be1,be2,be3,be4
c 6745 al1=expo(al1)
c      al2=expo(al2)
c      al3=expo(al3)
c      al4=expo(al4)
c      r=al1*(a(ij  )*dcos(be1)+a(ij+1)*dsin(be1))
c     * +al2*(a(ij+2)*dcos(be2)+a(ij+3)*dsin(be2))
c     * +al3*(a(ij+4)*dcos(be3)+a(ij+5)*dsin(be3))
c     * +al4*(a(ij+6)*dcos(be4)+a(ij+7)*dsin(be4))
c      if((l.ne.30).and.(l.ne.25)) goto 6746
c      if(i.ge.10) goto 6746
c     write(6,*)al1,al2,al3,al4
c     write(6,*)k,r
c6746  goto(12,14),itest1

c  ******************************************************************
c     boucle 201 : calcul des exponentielles
c  ******************************************************************
c 201  dely=dabs(x(j)-y)
c      dely1=dely*pi1
c      al1=argq(2*k-1)*dely1
c      be1=argq(2*k)*dely1
c      al2=argq(2*k-1)*(360.-dely)*pi1
c      be2=argq(2*k)*(360.-dely)*pi1
c      al1=expo(al1)
c      al2=expo(al2)
c      p=al1*dcos(be1)
c      q=al1*dsin(be1)
c      pp=al2*dcos(be2)
c      qq=al2*dsin(be2)
c      goto(301,302),itest2
c      return
c      end
