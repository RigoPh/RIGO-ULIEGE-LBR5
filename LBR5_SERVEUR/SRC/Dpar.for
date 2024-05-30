      subroutine dpar(m,px,coeff,dcoeff,dpx,dvar,isym,nxi,nbrxi,mt)
c     ****************
      implicit double precision(a-h,o-z)

      dimension coeff(32,41),dcoeff(32,33,9),dpx(8,9),dvar(33,9,16),
     *          px(8),nxi(9)

c***********************************************************************
c
c    expression des d�riv�es des disa, disb, disc et disd.
c    -----------------------------------------------------
c   dvar(13,9,16) avec 13 variables (u v w� ny nyx my ry w w�� w��� u� v
c                       9 variables de conception,
c                   et 16 termes (pour disc, disa, disb et disd).

c       dvar = dvara,dvarb,dvarc,dvard

c     modif: 14-6-95                                     cr�er : 28-2-94
c***********************************************************************
c     ordre des termes
c       1 : exp(-alpha q y) * cos (b�ta q y)
c       2 : exp(-alpha q y * sin (b�ta q y)
c       3 : exp(-alpha q (2pi-y)) * cos (b�ta q (2pi-y))
c       4 : exp(-alpha q (2pi-y)) * sin (b�ta q (2pi-y))

      call temp(coeff,dcoeff,px,dpx,dvar, 1,m,isym,1, 1,nxi,nbrxi)
      call temp(coeff,dcoeff,px,dpx,dvar, 2,m,isym,2,-1,nxi,nbrxi)
      call temp(coeff,dcoeff,px,dpx,dvar, 3,m,isym,3,-1,nxi,nbrxi)
      call temp(coeff,dcoeff,px,dpx,dvar, 4,m,isym,4, 1,nxi,nbrxi)
      call temp(coeff,dcoeff,px,dpx,dvar, 6,m,isym,5,-1,nxi,nbrxi)
      call temp(coeff,dcoeff,px,dpx,dvar, 8,m,isym,6, 1,nxi,nbrxi)
      call temp(coeff,dcoeff,px,dpx,dvar,14,m,isym,7,-1,nxi,nbrxi)
      call temp(coeff,dcoeff,px,dpx,dvar,16,m,isym,8, 1,nxi,nbrxi)
      call temp(coeff,dcoeff,px,dpx,dvar,37,m,isym,9, 1,nxi,nbrxi)
      call temp(coeff,dcoeff,px,dpx,dvar,38,m,isym,10,-1,nxi,nbrxi)
      call temp(coeff,dcoeff,px,dpx,dvar,39,m,isym,11,-1,nxi,nbrxi)
      call temp(coeff,dcoeff,px,dpx,dvar,40,m,isym,12, 1,nxi,nbrxi)
      call temp(coeff,dcoeff,px,dpx,dvar,41,m,isym,13,-1,nxi,nbrxi)

      do 1 j=1,mt
       k1=15+j*2
       k2=12+j*2
       call temp(coeff,dcoeff,px,dpx,dvar,k1,m,isym,k2,1,nxi,nbrxi)
       k1=k1+1
       k2=k2+1
       call temp(coeff,dcoeff,px,dpx,dvar,k1,m,isym,k2,1,nxi,nbrxi)
    1 continue

      return
      end

c *************************************************************************
c *************************************************************************

      subroutine temp(coeff,dcoeff,px,dpx,dvar,i,m,isym,ii,isym2,
     *                nxi,nbrxi)

      implicit double precision(a-h,o-z)
      dimension coeff(32,41),dcoeff(32,33,9),dpx(8,9),dvar(33,9,16),
     *          px(8),nxi(9)

c     i =  n� des var.: u=1 v=2 w�=3 ny=4 nyx=6 my=8 ry=14 w=16    dans
c                       w��=37  w���=38   u�=39 v�=40      v��=41
c     ii=  n� des var.: u=1 v=2 w�=3 ny=4 nyx=5 my=6 ry=7  w=8     dans
c                       w��=9   w���=10   u�=11 v�=12      v��=13
c     isym  est relatif au cas de sollicitation (disa,disb, etc), sym�tr
c     isym2 est relatif aux variables (u,v,x, etc), sym�trique ou non

      do 2 j=1,m
        jj=(j-1)*4
        j2=j*2
        j8=(j-1)*8

      do 1 kk=1,nbrxi
      k=nxi(kk)
      dvar(ii,k,jj+1)=
     *   dcoeff(j8+1,ii,k) * px(j2-1)  +  dcoeff(j8+2,ii,k) * px(j2)
     * +  coeff(j8+1,i)   *dpx(j2-1,k)+   coeff(j8+2,i)   *dpx(j2,k)
      dvar(ii,k,jj+2)=
     *   dcoeff(j8+3,ii,k) * px(j2-1)  +  dcoeff(j8+4,ii,k) * px(j2)
     * +  coeff(j8+3,i)   *dpx(j2-1,k)+   coeff(j8+4,i)   *dpx(j2,k)
      dvar(ii,k,jj+3)=dvar(ii,k,jj+1)*isym*isym2
      dvar(ii,k,jj+4)=dvar(ii,k,jj+2)*isym*isym2
    1 continue

    2 continue

      return
      end
