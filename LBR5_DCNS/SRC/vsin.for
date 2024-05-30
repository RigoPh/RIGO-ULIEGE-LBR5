      real*8 function vsin(a,b)                                         vsi00010
      implicit real*8(a-h,o-z)                                          vsi00020
      pi=acos(-1.d00)                                                   vsi00030
      x=a                                                               vsi00040
      y=b                                                               vsi00050
      if(dabs(y).le.0.5)goto 1                                          vsi00060
      x=x+y                                                             vsi00070
      y=0.d00                                                           vsi00080
  1   continue                                                          vsi00090
c                                                                       vsi00100
  3   if(x.ge.0.) goto 2                                                vsi00110
      x=x+360.                                                          vsi00120
      goto 3                                                            vsi00130
  2   continue                                                          vsi00140
c                                                                       vsi00150
      nn=1                                                              vsi00160
      n=0                                                               vsi00170
  5   if(x.lt.180.) goto 4                                              vsi00180
      x=x-180.                                                          vsi00190
      n=n+1                                                             vsi00200
      goto 5                                                            vsi00210
 4    if(n.ne.0) nn=(-1)**n                                             vsi00220
c                                                                       vsi00230
      if((x+y).lt.90.) goto 6                                           vsi00240
      x=180.-x                                                          vsi00250
      y=-y                                                              vsi00260
  6   vsin=nn*dsin((x+y)*pi/180.)                                       vsi00270
      if((x.le.90.).and.(x.gt.89.99)) vsin=nn*dcos(y*pi/180.)           vsi00280
      return                                                            vsi00290
      end                                                               vsi00300
