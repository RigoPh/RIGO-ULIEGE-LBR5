      real*8 function vcos(a,b)
      implicit real*8(a-h,o-z)                                          vco00020
      pi=acos(-1.d00)                                                   vco00030
      x=a                                                               vco00040
      y=b                                                               vco00050
      if(dabs(y).le.0.5)goto 1                                          vco00060
      x=x+y                                                             vco00070
      y=0.d00                                                           vco00080
  1   continue                                                          vco00090
c                                                                       vco00100
  3   if(x.ge.0.) goto 2                                                vco00110
      x=x+360.                                                          vco00120
      goto 3                                                            vco00130
  2   continue                                                          vco00140
c                                                                       vco00150
      nn=1                                                              vco00160
      n=0                                                               vco00170
  5   if(x.lt.180.) goto 4                                              vco00180
      x=x-180.                                                          vco00190
      n=n+1                                                             vco00200
      goto 5                                                            vco00210
 4    if(n.ne.0) nn=(-1)**n                                             vco00220
c                                                                       vco00230
      if((x+y).ge.90.) then                                             vco00240
      x=180.-x                                                          vco00250
      y=-y                                                              vco00260
      vcos=-nn*dcos((x+y)*pi/180.)                                      vco00270
      else                                                              vco00280
      vcos=nn*dcos((x+y)*pi/180.)                                       vco00290
      endif                                                             vco00300
      if((x.le.90.).and.(x.gt.89.99)) vcos=nn*dsin(y*pi/180.)           vco00310
      return                                                            vco00320
      end                                                               vco00330
