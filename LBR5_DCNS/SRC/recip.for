      subroutine recip(n,na,a)
      implicit real*8 (a-h,o-z)
      integer*2 m1,m2
      dimension a(na,na),bl(1000,1000),m1(1000),m2(1000)

c***********************************************************************
c
c     subroutine recip
c     *****************
c     calcul de l'inverse d'une matrice a(n,n) de dimension n x n (input/output)

c     type : inversion d'une matrice complete en memoire centrale.
c     *****
c     les vecteurs bl(n,n) et m1(n),m2(n) sont des vecteurs de travail.
c     la matrice inverse est stokée dans a (matrice de départ est donc perdue)

c     modifié : 18-8-94 			             créer: 10-8-94
c     (ph. rigo pour module optimisation)		  (par wang. hengfeng)
c***********************************************************************

cc assign original value to bl and m2.
      do i=1,n
        do j=1,n
          bl(i,j)=0.0
        end do
        bl(i,i)=1.0
        m2(i)=0
      end do
cc normalize former matrix a and b.
      do i=1,n
        am=a(i,1)
        do j=1,n
          if (dabs(a(i,j)).gt.dabs(am)) am=a(i,j)
        end do
        if (am.eq.0.0) then
          write(*,*) ' error 1 in subroutine recip, stop'
          write(*,*) '  in equation n0',i,' amax=',am,'!!'

          write(6970,*) ' error 1 in subroutine recip, stop'
          write(6970,*) '  in equation n0',i,' amax=',am,'!!'

          write(66,*)' error 1 in subroutine recip, stop'
          write(66,*)'  in equation n0',i,' amax=',am,'!!'
          write(29,*)' error 1 in subroutine recip, stop'					!sept06		!bug
          write(29,*)'  in equation n0',i,' amax=',am,'!!'				!sept06		!bug
		stop
        end if
        do j=1,n
          a(i,j)=a(i,j)/am
        end do
        bl(i,i)=bl(i,i)/am
      end do
cc make a into unit i, and bl into reciprocal matrix of a at the same time
      do i=1,n
c       find large element
        am=0.0
        do j=1,n
          if (m2(j).eq.0) then
            if (dabs(a(j,i)).gt.dabs(am)) then
              am=a(j,i)
              m1(i)=j
            end if
          end if
        end do
c       if large element is too small, print error and stop.
        if (dabs(am).lt.1.0e-15) then
          write(*,*) ' error 2 in subroutine recip, stop'
          write(*,*) '  in equation n0',i,' pivot=',am,'!!'

          write(6970,*) ' error 2 in subroutine recip, stop'
          write(6970,*) '  in equation n0',i,' pivot=',am,'!!'

          write(66,*)' error 2 in subroutine recip, stop'
          write(66,*)'  in equation n0',i,' pivot=',am,'!!'
          write(29,*)' error 2 in subroutine recip, stop'					!sept06		!bug
          write(29,*)'  in equation n0',i,' pivot=',am,'!!'				!sept06		!bug
		stop
        end if
c       normalize the large element row.
        im=m1(i)
        m2(im)=1
        do j=1,n
          bl(im,j)=bl(im,j)/am
          if (j.ge.i) then
            a(im,j)=a(im,j)/am
          end if
        end do
c       eliminate elements on the large element column.
        do k=1,n
          if (k.ne.im) then
            if (a(k,i).ne.0.0) then
              ak=a(k,i)
              do j=1,n
                bl(k,j)=bl(k,j)-bl(im,j)*ak
                if (j.ge.i) then
                  a(k,j)=a(k,j)-a(im,j)*ak
                end if
              end do
            end if
          end if
        end do
      end do
cc restore bl, the reciprocal matrix of a, into a.
      do i=1,n
        do j=1,n
          a(i,j)=bl(m1(i),j)
        end do
      end do
cc terminate normally.
      return
      end
