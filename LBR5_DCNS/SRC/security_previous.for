	subroutine security(code1)

	implicit real*8 (a-h,o-z)
	integer*4 tstat(12),year(12),secur(7)								!s�curit�
	integer*4 istat,totdays,days,daysrem								!s�curit�
c	integer*4 hour,ttime(3)												!s�curit�
	integer seed
	integer icheck														

	character*16 code1, code2											!s�curit�
	character*1 cha														!s�curit� 
	character (len=*),parameter :: alphanumeric = '0123456789abcdefghi
     *jklmnoprstxyuvwzabcdefghijklmnoprstxyuvwz0123456789abcdefghijklmno
     *prstxyuvwzabcdefghijklmnoprstxyuvwz0123456789abcdefghijklmnoprstxy
     *uvwzabcdefghijklmnoprstxyuvwz0123456789abcdefghijklmnoprstxyuvwzab
     *cdefghijklmnoprstxyuvwz0123456789abcde'								!s�curit� !bibliotheque de characters

	data year/31,28,31,30,31,30,31,31,30,31,30,31/						!s�curit�
		
c*********************************************************************************

c	calcul du seed 
c	--------------
	call annuld(secur,7)												!s�curit�
	call annuld(ttime,3)												!s�curit�
	call annuld(tstat,12)												!s�curit�

	istat=fstat(55,tstat)												!s�curit�
	totdays=tstat(10)/24/60/60											!s�curit�
	days=0																!s�curit�
	i=1																	!s�curit�
	do while(days.lt.totdays)											!s�curit�
	  i=i+1																!s�curit�
	  if((i-2-((i-2)/4)*4).ge.1) then									!s�curit�
	    days=2*365+(i-2)/4*(366+3*365)+366+((i-2-((i-2)/4)*4)-1)*365	!s�curit�
	  else																!s�curit�
	    days=2*365+(i-2)/4*(366+3*365)									!s�curit�
	  endif																!s�curit�
	enddo																!s�curit�
	i=i-1																!s�curit�
	secur(6)=1970+i														!s�curit�
	if((i-2-((i-2)/4)*4).ge.1) then										!s�curit�
	  days=2*365+(i-2)/4*(366+3*365)+366+((i-2-((i-2)/4)*4)-1)*365		!s�curit�
	else																!s�curit�
	  days=2*365+(i-2)/4*(366+3*365)									!s�curit�
	endif																!s�curit�
	daysrem=totdays-days												!s�curit�
	days=0																!s�curit�
	i=0																	!s�curit�
	do while(days.lt.daysrem)											!s�curit�
	  i=i+1																!s�curit�
	  if((i.eq.2).and.(mod(secur(6),4).eq.0)) then						!s�curit�
	    days=days+year(i)+1												!s�curit�
	  else																!s�curit�
	    days=days+year(i)												!s�curit�
	  endif																!s�curit�
	enddo																!s�curit�
	secur(5)=i															!s�curit�
	if((i.eq.2).and.(mod(secur(6),4).eq.0)) then						!s�curit�
	  secur(4)=daysrem-(days-(year(i)+1))+1								!s�curit�
	else																!s�curit�
	  secur(4)=daysrem-(days-year(i))+1									!s�curit�
	endif																!s�curit�
	secur(3)=tstat(10)/60/60-totdays*24									!s�curit�
	secur(2)=tstat(10)/60-totdays*24*60-secur(3)*60						!s�curit�
	secur(1)=tstat(10)-totdays*24*60*60-secur(3)*60*60-secur(2)*60		!s�curit�
c	secur(1)=tstat(9)-totdays*24*60*60-secur(3)*60*60-secur(2)*60		!s�curit�
	secur(7)=tstat(8)													!s�curit�
		
c	time=rtc()															!s�curit�
c	totdays=int(time)/24/60/60											!s�curit�
c	hour=int(time)/60/60-totdays*24										!s�curit�
c	call itime(ttime)													!s�curit�
c	secur(3)=secur(3)+(ttime(1)-hour)									!s�curit�

	icheck = 0
	seed=0
																	!s�curit�
100	do i=1,7															!s�curit�
	  seed=seed+secur(i)												!s�curit�
	enddo																!s�curit�
	
	

c	g�n�ration de la signature
c	--------------------------
	do i = 1, 16	!16 est la taille du code
	  j = mod((57 * (mod((57 * seed + 1), 256)) + 1), 256)
	  if(j.eq.0) j=1
	  cha = alphanumeric(j:j)
	  code2(i:i) = cha		
	  seed = j
      enddo
	
c	comparaison des signatures gui et solveur
c	-----------------------------------------
	if(code1.ne.code2) then
		if(icheck.eq.0) then
			seed = 1
			icheck = 1
			goto 100
		else		 
			write(*,*) 'security check failed. execution stopped.'
			write(6970,*) 'security check failed. execution stopped.'
			pause
			stop
		endif
	endif	

      return
      end