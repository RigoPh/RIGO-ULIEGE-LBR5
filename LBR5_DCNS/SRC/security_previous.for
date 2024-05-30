	subroutine security(code1)

	implicit real*8 (a-h,o-z)
	integer*4 tstat(12),year(12),secur(7)								!sécurité
	integer*4 istat,totdays,days,daysrem								!sécurité
c	integer*4 hour,ttime(3)												!sécurité
	integer seed
	integer icheck														

	character*16 code1, code2											!sécurité
	character*1 cha														!sécurité 
	character (len=*),parameter :: alphanumeric = '0123456789abcdefghi
     *jklmnoprstxyuvwzabcdefghijklmnoprstxyuvwz0123456789abcdefghijklmno
     *prstxyuvwzabcdefghijklmnoprstxyuvwz0123456789abcdefghijklmnoprstxy
     *uvwzabcdefghijklmnoprstxyuvwz0123456789abcdefghijklmnoprstxyuvwzab
     *cdefghijklmnoprstxyuvwz0123456789abcde'								!sécurité !bibliotheque de characters

	data year/31,28,31,30,31,30,31,31,30,31,30,31/						!sécurité
		
c*********************************************************************************

c	calcul du seed 
c	--------------
	call annuld(secur,7)												!sécurité
	call annuld(ttime,3)												!sécurité
	call annuld(tstat,12)												!sécurité

	istat=fstat(55,tstat)												!sécurité
	totdays=tstat(10)/24/60/60											!sécurité
	days=0																!sécurité
	i=1																	!sécurité
	do while(days.lt.totdays)											!sécurité
	  i=i+1																!sécurité
	  if((i-2-((i-2)/4)*4).ge.1) then									!sécurité
	    days=2*365+(i-2)/4*(366+3*365)+366+((i-2-((i-2)/4)*4)-1)*365	!sécurité
	  else																!sécurité
	    days=2*365+(i-2)/4*(366+3*365)									!sécurité
	  endif																!sécurité
	enddo																!sécurité
	i=i-1																!sécurité
	secur(6)=1970+i														!sécurité
	if((i-2-((i-2)/4)*4).ge.1) then										!sécurité
	  days=2*365+(i-2)/4*(366+3*365)+366+((i-2-((i-2)/4)*4)-1)*365		!sécurité
	else																!sécurité
	  days=2*365+(i-2)/4*(366+3*365)									!sécurité
	endif																!sécurité
	daysrem=totdays-days												!sécurité
	days=0																!sécurité
	i=0																	!sécurité
	do while(days.lt.daysrem)											!sécurité
	  i=i+1																!sécurité
	  if((i.eq.2).and.(mod(secur(6),4).eq.0)) then						!sécurité
	    days=days+year(i)+1												!sécurité
	  else																!sécurité
	    days=days+year(i)												!sécurité
	  endif																!sécurité
	enddo																!sécurité
	secur(5)=i															!sécurité
	if((i.eq.2).and.(mod(secur(6),4).eq.0)) then						!sécurité
	  secur(4)=daysrem-(days-(year(i)+1))+1								!sécurité
	else																!sécurité
	  secur(4)=daysrem-(days-year(i))+1									!sécurité
	endif																!sécurité
	secur(3)=tstat(10)/60/60-totdays*24									!sécurité
	secur(2)=tstat(10)/60-totdays*24*60-secur(3)*60						!sécurité
	secur(1)=tstat(10)-totdays*24*60*60-secur(3)*60*60-secur(2)*60		!sécurité
c	secur(1)=tstat(9)-totdays*24*60*60-secur(3)*60*60-secur(2)*60		!sécurité
	secur(7)=tstat(8)													!sécurité
		
c	time=rtc()															!sécurité
c	totdays=int(time)/24/60/60											!sécurité
c	hour=int(time)/60/60-totdays*24										!sécurité
c	call itime(ttime)													!sécurité
c	secur(3)=secur(3)+(ttime(1)-hour)									!sécurité

	icheck = 0
	seed=0
																	!sécurité
100	do i=1,7															!sécurité
	  seed=seed+secur(i)												!sécurité
	enddo																!sécurité
	
	

c	génération de la signature
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