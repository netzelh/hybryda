MODULE io
implicit none

CONTAINS

SUBROUTINE flagi(f,fx,star,periodChar,blazkoF,modF,zoF,remF,trendF,p2F,outputFile)
implicit none

	character (len=*) :: star,outputFile,periodChar
	real (kind=8) :: f,fx
	logical :: blazkoF,modF,zoF,remF,trendF,p2F

	character (len=500) :: line,pom
	character (len=20) :: sx_ch
	!character (len=*) :: bl
	real :: sx
	
	sx=f/fx
	
	write (sx_ch,*) sx
	
	line=trim(star)//" "//trim(periodChar)//" "//trim(sx_ch)
	
	if (blazkoF) then
		pom=line
		line=trim(pom)//" BL "
	end if
			
	if (trendF) then
		pom=line
		line=trim(pom)//" T "
	end if

	if (zoF) then
		pom=line
		line=trim(pom)//" ZO "
	end if
	
	if (p2F) then
		pom=line
		line=trim(pom)//" P2 "
	end if
	
	if (modF) then
		pom=line
		line=trim(pom)//" MOD "
	end if
	
!	if (czy_al) then
!		pom=line
!		line=trim(pom)//" AL "
!	end if
	
	if (outputFile.eq.' ') then
		write (*,*) trim(line)
	else
		call system('echo " '//trim(line)//' " >> '//trim(outputFile))
	end if

end subroutine

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

SUBROUTINE get_arg(star,period,period_ch,f,r)
implicit none

	character (len=30) :: star
	character (len=30) :: period_ch
	character (len=15) :: t_ch
	integer :: na
	integer :: u
	real :: t,r
	real, parameter :: df=5
	real(kind=8) :: period,f
	
	na=iargc()
	
	select case(na)
		case (2)
			r=0.001
		case (3)
			call getarg(3,t_ch)
			read (t_ch,*) t
			r=2.0/t
	end select
	
	call getarg(1,star)
	call getarg(2,period_ch)
	read(period_ch,*) period	
	f=dble(1.0)/period

end subroutine

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

SUBROUTINE get_file(plik,fileNumber,fs,n)
implicit none

	character (len=30) :: plik
	real(kind=8), dimension (:,:), allocatable :: fs
	integer :: fileNumber  !file number
	integer :: n !liczba wierszy

	open(fileNumber,file=plik)
	write (*,*) 'Plik: ',plik
	n=ile_wierszy(fileNumber)
	write (*,*) n,'rows to read'
	if (.not. allocated(fs)) allocate (fs(3,n)) !1-freq,2-amp,3-zero/jeden
	
	call getp(fs,fileNumber,n)
	
	close (fileNumber)

end subroutine

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

SUBROUTINE getp(fs,fileNumber,n)
implicit none

	real(kind=8), dimension (:,:), allocatable :: fs
	integer :: fileNumber,n
	integer :: i
	
	write (*,*) 'getting data...'
	do i=1,n
		read(fileNumber,*) fs(1,i),fs(2,i)
		fs(3,i)=1.0
	end do
	

	
!	write (*,*) 'OK, wczytano dane do programu'
	
end subroutine

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

INTEGER FUNCTION ile_wierszy(fileNumber)
implicit none

	integer :: fileNumber
	real :: a,b !zmienne pomocnicze
	
	ile_wierszy=0
	
	do
		read (fileNumber,*,end=100) a,b
		ile_wierszy=ile_wierszy+1
	end do

	100 rewind(fileNumber)
	
	!write (*,*) 'Wierszy do wczytania: ',ile_wierszy
		
	return

end function

end module io
