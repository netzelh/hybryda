program res3 !wywolanie programu: <nazwa_gwiazdy_bez_rozszerzenia> <okres> [przedzial czasowy danych]
use io
use pik
use frequencies
implicit none

	character (len=30) :: fileName,star
	character (len=30) :: periodChar
	character (len=500) :: sys 
	
	integer :: fileNumber=10 
	integer :: nrows 
	integer :: i,k
	integer :: win=20 ! numer pliku z oknem spektralnym
	integer :: trendwin=30 !numer pliku z oknem spektralnym trendu
	integer :: callFdecomp
	
	real(kind=8), dimension (:,:), allocatable :: fs
	real(kind=8), dimension (:,:), allocatable :: frequency,amplitude
	real(kind=8), dimension (20) :: freqToFit

	real(kind=8) :: freqmax,ampmax
	real(kind=8) :: fmin,fmax
	real(kind=8) :: period,f
	real(kind=8) :: ft,at,snt !trend
	real(kind=8) :: fp,ap,snp !w pobliżu piku głównego
	real(kind=8) :: f2,a2,sn2 !efekt instrumentalny w 2
!	real(kind=8) :: f1,a1,sn1 !efekt instrumentalny w 1
	real(kind=8) :: fp2,ap2,snp2 !w pobliżu 2f
	real(kind=8) :: fx,ax,snx !w szukanym zakresie, szukana czestosc X
	
	real :: r ! rozdzielczosc
	
	logical :: ifZo,ifMod,ifAl,ifBlazko,ifRem,ifTrend,ifSignalAt2

call get_arg (star,period,f,r)
write (*,*) 'get_arg ok'
call text(star)
fileName=trim(star)//'.res'
write (*,*) 'changed name ok'
call flagIni(ifZo,ifMod,ifAl,ifBlazko,ifRem)
call trendIni(ifTrend,ifSignalAt2)
write (*,*) 'flags initiated'
call freqIni(freqToFit,f,20)
write (*,*) 'frequency table initiated'
call makeInputFile(star,freqToFit)
write (*,*) 'prepared input file'

callFdecomp=1

do while (callFdecomp.le.2)
	call system ('fdecomp komendy.exec')
	write (*,*) 'fdecomp ok'

	call get_file(fileName,fileNumber,fs,nrows)
	write (*,*) 'data written to the table'

	call trend(fs,nrows,ft,at,snt,r,ifTrend)		!max dla malych czestotliwosci f<0.003
	if (callFdecomp.eq.1) call addFreq(dble(0.00002),freqToFit)
	if (callFdecomp.eq.2) call ignoreAliases(fs,nrows,ft,r)
		
	callFdecomp=2
end do

call pik2 (fs,nrows,f2,a2,sn2,r,ifSignalAt2)		!pik w okolicach 2
!call pik_gl (fs,nrows,fp,ap,snp,f,r,ifZo,ifBlazko)			!max w poblizu piku glownego - sprawdzanie czy efekt Blazki (odrzucenie gwiazdy do innej analizy)
!call szukaj_px (fs,nrows,fx,ax,snx,f,r,ifMod,ifAl) 				!max w szukanym zakresie

!fragment do testowania
!open (50,file='ppp.txt')
!do i=1,n
!	write (50,*) fs(1,i),fs(2,i),fs(3,i)
!end do
!close (50)
!~~~~~~~~~~~~~~~~~~
	
deallocate(fs)
close(fileNumber)

!call flagi(f,fx,fileName,periodChar,czy_al,czy_bl,czy_mod,czy_p2,czy_trend,czy_zo)

end program res3
