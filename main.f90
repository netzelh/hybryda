program res3 !wywolanie programu: <nazwa_gwiazdy_bez_rozszerzenia> <okres> [przedzial czasowy danych]
use io
use pik
use frequencies
implicit none

	character (len=30) :: periodChar
	character (len=50) :: fileName=' ',star=' '
	character (len=50) :: outputFile=' '
	character (len=500) :: sys 
	
	integer :: fileNumber=10 
	integer :: nrows 
	integer :: i,k,n
	integer :: win=20 ! numer pliku z oknem spektralnym
	integer :: trendwin=30 !numer pliku z oknem spektralnym trendu
	integer :: callFdecomp
	integer :: countPeaks
	
	real(kind=8), dimension (:,:), allocatable :: fs
	real(kind=8), dimension (:,:), allocatable :: frequency,amplitude

	real(kind=8) :: freqmax,ampmax
	real(kind=8) :: fmin,fmax
	real(kind=8) :: period,f
	real(kind=8) :: ft,at,snt !trend
	real(kind=8) :: fp,ap,snp !w pobliżu piku głównego
	real(kind=8) :: f2,a2,sn2 !efekt instrumentalny w 2
!	real(kind=8) :: f1,a1,sn1 !efekt instrumentalny w 1
	real(kind=8) :: fp2,ap2,snp2 !w pobliżu 2f
	real(kind=8) :: fx,ax,snx !w szukanym zakresie, szukana czestosc X
	real(kind=8) :: lastBlFreq=0.0
	
	real :: r ! rozdzielczosc
	
	logical :: ifZo,ifMod,ifAl,ifBlazko,ifRem,ifTrend,ifSignalAt2,ifAny,tooManyPeaks=.false.,newCalc=.false.
	logical :: blazkoF=.false.,modF=.false.,remF=.false.,trendF=.false.,zoF=.false.,p2F=.false.,blazkoLeftF=.false.
	logical :: tooManyAdded

call get_arg (star,period,periodChar,f,r)
write (*,*) 'got arg for star: ',trim(star)
call text(star)
fileName=trim(star)//'.res'
write (*,*) 'changed name: ',trim(fileName)
call flagIni(ifZo,ifMod,ifAl,ifBlazko,ifRem)
call trendIni(ifTrend,ifSignalAt2)
write (*,*) 'flags initiated'
call freqIni(freqToFit,f)
write (*,*) 'frequency table and first intput file initiated'

ifBlazko=.true.
do while (ifBlazko.eqv..true.)
	trendF=.false.
	p2F=.false.
	callFdecomp=1
	ifTrend=.true.
	do while ((callFdecomp.le.2).and.(ifTrend.eqv..true.))
		call system ('rm '//trim(fileName))
		if (callFdecomp.ne.1) deallocate (fs)
		call system ('fdecomp komendy.exec')
		write (*,*) 'fdecomp ok'
		write (*,*) 'prepared file ',trim(fileName)
		call get_file(fileName,fileNumber,fs,nrows)
		write (*,*) 'data written to the table'

		write (*,*) '>> Looking for trend...'
		call trend(fs,nrows,ft,at,snt,r,ifTrend)		!max dla malych czestotliwosci f<0.003
		if (ifTrend.eqv..true.) then
			write (*,*) 'There is signal for low frequencies (trend)'
			if (callFdecomp.eq.1) then
				write (*,*) 'adding secular term'
				call addFreq(dble(0.00002),freqToFit,tooManyAdded)
				call inputFile(freqToFit)
			end if
			if (callFdecomp.eq.2) then
				trendF=.true.
				write (*,*) 'Unable to remove trend, ignoring aliases'
				call ignoreAliases(fs,nrows,ft,r)
			end if
				
			callFdecomp=callFdecomp+1
		else
			write (*,*) 'No trend'
		end if
	end do

	write (*,*) '>> Looking for signal at frequency = 2'
	call signalAt2 (fs,nrows,f2,a2,sn2,r,ifSignalAt2)		!pik w okolicach 2
	if (ifSignalAt2.eqv..true.) then
		p2F=.true.
		write (*,*) 'There is signal at 2, ignoring aliases'
		call ignoreAliases(fs,nrows,f2,r)
	else if (ifSignalAt2.eqv..false.) then
		write (*,*) 'No signal at 2'
	end if


	countPeaks=0
	do
		write (*,*) '>> Looking for signal near f0'
		if (lastBlFreq.ne.0.0) then
			fmax=max(f+2*abs(f-fp)+r,f+0.2)
			fmin=min(f-2*abs(f-fp)-r,f-0.2)
		else
			fmin=dble(f-0.2)
			fmax=dble(f+0.2)
		end if
		call pik_gl (fs,nrows,fp,ap,snp,f,fmin,fmax,r,ifZo,ifBlazko,ifAny)			!max w poblizu piku glownego - sprawdzanie czy efekt Blazki lub zmiana okresu
		countPeaks=countPeaks+1
		if (countPeaks.gt.7) then
			write (*,*) 'Reached maximum number od peaks near f0 (7). No further analysis is usefull, ignoring aliases'
			tooManyPeaks=.true.
			call ignoreAliases(fs,nrows,fp,r)
			exit
		end if
		if (ifAny.eqv..true.) then
			if (ifZo.eqv..true.) then
				zoF=.true.
				write (*,*) 'There is unresolved signal near f0, ignoring aliases'
				call ignoreAliases(fs,nrows,fp,r)
			else if (ifBlazko.eqv..true.) then
				blazkoF=.true.
				write (*,*) 'There is Blazko effect'
				if (abs(fp-lastBlFreq).lt.r) then
					write (*,*) 'Unable to remove frequency of Blazko effect, ignoring aliases' 
					call ignoreAliases(fs,nrows,fp,r)
				else
					write (*,*) 'Adding new frequency and recalculating...'
					lastBlFreq=fp
					call addFreq(abs(fp-f),freqToFit,tooManyAdded)
					if (tooManyAdded) then
						write (*,*) 'Too many frequencies to add, No further analysis is usefull, ignoring aliases'
						call ignoreAliases(fs,nrows,fp,r)
						blazkoLeftF=.true.
						exit
					end if
					call inputFile(freqToFit)
					exit
				end if
			end if
		else if (ifAny.eqv..false.) then
			write (*,*) 'No significant signal near f0'
			exit
		end if
	end do
	if (tooManyAdded) exit
end do

write (*,*) '>> Looking for non-radial mode...'
call szukaj_px (fs,nrows,fx,ax,snx,f,r,ifMod,ifAl) 				!max w szukanym zakresie
if (ifMod.eqv..true.) then
	modF=.true.
	write (*,*) 'Found a candidate!'
else
	write (*,*) 'No interesting signal'
end if


!fragment do testowania
!open (50,file='ppp.txt')
!do i=1,n
!	write (50,*) fs(1,i),fs(2,i),fs(3,i)
!end do
!close (50)
!~~~~~~~~~~~~~~~~~~
	
deallocate(fs)
close(fileNumber)

call flagi(f,fx,star,periodChar,blazkoF,modF,zoF,remF,trendF,p2F,blazkoLeftF,outputFile)

end program res3
