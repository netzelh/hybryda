MODULE frequencies
implicit none

	character, dimension(1:7) :: freqLetter=(/'g','h','i','j','k','l','m'/)
	logical ::	usedSec=.false.
	integer :: lastUsedLetter=0, countBlazkoFreq=0
	character (len=50) :: read,fitfl,dft,writefs,exit
	
CONTAINS

SUBROUTINE text(star)

	character (len=30) :: star

	read='read a '//trim(star)//'.dat 3 1 2'
	fitfl='fitfl a lista.fre'
	dft='dft a 0 10 10 res'
	writefs='writefs a '//trim(star)//'.res'
	exit='exit'

end subroutine

SUBROUTINE freqIni(table,f,n)
implicit none

	real(kind=8), dimension(*) :: table
	integer :: i,n
	real(kind=8) :: f
	
do i=1,5
	table(i)=f*i
end do

do i=6,n
	table(i)=-1.0
end do

end subroutine

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

SUBROUTINE input_file(star,freq)
implicit none

	character (len=*) :: star
	real(kind=8) :: freq
	
	open (20,file='lista.fre')
		write (20,*) 'f'
		write (20,*) '2f'
		write (20,*) '3f'
		write (20,*) '4f'
		write (20,*) '5f'
	close (20)

	open(10,file='komendy.exec')
		write (10,*) read
		write (10,*) 'f= ',freq
		write (10,*) fitfl
		write (10,*) dft
		write (10,*) writefs
		write (10,*) exit
	close (10)

end subroutine

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subroutine addFreq(freq,freqToFit)
implicit none
	
	real(kind=8) :: freq
	integer :: nfreq, i
	real(kind=8), dimension(20) :: freqToFit
	
	nfreq=0
	do i=1,20
		if (freqToFit(i).ne.-1.0) then
			nfreq=nfreq+1
		end if
	end do
	
	if (freq.eq.0.00002) then
		usedSec=.true.
		freqToFit(6)=0.00002
		open(10,file='komendy.exec')
			write (10,*) read
			write (10,*) 'f= ',freqToFit(1)
			write (10,*) 's= ',freq
			write (10,*) fitfl
			write (10,*) dft
			write (10,*) writefs
			write (10,*) exit
		close (10)
		call system('echo ''s'' >> lista.fre')
	end if
	
end subroutine

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



end module frequencies
