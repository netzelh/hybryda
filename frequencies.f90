MODULE frequencies
implicit none

	type fit
		real(kind=8) :: freq
		character :: namef
		logical :: exst
	end type
	
	type(fit),dimension(9) :: freqToFit

	character, dimension(1:7) :: freqLetter=(/'g','h','i','j','k','l','m'/)
	logical ::	usedSec=.false.
	integer :: lastUsedLetter=0, countBlazkoFreq=0
	character (len=50) :: read,fitfl,dft,writefs,exit
	

	
CONTAINS
!....................................................
SUBROUTINE text(star)

	character (len=30) :: star

	read='read a '//trim(star)//'.dat 3 1 2'
	fitfl='fitfl a lista.fre'
	dft='dft a 0 10 10 res'
	writefs='writefs a '//trim(star)//'.res'
	exit='exit'

end subroutine
!....................................................
SUBROUTINE freqIni(freqToFit,f)
implicit none

	type(fit), dimension(9) :: freqToFit
	integer :: i
	real(kind=8) :: f
	
freqToFit(1)%namef='f'
freqToFit(2)%namef='s'
freqToFit(3)%namef='g'
freqToFit(4)%namef='h'
freqToFit(5)%namef='i'
freqToFit(6)%namef='j'
freqToFit(7)%namef='k'
freqToFit(8)%namef='l'
freqToFit(9)%namef='m'

freqToFit(1)%freq=f

freqToFit(1)%exst=.true.
freqToFit(2)%exst=.false.
freqToFit(3)%exst=.false.
freqToFit(4)%exst=.false.
freqToFit(5)%exst=.false.
freqToFit(6)%exst=.false.
freqToFit(7)%exst=.false.
freqToFit(8)%exst=.false.
freqToFit(9)%exst=.false.

open (20,file='lista.fre')
	write (20,*) 'f'
	write (20,*) '2f'
	write (20,*) '3f'
	write (20,*) '4f'
	write (20,*) '5f'
close (20)

open(10,file='komendy.exec')
	write (10,*) read
	write (10,100) 'f= ',f
	write (10,*) fitfl
	write (10,*) dft
	write (10,*) writefs
	write (10,*) exit
close (10)

100 FORMAT (a2,f18.16)

end subroutine

!....................................................

SUBROUTINE inputFile(freqToFit)
implicit none

	type(fit), dimension(9) :: freqToFit
	integer :: i
	
	
	open(10,file='komendy.exec')
		write (10,*) read
		write (10,*) 'f= ',freqToFit(1)
		do i=2,9
			if (freqToFit(i)%exst.eqv..true.) then
				write (10,200) freqToFit(i)%namef,'=',freqToFit(i)%freq
			end if
		end do
		write (10,*) fitfl
		write (10,*) dft
		write (10,*) writefs
		write (10,*) exit
	close (10)
	
	open(20,file='lista.fre')
		write (20,*) 'f'
		write (20,*) '2f'
		write (20,*) '3f'
		write (20,*) '4f'
		write (20,*) '5f'
		if (freqToFit(2)%exst.eqv..true.) write (20,*) 's'
		do i=3,9
			if (freqToFit(i)%exst.eqv..true.) then
				write (20,*) 'f+',freqToFit(i)%namef
				write (20,*) 'f-',freqToFit(i)%namef
				write (20,*) '2f+',freqToFit(i)%namef
				write (20,*) '2f-',freqToFit(i)%namef
				write (20,*) '3f+',freqToFit(i)%namef
				write (20,*) '3f-',freqToFit(i)%namef
				write (20,*) '4f+',freqToFit(i)%namef
				write (20,*) '4f-',freqToFit(i)%namef
				write (20,*) '5f+',freqToFit(i)%namef
				write (20,*) '5f-',freqToFit(i)%namef
			end if
		end do
	close(20)
	
	200 FORMAT (2a1,f18.16)
	
end subroutine

!....................................................

subroutine addFreq(freq,freqToFit)
implicit none
	
	real(kind=8) :: freq
	integer :: nfreq, i
	type(fit), dimension(9) :: freqToFit
	
if (freq.eq.0.00002) then
	freqToFit(2)%freq=0.00002
	freqToFit(2)%exst=.true.
else
	do i=1,9
		if (freqToFit(i)%exst.eqv..false.) then
			freqToFit(i)%freq=freq
			freqToFit(i)%exst=.true.
			exit
		end if
	end do
end if

end subroutine

!....................................................



end module frequencies
