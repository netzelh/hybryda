MODULE pik
use tools
implicit none


CONTAINS


subroutine signalAt2(fs,n,f2,a2,sn2,r,czy_p2)
!#################################################
!#                                               #
!# Procedura wyszukuje maksimum w pobliżu f=2    #
!# i sprawdza czy jego SNR jest większy od 4     #
!# input:                                        #
!#   fs - ....                                   #
!#   n - liczba obserwacji                       #
!#   ......                                      #
!# output:                                       #
!#   r - ...                                     #
!#                                               #
!#################################################
implicit none

	real(kind=8), dimension (:,:), allocatable :: fs
	integer      :: n !liczba wierszy
	real(kind=8) :: f2,a2,sn2
	real         :: r ! rozdzielczosc
	integer :: i,j,k
	logical :: czy_p2
			

	call maximum(fs,n,f2,a2,dble(1.98),dble(2.02),k)
	if (SNR(fs,n,f2,0.2).gt.4.0) then
		czy_p2=.true.
	else
		czy_p2=.false.
	end if

end subroutine

subroutine szukaj_px(fs,n,fx,ax,snx,f,r,czy_mod,czy_al)
implicit none

	real (kind=8) :: fx,ax,snx,f
	real :: r
	real(kind=8), dimension (:,:), allocatable :: fs
	integer :: n,k
	logical :: czy_mod, czy_al
	
	real (kind=8) :: fmin,fmax
	

	fmin=f/0.64
	fmax=f/0.58

	call maximum(fs,n,fx,ax,fmin,fmax,k)
	if (SNR(fs,n,fx,0.2).gt.4.0) then
		czy_mod=.true.
		if (fs(3,k).eq.2.0) czy_al=.true.
	end if

end subroutine

subroutine pik_gl(fs,n,fp,ap,snp,f,fmin,fmax,r,czy_zo,czy_bl,ifAny)
implicit none

	real(kind=8), dimension (:,:), allocatable :: fs
	real (kind=8) :: fp,ap,snp,fmin,fmax,f
	real :: r
	integer :: n,k
	logical :: czy_zo, czy_bl, ifAny

	

		call maximum(fs,n,fp,ap,fmin,fmax,k)
		if (SNR(fs,n,fp,0.2).gt.4.0) then
			ifAny=.true.
			if (abs(fp-f).lt.r) then
				czy_zo=.true.		
			else
				czy_bl=.true.
		!		fmax=max(f+2*abs(f-fp)+r,f+0.2)
		!		fmin=min(f-2*abs(f-fp)-r,f-0.2)
			end if
		else
			ifAny=.false.
		end if

end subroutine

subroutine trend(fs,n,ft,at,snt,r,czy_trend)
implicit none

	integer :: k
	real (kind=8) :: at,snt
	logical :: czy_trend
	
	real(kind=8), dimension (:,:), allocatable :: fs
	integer :: n !liczba wierszy
	real(kind=8) :: ft
	real :: r ! rozdzielczosc

	integer :: i,j
			

	call maximum(fs,n,ft,at,dble(0.0),dble(0.003),k)
	if (SNR(fs,n,ft,0.2).gt.4.0) then
		czy_trend=.true.
	else
		czy_trend=.false.
	end if

end subroutine




end module pik
