module global

use omp_lib
use par_zig_mod
implicit none

integer, parameter :: SP = kind(1.0), DP = kind(1.0d0)
integer, parameter :: REAL_KIND = SP

integer, parameter :: nfin=10, nflog=11

integer, parameter :: DEAD = 0, ALIVE = 1, ACTIVATED = 2, DIVIDING = 3, MET = 4

type Tcell_type
    integer :: ID
    integer :: nc       ! if nc=3, two CAR, otherwise one, given by nc (=1 or 2)
    integer :: status
    real(SP) :: carLevel(2)
    real(SP) :: potency
    real(SP) :: exhaustion
!    integer :: prolif   ! number of possible divisions (remaining)
    integer :: nMeetings
end type

type Tumourcell_type
    integer :: status
    real(SP) :: targetExp(2)
    real(SP) :: suscept
end type

integer :: Ndays, Nhours, hour
integer :: nTcells, nTumcells
integer :: nTcells0, nTumcells0
integer :: NTumPerTcell
integer :: nTcellsmax
integer :: Mnodes
integer :: concept
integer :: seed(2)
logical :: par_zig_init
real(SP) :: pmeet_rate, Pkillmax
real(SP) :: exhaustion_base, exhaustion_jump, exhaustion_limit
real(SP) :: doubleExpProb
real(SP) :: killThreshold
real(SP) :: tumourDivideProb
logical :: useKillProb

! Distributions
real(SP) :: mean, std   ! use these parameters for all distributions
! Target expression parameters
real(SP) :: targetMedian(2), targetShape(2)
! CAR expression levels
real(SP) :: CARMedian(2), CARShape(2)
! Tumour cell susceptibility levels
real(SP) :: susceptMedian, susceptShape
! T cell killing capability
real(SP) :: killMedian, killShape
! T cell proliferative capability
real(SP) :: prolifMedian, prolifShape


type(Tcell_type), allocatable, target :: Tcell(:)
type(Tumourcell_type), allocatable, target :: Tumourcell(:)

character*(2048) :: inputfile
character*(2048) :: logfile
character*(2048) :: logmsg

integer :: kcell_now

contains

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
subroutine logger(msg)
character*(*) :: msg
integer :: error
logical :: logfile_isopen
character*(1) :: LF = char(94)

error = 0
inquire(unit=nflog,OPENED=logfile_isopen)
!if (use_TCP) then
!    if (awp_0%is_open) then
!        call winsock_send(awp_0,trim(msg)//LF,len_trim(msg)+1,error)
!    elseif (logfile_isopen) then
!        write(nflog,*) trim(msg)
!    else
!        write(99,*) trim(msg)
!    endif
!else
!	write(*,*) trim(msg)
!endif
if (logfile_isopen) then
	write(nflog,'(a)') trim(msg)
!	if (error /= 0) then
!	    write(nflog,'(a,i4)') 'winsock_send error: ',error
!	    close(nflog)
!	endif
endif
if (error /= 0) stop
end subroutine

!----------------------------------------------------------------------------------------- 
!-----------------------------------------------------------------------------------------
subroutine RngInitialisation
integer, allocatable :: zig_seed(:)
integer :: i
integer :: npar, grainsize = 32

npar = Mnodes
write(logmsg,*) 'npar = ',npar,seed
call logger(logmsg)
allocate(zig_seed(0:npar-1))
do i = 0,npar-1
    zig_seed(i) = seed(1)*seed(2)*(i+1)
enddo
call par_zigset(npar,zig_seed,grainsize)
par_zig_init = .true.
end subroutine


!---------------------------------------------------------------------
! Uniform randomly generates an integer I: n1 <= I <= n2
!---------------------------------------------------------------------
integer function random_int(n1,n2,kpar)
integer :: n1,n2,kpar
integer :: k,R

if (n1 == n2) then
    random_int = n1
elseif (n1 > n2) then
    write(logmsg,*) 'ERROR: random_int: n1 > n2: ',n1,n2
    call logger(logmsg)
    stop
endif
R = par_shr3(kpar)
k = abs(R)
random_int = n1 + mod(k,(n2-n1+1))
end function

!--------------------------------------------------------------------------------
! Make a random choice of an integer from 1 - N on the basis of probabilities in
! the array p(:) (assumed to be normalized).
!--------------------------------------------------------------------------------
integer function random_choice(p,N,kpar)
integer :: N,kpar
real(REAL_KIND) :: p(:)
integer :: k
real(REAL_KIND) :: R, psum

R = par_uni(kpar)
psum = 0
do k = 1,N
    psum = psum + p(k)
    if (R <= psum) then
        random_choice = k
        return
    endif
enddo
write(logmsg,*) 'ERROR: random_choice: ',N,p
call logger(logmsg)
stop
end function

!--------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------
real(REAL_KIND) function rv_normal(p1,p2,kpar)
integer :: kpar
real(REAL_KIND) :: p1,p2
real(REAL_KIND) :: R

R = par_rnor(kpar)
rv_normal = p1+R*p2
end function

!--------------------------------------------------------------------------------------
! When Y is normal N(p1,p2) then X = exp(Y) is lognormal with
!   median = m = exp(p1)
!   shape  = s = exp(p2)
! Also median = m = mean/(s^2/2)
! kpar = parallel process number
!--------------------------------------------------------------------------------------
real(REAL_KIND) function rv_lognormal(p1,p2,kpar)
integer :: kpar
real(REAL_KIND) :: p1,p2
real(REAL_KIND) :: R,z

R = par_rnor(kpar)
z = p1 + R*p2
rv_lognormal = exp(z)
end function

!--------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------
subroutine check_lognormal(median,shape)
real(SP) :: median, shape
real(SP) :: sigma, mean, p1, p2

sigma = log(shape)
p1 = log(median)	
p2 = sigma
mean = exp(p1 + 0.5*p2**2)	! mean = median.exp(sigma^2/2)
write(logmsg,'(a,2e12.4)') 'shape, sigma: ',shape,sigma
call logger(logmsg)
write(logmsg,'(a,2e12.4)') 'median, mean: ',median,mean
call logger(logmsg)
end subroutine

end module