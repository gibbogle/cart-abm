module global

use omp_lib
use par_zig_mod
use winsock
implicit none

integer, parameter :: SP = kind(1.0), DP = kind(1.0d0)
integer, parameter :: REAL_KIND = SP

integer, parameter :: nfin=10, nfout=11, nflog=12

integer, parameter :: DEAD = 0, ALIVE = 1, ACTIVATED = 2, DIVIDING = 3, MET = 4
real(SP), parameter :: deltaT = 6   ! hours

type Tcell_type
    integer :: ID
    integer :: nc           ! if nc=3, two CAR expressed, otherwise one, given by nc (=1 or 2)
    integer :: status
    real(SP) :: carLevel(2)
    real(SP) :: potency
    real(SP) :: exhaustion
!    integer :: prolif      ! number of possible divisions (remaining)
    integer :: nMeetings
    integer :: nkilled
    integer :: generation
    real(SP) :: totalStim
end type

type Tumourcell_type
    integer :: status
    real(SP) :: targetExp(2)
    real(SP) :: suscept
end type

integer :: nDays, nSteps, istep
integer :: nTcells, nTumcells
integer :: nTcells0, nTumcells0
integer :: nTumPerTcell
integer :: nTcellsmax
integer :: nCARs
integer :: Mnodes, ncpu_input
integer :: concept
integer :: seed(2)
logical :: par_zig_init
real(SP) :: pMeetRate, Pkillmax
real(SP) :: exhaustionBase      ! used to initialise T cell exhaustion levels
real(SP) :: exhaustionRate      ! increase in exhaustion per unit of Q
real(SP) :: exhaustionThreshold ! exhaustion limit for cell survival
real(SP) :: deathProbMax        ! maximum spontaneous death probability (per time step)
real(SP) :: activationThreshold ! threshold level of totalStim for killing capability
real(SP) :: divideThreshold     ! threshold level of totalStim for cell division
real(SP) :: killThreshold       ! threshold level of killing likelihood (Pkill)
real(SP) :: doubleExpProb       ! in Concept 3, fraction of T cells with 2 CARs
real(SP) :: tumourDivideProb    ! probability of tumour cell division in a time step
real(SP) :: stimDecayRate       ! rate of loss of totalStim per hour
real(SP) :: Qfactor             ! factor multiplying Q in integrating totalStim
logical :: useKillProb          ! interpret Pkill as a killing probability (instead of using killThreshold)

! Distributions
real(SP) :: meanExhaustion, stdExhaustion   ! to set initial levels of T cell exhasution
! Target expression parameters
real(SP) :: targetExpMedian(2), targetExpShape(2)
! CAR expression levels
real(SP) :: CARMedian(2), CARShape(2)
! Tumour cell susceptibility levels
real(SP) :: susceptMedian, susceptShape
! T cell killing capability
real(SP) :: potencyMedian, potencyShape
! T cell proliferative capability
!real(SP) :: prolifMedian, prolifShape

! Clone results
integer, allocatable :: nclones(:)
integer, allocatable :: totalKilled(:)

type(Tcell_type), allocatable, target :: Tcell(:)
type(Tumourcell_type), allocatable, target :: Tumourcell(:)

character*(128) :: inputfile
character*(128) :: outputfile
character*(128) :: logfile

! GUI stuff
integer, parameter :: TCP_PORT_0 = 5000		! main communication port (logging) 
integer, parameter :: TCP_PORT_1 = 5001		! data transfer port (plotting)
character*(2048) :: logmsg
TYPE(winsockport) :: awp_0, awp_1, awp_2, awp_3
logical :: use_TCP = .true.         ! turned off in main()
logical :: use_CPORT1
logical :: stopped, clear_to_send, simulation_start

integer :: kcell_now
integer :: kdbug = -440

!DEC$ ATTRIBUTES DLLEXPORT :: nsteps

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
if (use_TCP) then
    if (awp_0%is_open) then
        call winsock_send(awp_0,trim(msg)//LF,len_trim(msg)+1,error)
    elseif (logfile_isopen) then
        write(nflog,*) trim(msg)
    else
        write(99,*) trim(msg)
    endif
else
	write(*,*) trim(msg)
endif
if (logfile_isopen) then
	write(nflog,'(a)') trim(msg)
	if (error /= 0) then
	    write(nflog,'(a,i4)') 'winsock_send error: ',error
	    close(nflog)
	endif
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
if (R == -2147483648) R = par_shr3(kpar)
k = abs(R)

random_int = n1 + mod(k,(n2-n1+1))
if (random_int < 1) then
    write(*,*) 'RandomInt: n1,n2,R,k,random_int: ',n1,n2,R,k,random_int
    stop
endif
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

!----------------------------------------------------------------------------------------
! Convert a half life in hours to a decay rate /hour
!----------------------------------------------------------------------------------------
real function DecayRate(halflife)
real :: halflife

DecayRate = log(2.0)/halflife    ! rate/hour
end function

!--------------------------------------------------------------------------------
!     NON-RECURSIVE STACK VERSION OF QUICKSORT FROM N.WIRTH'S PASCAL
!     BOOK, 'ALGORITHMS + DATA STRUCTURES = PROGRAMS'.
!     SINGLE PRECISION, ALSO CHANGES THE ORDER OF THE ASSOCIATED ARRAY T.
! Ascending order sort
!--------------------------------------------------------------------------------
SUBROUTINE qsort(a, n, t)
IMPLICIT NONE

INTEGER, INTENT(IN)    :: n
REAL, INTENT(INOUT)    :: a(n)
INTEGER, INTENT(INOUT) :: t(n)

!     Local Variables

INTEGER                :: i, j, k, l, r, s, stackl(15), stackr(15), ww
REAL                   :: w, x

s = 1
stackl(1) = 1
stackr(1) = n

!     KEEP TAKING THE TOP REQUEST FROM THE STACK UNTIL S = 0.

10 CONTINUE
l = stackl(s)
r = stackr(s)
s = s - 1

!     KEEP SPLITTING A(L), ... , A(R) UNTIL L >= R.

20 CONTINUE
i = l
j = r
k = (l+r) / 2
x = a(k)

!     REPEAT UNTIL I > J.

DO
  DO
    IF (a(i).LT.x) THEN                ! Search from lower end
      i = i + 1
      CYCLE
    ELSE
      EXIT
    END IF
  END DO

  DO
    IF (x.LT.a(j)) THEN                ! Search from upper end
      j = j - 1
      CYCLE
    ELSE
      EXIT
    END IF
  END DO

  IF (i.LE.j) THEN                     ! Swap positions i & j
    w = a(i)
    ww = t(i)
    a(i) = a(j)
    t(i) = t(j)
    a(j) = w
    t(j) = ww
    i = i + 1
    j = j - 1
    IF (i.GT.j) EXIT
  ELSE
    EXIT
  END IF
END DO

IF (j-l.GE.r-i) THEN
  IF (l.LT.j) THEN
    s = s + 1
    stackl(s) = l
    stackr(s) = j
  END IF
  l = i
ELSE
  IF (i.LT.r) THEN
    s = s + 1
    stackl(s) = i
    stackr(s) = r
  END IF
  r = j
END IF

IF (l.LT.r) GO TO 20
IF (s.NE.0) GO TO 10

RETURN
END SUBROUTINE qsort

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
subroutine DisableTCP
!DEC$ ATTRIBUTES DLLEXPORT :: disableTCP
!DEC$ ATTRIBUTES STDCALL, REFERENCE, MIXED_STR_LEN_ARG, ALIAS:"DISABLETCP" :: disableTCP

use_TCP = .false.   ! because this is called from monolayer_main()	
end subroutine


end module