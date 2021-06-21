module cart

use global
implicit none

contains

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
subroutine readparameters
integer :: iuse_kill_prob
real(SP) :: stimHalfLife

read(nfin,*) concept            ! = 0, 1, 2 or 3. 1 = both CARs on every cell, 1:1. 2 = mix of cells with Car #1, #2.  3 = mix of cells with 2 CARs and with only 1 (type 1 or 2).
read(nfin,*) nTcells0           ! initial population of T cells
read(nfin,*) nTumPerTcell       ! number of tumour cells per T cell
read(nfin,*) nDays              ! length of simulation
read(nfin,*) seed(1)
read(nfin,*) seed(2)
read(nfin,*) ncpu_input
! T cell parameters
read(nfin,*) CARMedian(1)
read(nfin,*) CARMedian(2)
read(nfin,*) CARShape(1)
read(nfin,*) CARShape(2)
read(nfin,*) potencyMedian
read(nfin,*) potencyShape
read(nfin,*) meanExhaustion
read(nfin,*) stdExhaustion
read(nfin,*) exhaustionBase
read(nfin,*) exhaustionRate
read(nfin,*) exhaustionThreshold
read(nfin,*) deathProbMax
read(nfin,*) doubleExpProb      ! probability of a cell having both CARs expressed (in concept 3)
read(nfin,*) Qfactor
read(nfin,*) activationThreshold
read(nfin,*) divideThreshold
read(nfin,*) stimHalflife
! Tumour cell parameters
read(nfin,*) targetExpMedian(1)
read(nfin,*) targetExpMedian(2)
read(nfin,*) targetExpShape(1)
read(nfin,*) targetExpShape(2)
read(nfin,*) susceptMedian
read(nfin,*) susceptShape
read(nfin,*) pMeetRate
read(nfin,*) iuse_kill_prob
read(nfin,*) Pkillmax
read(nfin,*) killThreshold
read(nfin,*) tumourDivideProb
close(nfin)

exhaustionRate = DeltaT*exhaustionRate
useKillProb = (iuse_kill_prob == 1)
stimDecayRate = DecayRate(stimHalflife)
if (concept == 0) then
    nCARS = 1
else
    nCARS = 2
endif

!call check_lognormal(CARMedian(1), CARShape(1))

end subroutine

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
subroutine setup(ncpu,infile,outfile,ok)
logical :: ok
integer :: sz_Tcell,sz_Tumcell, ncpu
character*(*) :: infile, outfile

open(nfin,file=infile,status='old')
!open(nfout,file=outfile,status='replace')  ! not used

call readparameters
if (ncpu == 0) then
	ncpu = ncpu_input
endif
Mnodes = ncpu
call omp_initialisation(ok)

nSteps = (nDays*24.0)/deltaT
!write(*,*) 'ncpu, nsteps: ',ncpu,nsteps

nTcellsmax = 10000*nTcells0
nTumcells0 = nTcells0*NTumPerTcell
allocate(Tcell(nTcellsmax))
allocate(Tumourcell(int(1.5*nTumcells0)))
nTcells = nTcells0
nTumcells = nTumcells0
call RNGInitialisation
call generate_cells
allocate(nclones(nTcells))
allocate(totalKilled(nTcells))
nclones = 0
totalKilled = 0
totalKilled = 0
sz_Tcell = sizeof(Tcell(1))
sz_Tumcell = sizeof(Tumourcell(1))
write(nflog,'(a,2i10)') 'nTcellsmax, 1.5*nTumcells0: ',nTcellsmax, int(1.5*nTumcells0)
write(nflog,'(a,2i6)') 'sizeof Tcell_type, Tumourcell_type: ',sz_Tcell,sz_Tumcell
write(nflog,'(a,2f8.3)') 'MBytes allocated: Tcell,Tumourcell: ', &
    (nTcellsmax/1000000.)*sz_Tcell,(1.5*nTumcells0*sz_Tumcell)/1000000.
end subroutine

!----------------------------------------------------------------------------------------- 
!-----------------------------------------------------------------------------------------
subroutine omp_initialisation(ok)
logical :: ok
integer :: npr, nth

ok = .true.
!if (Mnodes == 1) return
#if defined(OPENMP) || defined(_OPENMP)
!write(logmsg,'(a,i2)') 'Requested Mnodes: ',Mnodes
!call logger(logmsg)
npr = omp_get_num_procs()
!write(logmsg,'(a,i2)') 'Machine processors: ',npr
!call logger(logmsg)

nth = omp_get_max_threads()
!write(logmsg,'(a,i2)') 'Max threads available: ',nth
!call logger(logmsg)
if (nth < Mnodes) then
    Mnodes = nth
!    write(logmsg,'(a,i2)') 'Setting Mnodes = max thread count: ',nth
!	call logger(logmsg)
endif

call omp_set_num_threads(Mnodes)
!$omp parallel
nth = omp_get_num_threads()
!write(logmsg,*) 'Threads, max: ',nth,omp_get_max_threads()
!call logger(logmsg)
!$omp end parallel
#endif

call logger('did omp_initialisation')
!call test_omp1

end subroutine

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
subroutine generate_cells
integer :: kcell

do kcell = 1,nTcells
    call makeTcell(kcell)
enddo
do kcell = 1,nTumcells
    call makeTumourcell(kcell)
enddo
end subroutine

!-----------------------------------------------------------------------------------------
! Note: potency and exhaustion random variables have the same Gaussian distribution, appropriately scaled.
! The idea is that std is sufficiently small that rv_normal is almost never < 0.
! With mean = 1, std <= 0.2 should be OK.
! This is instead of using log-normal distributions.
! CAR levels are lognormally distributed
!-----------------------------------------------------------------------------------------
subroutine makeTcell(kcell)
integer :: kcell
integer :: ic, nc, kpar=0
real(SP) :: p1, p2, R, p(3), CARlevel
type(Tcell_type), pointer :: Tp
!real(SP) :: prolif_base = 6

Tp => Tcell(kcell)
Tp%ID = kcell
Tp%status = ALIVE
Tp%totalStim = 0
Tp%generation = 1
p1 = meanExhaustion
p2 = stdExhaustion
R = max(0.0,rv_normal(p1,p2,kpar))
Tp%exhaustion = R*exhaustionBase
p1 = potencyMedian
p2 = potencyShape
R = max(0.0,rv_lognormal(p1,p2,kpar))
Tp%potency = R
if (nCARS == 1) then    ! only one CAR type (concept 0) 
    nc = 1
else
    if (concept == 1) then
        nc = 3
    elseif (concept == 2) then
        R = par_uni(kpar)
        if (R < 0.5) then
            nc = 1
        else
            nc = 2
        endif
    elseif (concept == 3) then
        p(3) = doubleExpProb
        p(1) = 0.5*(1 - p(3))
        p(2) = p(1)
        if (p(3) == 1.0) then
            nc = 3
        else
            nc = random_choice(p,3,kpar)
        endif
    endif
endif
Tp%nc = nc
if (nc == 3) then
    if (concept == 1) then  ! both receptors have the same level (use CAR #1)
        p1 = CARMedian(1)
        p2 = CARShape(1)
        CARlevel = rv_lognormal(p1,p2,kpar)
        Tp%CARlevel(1:2) = CARlevel
    else
        do ic = 1,2
            p1 = CARMedian(ic)
            p2 = CARShape(ic)
            Tp%CARlevel(ic) = rv_lognormal(p1,p2,kpar)
        enddo
    endif
else
    p1 = CARMedian(nc)
    p2 = CARShape(nc)
    Tp%CARlevel(nc) = rv_lognormal(p1,p2,kpar)  ! nc = 1 or 2
    Tp%CARlevel(3-nc) = 0                       ! 3-nc = 2 or 1
endif
Tp%nMeetings = 0
if (Tp%Carlevel(1) < 0.05) then
    write(nflog,'(a,i6,f6.3)') 'kcell, CARlevel: ',kcell,Tp%CARlevel(1)
endif
end subroutine 

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
subroutine makeTumourcell(kcell)
integer :: kcell
integer :: ic, kpar=0
real(SP) :: p1, p2, R
type(Tumourcell_type), pointer :: Tump

Tump => Tumourcell(kcell)
Tump%status = ALIVE
p1 = susceptMedian
p2 = susceptShape
R = max(0.0,rv_lognormal(p1,p2,kpar))
Tump%suscept = R
do ic = 1,nCARs
    p1 = targetExpMedian(ic)
    p2 = targetExpShape(ic)
    R = max(0.0,rv_lognormal(p1,p2,kpar))
    Tump%targetExp(ic) = R
enddo
end subroutine 

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
function EncounterProb() result(p)
real(SP) :: p

p = nTumcells*pMeetRate
end function

!-----------------------------------------------------------------------------------------
! Choose a random tumour cell, but not one that has already been met, or has been killed.
!-----------------------------------------------------------------------------------------
function RandomTumourCell result(k)
integer :: k
integer :: nt, kpar=0
logical :: retry

nt = 0
retry = .true.
do while (retry)
    k = random_int(1,nTumcells,kpar)
    if (k < 1) then
        write(*,*) 'RandomTumourCell: nTumcells, k: ',nTumcells,k
        stop
    endif
    retry = (TumourCell(k)%status == MET .or. TumourCell(k)%status == DEAD)   ! reasons to try again
    nt = nt+1
    if (nt > 100) then
!        write(*,*) 'RandomTumourCell: too many tries: ',nt
        k = 0
        exit
    endif
enddo
end function

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
subroutine CanKill(Tp,Tump,Q,kill)
type(Tcell_type), pointer :: Tp
type(Tumourcell_type), pointer :: Tump
real(SP) :: Q   ! intensity of TCR signal
logical :: kill
real(SP) :: Pkill, R
integer :: ic, nc, kpar=0

nc = Tp%nc
if (nc == 3) then
    Q = 0
    do ic = 1,nCARs
        Q = Q + Tp%CARlevel(ic)*Tump%targetExp(ic)
    enddo
else
    Q = Tp%CARlevel(nc)*Tump%targetExp(nc)
endif
if ((Tp%generation == 1) .and. (Tp%totalStim < activationThreshold)) then
!    if (kcell_now == 1) &
!    write(*,*) 'CanKill: generation 1: totalStim, Q: ',Tp%totalStim, Q
    kill = .false.
    return
endif

Pkill = Q*Tp%potency*Tump%suscept
!write(*,*) 'CanKill: Pkill: ',Pkill
if (useKillProb) then
!    Pkill = Pkill/Pkillmax  ! to normalise
    R = par_uni(kpar)
    kill = (R < Pkill/Pkillmax)
!    if (kill .and. (kcell_now == 1)) write(nflog,'(a,3f8.4)') 'Pkill, R: ',Pkill,Pkill/Pkillmax,R
else    ! use threshold
    kill = (Pkill > killThreshold)
!    if (kill .and. (kcell_now == 1)) write(nflog,'(a,2f8.4)') 'Pkill,killThreshold: ',Pkill,killThreshold
endif
end subroutine

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
subroutine IncrementExhaustion(Tp,Q)
type(Tcell_type), pointer :: Tp
real(SP) :: Q
real(SP) :: exhaustLimit = 5

!Tp%exhaustion = Tp%exhaustion + min(exhaustLimit,Q*exhaustionRate)
Tp%exhaustion = Tp%exhaustion + Q*exhaustionRate
end subroutine

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
subroutine CountClones(nIDs)
integer :: ID, kcell, nIDs
type(Tcell_type), pointer :: Tp

nclones = 0
do kcell = 1,nTcells
    Tp => Tcell(kcell)
    if (Tp%status /= DEAD) then
        ID = Tp%ID
        nclones(ID) = nclones(ID) + 1
    endif
enddo
nIDs = 0
do ID = 1,nTcells0
    if (nclones(ID) > 0) nIDs = nIDs + 1
enddo
!write(*,*) 'nIDs: ',nIDs
end subroutine

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
!subroutine runner
!
!do istep = 1,nsteps
!    call simulate
!enddo
!end subroutine

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
subroutine get_initial_values(init) bind(C)
!DEC$ ATTRIBUTES DLLEXPORT :: get_initial_values  
use, intrinsic :: iso_c_binding
integer(c_int) :: init(4)

init(1) = 0
init(2) = nTcells0
init(3) = nTumcells0
init(4) = nTcells0
end subroutine

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
subroutine simulate_step(nTalive,nTumalive,nIDs,res) bind(C)
!DEC$ ATTRIBUTES DLLEXPORT :: simulate_step  
use, intrinsic :: iso_c_binding
integer(c_int) :: nTalive, nTumalive, nIDs, res
integer :: kcell, ktumour, nkilled, ndied, nadded, nTemp, nmet, nTumDivided, kpar=0
integer :: gen0, ID, k
integer :: totalDied, nmisses
type(Tcell_type), pointer :: Tp
type(Tumourcell_type), pointer :: Tump
real(SP) :: p_meet, Q, R, pdeath
logical :: kill

!totalDied = 0
!do istep = 1,nSteps
istep = istep + 1
res = 0
    nkilled = 0
    ndied = 0
    nadded = 0
    nTumalive = nTumcells
    nTemp = nTcells
    nTalive = 0
    p_meet = EncounterProb()
    nmet = 0
    do kcell = 1,nTcells
        kcell_now = kcell
        nmisses = 0
!        if (mod(kcell,100) == 0) write(*,*) 'kcell: ',kcell
        Tp => Tcell(kcell)
        if (Tp%status /= DEAD) then
            nTalive = nTalive + 1
            R = par_uni(kpar)
            if (R < p_meet) then    ! T cell encounters a tumour cell
                ! Need to select a tumour cell randomly
                ktumour = RandomTumourCell()
                if (ktumour == 0) then
                    nmisses = nmisses+1
!                    cycle
                endif
!                write(*,*) 'Meeting: ',kcell,ktumour,R,p_meet
                if (ktumour > 0) then
                    Tump => TumourCell(ktumour)
                    Tump%status = MET
                    nmet = nmet + 1
                    call CanKill(Tp,Tump,Q,kill)
!                    if (Tp%totalStim > activationThreshold) then
!                        write(*,*) 'Activated: totalStim: ',Tp%totalStim
!                        stop
!                    endif
                    Tp%totalStim = Tp%totalStim + Qfactor*Q
!                    if (kcell == 1) &
!                    write(*,*) 'kcell,totalStim, Qfactor, Q: ',kcell,Tp%totalStim, Qfactor, Q
                    
                    call IncrementExhaustion(Tp,Q)
                    Tp%nMeetings = Tp%nMeetings + 1
                    if (kill) then
!                        write(*,*) 'kill: kcell,ktumour,Q: ',kcell,ktumour,Q
                        ID = Tp%ID
                        nkilled = nkilled + 1
                        totalKilled(ID) = totalKilled(ID) + 1                   
                        nTumalive = nTumalive - 1
                        if (nTumalive == 0) then
                            write(*,*) 'All tumour cells are dead'
                            write(nflog,*) 'All tumour cells are dead'
                            call logger('All tumour cells are dead')
                            res = 1
                            return
!                            stop
                        endif
                        Tump%status = DEAD
                    endif
                endif
            endif
        endif
!        if (nmisses > 1) write(*,*) 'kcell, nmisses: ',kcell,nmisses
    enddo
    
    nTemp = nTcells
    do kcell = 1,nTcells
        Tp => Tcell(kcell)
        if (Tp%status /= DEAD) then
            if (Tp%exhaustion > exhaustionThreshold) then
                Tp%status = DEAD
                ndied = ndied + 1
                totalDied = totalDied + 1
                nTalive = nTalive - 1
                cycle
            endif
            if (istep > 10) then    !.and. Tp%totalStim < divideThreshold/4) then   ! this is dubious!
                pdeath = deathProbMax*(1 - Tp%totalStim/divideThreshold)
                R = par_uni(kpar)
                if (R < pdeath) then
                    Tp%status = DEAD
                    ndied = ndied + 1
                    totalDied = totalDied + 1
                    nTalive = nTalive - 1
                    cycle
                endif
            endif
            Tp%totalStim = Tp%totalStim*exp(-stimDecayRate*deltaT)
            if (kcell == kdbug) write(nflog,'(a,2i4,2f8.3)') 'istep,generation,totalStim,exhaustion: ',istep, &
                Tp%generation,Tp%totalStim,Tp%exhaustion
            ! T cell division
            if (Tp%totalStim > divideThreshold) then
                nTemp = nTemp + 1
                nTalive = nTalive + 1
                if (nTemp > nTcellsmax) then
                    write(*,*) 'Error: nTcellsmax exceeded: ',nTcellsmax
                    write(nflog,*) 'Error: nTcellsmax exceeded: ',nTcellsmax
                    write(logmsg,*) 'Error: nTcellsmax exceeded: ',nTcellsmax
                    call logger(logmsg)
                    res = 1
                    return
!                    stop
                endif
                gen0 = Tp%generation
                Tcell(nTemp) = Tcell(kcell)
                Tp%totalStim = Tp%totalStim/2
                Tcell(nTemp)%totalStim = Tp%totalStim
                Tp%generation = gen0 + 1
                Tcell(nTemp)%generation = gen0 + 1
                nadded = nadded + 1
            endif
        endif
    enddo
    nTcells = nTemp
    
    if (nkilled > 0) then   ! need to recreate the tumour cell list
        ktumour = 0
        do kcell = 1,nTumcells
            if (TumourCell(kcell)%status /= DEAD) then
                ktumour = ktumour + 1
                TumourCell(ktumour) = TumourCell(kcell)
                TumourCell(ktumour)%status = ALIVE
            endif
        enddo
!        write(*,'(a,4i8)') 'ktumour,nTumcells,nkilled: ',ktumour,nTumcells,nkilled
        nTumcells = ktumour
    endif
    ! Tumour cell division
    nTemp = nTumCells
    nTumDivided = 0
    do kcell = 1,nTemp   
        R = par_uni(kpar)
        if (R < tumourDivideProb) then
            nTumDivided = nTumDivided + 1
            nTumCells = nTumCells + 1
            nTumalive = nTumalive + 1
            TumourCell(nTumCells) = TumourCell(kcell)
        endif
    enddo
    if (nTumcells /= (nTemp + nTumDivided)) then
        write(*,*) 'Inconsistent tumour cell counts: ',nTumcells, (nTemp + nTumDivided)
        write(nflog,*) 'Inconsistent tumour cell counts: ',nTumcells, (nTemp + nTumDivided)
        write(logmsg,*) 'Inconsistent tumour cell counts: ',nTumcells, (nTemp + nTumDivided)
        call logger(logmsg)
        res = 2
        return
!        stop
    endif
    if (nTumcells /= nTumalive) then
        write(*,*) 'Inconsistent tumour cell counts: nTumcells, nTumalive: ',nTumcells, nTumalive
        write(nflog,*) 'Inconsistent tumour cell counts: nTumcells, nTumalive: ',nTumcells, nTumalive
        write(logmsg,*) 'Inconsistent tumour cell counts: nTumcells, nTumalive: ',nTumcells, nTumalive
        call logger(logmsg)
        res = 2
        return
!        stop
    endif
    call CountClones(nIDs)
    if (nTalive == 0) then
        write(*,*) 'All T cells are dead'
        write(nflog,*) 'All T cells are dead'
        call logger('All T cells are dead')
        res = 1
        return
!        stop
    endif
    if (.not.use_TCP) then
        write(*,*) '# of live T, tumour cells, nIDs: ',nTalive,nTumalive,nIDs
    endif
!    write(logmsg,*) '# of live T, tumour cells: ',nTalive,nTumalive
!    call logger(logmsg)
!enddo

if (istep == nsteps) then
    call account(nTalive,nTumalive,nIDs)
endif

end subroutine

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
subroutine account(nTalive,nTumalive,nIDs)
integer :: nTalive,nTumalive,nIDs
integer k, ID
real, allocatable :: a(:)
integer, allocatable :: t(:)

write(logmsg,*) '# of live T cells, tumour cells, T cell lineages: ',nTalive,nTumalive,nIDs
call logger(logmsg)

allocate(a(nTcells0))
allocate(t(nTcells0))
k = 0
do ID = 1,nTcells0
    if (nclones(ID) > 0) then
        !write(nflog,'(2i8)') ID,nclones(ID)
        k = k+1
        a(k) = totalKilled(ID)
        t(k) = ID
    endif
enddo
!write(*,*) 'sum of totalKilled: ',sum(totalKilled(:))
call qsort(a,nTcells0,t)
write(nflog,*) 'Top 100 killing T cell lineages:'
write(nflog,'(2i6)') (t(k),int(a(k)),k=nTcells0,nTcells0-100,-1)
if (.not.use_TCP) then
    write(*,*) 'Top 10 killing T cell lineages:'
    write(*,'(2i6)') (t(k),int(a(k)),k=nTcells0,nTcells0-10,-1)
endif
write(logmsg,*) 'Top 10 killing T cell lineages:'
call logger(logmsg)
do k = nTcells0,nTcells0-10,-1
    write(logmsg,'(2i6)') (t(k),int(a(k)))
    call logger(logmsg)
enddo
deallocate(a)
deallocate(t)
end subroutine

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
subroutine execute(ncpu,infile_array,inbuflen,outfile_array,outbuflen) BIND(C)
!DEC$ ATTRIBUTES DLLEXPORT :: execute
use, intrinsic :: iso_c_binding
character(c_char) :: infile_array(1024), outfile_array(1024)
integer(c_int) :: ncpu, inbuflen, outbuflen
character*(128) :: infile, outfile
logical :: ok, success, isopen
integer :: i, res

use_CPORT1 = .false.	! DIRECT CALLING FROM C++
infile = ''
do i = 1,inbuflen
	infile(i:i) = infile_array(i)
enddo
outfile = ''    ! outfile is not used
do i = 1,outbuflen
	outfile(i:i) = outfile_array(i)
enddo

inquire(unit=nflog,OPENED=isopen)
if (.not.isopen) then
    open(nflog,file='cart.log',status='replace')
endif
awp_0%is_open = .false.
awp_1%is_open = .false.

!#ifdef GFORTRAN
!    write(logmsg,'(a)') 'Built with GFORTRAN'
!	call logger(logmsg)
!#endif

logmsg = 'OS??'
#ifdef LINUX
    write(logmsg,'(a)') 'OS is Linux'
#endif
#ifdef OSX
    write(logmsg,'(a)') 'OS is OS-X'
#endif
#ifdef _WIN32
    write(logmsg,'(a)') 'OS is Windows'
#endif
#ifdef WINDOWS
    write(logmsg,'(a)') 'OS is Windows'
#endif
call logger(logmsg)

!#if defined(OPENMP) || defined(_OPENMP)
!    write(logmsg,'(a)') 'Executing with OpenMP'
!	call logger(logmsg)
!#endif

write(logmsg,*) 'inputfile:  ', infile
call logger(logmsg)
if (use_tcp) then
	call connecter(ok)
	if (.not.ok) then
		call logger('Failed to make TCP connections')
		return
	endif
endif
ok = .true.
call setup(ncpu,infile,outfile,ok)
if (ok) then
	clear_to_send = .true.
	simulation_start = .true.
	istep = 0
	res = 0
else
	call logger('=== Setup failed ===')
	res = 1
	stop
endif

end subroutine

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
subroutine connection(awp,port,error)
TYPE(winsockport) :: awp
integer :: port, error
integer :: address = 0
!!!character*(64) :: ip_address = "127.0.0.1"C      ! need a portable way to make a null-terminated C string
character*(64) :: host_name = "localhost"

if (.not.winsock_init(1)) then
    call logger("winsock_init failed")
    stop
endif
!write(nftemp,*) 'did winsock_init'

awp%handle = 0
awp%host_name = host_name
awp%ip_port = port
awp%protocol = IPPROTO_TCP
call Set_Winsock_Port (awp,error)

if (.not.awp%is_open) then
    write(nflog,*) 'Error: connection: awp not open: ',port
endif
end subroutine

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
subroutine connecter(ok)
logical :: ok
integer :: error

! Main connection
ok = .true.
call connection(awp_0,TCP_PORT_0,error)
if (awp_0%handle < 0 .or. error /= 0) then
    write(logmsg,'(a)') 'TCP connection to TCP_PORT_0 failed'
    call logger(logmsg)
    ok = .false.
    return
endif
if (.not.awp_0%is_open) then
	write(logmsg,'(a)') 'No connection to TCP_PORT_0'
    call logger(logmsg)
    ok = .false.
    return
endif
write(logmsg,'(a)') 'Connected to TCP_PORT_0  '
call logger(logmsg)

if (use_CPORT1) then
	call connection(awp_1,TCP_PORT_1,error)
	if (awp_1%handle < 0 .or. error /= 0) then
		write(logmsg,'(a)') 'TCP connection to TCP_PORT_1 failed'
		call logger(logmsg)
		ok = .false.
		return
	endif
	if (.not.awp_1%is_open) then
		write(logmsg,'(a)') 'No connection to TCP_PORT_1'
		call logger(logmsg)
		ok = .false.
		return
	endif
	write(logmsg,'(a)') 'Connected to TCP_PORT_1  '
	call logger(logmsg)
endif
! Allow time for completion of the connection 
call sleeper(2)
end subroutine

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
subroutine terminate_run(res) BIND(C)
!DEC$ ATTRIBUTES DLLEXPORT :: terminate_run 
use, intrinsic :: iso_c_binding
integer(c_int) :: res
character*(8), parameter :: quit = '__EXIT__'
integer :: error, i

call wrapup

if (res == 0) then
	call logger(' Execution successful!')
else
	call logger('  === Execution failed ===')
	call sleeper(1)
endif

if (use_TCP) then
	if (stopped) then
	    call winsock_close(awp_0)
	    if (use_CPORT1) call winsock_close(awp_1)
	else
	    call winsock_send(awp_0,quit,8,error)
	    call winsock_close(awp_0)
		if (use_CPORT1) then
			call winsock_send(awp_1,quit,8,error)
			call winsock_close(awp_1)
		endif
	endif
endif
!close(nflog)

end subroutine

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
subroutine wrapup
logical :: isopen

!call logger('doing wrapup ...')

deallocate(Tcell)
deallocate(Tumourcell)
deallocate(nclones)
deallocate(totalKilled)

call par_zigfree

! Close all open files
inquire(unit=nfout,OPENED=isopen)
if (isopen) then
	close(nfout)
!	call logger('closed nfout')
endif
end subroutine

end module




