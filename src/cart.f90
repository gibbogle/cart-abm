module cart

use global
implicit none

contains

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
subroutine readparameters
integer :: iuse_kill_prob

read(nfin,*) concept
read(nfin,*) nTcells0
read(nfin,*) nTumPerTcell
read(nfin,*) seed(1)
read(nfin,*) seed(2)
read(nfin,*) mean
read(nfin,*) std
read(nfin,*) doubleExpProb
read(nfin,*) exhaustionBase
read(nfin,*) exhaustionRate
read(nfin,*) exhaustionThreshold
read(nfin,*) activationThreshold
read(nfin,*) divideThreshold
read(nfin,*) killThreshold
read(nfin,*) stimDecayFactor
read(nfin,*) Qfactor
read(nfin,*) pMeetRate
read(nfin,*) Pkillmax
read(nfin,*) iuse_kill_prob
read(nfin,*) tumourDivideProb
useKillProb = (iuse_kill_prob == 1)
read(nfin,*) CARMedian(1)
read(nfin,*) CARMedian(2)
read(nfin,*) CARShape(1)
read(nfin,*) CARShape(2)
close(nfin)

!call check_lognormal(CARMedian(1), CARShape(1))

end subroutine

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
subroutine init
logical :: ok

open(nfin,file=inputfile,status='old')
open(nflog,file=logfile,status='replace')

call omp_initialisation(ok)
call readparameters
nTcellsmax = 10000*nTcells0
nTumcells0 = nTcells0*NTumPerTcell
allocate(Tcell(nTcellsmax))
allocate(Tumourcell(int(1.5*nTumcells0)))
nTcells = nTcells0
nTumcells = nTumcells0
call RNGInitialisation
call generate_cells

end subroutine

!----------------------------------------------------------------------------------------- 
!-----------------------------------------------------------------------------------------
subroutine omp_initialisation(ok)
logical :: ok
integer :: npr, nth

ok = .true.
!if (Mnodes == 1) return
#if defined(OPENMP) || defined(_OPENMP)
write(logmsg,'(a,i2)') 'Requested Mnodes: ',Mnodes
call logger(logmsg)
npr = omp_get_num_procs()
write(logmsg,'(a,i2)') 'Machine processors: ',npr
call logger(logmsg)

nth = omp_get_max_threads()
write(logmsg,'(a,i2)') 'Max threads available: ',nth
call logger(logmsg)
if (nth < Mnodes) then
    Mnodes = nth
    write(logmsg,'(a,i2)') 'Setting Mnodes = max thread count: ',nth
	call logger(logmsg)
endif

call omp_set_num_threads(Mnodes)
!$omp parallel
nth = omp_get_num_threads()
write(logmsg,*) 'Threads, max: ',nth,omp_get_max_threads()
call logger(logmsg)
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
! Note: all random variables have the same Gaussian distribution, appropriately scaled.
! The idea is that std is sufficiently small that rv_normal is almost never < 0.
! With mean = 1, std <= 0.2 should be OK.
! This is instead of using log-normal distributions.
!-----------------------------------------------------------------------------------------
subroutine makeTcell(kcell)
integer :: kcell
integer :: ic, nc, kpar=0
real(SP) :: p1, p2, R, p(3)
type(Tcell_type), pointer :: Tp
real(SP) :: prolif_base = 6

Tp => Tcell(kcell)
Tp%ID = kcell
Tp%status = ALIVE
Tp%totalStim = 0
Tp%generation = 1
p1 = mean
p2 = std
R = max(0.0,rv_normal(p1,p2,kpar))
Tp%potency = R
R = max(0.0,rv_normal(p1,p2,kpar))
!Tp%prolif = int(R*prolif_base)
R = max(0.0,rv_normal(p1,p2,kpar))
Tp%exhaustion = R*exhaustionBase
if (nCARS == 1) then
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
    do ic = 1,2
        R = max(0.0,rv_normal(p1,p2,kpar))
        Tp%CARlevel(ic) = R
    enddo
else
!    R = max(0.0,rv_normal(p1,p2,kpar))
!    R = rv_lognormal(p1,p2,kpar)
!    Tp%CARlevel(nc) = R
!    Tp%CARlevel(3-nc) = 0
    p1 = CARMedian(nc)
    p2 = CARShape(nc)
    Tp%CARlevel(nc) = rv_lognormal(p1,p2,kpar)
    Tp%CARlevel(3-nc) = 0
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
p1 = mean
p2 = std
R = max(0.0,rv_normal(p1,p2,kpar))
Tump%suscept = R
do ic = 1,nCARs
    R = max(0.0,rv_normal(p1,p2,kpar))
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
    retry = (TumourCell(k)%status == MET .or. TumourCell(k)%status == DEAD)   ! reasons to try again
    nt = nt+1
    if (nt > 100) then
!        write(*,*) 'RandomTumourCell: too many tries: ',nt
        k = 0
        exit
    endif
enddo
!if (nt > 1) write(*,*) 'tries: ',nt
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
if (Tp%totalStim < activationThreshold) then
    kill = .false.
    return
endif

Pkill = Q*Tp%potency*Tump%suscept
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
real(SP) :: exhaustLimit = 1.5

Tp%exhaustion = Tp%exhaustion + min(exhaustLimit,Q*exhaustionRate)
end subroutine

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
subroutine CountClones
integer, allocatable :: nclones(:)
integer :: ID, kcell, nIDs
type(Tcell_type), pointer :: Tp

allocate(nclones(nTcells0))
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
write(*,*) 'nIDs: ',nIDs
end subroutine

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
subroutine runner
integer :: kcell, ktumour, nkilled, ndied, nadded, nTalive, nTumalive, nTemp, nmet, nTumDivided, kpar=0
integer :: istep, gen0
integer :: totalKilled, totalDied
type(Tcell_type), pointer :: Tp
type(Tumourcell_type), pointer :: Tump
real(SP) :: p_meet, Q, R
logical :: kill

nSteps = (nDays*24.0)/deltaT
totalKilled = 0
totalDied = 0
do istep = 1,nSteps
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
!        if (mod(kcell,100) == 0) write(*,*) 'kcell: ',kcell
        Tp => Tcell(kcell)
        if (Tp%status /= DEAD) then
            nTalive = nTalive + 1
            R = par_uni(kpar)
            if (R < p_meet) then    ! T cell encounters a tumour cell
!                write(*,*) 'Meeting: ',kcell,ktumour,R,p_meet
                ! Need to select a tumour cell randomly
                ktumour = RandomTumourCell()
                if (ktumour == 0) cycle
                Tump => TumourCell(ktumour)
                Tump%status = MET
                nmet = nmet + 1
                call CanKill(Tp,Tump,Q,kill)
                Tp%totalStim = Tp%totalStim + Qfactor*Q
                call IncrementExhaustion(Tp,Q)
                Tp%nMeetings = Tp%nMeetings + 1
                if (kill) then
!                    if (kcell == 1) then
!                        write(nflog,*) 'kills: step,kcell,ktumour: ',istep,kcell,ktumour
!                    endif
                    nkilled = nkilled + 1
                    totalKilled = totalKilled + 1                   
                    nTumalive = nTumalive - 1
                    if (nTumalive == 0) then
                        write(*,*) 'All tumour cells are dead'
                        stop
                    endif
                    Tump%status = DEAD
                endif
            endif
        endif
    enddo
    
    nTemp = nTcells
    do kcell = 1,nTcells
        Tp => Tcell(kcell)
        if (Tp%status /= DEAD) then
!            Tp%exhaustion = Tp%exhaustion + 0.5
            if (Tp%exhaustion > exhaustionThreshold) then
                Tp%status = DEAD
                ndied = ndied + 1
                totalDied = totalDied + 1
                nTalive = nTalive - 1
                cycle
            endif
            if (istep > 40 .and. Tp%totalStim < divideThreshold) then
                R = par_uni(kpar)
                if (R < 0.05) then
                    Tp%status = DEAD
                    ndied = ndied + 1
                    totalDied = totalDied + 1
                    nTalive = nTalive - 1
                    cycle
                endif
            endif
            Tp%totalStim = (1 - stimDecayFactor)*Tp%totalStim
    if (kcell == kdbug) write(nflog,'(a,2i4,2f8.3)') 'istep,generation,totalStim,exhaustion: ',istep, &
        Tp%generation,Tp%totalStim,Tp%exhaustion
            if (Tp%totalStim > divideThreshold) then
                nTemp = nTemp + 1
                nTalive = nTalive + 1
                if (nTemp > nTcellsmax) then
                    write(*,*) 'Error: nTcellsmax exceeded: ',nTcellsmax
                    stop
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
    ! Need to add tumour cell division
    nTemp = nTumCells
    nTumDivided = 0
    do kcell = 1,nTemp   
        R = par_uni(kpar)
        if (R < tumourDivideProb) then
            nTumDivided = nTumDivided + 1
            nTumCells = nTumCells + 1
            TumourCell(nTumCells) = TumourCell(kcell)
        endif
    enddo
    if (nTumcells /= (nTemp + nTumDivided)) then
        write(*,*) 'Inconsistent tumour cell counts: ',nTumcells, (nTemp + nTumDivided)
        stop
    endif
    write(*,'(a,4i8)') 'step,nTcells,nTumcells,killed: ',istep,nTalive,nTumcells,totalKilled
    if (nTalive == 0) then
        write(*,*) 'All T cells are dead'
        stop
    endif
    call CountClones()
enddo       
end subroutine

end module




