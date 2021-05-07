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
read(nfin,*) exhaustion_base
read(nfin,*) exhaustion_jump
read(nfin,*) exhaustion_limit
read(nfin,*) pmeet_rate
read(nfin,*) Pkillmax
read(nfin,*) iuse_kill_prob
read(nfin,*) killThreshold
read(nfin,*) tumourDivideProb
useKillProb = (iuse_kill_prob == 1)
!read(nfin,*) CARMedian(1)
!read(nfin,*) CARMedian(2)
!read(nfin,*) CARShape(1)
!read(nfin,*) CARShape(2)
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
nTcellsmax = 1000*nTcells0
nTumcells0 = nTcells0*NTumPerTcell
allocate(Tcell(nTcellsmax))
allocate(Tumourcell(2*nTumcells0))
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
p1 = mean
p2 = std
R = max(0.0,rv_normal(p1,p2,kpar))
Tp%potency = R
R = max(0.0,rv_normal(p1,p2,kpar))
!Tp%prolif = int(R*prolif_base)
R = max(0.0,rv_normal(p1,p2,kpar))
Tp%exhaustion = R*exhaustion_base
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
Tp%nc = nc
if (nc == 3) then
    do ic = 1,2
        R = max(0.0,rv_normal(p1,p2,kpar))
        Tp%CARlevel(ic) = R
    enddo
else
    R = max(0.0,rv_normal(p1,p2,kpar))
    Tp%CARlevel(nc) = R
    Tp%CARlevel(3-nc) = 0
endif
Tp%nMeetings = 0

!    p1 = CARMedian(ic)
!    p2 = CARShape(ic)
!    Tp%CARlevel(ic) = rv_lognormal(p1,p2,kpar)
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
do ic = 1,2
    R = max(0.0,rv_normal(p1,p2,kpar))
    Tump%targetExp(ic) = R
enddo
end subroutine 

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
function EncounterProb() result(p)
real(SP) :: p

p = nTumcells*pmeet_rate
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

!activated = .false.
nc = Tp%nc
if (nc == 3) then
    Q = 0
    do ic = 1,2
        Q = Q + Tp%CARlevel(ic)*Tump%targetExp(ic)
!        if (Q > Tactivation) then
!            activated = .true.
!            exit
!        endif
    enddo
else
    Q = Tp%CARlevel(nc)*Tump%targetExp(nc)
!    if (Q > Tactivation) then
!        activated = .true.
!    endif
endif
Pkill = Q*Tp%potency*Tump%suscept
if (useKillProb) then
!    Pkill = Pkill/Pkillmax  ! to normalise
    R = par_uni(kpar)
    kill = (R < Pkill/Pkillmax)
    if (kill .and. (kcell_now == 1)) write(nflog,'(a,3f8.4)') 'Pkill, R: ',Pkill,Pkill/Pkillmax,R
else    ! use threshold
    kill = (Pkill > killThreshold)
    if (kill .and. (kcell_now == 1)) write(nflog,'(a,2f8.4)') 'Pkill,killThreshold: ',Pkill,killThreshold
endif
end subroutine

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
subroutine IncrementExhaustion(Tp,Q)
type(Tcell_type), pointer :: Tp
real(SP) :: Q

Tp%exhaustion = Tp%exhaustion + Q*exhaustion_jump
end subroutine

!-----------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------
subroutine runner
integer :: kcell, ktumour, nkilled, ndied, nadded, nTalive, nTumalive, nTemp, nmet, nTumDivided, kpar=0
integer :: day
type(Tcell_type), pointer :: Tp
type(Tumourcell_type), pointer :: Tump
real(SP) :: p_meet, Q, R
logical :: kill

!nhours = 24*ndays
!do hour = 1,nhours
do day = 1,ndays
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
                call IncrementExhaustion(Tp,Q)
                Tp%nMeetings = Tp%nMeetings + 1
                if (kill) then
                    if (kcell == 1) then
                        write(nflog,*) 'kills: day,kcell,ktumour: ',day,kcell,ktumour
                    endif
                    nkilled = nkilled + 1
                    nTumalive = nTumalive - 1
                    if (nTumalive == 0) then
                        write(*,*) 'All tumour cells are dead'
                        stop
                    endif
                    Tump%status = DEAD
                    if (Tp%exhaustion > exhaustion_limit) then
                        Tp%status = dead
                        ndied = ndied + 1
                        nTalive = nTalive - 1
                    else
                        nTemp = nTemp + 1
                        if (nTemp > nTcellsmax) then
                            write(*,*) 'Error: nTcellsmax exceeded: ',nTcellsmax
                            stop
                        endif
                        Tcell(nTemp) = Tcell(kcell)
                        nadded = nadded + 1
                    endif
                endif
            endif
        endif
    enddo
!    write(*,*) 'nmet: ',nmet
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
    write(*,*) 'day,nTcells,nTumcells: ',day,nTalive,nTumcells,nTumDivided
    if (nTalive == 0) then
        write(*,*) 'All T cells are dead'
        stop
    endif
enddo       
end subroutine

end module




