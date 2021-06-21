! Main program
program main
use cart
implicit none
integer :: i, nlen, cnt, status, ncpu, inbuflen, outbuflen
integer :: nTalive, nTumalive, nIDs, res
character*(256) :: b, c, progname, str
character*(128) :: infile,outfile

call DisableTCP
call get_command (b, nlen, status)
if (status .ne. 0) then
    write (*,*) 'get_command failed with status = ', status
    stop
end if
!write (*,*) 'command line = ', b(1:len)
call get_command_argument (0, c, nlen, status)
if (status .ne. 0) then
    write (*,*) 'Getting command name failed with status = ', status
    stop
end if
!write (*,*) 'command name = ', c(1:len)
progname = c(1:nlen)
cnt = command_argument_count ()
write (*,*) 'number of command arguments = ', cnt
if (cnt < 1) then
    write(*,*) 'Use: ',trim(progname),' input_file'
    stop
endif

!if (cnt < 2) then
!    write(*,*) 'Use: ',trim(progname),' num_cpu input_file'
!    write(*,*) 'or:  ',trim(progname),' num_cpu input_file output_file'
!    write(*,*) 'or:  ',trim(progname),' num_cpu input_file output_file colony_days'
!    write(*,*) '  simulate colony if colony_days > 0'
!    stop
!endif
ncpu = 0
do i = 1,cnt
    call get_command_argument (i, c, nlen, status)
    if (status .ne. 0) then
        write (*,*) 'get_command_argument failed: status = ', status, ' arg = ', i
        stop
    end if
    if (i == 1) then
        infile = c(1:nlen)																! --> infile
        write(*,*) 'Input file: ',trim(infile)
    endif
    if (i == 2) then
        str = c(1:nlen)
        read(str,*) ncpu
        write(*,*) 'mainprog ncpu: ',ncpu
    endif
enddo

outfile = 'cart.out'
inbuflen = len(infile)
outbuflen = len(outfile)
write(*,*) 'call execute'
call execute(ncpu,infile,inbuflen,outfile,outbuflen)
write(*,*) 'nsteps: ',nsteps

do i = 1,nsteps
    call simulate_step(nTalive,nTumalive,nIDs,res)
    write(*,'(a,4i10)') 'step,nTcells,nTumcells,nIDs: ',i,nTalive,nTumalive,nIDs
enddo
end program
