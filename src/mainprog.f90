! Main program
program main
use cart
implicit none
integer :: i, nlen, cnt, status, ncpu
character*(256) :: b, c, progname, str

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
        inputfile = c(1:nlen)																! --> infile
        write(*,*) 'Input file: ',trim(inputfile)
    endif
    if (i == 2) then
        str = c(1:nlen)
        read(str,*) ncpu
        write(*,*) 'mainprog ncpu: ',ncpu
    endif
enddo

!nDays = 25
!Mnodes = 2
!nCARs = 1
!inputfile = 'cart.inp'
logfile = 'cart.log'
call init(ncpu)
call runner

end program
