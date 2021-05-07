! Main program
program main
use cart
implicit none

nDays = 25
Mnodes = 2
nTcells0 = 10000
nCARs = 1
inputfile = 'cart.inp'
logfile = 'cart.log'
call init
call runner

end program
