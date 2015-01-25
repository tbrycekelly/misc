!========== OCP5930 Assignment 1 Source ===========!
! Author	Thomas Bryce Kelly
! Email		tbk14@my.fsu.edu
! See this and more code over at <https://github.com/tbrycekelly>
! Checkout http://tkelly.me/ for other information
!
! Copywrite	GNU GENERAL PUBLIC LICENSE V3.0
!			see <http://www.gnu.org/licenses/gpl.html> for details.
! ==================================================!

program test

integer n, t	! [n] is the number of steps to take, [t] is the time component
real*4 x,y, pi, k, omega, phi, A	!x,y are coordinates, all others are listed below
logical::exist		!Bool for the existence of the output file
pi = 3.1415926535897	!Const

!~~~~~~ These are the contants of the equation y=Asin(kx-wt+phi) ~~~~~~~!
n = 1000		!Number of data points in window (int)		!
A = 1			!Amplitude					!
k = 10			!k	(dependence on x)			!
omega = 0.7		!Omega	(Dependence on t)			!
phi = 0			!Phase shift					!
!~~~~~~		~~~~~~		~~~~~~~		~~~~~~~		~~~~~~~~!

!Time to open the output file <points.csv>
inquire(file="points.csv", exist=exist)		!Test if file exists

!Dont want IO error...
if(exist) then
  ! Erase the old file before creating new one.
  open(12, file="points.csv", status="old")
  close(12, status="delete")
endif

! Create new file <points.csv>
open(12, file="points.csv", status="new", action="write")

!Write the header
write (12,*) "X,Y,t"

do 12 t = 0, 3		!Loop for each value of t we want to test
  x = 0			!Init [x] value each time a new [t] is used.
  do 10 i = 0, n	!Loop for each [x] value in window ([n] of them)
    x = x+1.0/n		!Step the [x] value
    y = A*sin(k*x - omega*t + phi)	! y = Asin(kx-wt+phi)
    write (12,*) x, ',',y,',',t		!Write to output file
  end do
end do

end program test
