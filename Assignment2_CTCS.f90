program main

real*8, dimension(25,10000) :: v
real*8 c, dx, dt, pi, r, sam		!Declaration of variables
logical::exist				!Does the output file exist
integer length, k

!===================== Set Values=============================!
length = 25
pi = 3.1415926535897			!define <pi>
c = 0.01				!Give <c> a value
dx = 1.0/25.0				!Let <dx> = 1/25 -- dimensionless
dt = dx/(20*c)				!Define <dt> so that CFL criteria satisfied -- dimensionless
!==============================================================!

r =c*dt/dx

write (*,*) "r = ", r, "dx = ", dx, "dt = ", dt

!~~~~~~~~~~~~~~~~~~~~~~~~File IO Setup~~~~~~~~~~~~~~~~~~~~~~~~~!
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

!===========================Data Time===========================!

!Initialize array
do i=1,length
  v(i,1) = sin(2*pi*(i*dx))+1
  write(12,*) 2*i*dx-1,",", v(i,1),",", 0
end do

!do i=1,length
!  if (i > 0.5 * length) then
!    v(i,1) = 0.0
!  else
!    v(i,1) = 1.0
!  end if
!  write(12,*) 2*i*dx-1,",", v(i,1),",", 0
!end do

k =1

!new first point = old first point + c*dt*(far right - 2* old first point + old point two)/dx^2
  v(1,k+1) = v(1,k)+r*( -1*v(length,k)+v(2,k) )			!Left end
  write (12,*) 2*dx-1,",", v(1,k+1),",", k*dt
  
  !write (*,*) "The slope on the left end point is ", v(length,k)-2*v(1,k)+v(2,k)
  
  do i = 2,length-1	!Loop for each point
    ! [new point] = [old point] + -c*dt([old point - 1]+[old point + 1] - 2[old point])/dx^2
    v(i, k+1) = v(i,k)+r*(-1*v(i-1,k)+v(i+1,k))
    !Write the value
    !write (*,*) 2*i*dx-1,",", v(i,k+1),",", k*dt
    write (12,*) 2*i*dx-1,",", v(i,k+1),",", k*dt
  end do

  !Right end point must be done seperately (special case)
  v(length,k+1) = v(length,k)+r*( -1*v(length-1,k)+v(1,k) )			!Right endss
  write (12,*) 2*length*dx-1, ",", v(length,k+1),",", k*dt
  
!Centered in time:
do k=2,10000	!Loop for each time: t=k*dt
  !new first point = old first point + c*dt*(far right - 2* old first point + old point two)/dx^2
  v(1,k+1) = v(1,k-1)+r*( -1*v(length,k)+v(2,k) )			!Left end
  write (12,*) 2*dx-1,",", v(1,k+1),",", k*dt
  
  !write (*,*) "The slope on the left end point is ", v(length,k)-2*v(1,k)+v(2,k)
  
  do i = 2,length-1	!Loop for each point
    ! [new point] = [old point] + -c*dt([old point - 1]+[old point + 1] - 2[old point])/dx^2
    v(i, k+1) = v(i,k-1)+r*(-1*v(i-1,k)+v(i+1,k))
    !Write the value
    !write (*,*) i*dx,",", v(i,k+1),",", k*dt
    write (12,*) 2*i*dx-1,",", v(i,k+1),",", k*dt
  end do

  !Right end point must be done seperately (special case)
  v(length,k+1) = v(length,k-1)+r*( -1*v(length-1,k)+v(1,k) )			!Right endss
  write (12,*) 2*length*dx-1, ",", v(length,k+1),",", k*dt
  
end do

close(12)	!Close the output file <points.csv>
stop
end program
