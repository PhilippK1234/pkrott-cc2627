PROGRAM test
  IMPLICIT NONE
  REAL :: diameter, radius, area
  REAL,PARAMETER :: pi=3.14
  READ(*,*) diameter
  radius = diameter/2
  area = pi*(radius)**2
  PRINT *, area
END PROGRAM test
