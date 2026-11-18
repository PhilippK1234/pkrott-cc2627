PROGRAM verlet2
  IMPLICIT NONE
  INTEGER :: k, nk, i, num
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(p=13, r=300)
  REAL (KIND=wp) :: tau
  REAL (KIND=wp), DIMENSION(:, :), ALLOCATABLE :: x, v, f, fnext
  REAL (KIND=wp), DIMENSION(:), ALLOCATABLE :: mass

  OPEN(UNIT=11, FILE="verlet.in", STATUS="old", ACTION="read")

  READ(UNIT=11, FMT=*) nk, tau
  READ(UNIT=11, FMT=*) num
  
  ALLOCATE( x(num, 4) )
  ALLOCATE( v(num, 3) )
  ALLOCATE( f(num, 3) )
  ALLOCATE( fnext(num, 3) )
  ALLOCATE( mass(num) )

  DO i=1, num
    x(i, 1) = i
  ENDDO

  DO i=1, num
    READ(UNIT=11, FMT=*) mass(i), x(i, 2), x(i, 3), x(i, 4), v(i, 1), v(i, 2), v(i, 3)
  ENDDO

  CLOSE (11)

  DO i=1, num
    f(i, :) = [0.0_wp, 0.1_wp, 0.0_wp]
  ENDDO

  PRINT*, "x1:", x(1, :)
  PRINT*, "x2:", x(2, :)


ENDPROGRAM
