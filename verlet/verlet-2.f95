PROGRAM verlet2
  IMPLICIT NONE
  INTEGER :: k, nk, i, num
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(p=13, r=300)
  REAL (KIND=wp) :: tau
  REAL (KIND=wp), DIMENSION(:, :), ALLOCATABLE :: x, v, f, fnext
  REAL (KIND=wp), DIMENSION(:), ALLOCATABLE :: mass

  ! Read from input file

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

  ! calculate verlet algorithm

  DO k = 1, nk
    PRINT*, "---- k=", k, "----"
    DO i=1, num
      x(i, 2:4) = x(i, 2:4) + tau * v(i, :) + tau**2.0 * (f(i, :)/(2.0*mass(i)))
      fnext = f
      v(i, :) = v(i, :) + tau/(2.0*mass(i)) * (f(i, :) + fnext(i, :))
      PRINT*, x(i, :)
    ENDDO
  ENDDO



ENDPROGRAM
