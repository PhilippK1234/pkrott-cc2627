PROGRAM verlettest 
  IMPLICIT NONE
  INTEGER :: mass, k, i
  REAL, PARAMETER :: tau = 0.2
  REAL :: x1, x2, x3, f1, f2, f3, v1, v2, v3, analytical_x2
  REAL, DIMENSION(:), ALLOCATABLE :: pos, f, v

  ALLOCATE ( pos (3) )
  ALLOCATE ( f (3) )
  ALLOCATE ( v (3) )

  pos = [0.0, 0.0, 0.0]
  f = [0.0, 0.1, 0.0]
  v = [0.0, 0.0, 0.0]
  
  
  mass = 1
  k = 600

  PRINT *, pos

  DO i = 1, k
    pos = pos + tau * v + tau**2 * (f/(2*mass))

    v = v + tau/(2*mass) * (2*f)

    PRINT *, pos
  ENDDO

  !PRINT *, analytical_x2
ENDPROGRAM
