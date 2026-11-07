PROGRAM verlet
  IMPLICIT NONE
  INTEGER :: mass, k, i
  REAL, PARAMETER :: tau = 0.2
  REAL :: x1, x2, x3, f1, f2, f3, v1, v2, v3, analytical_x2
  !REAL (KIND=wp), DIMENSION(:), ALLOCATABLE :: x, f, v

  !ALLOCATE ( x (3) )
  !ALLOCATE ( f (3) )
  !ALLOCATE ( v (3) )

  mass = 1
  k = 600
  x1 = 0.0
  x2 = 0.0 
  x3 = 0.0
  f1 = 0.0
  f2 = 0.1
  f3 = 0.0
  v1 = 0.0
  v2 = 0.0
  v3 = 0.0

  analytical_x2 = x2 + v2 + 0.5 * f2/mass * (tau * k)**2 

  PRINT *, x1, x2, x3

  DO i = 1, k
    x1 = x1 + tau * v1 + tau**2 * (f1/(2*mass))
    x2 = x2 + tau * v2 + tau**2 * (f2/(2*mass))
    x3 = x3 + tau * v3 + tau**2 * (f3/(2*mass))

    v1 = v1 + tau/(2*mass) * (2*f1)
    v2 = v2 + tau/(2*mass) * (2*f2)
    v3 = v3 + tau/(2*mass) * (2*f3)

    PRINT *, x1, x2, x3
  ENDDO

  PRINT *, analytical_x2
ENDPROGRAM
