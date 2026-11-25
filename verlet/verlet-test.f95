PROGRAM verlettest 
  IMPLICIT NONE
  
  ! variable declaration
  INTEGER :: k, i
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND (p=13, r=300)
  REAL (KIND=wp), PARAMETER :: tau = 0.2
  REAL (KIND=wp) :: mass
  REAL (KIND=wp), DIMENSION(3) :: pos, f, v

  ! variable initialization
  pos = [0.0, 0.0, 0.0]
  f = [0.0, 0.1, 0.0]
  v = [0.0, 0.0, 0.0]
  
  mass = 1.0_wp
  k = 600

  PRINT *, pos

  ! loop on timesteps
  DO i = 1, k

    pos = pos + tau * v + tau**2.0 * (f/(2.0*mass))
    v = v + tau/(2.0*mass) * (2.0*f)
    PRINT *, pos

  ENDDO

ENDPROGRAM
