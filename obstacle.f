c---- subroutine obstacle------------------- A.Posa - July 2012 --------
C
C     PURPOSE: Represents an obstacle on the surface of the
C     immersed-boundary
C
C-----------------------------------------------------------------------
      subroutine obstacle(us,vs,ws,p,nx,ny,nz,ind)

c      IMPLICIT NONE
      include 'common.h'
c
c.... input/output var. and arrays
      integer nx,ny,nz,ind
      real    us(nx,ny,nz),vs(nx,ny,nz),ws(nx,ny,nz),p(nx,ny,nz)
c
C.... local var. and arrays
      integer i,j,k
      integer i1,i2,k1,k2,k1g,k2g

!      parameter (i1=201,i2=210,k1g=130,k2g=145) full resolution
      parameter (i1=91,i2=97,k1g=356,k2g=370)

      k1=k1g-myrank*(nz-2)
      if(k1.le.1) then
        k1=kz1
      elseif(k1.ge.nz) then
        k1=nz+1
      endif
      k2=k2g-myrank*(nz-2)
      if(k2.ge.nz) then
        k2=kz2
      elseif(k2.le.1) then
        k2=0
      endif

      if(ind.eq.1) goto 100

      DO k=k1,k2
      DO j=jy1,jy2
      DO i=i1,i2
        us(i,j,k) = 0.0
        vs(i,j,k) = 0.0
        ws(i,j,k) = 0.0
      ENDDO
      ENDDO
      ENDDO

      us(:,1,:)  = us(:,ny-1,:)
      us(:,ny,:) = us(:,2,:)
      vs(:,1,:)  = vs(:,ny-1,:)
      vs(:,ny,:) = vs(:,2,:)
      ws(:,1,:)  = ws(:,ny-1,:)
      ws(:,ny,:) = ws(:,2,:)

      CALL REFRESHBC(US,NX*NY,NZ)
      CALL REFRESHBC(VS,NX*NY,NZ)
      CALL REFRESHBC(WS,NX*NY,NZ)

      return

 100  continue

      DO k=k1,k2+1
      DO j=jy1,jy2
      DO i=i1,i2+1
        p(i,j,k) = 0.0
      ENDDO
      ENDDO
      ENDDO

      CALL REFRESHBC(P,NX*NY,NZ)
      
      return

      end

