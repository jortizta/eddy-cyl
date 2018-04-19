	subroutine add_mean(uo,vo,wo,xc,yc,nx,ny,nz,stat)
	 !Passed Variables
	INCLUDE 'common.h'
	INCLUDE 'mpif.h'
	integer	:: nx,ny,nz
<<<<<<< HEAD
 	integer,intent(out)    :: stat
=======
	integer,intent(out)    :: stat 
>>>>>>> 5b588383f25931df60a9b92e8cd307edb59365e4
	real		       :: uo(nx,ny,nz),vo(nx,ny,nz),wo(nx,ny,nz)
	real 		       :: xc(nx),yc(ny),xx,yy,r

!Local Variables
<<<<<<< HEAD
 	integer                :: i,err1,j,ok1

	err1=0
	uo=0.0d0
	vo=0.0d0
	wo=1.0d0
=======
	integer                :: i,err1,j,ok1
	
        err1=0
        uo=0.0d0
        vo=0.0d0
        wo=1.0d0
>>>>>>> 5b588383f25931df60a9b92e8cd307edb59365e4
! 	call add_turbo(uo,vo,wo,nx,ny,nz,xc,err1)
! 	do k=1,nz
! 	do j=1,ny
	do i=1,nx
     	xx = xc(i)
!      	yy = yc(j)
!      	r   = dsqrt( yy**2.d0 + xx**2.d0 )
!      	wo(i,j,:) = wo(i,j,:) +  0.110d0*dexp(-xx**2.d0/(2.d0*2.0d0**2.d0))
!   	wo(i,:,:) = wo(i,:,:) +  0.110d0*dexp(-xx**2.d0/(2.d0*0.5d0**2.d0))
<<<<<<< HEAD
        enddo
=======
	enddo 
>>>>>>> 5b588383f25931df60a9b92e8cd307edb59365e4
!    	enddo
! 	enddo

! 	call sponge_setup(uo,vo,wo,nx,ny,nz,xc,ok1)

	stat=max(0,err1)
	return
	end

	subroutine add_density(xc,yc,nx,ny,nz,dens,stat)
<<<<<<< HEAD
!@h
!   Comments:
!     Options are linear, two-layer, and Jdeep.
!@q
	use  density_bg
	include 'common.h'
	include 'mpif.h'

!Passed Variables
	integer	:: nx,ny,nz
 	integer,intent(out)    :: stat
	reaL		       :: dens(nx,ny,nz),xc(nx),yc(ny)
!Local Variables
 	integer                :: i,err1,j
=======

!     Options are linear, two-layer, and Jdeep. 

	use density_bg
	include 'common.h'
	include 'mpif.h'
	
!Passed Variables
	integer	:: nx,ny,nz
	integer,intent(out)    :: stat 
	reaL		       :: dens(nx,ny,nz),xc(nx),yc(ny)
!Local Variables
	integer                :: i,err1,j
>>>>>>> 5b588383f25931df60a9b92e8cd307edb59365e4
!  	real(r8)               :: zz
	ALLOCATE(dens_bg(nx,ny))
!Linear      denP1=d(rho)/dx_3   denP2=(unused)            denP3=(unused)
!TwoLayer    denP1=Delta_rho     denP2=delta_omega         denP3=(unused)
!Jdeep       denP1=Jm            denP2=Jd                  denP3=z0
!Pycnocline  denP1=d(rho)/dx_3   denP2= # transition pts   denP3=(unused)
<<<<<<< HEAD
 	err1=0
=======
        err1=0
>>>>>>> 5b588383f25931df60a9b92e8cd307edb59365e4
	dens=0.0d0

!  	select case(density_profile)

!   	case('Linear')
	do i=1,nx
	do j=1,ny
!      	zz = zc(k)-DX3c
!     	dens(i,:,:) = dens(i,:,:) + denP1*(xc(i))
        if(idens.eq.1) then
<<<<<<< HEAD
	        dens(i,j,:)   =  dens(i,j,:)+ denP1*(rp(i))*sin(yc(j))
    	    dens_bg(i,j)  =  denP1*(rp(i))*sin(yc(j))
=======
        dens(i,j,:)   =  dens(i,j,:)+ denP1*(rp(i))*sin(yc(j)) 
        dens_bg(i,j)  =  denP1*(rp(i))*sin(yc(j))
>>>>>>> 5b588383f25931df60a9b92e8cd307edb59365e4

c	dens(i,j,:)   =  dens(i,j,:) + denP1*(xc(i)-1.0)
c	dens_bg(i,j)  =  denP1*(xc(i)-1.0)

        else
        dens(i,j,:)=0.0
        dens_bg(i,j)=0.0
        endif
	enddo
	enddo

!   	case('TwoLayer')
!    	do k=sz-1,ez+1
!     	zz = zc(k)-DX3c
!     	rho(:,:,k) = rho(:,:,k) + rho_0 - (denP1*0.5d0)*dtanh(2.d0*zz/denP2)
!     	rho_bg(k)  =              rho_0 - (denP1*0.5d0)*dtanh(2.d0*zz/denP2)
!    	enddo
!
!   case('Jdeep')
!    do k=sz-1,ez+1
!    zz = zc(k)-DX3c
!    rho(:,:,k) = rho(:,:,k)+ rho_0 - 1.d0/g*( (denP1+denP2)/2.d0*zz + &
!                 (denP1-denP2)/2.d0*dlog(dabs(dcosh((zz-denP3))/dcosh(denP3)) ))
!    rho_bg(k)  =           + rho_0 - 1.d0/g*( (denP1+denP2)/2.d0*zz + &
!                 (denP1-denP2)/2.d0*dlog(dabs(dcosh((zz-denP3))/dcosh(denP3)) ))
!    enddo
!
!   case('Pycnocline')
!    do k=ez+1,sz-1,-1
!      zz = zc(k)-DX3c
!      if     (zz .ge. 0.d0) then   ! constant density
!        rho(:,:,k) = rho(:,:,k) + rho_0
!        rho_bg(k)  = rho_0
!      else                         ! constant stretching
!        rho(:,:,k) = rho(:,:,k) + rho_0 + denP1*(zz)
!        rho_bg(k)  = rho_0 + denP1*(zz)
!      endif
!    enddo
!
!   case DEFAULT
!    write(IOUT,'(a)') "ABORTING DENSITY PROFILE "//trim(density_profile)//" NOT IMPLEMENTED"
!    stat=1
!    return
!   end select
<<<<<<< HEAD
!
       if (MYRANK==0) then
	     write(*,'(a,3(1x,f12.6))')
     &  "ADDED DENSITY PROFILE:with parameters 1-1=",denP1
	   endif
=======
! 
         if (MYRANK==0) then
	    write(*,'(a,3(1x,f12.6))') "    ADDED DENSITY PROFILE:with 
     &      parameters 1-1=",denP1
	 endif
>>>>>>> 5b588383f25931df60a9b92e8cd307edb59365e4

	 CALL BOUNDARY_DENS(DENS,XC,YC,NX,NY,NZ)
	 CALL REFRESHBC(DENS,NX*NY,NZ)


	stat=max(0,err1)
	return
	end subroutine add_density


<<<<<<< HEAD
       SUBROUTINE SPONGE_SETUP(NX,NY,NZ,NZG,XC,XU,ZWG,ZCG,stat)

=======
        SUBROUTINE SPONGE_SETUP(NX,NY,NZ,NZG,XC,XU,ZWG,ZCG,stat)
	
>>>>>>> 5b588383f25931df60a9b92e8cd307edb59365e4
        USE SPNGE
        INCLUDE 'common.h'
        INCLUDE 'mpif.h'

        INTEGER stat
        INTEGER NX,NY,NZ,NZG
        REAL ZWG(NZG),ZCG(NZG),XC(NX),XU(NX)
	INTEGER I,J,K
	ALLOCATE(PHIX1(NX,2))
        ALLOCATE(dfxug(NZG),dfxwg(NZG),dfxul(NZ),dfxwl(NZ),dfxu(NX),dfxw(NX))
	ALLOCATE(X1inf(NX,NY,2,5))
<<<<<<< HEAD
        ALLOCATE(dfxpg(NZG),dfxpl(NZ))
	x1spng(1)=40
        x1spng(2)=10
      !        IF(MYRANK.eq.0) then

c	write(*,*) "myrank,IBU,IEU,IX1,IX2 = ", myrank, IBU,IEU,IX1,IX2
c   !! Sponge in the radial direction
=======
	x1spng(1)=10 !sponge thickness in the inlet
        x1spng(2)=10 !sponge thickness in the radial direction

       ! Sponge in the radial direction
>>>>>>> 5b588383f25931df60a9b92e8cd307edb59365e4

       DO I=IBU,IEU
	     dfxu(i)=0.0
	     if (i.GE.IEU-x1spng(2).AND.x1spng(2).GT.0) then
	      dfxu(i)=5.0*((xu(i)-xu(IEU-x1spng(2)))/(xu(IEU)-xu(IEU-x1spng(2))))**2.d0
	     endif
       ENDDO
 
       ! Sponge in the radial direction

       DO I=IX1,IX2
	     dfxw(i)=0.0
	     if (i.GE.IX2-x1spng(2).AND.x1spng(2).GT.0) then
	      dfxw(i)=5.0*((xu(i)-xu(IX2-x1spng(2)))/(xu(IX2)-xu(IX2-x1spng(2))))**2.d0
	     endif
       ENDDO
c    !! Sponge in the radial direction done


c    !! Sponge in the outlet

      dfxpg=0.0
      DO K=NZG/2,NZG,+1
        if (K.GE.NZG-(x1spng(1)+1).AND.x1spng(1).GT.0) then
           dfxpg(K) = 5.0d0*((zcg(k)-zcg(NZG-(x1spng(1)+1)))/
     &     (zcg(NZG-1)-zcg(NZG-(x1spng(1)+1))))**2.d0 
       endif
      ENDDO

      DO K=KZ1,KZ2
        dfxpl(k)=dfxpg((KZ2-1)*(myrank)+K)
      ENDDO

c     !! End Sponge in the outlet


<<<<<<< HEAD
c    !! Sponge in the inlet direction
       DO K=NZG,1,-1
=======
       ! Sponge in the inlet

        DO K=NZG,1,-1
>>>>>>> 5b588383f25931df60a9b92e8cd307edb59365e4
         dfxug(K)=0.0
         if (K.LE.x1spng(1)+1.AND.x1spng(1).GT.0) then
          dfxug(K)=20.0*((zcg(k)-zcg(x1spng(1)+1))/(zcg(2)-zcg(x1spng(1)+1)))**2.d0
          IF(MYRANK.eq.0)write(*,*)"GLOBAL",dfxug(K),K
         endif
<<<<<<< HEAD
       ENDDO
!        CALL MPI_BCAST(dfxug ,1,MTYPE,0,MPI_COMM_EDDY,IERR)
!        ENDIF
=======
        ENDDO
>>>>>>> 5b588383f25931df60a9b92e8cd307edb59365e4

	   write(*,*) "myrank,KZ1,KZ2= ", myrank,KZ1,KZ2

       DO K=KZ1,KZ2
         dfxul(K)=dfxug((KZ2-1)*(myrank)+K)
<<<<<<< HEAD
c$$$          if(myrank.eq.0)write(*,*)dfxul(K),K,myrank
c$$$          if(myrank.eq.1)write(*,*)dfxul(K),K,myrank
c$$$          if(myrank.eq.2)write(*,*)dfxul(K),K,myrank
c$$$          if(myrank.eq.3)write(*,*)dfxul(K),K,myrank
c$$$          if(myrank.eq.4)write(*,*)dfxul(K),K,myrank
       ENDDO

c	 write(*,*) "myrank, mysize = ", myrank, mysize

=======
         ENDDO

      
>>>>>>> 5b588383f25931df60a9b92e8cd307edb59365e4

       ! Sponge in the inlet

       DO K=NZG,1,-1
         dfxwg(K)=0.0
          if (K.LE.x1spng(1)+1.AND.x1spng(1).GT.0) then
           dfxwg(K)=20.0*((zwg(k)-zwg(x1spng(1)+1))/(zwg(2)-zwg(x1spng(1)+1)))**2.d0
           IF(MYRANK.eq.0)write(*,*)"GLOBAL",dfxwg(K),K
         endif
       ENDDO
<<<<<<< HEAD
       
       DO K=KZ1,KZ2

c$$$	    if(myrank==1) then
c$$$	       write(6,*) "((KZ2-1)*(mysize+1)+K) =  ", ((KZ2-1)*(mysize+1)+K)
c$$$	       write(6,*) "KZ2 = ", KZ2
c$$$	       write(6,*) "mysize = ", mysize
c$$$	    endif

c         dfxwl(K)=dfxwg((KZ2-1)*(mysize+1)+K)
=======
         DO K=KZ1,KZ2
>>>>>>> 5b588383f25931df60a9b92e8cd307edb59365e4
         dfxwl(K)=dfxwg((KZ2-1)*(myrank)+K)
         ENDDO

<<<<<<< HEAD
       ENDDO
c      !! Sponge in the outlet domain
       
     

        stat=0
	if (MYRANK==0) write(*,'(a)') "SPONGE SETUP COMPLETED"
=======

	stat=0
        if (MYRANK==0) write(*,'(a)') "SPONGE SETUP COMPLETED"
>>>>>>> 5b588383f25931df60a9b92e8cd307edb59365e4
! 	WRITE(*,*), UO(:,:,:)
       RETURN
       END




<<<<<<< HEAD
!     	SUBROUTINE SPONGE_SETUP(UO,VO,WO,DENS,P,NX,NY,NZ,XC,stat)
!
!	USE SPNGE
!        INCLUDE 'common.h'
!        INCLUDE 'mpif.h'
!
!
!	INTEGER I,J,K
!	INTEGER stat
!	INTEGER IBND,NX,NY,NZ
!	REAL UO(NX,NY,NZ),VO(NX,NY,NZ),WO(NX,NY,NZ),XC(NX),DENS(NX,NY,NZ),P(NX,NY,NZ)
!
!	ALLOCATE(PHIX1(NX,2))
!	ALLOCATE(X1inf(NX,NY,2,5))


!
!	x1spng(1)=10
!	x1spng(2)=10
!	ampl=1.0d0
!	DO IBND=1,6
!	IF(ITYPE(1)==82)THEN
!	UO(ib1,:,:)=0.0d0
!	VO(ib1,:,:)=0.0d0
!	WO(ib1,:,:)=0.0d0
!	DO J=1,NY
!	DO K=1,NZ
!	X1inf(J,K,1,1) = uo(1,J,K)
!        X1inf(J,K,1,2) = vo(1,J,K)
!        X1inf(J,K,1,3) = wo(1,J,K)
!        X1inf(J,K,1,4) = p(1,J,K)
!        X1inf(J,K,1,5) = dens(1,J,K)
!	ENDDO
!	ENDDO
! 	WRITE(*,*), MAXVAL(X1inf(:,:,1,1)),MAXVAL(X1inf(:,:,1,2)),MAXVAL(X1inf(:,:,1,3))
!	ELSEIF(ITYPE(2)==82)THEN
!	UO(ib2,:,:)=1.0d0
!	VO(ib2,:,:)=0.0d0
!	WO(ib2,:,:)=0.0d0
!	DO J=1,NY
!	DO K=1,NZ
!	X1inf(J,K,2,1) = uo(NX,J,K)
!        X1inf(J,K,2,2) = vo(NX,J,K)
!        X1inf(J,K,2,3) = wo(NX,J,K)
!        X1inf(J,K,2,4) = p(NX,J,K)
!        X1inf(J,K,2,5) = dens(NX,J,K)
!	ENDDO
!	ENDDO
! 	WRITE(*,*), MAXVAL(X1inf(:,:,2,1)),MAXVAL(X1inf(:,:,2,2)),MAXVAL(X1inf(:,:,2,3))
!	ENDIF
!	ENDDO
!*************************************************************
!***************Sponge Damping Function Setup*****************
!*************************************************************

 ! top is quadratic sponge, 2nd line is for an exponential sponge

!	DO I=1,NX
!	PHIX1(I,1) = 0.0d0
!	PHIX1(I,2) = 0.0d0
!	IF(I.LE.X1SPNG(1).AND.X1SPNG(1).GT.0) THEN
!	PHIX1(I,1) = ( (XC(I)-XC(X1SPNG(1)+1)) / (XC(2)-XC(X1SPNG(1)+1)) )**2.d0
! 	WRITE(*,*), PHIX1(I,1)
!	ENDIF
!	IF (I.GE.NX-X1SPNG(2).AND.X1SPNG(2).GT.0) then
!    	PHIX1(I,2) = ( (XC(i)-XC(NX-X1SPNG(2))) / (XC(NX-1)-XC(NX-X1SPNG(2))) )**2.d0
! 	WRITE(*,*), PHIX1(I,2)
!	endif
!	ENDDO
!	stat=0
!	if (MYRANK==0) write(*,'(a)') "SPONGE SETUP COMPLETED"
! 	WRITE(*,*), UO(:,:,:)
!       RETURN
!       END

=======
>>>>>>> 5b588383f25931df60a9b92e8cd307edb59365e4

	SUBROUTINE SPONGE(VAR_NEW,VAR_OLD,VAR,NX,NY,NZ,err)

	USE SPNGE
        INCLUDE 'common.h'
        INCLUDE 'mpif.h'


	REAL VAR_OLD(NX,NY,NZ),VAR_NEW(NX,NY,NZ)
	INTEGER IBND,I,J,K,NX,NY,NZ,FACE,err,VAR

!	DO IBND=1,6
! 	IF(ITYPE(IBND)==500.or.ITYPE(IBND)==300.or.ITYPE(IBND)==305.or.ITYPE(IBND)==730) RETURN

!	IF(ITYPE(1)==82) then
!	 do k=2,nz
!    	 do j=2,ny
!      	 do i=2,2+x1spng(1)-1
!       	 VAR_NEW(i,j,k) = VAR_NEW(i,j,k) - 4.0d0*PhiX1(i,1)*(VAR_OLD(i,j,k)-X1inf(j,k,1,var) )
! 	IF(VAR==5)VAR_NEW(i,j,k)=0.0d0
!         enddo
!         enddo
!         enddo
!  	IF(VAR==5)write(*,*),MAXVAL(VAR_NEW(:,:,:))
!	 ELSEIF(ITYPE(2)==82) then
!	 do k=2,nz
!    	 do j=2,ny
!	 do i=nx-1-x1spng(2)+1,nx-1
!	 VAR_NEW(i,j,k) = VAR_NEW(i,j,k) - 4.0d0*PhiX1(i,2)*(VAR_OLD(i,j,k)-X1inf(j,k,2,var) )
! 	 IF(VAR==5)VAR_NEW(i,j,k)=0.0d0
!	 enddo
!         enddo
!         enddo
! 	 IF(VAR==5)write(*,*),MAXVAL(VAR_NEW(:,:,:))
!	 ENDIF
!	 ENDDO


	err=0
	RETURN
	END

	SUBROUTINE add_turbo(uo,vo,wo,nx,ny,nz,xc,err)

	INCLUDE 'common.h'
        INCLUDE 'mpif.h'
	INTEGER I,J,K
	INTEGER err,ok1
	INTEGER NX,NY,NZ
	REAL UO(NX,NY,NZ),VO(NX,NY,NZ),WO(NX,NY,NZ),XC(NX)
	REAL cropP1,cropP2

	      call add_random_noise(uo(:,:,:),nx,ny,nz,1.0)
              call add_random_noise(vo(:,:,:),nx,ny,nz,1.0)
              call add_random_noise(wo(:,:,:),nx,ny,nz,1.0)

	      call crop(uo,vo,wo,nx,ny,nz,xc,cropP1,cropP2,ok1)

	err=0
	RETURN
	END

	SUBROUTINE crop(uo,vo,wo,nx,ny,nz,xc,cropP1,cropP2,ok1)

	INCLUDE 'common.h'
        INCLUDE 'mpif.h'
	INTEGER I,J,K
	INTEGER err,ok1
	INTEGER NX,NY,NZ
	REAL UO(NX,NY,NZ),VO(NX,NY,NZ),WO(NX,NY,NZ),XC(NX)
	REAL cropP1,cropP2,xx,func
	real    wrk1d(nx),uprms(nx),vprms(nx),wprms(nx),umean,vmean,wmean

	cropP1=0.055
	cropP2=0.5
c.... Compute rms of noise
 	  uprms = 0.0
          vprms = 0.0
          wprms = 0.0

! 	 call avgX1X2x3(uo,umean,uprms)
!          call avgX1X2x3(vo,vmean,vprms)
!          call avgX1X2x3(wo,wmean,wprms)

          do i=1,nx
          uprms(i) = uprms(i) + sum(uo(i,2:ny-1,2:nz-1)**2.0)/real(ny-2)/real(nz-2)
          vprms(i) = vprms(i) + sum(vo(i,2:ny-1,2:nz-1)**2.0)/real(ny-2)/real(nz-2)
          wprms(i) = wprms(i) + sum((wo(i,2:ny-1,2:nz-1))**2.0)/real(ny-2)/real(nz-2)
          enddo

          wrk1D = uprms/real(mysize)
          CALL MPI_ALLREDUCE(wrk1D,uprms,nx,mtype,mpi_sum,mpi_comm_eddy,ierr)
          uprms = sqrt(uprms)
          wrk1D = vprms/real(mysize)
          CALL MPI_ALLREDUCE(wrk1D,vprms,nx,mtype,mpi_sum,mpi_comm_eddy,ierr)
          vprms = sqrt(vprms)
          wrk1D = wprms/real(mysize)
          CALL MPI_ALLREDUCE(wrk1D,wprms,nx,mtype,mpi_sum,mpi_comm_eddy,ierr)
          wprms = sqrt(wprms)

	  do i=1,nx
          xx = xc(i)
          func = (1.d0+(xx/cropP2)**2)  * ( dexp(-(xx)**2/(2.d0*cropP2**2) ) )
          uo(i,:,:)   = func*(cropP1/uprms(i))*(uo(i,:,:))
	  func = (1.d0+(xx/cropP2)**2)  * ( dexp(-(xx)**2/(2.d0*cropP2**2) ) )
	  vo(i,:,:)   = func*(cropP1/vprms(i))*(vo(i,:,:))
	  func = (1.d0+(xx/cropP2)**2)  * ( dexp(-(xx)**2/(2.d0*cropP2**2) ) )
          wo(i,:,:)   = func*(cropP1/wprms(i))*(wo(i,:,:))
          enddo

	ok1=0
	RETURN
	END

! 	subroutine avgX1X2X3(Var,outm,outp)
! 	INCLUDE 'common.h'
!         INCLUDE 'mpif.h'
! 	INTEGER I,J,K
! 	INTEGER NX,NY,NZ
! 	REAL VAR(NX,NY,NZ)
! 	real outm,outp,Volume,rsum,temp
! 	integer  :: size3dpoint
! 	integer  :: ierr
! 	Volume = 0.d0
!  	rsum=0.0d0
!  	do k=sz-1,ez+1
!   	do j=sy-1,ey+1
!    	do i=sx-1,ex+1
!     	rsum = rsum + Var(i,j,k)*ap(i)*bp(j)*cp(k)
!     	Volume = Volume + ap(i)*bp(j)*cp(k)
!    	enddo
!   	enddo
!  	enddo
!
! 	outm = rsum/Volume
! 	size3dpoint = 1
!  	CALL MPI_ALLREDUCE(outm,temp,size3dPoint,realtype,MPI_SUM,commX1X2X3,ierr)
!  	outm = temp/dble(sizeX1X2X3)
!
! 	rsum=0.0d0
!  	do k=sz-1,ez+1
!   	do j=sy-1,ey+1
!    	do i=sx-1,ex+1
!     	rsum = rsum + ( (Var(i,j,k) - outm)**2)*ap(i)*bp(j)*cp(k)
!    	enddo
!   	enddo
!  	enddo
!
! 	CALL MPI_ALLREDUCE(outp,temp,size3dPoint,realtype,MPI_SUM,commX1X2X3,ierr)
!         outp = dsqrt(temp/dble(sizeX1X2X3))
!
!         outp  = rsum/Volume
! 	return
!         end subroutine avgX1X2X3
