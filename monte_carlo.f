   
      subroutine selectSubset_v1 ( iPer0, iLevel, n1, nMonte, nPer, sa, SaV, UHS, UHSv, cms, cmsV, 
     1      sigma, sigmaV, rho, rhoV, iset_save, MAXLEVEL, MAXPER, period, iSelect, nSelect, nTH,
     2      HVFlag )

      implicit none
      integer MAXLEVEL, MAXPER, nTH, nPer, iLevel, N1(1), nMonte
      integer iSelect(MAXLEVEL,1), nSelect(1), iset_save(MAXLEVEL,1)
      integer iPer0, HVFlag
      real cms(MAXPER,1), cmsV(MAXPER,1), sigma(MAXPER,1), sigmaV(MAXPER,1)
      real sa(MAXPER,1), saV(MAXPER,1), UHS(MAXPER,1), UHSv(MAXPER,1), period(1),
     1     rho(MAXPER,1), rhov(MAXPER,1)

c     Local variables
      real tol, sigMax, scale, sa1(MAXPER,1000), t1, x, sa1_v(MAXPER,1000), x_v, t1_v
      integer iPer1, iPer2, i, k, iper, iMOnte, nn, j, i1
      integer TH_index(1000), iSet(100), iseed, i2
      real bestLike, bestlike_v
      real ran1, scale_v
      real*8 sum, sum_v
      real UHS_Interp, UHS_Interp_v, sigma_CMS(MAXPER), sigmaV_CMS(MAXPER)
      
      iPer1 = iPer0-3   !17
      iPer2 = iPer0+3   !23     
      
      if ( n1(iLevel) > nSelect(iLevel) )  then 
        n1(iLevel) = nSelect(iLevel)    
      endif  
      
c     Calculate sigma around the CMS    
      do iPer = 1, nPer
          sigma_cms(iPer)  = sqrt( 1 - rho(iPer,iLevel)**2) * Sigma(iPer,iLevel)
          if ( sigma_cms(iPer) .lt. 0.01 )  sigma_cms(iPer) = 0.01
          if ( HVFlag == 0 ) then
            sigmaV_cms(iPer) = sqrt( 1 - rhoV(iPer,iLevel)**2) * SigmaV(iPer,iLevel)          
            if ( sigmaV_cms(iPer) .lt. 0.01 ) sigmaV_cms(iPer) = 0.01
          endif
      enddo      

c     Scale TH to the UHS at T0
      do i=1,nTH
        scale = UHS(iPer0,iLevel) / sa(iPer0,i)
        if ( HVFlag == 0 )  scale_v = UHSv(iPer0,iLevel) / saV(iPer0,i)
        do iPer=1,nPer
          sa1(iPer,i) = sa(iPer,i) * scale
          if ( HVFlag == 0 )    sa1_v(iPer,i) = saV(iPer,i) * scale_v
        enddo
      enddo

c     Inititalize best likelihood
      bestLike = -1.0E30
      bestLike_v = -1.0E30
      iSeed = 3771

c     Loop over realizations
      do iMonte=1,nMonte

c       Set indexes
        nn = nSelect(iLevel)
        do j=1,nn
          TH_index(j) = iSelect(iLevel,j)
        enddo

c       Select random set of N1 (without replacement)
        do i=1,N1(iLevel)
          x = ran1( iseed )
          i1 = int(x*nn) + 1
          if ( i1 .gt. nn ) i1 = nn
          iSet(i) = TH_index(i1)
          do j=i1+1,nn
            TH_index(j-1) = TH_index(j)
          enddo
          nn = nn -1
        enddo

c       Find the likelihood for this realization
        sum = 0.
        sum_v = 0.
        do i=1,N1(iLevel)
          do iper=1,nPer
            if ( iPer .le. iPer1 .or. iPer .ge. iPer2 ) then
              i2 =iSet(i)
              !! Convert to std normal variable
              x = alog( sa1(iPer,i2) / cms(iPer,iLevel) ) / sigma_cms(iPer)
              if ( HVFlag == 0 )    x_v = alog( sa1_V(iPer,i2) / cmsV(iPer, iLevel) ) / sigmaV_cms(iPer)                
c              write (*,'( 4i5,3f10.4)') i, iPer, iSet(i), i2, sa1(iPer,i2)
c              pause
              t1 = - alog(sigma_cms(iPer)) - (1/2.)*(x**2)   !! CHECK
              if ( HVFlag == 0 )    t1_v = - alog(sigmaV_cms(iPer)) - (1/2.)*(x_v**2) 
              sum = sum + t1
              if ( HVFlag == 0 )    sum_v = sum_v + t1_v          
            endif
          enddo
        enddo

c       Check if this is a better set
        if ( HVflag == 0 ) then
            if ( sum .gt. bestLike .and. sum_v .gt. bestlike_v ) then
              bestLike = sum
              bestLike_v = sum_v
              do i=1,N1(iLevel)
                iSet_save(iLevel,i) = iSet(i)
              enddo
       !       write (*,'( 2i5,e12.5,10i5)') ilevel, n1(iLevel), bestLike, (iSet_save(iLevel,i),i=1,n1(iLevel))
            endif
        else
            if ( sum .gt. bestLike ) then
              bestLike = sum
              bestLike_v = -999
              do i=1,N1(iLevel)
                iSet_save(iLevel,i) = iSet(i)
              enddo
       !       write (*,'( 2i5,e12.5,10i5)') ilevel, n1(iLevel), bestLike, (iSet_save(iLevel,i),i=1,n1(iLevel))
            endif        
        endif        
      enddo 
         
       
      return
      end