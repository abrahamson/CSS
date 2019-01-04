      subroutine Select_TH ( iPer0, nTH, nPer, sa, SaV, UHS, UHSv, cms, cmsV, sigma, sigmaV, 
     1      rho, rhoV, iSelect, nSelect, iLevel, MAXLEVEL, MAXPER, HVFlag, PGV, PGVmin, PGVmax )  
                  
      implicit none
      integer MAXLEVEL, MAXPER, nTH, nPer, iLevel
      integer iSelect(MAXLEVEL,1), nSelect(1), iPer0, HVFlag
      real cms(MAXPER,1), cmsV(MAXPER,1), sigma(MAXPER,1), sigmaV(MAXPER,1), 
     1      rho(MAXPER,1), rhoV(MAXPER,1)
      real sa(MAXPER,1), UHS(MAXPER,1), UHSv(MAXPER,1), saV(MAXPER,1), PGV(1), PGVmin, PGVmax 

c     Local variables
      real tol, sigMax, scale, sa1(MAXPER), t1, x, scale_v, sa1_V(MAXPER), t1_v, x_v
      real sigma_CMS(MAXPER), sigmaV_CMS(MAXPER)
      integer i, k, iper, iPass
      real UHS_Interp, UHS_Interp_v, PGVh_1

c     Set tolarance for checking CS
      tol = 0.15
      sigMax = 2.5  
      
c     Calculate sigma around the CMS     
      do iPer = 1, nPer
          sigma_cms(iPer)  = sqrt( 1 - rho(iPer,iLevel)**2) * Sigma(iPer,iLevel)
          if ( sigma_cms(iPer) .lt. 0.01 )  sigma_cms(iPer) = 0.01
          if ( HVFlag == 0 ) then
            sigmaV_cms(iPer) = sqrt( 1 - rhoV(iPer,iLevel)**2) * SigmaV(iPer,iLevel)          
            if ( sigmaV_cms(iPer) .lt. 0.01 ) sigmaV_cms(iPer) = 0.01
          endif
      enddo
      

c     Loop over all TH
      k = 0
      do i=1,nTH

c       Scale TH to the UHS at T0
        scale = UHS(iPer0,iLevel) / sa(iPer0,i)
        if ( HVFlag == 0 )     scale_v = UHSv(iPer0,iLevel) / saV(iPer0,i)     
        
c       Check if scaled PGV_H falls within input min and max PGV values            
        PGVh_1 = PGV(i) * scale     
        if ( PGVh_1 < PGVmin .or. PGVh_1 > PGVmax ) goto 5      

        do iPer=1,nPer
          sa1(iPer) = sa(iPer,i) * scale
          if ( HVFlag == 0 )    sa1_v(iPer) = saV(iPer,i) * scale_v
        enddo
        
        

c       Check if the scaled spectrum fit the tolarance
        iPass = 1
        do iPer=1,nPer              
            t1 = alog( sa1(iPer) / cms(iPer,iLevel) )    ! calculate residual
            x =  t1 / sigma_cms(iPer)  ! calculate epsilon = nb of std dev
            if ( HVFlag == 0 ) then
                t1_v = alog( sa1_v(iPer) / cmsV(iPer,iLevel) )    ! calculate residual
                x_v =  t1_v / sigmaV_cms(iPer)  ! calculate epsilon = nb of std dev
            endif
            if (abs(x) .gt. sigMax .and. abs(t1) .gt. tol )  iPass = 0
            if ( HVFlag == 0 ) then          
                if ( abs(x_v) .gt. sigMax .and. abs(t1_v) .gt. tol  ) iPass =0
            endif
               
        enddo

c       Save if this passed tolarance
        if ( iPass .eq. 1 ) then
           k = k + 1
           iSelect(iLevel,k) = i
!           write (30+iLevel,'( 2i5,100f10.5)') iLevel, i, (Sa1(iPer),iPer=1,nPEr)
        endif
5       continue
      enddo
      nSelect(iLevel) = k
      

      return
      end

