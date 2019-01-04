      subroutine Interp_loglog ( nLevel, nPer_UHS, UHS, UHS_all, MAXPER, period_UHS, 
     1      period, nPer, period_all, nPer_all)

      real UHS(MAXPER,1), period_UHS(1), period(1), UHS_interp(MAXPER)
      real UHS_all(MAXPER,1), period_all(1)
      integer nPer, nPer_all

c     Loop over each level
      do iLevel=1,nLevel

c       Loop over each period
        do iPer=1,nPer_all

c         Find the first period in the UHS that is larger than the desired period
          do jPer=1,nPer_UHS
            if ( period_UHS(jPer) .ge. period_all(iPer) ) then
              j2 = jPer
              if ( j2 == 1) then
                j1 = 1
              else
                j1 = jPer-1
              endif
              goto 10
            endif
          enddo          

10        continue

c         Interpolate (log-log)
          if ( j1 == j2 ) then
              uhs_interp(iPer) = UHS(j1,iLevel)
          else
              slope = alog( UHS(j2,iLevel) / UHS(j1,iLevel) ) / alog( period_UHS(j2) / period_UHS(j1) )
              uhs_interp_log = alog(UHS(j1,iLevel)) + slope * alog( period_all(iPer) / period_UHS(j1) )        
              uhs_interp(iPer) = exp ( uhs_interp_log )
          endif
        enddo

c       Copy back to original array
        do iPer=1,nPer_all
          uhs_all(iPer,iLevel) = uhs_interp(iPer)
        enddo
        
c       Select the period of interest for matching UHS
        k = 0
        do j=1,nPer_all
            if ( period_all(j) .ge. period(1) .and. period_all(j) .le. period(nPer) ) then
              if ( k .eq. 0) iPer1 = j
              k = k + 1
              iPer2 = j
            endif
        enddo 
        
c       Save UHS in the desired range
        k = 0
        do j=iPer1,iPer2
          k = k + 1
          UHS(k,iLevel) = uhs_all(j,iLevel)
        enddo              

      enddo

      return
      end     
 
c -----------------------------------------------------------------------------
      subroutine Interp_loglin ( nLevel, nPer_UHS, rho, rho_all, MAXPER, period_UHS, 
     1   period, nPer, period_all, nPer_all) 

      implicit none
      integer nLevel, nPer_UHS, MAXPER, nPer, nPer_all
      real period(1), period_all(1), period_UHS(1), rho(MAXPER,1), rho_all(MAXPER,1)
      real rho_interp(MAXPER), slope
      integer iPer1, iPer2, iPer, j1, j2, j, k, jPer, ilevel

c     Loop over each level
      do iLevel=1,nLevel

c       Loop over each period
        do iPer=1,nPer_all

c           Find the first period in the UHS that is larger than the desired period
            do jPer=1,nPer_UHS
              if ( period_UHS(jPer) .ge. period_all(iPer) ) then
                j2 = jPer
                  if ( j2 == 1) then
                    j1 = 1
                  else
                    j1 = jPer-1
                  endif
                goto 10
              endif
            enddo          

10          continue

c           Interpolate (log-lin)
            if ( j1 == j2 ) then
                rho_interp(iPer) = rho(j1,iLevel)
            else
                slope = ( rho(j2,iLevel) - rho(j1,iLevel) ) / alog( period_UHS(j2) / period_UHS(j1) )
                rho_interp(iPer) = rho(j1,iLevel) + slope * alog( period_all(iPer) / period_UHS(j1) )    
            endif    
        enddo
      
c       Copy back to original array
        do iPer=1,nPer_all
          rho_all(iPer,iLevel) = rho_interp(iPer)
        enddo      
      
c       Select the period of interest for matching UHS
        k = 0
        do j=1,nPer_all
            if ( period_all(j) .ge. period(1) .and. period_all(j) .le. period(nPer) ) then
              if ( k .eq. 0) iPer1 = j
              k = k + 1
              iPer2 = j
            endif
        enddo 
        
c       Save UHS in the desired range
        k = 0
        do j=iPer1,iPer2
          k = k + 1
          rho(k,iLevel) = rho_all(j,iLevel)
        enddo
        
      enddo   

      return
      end     
      
c -----------------------------------------------------------------------------
      subroutine Interp_loglin_rho ( nLevel, nPer_UHS, rho, rho_all, MAXPER, period_UHS, 
     1   period, nPer, period_all, nPer_all, Per0) 

      implicit none
      integer nLevel, nPer_UHS, MAXPER, nPer, nPer_all
      real period(1), period_all(1), period_UHS(1), rho(MAXPER,1), rho_all(MAXPER,1)
      real rho_interp(MAXPER), slope, Per0
      integer iPer1, iPer2, iPer, j1, j2, j, k, jPer, ilevel
      real rho_temp(MAXPER,100), period_UHS_temp(MAXPER)
      integer flag, loc, nPer_UHS_temp
      
      ! add the rho =1 at the conditioning period if not part of the UHS period vector
      do jPer=1,nPer_UHS
        if (period_UHS(jPer) <= Per0) then
            loc = jPer
        endif
      enddo

      if ( period_UHS(loc) == Per0 ) then
        nPer_UHS_temp = nPer_UHS
        do iLevel=1,nLevel
            do jPer=1,nPer_UHS_temp
                period_UHS_temp(jPer) = period_UHS(jPer)
                rho_temp(jPer,iLevel) = rho(jPer,iLevel)
            enddo
        enddo
      else
        nPer_UHS_temp = nPer_UHS+1
        do iLevel=1,nLevel
            flag = 0
            do jPer=1,nPer_UHS_temp
                if ( jPer <= loc ) then
                    period_UHS_temp(jPer) = period_UHS(jPer)
                    rho_temp(jPer,iLevel) = rho(jPer,iLevel) 
                else
                    if ( flag == 0 )  then
                        period_UHS_temp(jPer) = Per0
                        rho_temp(jPer,iLevel) = 1
                        flag = 1 
                    else
                        period_UHS_temp(jPer) = period_UHS(jPer-1)
                        rho_temp(jPer,iLevel) = rho(jPer-1,iLevel)                           
                    endif
                endif 
            enddo
        enddo   
      endif
      
      
        

c     Loop over each level
      do iLevel=1,nLevel

c       Loop over each period
        do iPer=1,nPer_all

c           Find the first period in the UHS that is larger than the desired period
            do jPer=1,nPer_UHS_temp
              if ( period_UHS_temp(jPer) .ge. period_all(iPer) ) then
                j2 = jPer
                  if ( j2 == 1) then
                    j1 = 1
                  else
                    j1 = jPer-1
                  endif
                goto 10
              endif
            enddo          

10          continue

c           Interpolate (log-lin)
            if ( j1 == j2 ) then
                rho_interp(iPer) = rho_temp(j1,iLevel)
            else
                slope = ( rho_temp(j2,iLevel) - rho_temp(j1,iLevel) ) / alog( period_UHS_temp(j2) / period_UHS_temp(j1) )
                rho_interp(iPer) = rho_temp(j1,iLevel) + slope * alog( period_all(iPer) / period_UHS_temp(j1) )    
            endif    
        enddo
      
c       Copy back to original array
        do iPer=1,nPer_all
          rho_all(iPer,iLevel) = rho_interp(iPer)
        enddo      
      
c       Select the period of interest for matching UHS
        k = 0
        do j=1,nPer_all
            if ( period_all(j) .ge. period(1) .and. period_all(j) .le. period(nPer) ) then
              if ( k .eq. 0) iPer1 = j
              k = k + 1
              iPer2 = j
            endif
        enddo 
        
c       Save UHS in the desired range
        k = 0
        do j=iPer1,iPer2
          k = k + 1
          rho(k,iLevel) = rho_all(j,iLevel)
        enddo
        
      enddo   

      return
      end           
 
c -----------------------------------------------------------------------------  
       subroutine interp (x1,x2,y1,y2,x,y,iflag)

C This subroutine will perform the Log-linear interpolation
C of the given input values. This routine is used to interpolate
C the regression cofficients of the attenuation models for 
C spectral periods other than those defined in the model.

       real x1, x2, y1, y2, x, y
       integer iflag

C Check to see if the interpolation period is at an end point.
C Return the 'iflag' for output purposes with 
C             iflag = 0  No interpolation
C                   = 1  Interpolation need.

       if (x .eq. x1 .or. x .eq. x2) then
          iflag = 0
       else
          iflag = 1
       endif

C Set the PGA period to 100 Hz (i.e., 0.01 Sec).
       if (x1 .eq. 0.0) then
           x1 = 0.01
       endif

C Take the Log of the Period values.
       x1 = alog(x1)
       x2 = alog(x2)
       x  = alog(x)
C Perform the log-linear interpolation.
       y = y1 + (y2-y1)*((x-x1)/(x2-x1))       

C Convert the Log Periods back to period.
       x1 = exp(x1)
       x2 = exp(x2)
       x  = exp(x)

       return
       end