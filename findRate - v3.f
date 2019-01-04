 
      subroutine Findrate ( n1, nPer, period_all, sa_all, uhs_all, iset_save, MAXLEVEL, MAXPER, rate, nLEvel, 
     1  hazLevel, index_final, n3, Scale_final, level, nfit, delta, nPer_all, iPer1, iPer2,
     1  iPer0, SaV_all, UHSv_all, scale_final_v, rate0, maxAdj, 
     1  iPer1_strict, iPer2_strict, HVFlag, haz_outfile, hazV_outfile  )   

      implicit none
      integer MAXLEVEL, MAXPER, nTH, nPer, iLevel, N1(1), iPer0, iPer1, iPer2
      integer iset_save(MAXLEVEL,1), nLevel, index_final(1), n3, level(1), nPer_all
      real sa_all(MAXPER,1), UHS_all(MAXPER,1), period_all(1), hazLevel(1), rate(1), rate0(1)
      integer iPer1_strict, iPer2_strict, HVFlag
      real UHSv_all(MAXPER,1)
      real Scale_final(1), Scale_final_v(1)
      real SaV_all(MAXPER,1), maxAdj
      character*48 haz_outfile, hazV_outfile

c     Local variables
      real scale, t1, x 
      integer i, k, iper, iMOnte, nn, j, i1, j1, j2, jLevel, iZ
      integer iseed, i2, nFit, iFit, nTot, nLevel_TH
      real bestLike, delta
      real ran1
      real*8 sum
      real*8 misfit, misfit1, misfit2, misfit_v, misfit1_v, misfit2_v
      real*8 avemisfit_level, maxmisfit_level, avemisfit_v_level, maxmisfit_v_level
      real UHS_Interp, scale_v, UHS_Interp_v
      integer dim, iLevel_targ, TH_count, m, index_temp, vec(5000), vec_unique(5000)
      integer ntot_unique, flag
      real HazX, temp0, temp1, temp2, deltaHaz, rate_temp
      real, Allocatable ::  haz1(:,:), haz2(:,:), haz1_v(:,:), 
     1      haz2_v(:,:), haz0(:,:), haz0_v(:,:), haz_v(:,:), haz(:,:),
     2      sa1(:,:), sa1_V(:,:), sa1_final(:,:), sa1V_final(:,:)
      
      dim = n1(1) * (nlevel-1)**2
      
      ! Allocate ( rate0(dim) )
      Allocate ( haz1(nPer_all,nlevel-1), haz2(nPer_all,nlevel-1), haz1_v(nPer_all,nlevel-1), 
     1      haz2_v(nPer_all,nlevel-1), haz0(nPer_all,nlevel), haz0_v(nPer_all,nlevel), 
     2      haz_v(nPer_all,nlevel), haz(nPer_all,nlevel) )
      Allocate ( Sa1(nPer_all,dim), Sa1_V(nPer_all,dim), Sa1_final(nPer_all,dim), Sa1V_final(nPer_all,dim) )
      
      
c     Total number of TH to use per hazard level      
        nLevel_TH = nLevel-1
        j1 = 1
        j2 = nlevel - 1

        nTot = 0
        k = 0
        do jLevel=j1,j2 
            nTot = nTot + n1(jLevel)
            do i=1,n1(jlevel)
                k = k + 1
                vec(k) = iSet_save(jLevel,i)
            enddo
        enddo  
        
        ! find the number of unique TH per hazard level
        k = 1
        do i = 2, nTot
            flag = 0
            do j = 1, i-1
                if ( vec(i) == vec(j) ) then
                    flag = 1
                    goto 11
                endif
11              continue                
            enddo
            if ( flag == 0 ) k = k + 1        
        enddo 
        nTot_unique = k
        
        
         
      k = 0
      do iLevel=1,nLevel-1    

        TH_count = 0
        do jLevel=j1,j2     
 
c         Scale TH to the UHS at T0 (but add range to avoid pinching)
!          TH_count = 0
          do i=1,n1(jlevel)

c   Old method          
            !! Scaling to halfway between 2 hazard levels - log Interpolation           
            !! LAA
!            UHS_Interp =  EXP ( ( alog( UHS_all(iPer0+iPer1-1,iLevel) ) + alog( UHS_all(iPer0+iPer1-1,iLevel+1) ) ) / 2 )    !!LAA
!            UHS_Interp_v =  EXP ( ( alog( UHSvert_all(iPer0+iPer1-1,iLevel) ) + alog( UHSvert_all(iPer0+iPer1-1,iLevel+1) ) ) / 2 )    !!LAA

c New method - no pinching NAA 
             index_temp = iset_save(jlevel,i)  
             rate_temp =  (hazLevel(iLevel) - hazLevel(iLevel+1)) /nTot 
             if ( k .ne. 0  ) then
                do m = 1, k 
                    if ( index_final(m) == index_temp .and. level(m) == iLevel ) then
                        rate(m) = rate(m) + rate_temp
                        rate0(m) = rate0(m) + rate_temp
                        goto 101
                    endif
                enddo
            endif  
            TH_count =TH_count + 1
            temp0 = alog(UHS_all(iPer0+iPer1-1,iLevel+1) ) - alog(UHS_all(iPer0+iPer1-1,iLevel) )
             
!             deltaHaz = ( hazlevel(iLevel) - hazlevel(iLevel+1) ) / (nTot+1)        !! why (nTot+1) and not nTot
!             HazX = hazlevel(iLevel) - deltaHaz * (TH_count+0.5)
            deltaHaz = ( alog(hazlevel(iLevel)) - alog(hazlevel(iLevel+1)) ) / (nTot_unique+1)        !! why (nTot+1) and not nTot
            HazX = exp ( alog(hazlevel(iLevel)) - deltaHaz * (TH_count+1) )
             
            temp2 = (alog(HazX) - alog( hazLevel(iLevel) ))
     1                  / ( alog( hazLevel(iLevel+1)) - alog(hazlevel(iLevel)) )
     
            UHS_interp = exp(alog( UHS_all(iPer0+iPer1-1,iLevel) ) + temp2*temp0 )
            scale = UHS_Interp / sa_all(iPer0+iPer1-1,iSet_save(jLevel,i))   !! LAA
                         
            if ( HVFlag == 0 ) then
                temp1 = alog(UHSv_all(iPer0+iPer1-1,iLevel+1) ) - alog(UHSv_all(iPer0+iPer1-1,iLevel) )             
                UHS_interp_v = exp(alog( UHSv_all(iPer0+iPer1-1,iLevel) ) + temp2*temp1 )
                scale_v = UHS_Interp_v / SaV_all(iPer0+iPer1-1,iSet_save(jLevel,i))   !! LAA  
             endif
             
            k = k + 1
             do iPer=1,nPer_all
                Sa1(iPer,k) = (sa_all(iPer,iSet_save(jLevel,i)) * scale)
                if ( HVFlag == 0 )    Sa1_V(iPer,k) = (saV_all(iPer,iSet_save(jLevel,i)) * scale_v)
            enddo                
            index_final(k) = index_temp
            scale_final(k) = scale
            scale_final_v(k) = scale_v
            rate(k) = rate_temp 
            rate0(k) = rate_temp 
            level(k) = iLevel                         
            
101         continue                                       
          enddo
        enddo
      enddo
      n3 = k  
!      write(11,*)
!      write(11,*) 'Total number of TH per hazard level: ', nTot
!      write(11,*) 'Number of unique TH per hazard level: ', nTot_unique
!      write(11,*) 'Total number of TH (all levels): ', n3
      write(11,*)
!      write(*,*) ntot, n3
!      pause 'ok_n3'   

c     Compute the hazard from the intial set of rate for horiz 
      call CalcHaz ( nPer_all, nLevel-1, n3, sa1, uhs_all, rate, haz, MAXPER, MAXLEVEL )
      call CalcHaz ( nPer_all, nLevel, n3, sa1, uhs_all, rate, haz0, MAXPER, MAXLEVEL )
      
c     Compute the hazard from the intial set of rate for vert
      if ( HVFlag == 0 ) then
          call CalcHaz ( nPer_all, nLevel-1, n3, sa1_v, uhsv_all, rate, haz_v, MAXPER, MAXLEVEL )
          call CalcHaz ( nPer_all, nLevel, n3, sa1_v, uhsv_all, rate, haz0_v, MAXPER, MAXLEVEL )  
      endif   

c     Compute horiz and vertical misfits
      call calcMisfit ( nPer_all, iPer1, iPer2, iPer1_strict, iPer2_strict, nLevel-1, haz, hazLevel, misfit, 
     1  iLevel_targ, avemisfit_level, maxmisfit_level, MAXPER )
      if ( HVFlag == 0 ) then
        call calcMisfit ( nPer_all, iPer1, iPer2, iPer1_strict, iPer2_strict, nLevel-1, haz_v, hazLevel, misfit_v, 
     1      iLevel_targ, avemisfit_v_level, maxmisfit_v_level, MAXPER )
      endif

c     Adjust rate to better fit hazard (small changes in each iteration)
!      open (1, file = 'Output_misfit.out', status = 'replace' )
!      write(1,'( 2A10, 3A20)') 'iFit', 'iTH', 'Misfit'
      
13    continue      
      
      do iFit=1,nFit
        write (*,'( i5, 2x,''iteration on rate'')') iFit      

c       Loop over each scaled TH
        do i=1,n3 

c         Compute horiz and vert hazard if rate is changed (x%) for this TH
          do iPer= 1, nPer_all 
            do iLevel=1,nLevel-1
              if ( sa1(iper,i) .ge. uhs_all(iPer,iLevel)*0.99 .and. rate(i) .ne. 0. ) then        
                haz1(iPer,iLevel) = haz(iPer,iLevel) + rate(i) * delta
                haz2(iPer,iLevel) = haz(iPer,iLevel) - rate(i) * delta   
              else
                haz1(iPer,iLevel) = haz(iPer,iLevel) 
                haz2(iPer,iLevel) = haz(iPer,iLevel)
              endif
              
              if ( HVFlag == 0 ) then
                  if ( sa1_v(iper,i) .ge. uhsv_all(iPer,iLevel)*0.99 .and. rate(i) .ne. 0. ) then        
                    haz1_v(iPer,iLevel) = haz_v(iPer,iLevel) + rate(i) * delta
                    haz2_v(iPer,iLevel) = haz_v(iPer,iLevel) - rate(i) * delta   
                  else
                    haz1_v(iPer,iLevel) = haz_v(iPer,iLevel) 
                    haz2_v(iPer,iLevel) = haz_v(iPer,iLevel)
                  endif  
              endif            
            enddo
          enddo

c         Compute horiz and vertical misfit
          call calcMisfit ( nPer_all, iPer1, iPer2, iPer1_strict, iPer2_strict, nLevel-1, haz1, hazLevel, misfit1, 
     1          iLevel_targ, avemisfit_level, maxmisfit_level, MAXPER )
          call calcMisfit ( nPer_all, iPer1, iPer2, iPer1_strict, iPer2_strict, nLevel-1, haz2, hazLevel, misfit2, 
     1          iLevel_targ, avemisfit_level, maxmisfit_level, MAXPER )
          if ( HVFlag == 0 ) then
              call calcMisfit ( nPer_all, iPer1, iPer2, iPer1_strict, iPer2_strict, nLevel-1, haz1_v, hazLevel, misfit1_v, 
     1              iLevel_targ, avemisfit_v_level, maxmisfit_v_level, MAXPER )
              call calcMisfit ( nPer_all, iPer1, iPer2, iPer1_strict, iPer2_strict, nLevel-1, haz2_v, hazLevel, misfit2_v, 
     1              iLevel_targ, avemisfit_v_level, maxmisfit_v_level, MAXPER )  
           else
               misfit_v = -999
           endif        
          write (*,'( 2i7, 3f15.5)') iFit, i, misfit, misfit_v
         ! pause 'ok'
!          write (1,'( 2i10, 3f20.10)') iFit, i, misfit, misfit_v   !, misfit1, misfit2

          
c         if the new misfit is smaller than the previous one, change the rate of occurence of iTH  
          if ( HVFlag == 0 ) then          
              if ( misfit1 .le. misfit .and. misfit1_v .lt. misfit_v ) then
                misfit = misfit1
                misfit_v = misfit1_v
                rate(i) = rate(i) * (1.+delta)  
                do iPer=1,nPer_all          
                    do iLevel=1,nLevel-1 
                        haz(iPer,iLevel) = haz1(iPer,iLevel)   
                        haz_v(iPer,iLevel) = haz1_v(iPer,iLevel)  
                    enddo           
                enddo                              
              elseif ( misfit2 .le. misfit .and. misfit2_v .lt. misfit_v ) then
                 misfit = misfit2
                 misfit_v = misfit2_v
           !     rate(i) = rate(i) / (1. + delta)
                rate(i) = rate(i) * (1.-delta)      
                do iPer=1,nPer_all         
                    do iLevel=1,nLevel-1  
                        haz(iPer,iLevel) = haz2(iPer,iLevel)     
                        haz_v(iPer,iLevel) = haz2_v(iPer,iLevel) 
                    enddo      
                enddo                 
              endif
          else
              if ( misfit1 .le. misfit  ) then
                misfit = misfit1
                misfit_v = misfit1_v
                rate(i) = rate(i) * (1.+delta)  
                do iPer=1,nPer_all          
                    do iLevel=1,nLevel-1 
                        haz(iPer,iLevel) = haz1(iPer,iLevel)   
                        haz_v(iPer,iLevel) = haz1_v(iPer,iLevel)  
                    enddo           
                enddo                              
              elseif ( misfit2 .le. misfit  ) then
                 misfit = misfit2
                 misfit_v = misfit2_v
           !     rate(i) = rate(i) / (1. + delta)
                rate(i) = rate(i) * (1.-delta)      
                do iPer=1,nPer_all         
                    do iLevel=1,nLevel-1  
                        haz(iPer,iLevel) = haz2(iPer,iLevel)     
                        haz_v(iPer,iLevel) = haz2_v(iPer,iLevel) 
                    enddo      
                enddo                 
              endif   
           endif                      
          
          if ( rate(i) < 0 ) then
            write(*,*) 'Negative rate. Check delta value!'
            stop
          endif
                
        enddo       !! end of iterations over time series
        
        k = 0
        if ( iFit == nFit ) then
            do i=1,n3
                if ( rate0(i)/rate(i) > maxAdj .and. rate(i) .ne. 0. ) then
                    k = k + 1
                    rate(i) = 0.
                endif
            enddo
            call CalcHaz ( nPer_all, nLevel-1, n3, sa1, uhs_all, rate, haz, MAXPER, MAXLEVEL )
            call calcMisfit ( nPer_all, iPer1, iPer2, iPer1_strict, iPer2_strict, nLevel-1, haz, hazLevel, misfit, 
     1          iLevel_targ, avemisfit_level, maxmisfit_level, MAXPER )
            if ( HVFlag == 0 ) then
                call CalcHaz ( nPer_all, nLevel-1, n3, sa1_v, uhsv_all, rate, haz_v, MAXPER, MAXLEVEL )   
                call calcMisfit ( nPer_all, iPer1, iPer2, iPer1_strict, iPer2_strict, nLevel-1, haz_v, hazLevel, misfit_v, 
     1              iLevel_targ, avemisfit_v_level, maxmisfit_v_level, MAXPER ) 
            endif     
            write(*,*)
            write(*,*) k 
            write(11,*) 'Scenarios removed (RateInitial/RateFinal > x): ', k
!            pause 'scenarios removed'  
            write(*,*)     
            if ( k >= 1) then
                nFit = Int( nFit/3 )
               goto 13
            endif
        endif
        
      enddo         !! end of iterations over the nfits
!      close (1)
         
c     Consolidate the duplicate spectra at the same hazard level
c     with same scaling factors but having different rates
      k = 0
!      index_final(k) = index_final(1)
!      scale_final(k) = scale_final(1)
!      scale_final_v(k) = scale_final_v(1)
!      rate(k) = rate(1)  
!      rate0(k) = rate0(1) 
!      level(k) = level(1)

!      do i = 1, n3 
!        write (51,'( 2i5, 4e15.5)')  i, index_final(i), 
!     1      scale_final(i), scale_final_v(i), rate(i), rate0(i) 
!      enddo

      do i = 1, n3  
        if ( rate(i) == 0. )  goto 100
        do j = 1, k
            if ( i>1 .and. index_final(i) == index_final(j) .and. scale_final(i) == scale_final(j) .and.
     1            scale_final_v(i) == scale_final_v(j) ) then
                rate(j) = rate(j) + rate(i)
                rate0(j) = rate0(j) + rate0(i)
                goto 100
            endif
        enddo
        k = k + 1
        index_final(k) = index_final(i)
        scale_final(k) = scale_final(i)
        scale_final_v(k) = scale_final_v(i)
        rate(k) = rate(i)  
        rate0(k) = rate0(i) 
        level(k) = level(i)
        do iPer = 1, nPer_all  
            sa1_final(iPer,k) =  Sa1(iPer,i)
            sa1v_final(iPer,k) =  Sa1_V(iPer,i)
        enddo
100     continue              
      enddo
      n3 = k     
      
c     Compute the hazard from the final set of rates
      call CalcHaz ( nPer_all, nLevel, n3, sa1_final, uhs_all, rate, haz, MAXPER, MAXLEVEL )
      if ( HVFlag == 0 )    call CalcHaz ( nPer_all, nLevel, n3, sa1v_final, uhsv_all, rate, haz_v, MAXPER, MAXLEVEL )
      
      open (2, file = haz_outfile, status = 'replace')
      write ( 2,'(i5, A50)') iPer2-iPer1+1, 'nPeriod between iPer1 and iPer2'
      write (2,'(i5, A12)') nLevel, 'nHazLevels'     
      write(2, '( A5, 2A8, 4A15)')'iPer', 'Period', 'iLevel', 'Haz_Initial', 'Haz_Final', 'Haz_Target', 'Sa(g)'
      do iPer=iPer1,iPer2
        do iLevel=1,nLevel
            write (2,'( i5, f8.3, i8, 4e15.5)') iPer, Period_all(iPer), iLevel, haz0(iPEr,iLevel),haz(iPEr,iLEvel), 
     &          hazLevel(iLevel), UHS_all(iPer,iLevel)
        enddo
      enddo 
      close (2)   
      
      if ( HVflag == 0 ) then
      open (3, file = hazV_outfile, status = 'replace')
      write ( 3,'(i5, A50)') iPer2-iPer1+1, 'nPeriod between iPer1 and iPer2'
      write (3,'(i5, A12)') nLevel, 'nHazLevels'     
      write(3, '( A5, 2A8, 4A15)')'iPer', 'Period', 'iLevel', 'Haz_Initial', 'Haz_Final', 'Haz_Target', 'Sa(g)'
      do iPer=iPer1,iPer2
        do iLevel=1,nLevel
            write (3,'( i5, f8.3, i8, 4e15.5)') iPer, Period_all(iPer), iLevel, haz0_V(iPEr,iLevel),haz_V(iPEr,iLEvel), 
     &          hazLevel(iLevel), UHS_all(iPer,iLevel)
        enddo
      enddo 
      close (3)         
      
      endif                                    
      
      Deallocate ( haz1, haz2, haz1_v,haz2_v, haz0, haz0_v, 
     2      haz_v, haz, Sa1, Sa1_V, Sa1_final, Sa1V_final )
         
      return
      end

c ---------------------------------------------------------

      subroutine CalcHaz ( nPer, nLevel, n3, sa1, uhs, rate, haz, MAXPER, MAXLEVEL )
      implicit none
      integer MAXPER, MAXLEVEL
      real sa1(nPer,1), UHS(MAXPER,1), rate(1), haz(nPer,1)
      real*8 sum
      integer iPer, nPer, iLevel, nLevel, n3, i

      do iPer=1,nPer
        do iLevel=1,nLevel
          sum = 0.
          do i=1,n3
            if ( sa1(iper,i) .ge. uhs(iPer,iLevel)*0.99 ) then
              sum = sum + rate(i)
            endif
          enddo
          haz(iPer,iLevel) = sum

        enddo
      enddo
      return
      end

c -------------------------------------------------------------

      subroutine calcMisfit_Norm ( nPer, iPer1, iPer2, iPer1_strict, iPer2_strict, nLevel, haz, hazLevel, misfit, MAXPER )
      implicit none
      integer MAXPER
      real hazLevel(1), haz(nPer,1)
      real*8 sum, misfit
      integer iPer, nPer, iLevel, nLevel, iPer1, iPer2, iPer1_strict, iPer2_strict

c     Compute hazard misfit
      sum = 0.
      do iPer=1, nPer   !iPer1,iPer2
        do iLevel=1,nLevel  ! 2,nLevel
          if ( haz(iPer,iLevel) .ne. 0. ) then
            if ( iPer < iPer1_strict .and. iPer >= iPer1 ) then
                 sum = sum + 0.25* ( ( alog( haz(iPer, iLevel)/hazLevel(iLevel) ) )**2 )
            !    sum = sum + ( ( alog( haz(iPer, iLevel)/hazLevel(iLevel) ) )**2 )
            elseif ( iPer > iPer2_strict .and. iPer <= iPer2 ) then
                 sum = sum + 0.25* ( ( alog( haz(iPer, iLevel)/hazLevel(iLevel) ) )**2 )
           !     sum = sum + ( ( alog( haz(iPer, iLevel)/hazLevel(iLevel) ) )**2 )
            elseif ( iPer >= iPer1_strict .and. iPer <= iPer2_strict ) then
                sum = sum + ( alog( haz(iPer, iLevel)/hazLevel(iLevel) ) )**2
            endif
          endif
        enddo
      enddo
      misfit = sum
      return
      end

c -------------------------------------------------------------  

      subroutine calcMisfit ( nPer, iPer1, iPer2, iPer1_strict, iPer2_strict, nLevel, 
     1  haz, hazLevel, misfit, iLevel_targ, avemisfit_level, maxmisfit_level, MAXPER )
      
      implicit none
      integer MAXPER
      real hazLevel(1), haz(nPer,1)
      real*8 misfit, avemisfit_level, maxmisfit_level
      integer iPer, nPer, iLevel, nLevel, iPer1, iPer2, iPer1_strict, iPer2_strict
      integer iLevel_targ
      
c     Local variables
      integer k, k_level
      real*8 sum, sum_level, temp, delta  
      
c     Compute hazard misfit
      sum = 0.
      sum_level = 0.
      k = 0
      k_level = 0
      maxmisfit_level = 0.
      do iPer=1, nPer   !iPer1,iPer2
        do iLevel=1,nLevel  ! 2,nLevel
          if ( haz(iPer,iLevel) .ne. 0. ) then
            k = k + 1
            delta = hazLevel(iLevel) - haz(iPer, iLevel)
          !  temp = abs ( delta/hazLevel(iLevel) )
            temp = ( alog( haz(iPer, iLevel)/hazLevel(iLevel) ) )**2
            if ( iPer < iPer1_strict .and. iPer >= iPer1 ) then
                 if ( iLevel == iLevel_targ) then
                    k_level = k_level+1
                    sum_level = sum_level + 0.25*temp
                 endif
                 sum = sum + 0.25* temp
            elseif ( iPer > iPer2_strict .and. iPer <= iPer2 ) then
                 if ( iLevel == iLevel_targ) then
                    k_level = k_level+1
                    sum_level = sum_level + 0.25*temp
                    
                 endif                 
                 sum = sum + 0.25* temp
           elseif ( iPer >= iPer1_strict .and. iPer <= iPer2_strict ) then
                 if ( iLevel == iLevel_targ) then
                    k_level = k_level+1
                    sum_level = sum_level + temp
                    if ( temp > maxmisfit_level ) maxmisfit_level = temp
                 endif
                sum = sum + temp
            endif
            
            
          endif
        enddo
      enddo
      
      misfit = sum / k
      avemisfit_level = sum_level / k_level
      
      return
      end

c -------------------------------------------------------------
      subroutine calcMisfit_Sa ( nPer, iPer1, iPer2, iPer1_strict, iPer2_strict, nLevel, 
     1  UHS, UHS_comp, misfit, iLevel_targ, avemisfit_level, maxmisfit_level, iPer_max, MAXPER )
      
      implicit none
      integer MAXPER
      real UHS(MAXPER,1), UHS_comp(MAXPER,1)
      real*8 misfit, avemisfit_level, maxmisfit_level
      integer iPer, nPer, iLevel, nLevel, iPer1, iPer2, iPer1_strict, iPer2_strict
      integer iLevel_targ, iPer_max
      
c     Local variables
      integer k, k_level
      real*8 sum, sum_level, temp, delta  
      
c     Compute hazard misfit
      sum = 0.
      sum_level = 0.
      k = 0
      k_level = 0
      maxmisfit_level = 0.
      
      do iLevel=1,nLevel  ! 2,nLevel
        do iPer=1, nPer   !iPer1,iPer2
        
          !if ( haz(iPer,iLevel) .ne. 0. ) then
            k = k + 1
            delta = UHS(iPer,iLevel) - UHS_comp(iPer, iLevel)
            temp = abs ( delta/UHS(iPer,iLevel) )
            if ( iPer < iPer1_strict .and. iPer >= iPer1 ) then
                 if ( iLevel == iLevel_targ) then
                    k_level = k_level+1
                    sum_level = sum_level + 0.25*temp
                 endif
                 sum = sum + 0.25* temp
            elseif ( iPer > iPer2_strict .and. iPer <= iPer2 ) then
                 if ( iLevel == iLevel_targ) then
                    k_level = k_level+1
                    sum_level = sum_level + 0.25*temp
                 endif                 
                 sum = sum + 0.25* temp
           elseif ( iPer >= iPer1_strict .and. iPer <= iPer2_strict ) then
                 if ( iLevel == iLevel_targ) then
                    k_level = k_level+1
                    sum_level = sum_level + temp
                    if ( temp > maxmisfit_level ) then
                        maxmisfit_level = temp
                        iPer_max = iPer
                    endif
                 endif
                sum = sum + temp
            endif
            
            
         ! endif
        enddo
      enddo
      
      misfit = sum / k
      avemisfit_level = sum_level / k_level
      
      
      return
      end

c -------------------------------------------------------------

      subroutine Calc_UHS ( nLevel, nZ, Sa, hazlevel, nPer, haz, UHS_comp, MAXPER ) 
      
      implicit none
      integer nPer, MAXPER, nLevel, nZ
      real Sa(MAXPER,1), haz(nPer,1), HazLevel(1)
      real UHS_comp(MAXPER,1)
      
c     Local variables
      integer iPer, iTest, iSa   
      real x 
      
      
      do iTest = 1, nLevel     !! Test levels
        do iPer = 1, nPer       !! Going over each period
            do iSa = 2, nZ  !! Searching the haz curve at iPer
                if ( haz(iPer,iSa) .lt. hazlevel(iTest) ) then
                    if ( haz(iPer,iSa) == 0 ) then
                        UHS_comp(iPer, iTest) = Sa(iPer,iSa-1)
                    else
c                       Interpolate the hazard curve
                        x = (alog(hazlevel(itest)) - alog(haz(iPer,iSa-1)))/
     1                      (alog(haz(iPer,iSa))-alog(haz(iPer,iSa-1))) 
     1                      * (alog(Sa(iPer,iSa))-alog(Sa(iPer,iSa-1))) + alog(Sa(iPer,iSa-1))
                        UHS_comp(iPer, iTest) = exp(x)
                    endif                 
                    goto 10                        
                endif
            enddo
10          continue
        enddo
      enddo                    
        
      
      return
      end subroutine Calc_UHS
c-----------------------------------------------------------------------------------------------------            

      subroutine calcMisfit_UHS ( UHS, UHS_comp, misfit, avemisfit_level, maxmisfit_level, iPerMax_level,
     1      avemisfit_level_main, avemisfit_level_second, nPer_all, nLevel, iPer1, iPer2, iPer1_match, 
     1      iPer2_match, hazlevel, MAXPER )    
     
c     This subroutine calculate the average and maximum UHS misfit at all hazard levels
c     and the average misfit across all haz levels
              
      implicit none
      
c     Passed variables      
      integer MAXPER
      real UHS(MAXPER,1), UHS_comp(MAXPER,1)
      real*8 misfit, avemisfit_level(1), maxmisfit_level(1), avemisfit_level_main(1), 
     1      avemisfit_level_second(1) 
      integer iPerMax_level(1), nlevel, iper1_match, iper2_match, iper1, iper2, nper_all
      real hazlevel(1)
      
c     Local variables
      integer k, k_level, iLevel, iPer, iLevel_targ, k_level_main, k_level_second
      real*8 sum, sum_level, temp, delta, sum_level_main, sum_level_second
      
      
c     Compute UHS misfit
      sum = 0.
      k = 0
      
      do iLevel=1,  nLevel-1 
        k_level = 0
        k_level_main = 0
        k_level_second = 0
        sum_level = 0.
        sum_level_main = 0.
        sum_level_second = 0.
        maxmisfit_level(iLevel) = 0.
        do iPer=1,  nPer_all    ! Whole period range of UHS
            k = k + 1
            delta = UHS(iPer,iLevel) - UHS_comp(iPer, iLevel)
            temp = abs ( delta/UHS(iPer,iLevel) )
            k_level = k_level+1

            if ( abs(temp) >= abs (maxmisfit_level(iLevel)) ) then
                maxmisfit_level(iLevel) = temp
                iPerMax_level(iLevel) = iPer
            endif  
                         
            if ( iPer < iPer1_match .and. iPer >= iPer1 ) then      !In the period range of loose matching
                    k_level_second = k_level_second + 1                  
                    sum_level_second = sum_level_second + 0.25 * temp      
                    sum = sum + 0.25 * temp     
                    sum_level = sum_level + 0.25 * temp      
            elseif ( iPer > iPer2_match .and. iPer <= iPer2 ) then !In the period range of loose matching
                    k_level_second = k_level_second + 1
                    sum_level_second = sum_level_second + 0.25 * temp   
                    sum = sum + 0.25 * temp     
                    sum_level = sum_level + 0.25* temp           
           elseif ( iPer >= iPer1_match .and. iPer <= iPer2_match ) then   !In the period range of strict matching
                    k_level_main = k_level_main + 1
                    sum_level_main = sum_level_main + temp
                    sum = sum + temp
                    sum_level = sum_level + temp
            endif
            
        enddo 
        avemisfit_level(iLevel) = sum_level / k_level
        avemisfit_level_main(iLevel) = sum_level_main / k_level_main
        avemisfit_level_second(iLevel) = sum_level_second / k_level_second             
      enddo
      
      misfit = sum / k
      

      
      return
      end subroutine calcMisfit_UHS

c----------------------------------------------------------------------------------------------------- 