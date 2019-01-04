      subroutine wrt_tblOut ( nSet, index_final, rate, scale_final, scale_final_v, mag, 
     1      rRup, rjb, Vs30, recSeq, eqid, level, nlevel, nPer, nUnique, Per0, rate0,
     1      dur, AccfilenameH1, AccfilenameH2, AccfilenameV, CS_outfile, THnames_outfile,
     1      index_final_unique ) 
      
      implicit none
      integer nSet, index_final(1), recSeq(1), eqid(1), level(1)
      real rate(1), Scale_final(1), mag(1), Rrup(1), Vs30(1), Per0, Scale_final_v(1), rate0(1)
      real rjb(1), dur(1)
      integer nLevel, nPer, nUnique, index_final_unique(1)
      character*100 AccfilenameH1(1), AccfilenameH2(1), AccfilenameV(1)
      character*48 CS_outfile, THnames_outfile
      
c     Local variable
      integer iTH, i

c     To write out the results as a table of all the time series, RSN, eqID,
c     mag, Rrup, scale factor and rate
      open (110, file=CS_outfile, status = 'replace' )
      write ( 110,'(i5, A12)') nPer, 'nPeriod'
      write (110,'(i5, A12)') nLevel, 'nHazLevels'
      write (110,'(i5, A22)') nSet, 'Total nTH Used'
      write (110,'(i5, A22)') nUnique, 'Unique nTH Used'
      write(110,'(f6.3, A12)') Per0, 'Ref Period'
      write(110,999) 'Index', 'HazLevel', 'RSN', 'EqID',
     1      'Mag', 'Rrup', 'Rjb', 'Vs30', 'Dur', 
     1      'Scalefactor_h', 'Scalefactor_v', 'Rate', 'Rate_Initial',
     1      'AccFilename_H1', 'AccFilename_H2', 'AccFilename_V'
      do i = 1, nSet
        iTH = index_final(i)
        write(110, 1000 ) i, level(i), recSeq(iTH), eqid(iTH), mag(iTH), 
     1      rrup(iTH), rjb(iTH), Vs30(iTH), dur(iTH), scale_final(i), 
     1      scale_final_v(i), rate(i), rate0(i), AccfilenameH1(iTH),
     1      AccfilenameH2(iTH), AccfilenameV(iTH)   
      enddo      
999   format (2A12, 2A10, 5A8, 4A18, 2x, 3A40)      
1000  format (2i12, 2i10, 5f12.1, 4e18.8, 2x, 3A40)       
      
      close (110)
      
      ! write the names of the unique TH
      open (111, file=THnames_outfile, status = 'replace' )
      write ( 111,'(i5, A12)') nPer, 'nPeriod'
      write (111,'(i5, A12)') nLevel, 'nHazLevels'
      write (111,'(i5, A22)') nUnique, 'Unique nTH Used'
      write(111,'(f6.3, A12)') Per0, 'Ref Period'
      write(111,990) 'Index', 'RSN', 'AccFilename_H1', 'AccFilename_H2', 'AccFilename_V'
      do i = 1, nUnique
        iTH = index_final_unique(i)
        write(111, 1001 ) i, recSeq(iTH), 
     1      AccfilenameH1(iTH), AccfilenameH2(iTH), AccfilenameV(iTH)   
      enddo         
      
990   format (A12, A10, 2x, 3A40)  
1001  format (i12, i10, 2x, 3A40) 
      close (111)   
      
      return
      end subroutine wrt_tblOut
c-------------------------------------------------------------------------------------------   

      subroutine wrt_UHS ( nLevel, hazlevel, nPer, Period, UHS_comp, UHS, MAXPER, Per0,
     1      UHS_outfile )
     
      implicit none
      integer MAXPER, nLevel, nPer
      real UHS(MAXPER,1), UHS_comp(MAXPER,1), hazlevel(1), Period(1), Per0
      character*48 UHS_outfile
      
c     Local variables  
      integer iLevel, iPer    
      
      
c     To write out the file that compares the computed UHS using the scenario
c     spectra to the initial input UHS      
      open (111, file=UHS_outfile, status = 'replace' )  
      write ( 111,'(i5, A12)') nPer, 'nPeriod'
      write (111,'(i5, A12)') nLevel, 'nHazLevels'
      write(111,'(f6.3, A12)') Per0, 'Ref Period'      
      write(111,998) 'iLevel', 'HazLevel', 'Period', 'UHS_target','UHS_computed'
      
      do iLevel = 1, nLevel
        do iPer = 1, nPer
            write(111, 997) iLevel, hazLevel(iLevel), Period(iPer), UHS(iPer,iLevel),
     1          UHS_comp(iPer,iLevel)
        enddo
      enddo
998   format ( A8, A15, A15, 2A15 )
997   format ( i8, e15.5, f15.5, 2e15.5 )
      close (111)         
      
      return
      end subroutine wrt_UHS   
c-------------------------------------------------------------------------------------------    


      subroutine wrt_summary ( HVFlag, nSet, nUnique, misfitH, misfitV, avemisfitH_level, 
     1      avemisfitv_level, maxmisfitH_level, maxmisfitv_level, avemisfitH_level_main, avemisfitH_level_second, 
     1      avemisfitv_level_main, avemisfitv_level_second, period_all, iPerH_max, iPerV_max,
     1      hazlevel, nlevel, MAXPER ) 
      
            
      implicit none
      
c     Passed variables
      real*8 misfitH, misfitV, avemisfitH_level(1), maxmisfitH_level(1), avemisfitV_level(1), 
     1  maxmisfitV_level(1), avemisfitH_level_main(1), avemisfitH_level_second(1), 
     2  avemisfitV_level_main(1), avemisfitV_level_second(1)
      integer iPerH_max(1), iPerV_max(1), nlevel, MAXPER, HVFlag, nset, nunique
      real hazlevel(1), period_all(1)
            
c     Local variables   
      integer iTH, i, iLevel
      real per_temp(MAXPER)
      
      
c     Write misfit information to the summary file
      
!      write(11,*)
!      write(11,'(2x,A44,f5.1)') ' Maximum scale factor (<200!) horizontal:   ', maxScale(1) 
!      write(11,'(2x,A44,f5.1)') '                                vertical:   ', maxScale(2)
      write(11,*)
      
      write(11,*) 'Output Parameters: ' 
      write(11,*)      
      if ( HVFlag == 0 .or. HVFlag == 1 ) then
        write(11,'(2x,A69,f15.2,A1)') ' Average misfit in horizontal UHS over all hazard levels and periods:', misfitH*100,'%'
      endif
      if ( HVFlag == 0 .or. HVFlag == 2 ) then
        write(11,'(2x,A67,f17.2,A1)') ' Average misfit in vertical UHS over all hazard levels and periods:', misfitV*100,'%'
      endif
      write(11,*)
      if ( HVFlag == 0 .or. HVFlag == 1 ) then
        write(11,'(2x,A16)') ' HORIZONTAL UHS:'
      endif
      if ( HVFlag == 2 ) then
        write(11,'(2x,A16)') ' VERTICAL UHS:'
      endif      
      write(11,'(A25,21x,24es15.1)') ' Hazard Level:', ( HazLevel(iLevel), iLevel = 1, nLevel-1 )
      write(11,'(2x,A44,f14.2,24f15.2)') ' Average misfit over all freq range (%):', 
     1      (avemisfitH_level(iLevel)*100,iLevel = 1, nLevel-1)
      write(11,'(2x,A43,24f15.2)') ' Average misfit in main freq range (%):', 
     1      (avemisfitH_level_main(iLevel)*100,iLevel = 1, nLevel-1)
      write(11,'(2x,A48,f10.2,24f15.2)') ' Average misfit in secondary freq range (%):', 
     1      ( avemisfitH_level_second(iLevel)*100,iLevel = 1, nLevel-1)
      write(11,'(2x,A24,f34.2,24f15.2)') ' Maximum misfit (%):', (maxmisfitH_level(iLevel)*100,iLevel = 1, nLevel-1)
      do iLevel = 1, nLevel-1
        per_temp(iLevel) = Period_all(iPerH_max(iLevel)) 
      enddo
      write(11,'(2x,A34,f24.2,24f15.2)') ' Period of maximum misfit (s):',  (per_temp(iLevel), iLevel = 1, nLevel-1 )   
      write(11,*)

      if ( HVFlag == 0 ) then      
        write(11,'(2x,A14)') ' VERTICAL UHS:'
        write(11,'(A25,21x,24es15.1)') ' Hazard Level:', ( HazLevel(iLevel), iLevel = 1, nLevel-1 )
        write(11,'(2x,A44,f14.2,24f15.2)') ' Average misfit over all freq range (%):', 
     1      (avemisfitV_level(iLevel)*100,iLevel = 1, nLevel-1)
        write(11,'(2x,A43,24f15.2)') ' Average misfit in main freq range (%):', 
     1      (avemisfitV_level_main(iLevel)*100,iLevel = 1, nLevel-1)
        write(11,'(2x,A48,f10.2,24f15.2)') ' Average misfit in secondary freq range (%):', 
     1      (avemisfitV_level_second(iLevel)*100,iLevel = 1, nLevel-1) 
        write(11,'(2x,A24,f34.2,24f15.3)') ' Maximum misfit (%):', (maxmisfitV_level(iLevel)*100,iLevel = 1, nLevel-1) 
        do iLevel = 1, nLevel-1
            per_temp(iLevel) = Period_all(iPerV_max(iLevel)) 
        enddo      
        write(11,'(2x,A34,f24.2,24f15.3)') ' Period of maximum misfit (s):',  (per_temp(iLevel), iLevel = 1, nLevel-1 )       
        write(11,*)
      endif
      
!      write(11,'(2x,A86)') ' (Note: Positive misfit = Calculated > Target. Negative misfit = Calculated < Target.)'
!      write(11,'(2x,A99)') ' (      Acceptable to have <0.15 misfit for UHS in strict matching range and <0.20 in loose range.)'
!      write(11,'(2x,A79)') ' (      Average should be approximatively zero, but not consistently positive.)'
 

      write (11,*)
      write (11,'(2x,A34,I5)') ' Total number of scenario spectra:', nSet       
      write (11,'(2x,A35,I5)') ' Number of unique scenario spectra:', nUnique 
      close (11)      
           
      return
      end subroutine wrt_summary
c-------------------------------------------------------------------------------------------  

      subroutine wrt_haz ( nLevel, hazlevel, nPer, Period, haz, haz0, Z, 
     1      MAXPER, haz_outfile, Per0 )  
      
      implicit none
      integer MAXPER, nLevel, nPer
      real haz(MAXPER,1), hazlevel(1), Period(1), Per0, Z(MAXPER,1), haz0(MAXPER,1)
      character*48 haz_outfile
      
c     Local variables  
      integer iLevel, iPer    
      
      
c     To write out the file that compares the computed hazard curves using the scenario
c     spectra to the initial hazard curves   
      open (1111, file=haz_outfile, status = 'replace' )  
      write ( 1111,'(i5, A12)') nPer, 'nPeriod'
      write (1111,'(i5, A12)') nLevel, 'nHazLevels'
      write(1111,'(f6.3, A12)') Per0, 'Ref Period'      
      write(1111,'( A5, 2A8, 4A15)')'iPer', 'Period', 'iLevel', 'Haz_Initial', 'Haz_Final', 'Haz_Target', 'Sa(g)'
      
      do iPer = 1, nPer
        do iLevel = 1, nLevel
            write(1111, '( i5, f8.3, i8, 4e15.5)') iPer, period(iPer), iLevel, haz0(iPer,iLevel), haz(iPer,iLevel), 
     1          hazLevel(iLevel), Z(iPer,iLevel)
        enddo
      enddo
 
      close (1111)       
      
      return
      end subroutine wrt_haz  
c-------------------------------------------------------------------------------------------    
      subroutine  wrt_scenario( nPer, Period, nSet, rate, sa1, recSeq, index_final,
     1      MAXPER, Sa_outfile )     
     
      implicit none
      integer MAXPER, nSet, nPer, index_final(1), recSeq(1)
      real Sa1(nPer,1), Period(1), rate(1)
      character*48 Sa_outfile
      
c     Local variables  
      integer iPer, i    
      
      
c     To write out the file that compares the computed hazard curves using the scenario
c     spectra to the initial hazard curves   
      open (112, file=Sa_outfile, status = 'replace' )  
      write ( 112,'(i5, A20)') nSet, 'Total Nb of TH'
      write ( 112,'(i5, A20)') nPer, 'nPeriod'
      write(112,'(200f8.3)') (period(iPer), iPer = 1, nPer)   
      write(112, 155) 'RSN', 'Rate', 'Sa(g)...'
155   format (A5, 100A15)
      do i = 1, nSet
        write(112,'(i5, 500f15.10)') recSeq(index_final(i)), rate(i), (sa1(iPer,i),iPer=1,nPer)
      enddo
      close (112)    
     
     
      end subroutine wrt_scenario
c-------------------------------------------------------------------------------------------         