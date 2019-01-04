      program ScenarioSpectra
      
c     *************************************************************************
c     * Fortran code to compute scenario spectra                              *
c     * Original version by L. Al Atik and N. Abrahamson                      *
c     * Date: 2012                                                            *
c     * Modifications:                                                        *
c     * LAA, May 2017 : - User choice for H and/or V                          *
c     *                 - Make UHS_v and CMS specifed as user input           *
c     *                 - Fix the pinching issue                              *
c     *                 - Major rewriting of the program                      *
c     *																		  *
c     *************************************************************************     
           
      use declare
      implicit none
      
      real, Allocatable:: sa1_all(:,:), saV2_all(:,:), haz(:,:), haz_V(:,:)

      
      write(*,*) '+------------------------------------------------------+' 
      write(*,*) '| Scenario Spectra Program                             |'
      write(*,*) '| Ver. May 2017                                        |'
      write(*,*) '+------------------------------------------------------+' 
      write(*,*)     


c     read input parameters    
      call Read_input ( HVFlag, Per0, Per1_loose, Per2_loose, Per1_strict, Per2_strict, 
     1      Mmin, Mmax, distflag, Rmin, Rmax, Vs30min, Vs30max, 
     1      durmin, durmax, PGVmin, PGVmax, AImin, AImax, n, nMonte,  
     1      nFit, delta, maxAdj, UHSh_file, UHSv_file, CMSh_file, CMSv_file, 
     1      Horiz_flatfile, Vert_flatfile, sum_outfile, CS_outfile, SaH_outfile,
     1      SaV_outfile, Haz_outfile, HazV_outfile, UHS_outfile, UHSV_outfile,
     1      THnames_outfile )     
     
c     read UHS
      write(*,*) 'Reading UHS...'
      call Rd_UHS ( UHSh_file, nLevel, nPer_UHS, UHS, MAXPER, period_UHS, hazLevel )
      if ( HVFlag == 0 ) then
        call Rd_UHS ( UHSv_file, nLevel_temp, nPer_UHS_temp, UHSv, MAXPER, 
     1      period_UHS_temp, hazLevel_temp )  
        if ( nLevel_temp .ne. nLevel .or. nPer_UHS_temp .ne. nPer_UHS ) then
            stop 'Horizontal and vertical UHS must have the same nPer and nLevel'
        endif
        do i = 1, nLevel_temp
            if ( hazLevel_temp(i) .ne. hazLevel(i) ) then
                stop 'Horizontal and vertical UHS must have the same return periods'
            endif
        enddo
        do i = 1, nPer_UHS_temp
            if ( period_UHS_temp(i) .ne. period_UHS(i) ) then
                stop 'Horizontal and vertical UHS must have the same period vector'
            endif                
        enddo
      endif  
      
c     read CMS, sigma, rho
      write(*,*) 'Reading CMS...'   
      call Rd_CMS ( CMSh_file, nLevel, nPer_UHS, CMS, MAXPER, period_UHS, hazLevel, 
     1      sigma, rho )
      if ( HVFlag == 0 ) then
        call Rd_CMS ( CMSv_file, nLevel, nPer_UHS, CMSv, MAXPER, period_UHS, hazLevel, 
     1      sigmaV, rhoV )            
      endif 
      
c     read flatfile   
      write(*,*) 'Reading flatfile...'  
      call Read_flatfile ( Horiz_flatfile, period_UHS, nPer_UHS, nTH_temp, recSeq_temp, eqid_temp, staSN_Temp, 
     1      mag_temp, dip_temp, rake_temp, rRup_temp, rjb_temp, Repi_temp, rhypo_temp, Rx_temp, AZ_temp, Vs30_temp, 
     1      dur_temp, GMX_C1_temp, GMX_C2_temp, GMX_C3_temp, AI_temp, minfreq_H_temp, PGA_temp, PGV_temp, PGD_temp,
     1      sa_temp, MAXPER, period_temp, nPer_temp, AccfilenameH1_temp, AccfilenameH2_temp, AccfilenameV_temp, 0  )  
      if ( HVFlag == 0 ) then
        call Read_flatfile ( vert_flatfile, period_UHS, nPer_UHS, nTH_temp, recSeq_temp, eqid_temp, staSN_Temp, 
     1      mag_temp, dip_temp, rake_temp, rRup_temp, rjb_temp, Repi_temp, rhypo_temp, Rx_temp, AZ_temp, Vs30_temp, 
     1      dur_temp, GMX_C1_temp, GMX_C2_temp, GMX_C3_temp, AI_temp, minfreq_V_temp, PGA_V_temp, PGV_V_temp, PGD_V_temp,
     1      sa_V_temp, MAXPER, period_temp, nPer_temp, AccfilenameH1_temp, AccfilenameH2_temp, AccfilenameV_temp, 1  )       
      endif
!      write(*,*) nTH_temp
      call screen_TH ( HVFlag, Per1_loose, Per2_loose, iPer1_loose, iPer2_loose, 
     1      Per1_strict, Per2_strict, iPer1_strict, iPer2_strict, period_all, nPer_all,
     1      period_temp, nPer_temp, nTH, nTH_temp, Period_UHS, nPer_UHS,
     1      recSeq_temp, eqid_temp, staSN_Temp, mag_temp, dip_temp, rake_temp, rRup_temp, rjb_temp, 
     1      Repi_temp, rhypo_temp, Rx_temp, AZ_temp, Vs30_temp, dur_temp, GMX_C1_temp, 
     1      GMX_C2_temp, GMX_C3_temp, AI_temp, minfreq_H_temp, minfreq_V_temp, 
     1      AccfilenameH1_temp, AccfilenameH2_temp, AccfilenameV_temp, PGA_temp, PGV_temp, PGD_temp,
     1      PGA_V_temp, PGV_V_temp, PGD_V_temp, sa_temp, sa_V_temp,
     1      recSeq, eqid, staSN, mag, dip, rake, rRup, rjb, Repi, rhypo, Rx,
     1      AZ, Vs30, dur, GMX_C1, GMX_C2, GMX_C3, AI, minfreq_H, minfreq_V, 
     1      AccfilenameH1, AccfilenameH2, AccfilenameV, PGA, PGV, PGD,
     1      PGA_V, PGV_V, PGD_V, sa_all, saV_all, MAXPER, Mmin, Mmax, distflag, Rmin,  
     1      Rmax, Vs30min, Vs30max, durmin, durmax, PGVmin, PGVmax, AImin, AImax,
     1      nPer, period, sa, saV, Per0, iPer0 )
!      write(*,*) nTH
!      pause 'ok_nTH'

c     interpolate (log-log) UHS, CMS, sigma, rho to the periods of the TH spectra.
c     The interpolated UHS and CMS are overwritten to the same variable, ex. 'UHS'
      write(*,*) 'Interpolating UHS, CMS, sigma, rho...'  
      call Interp_loglog ( nLevel, nPer_UHS, UHS, UHS_all, MAXPER, period_UHS, period, nPer,
     1      period_all, nPer_all)
      call Interp_loglog ( nLevel, nPer_UHS, CMS, CMS_all, MAXPER, period_UHS, period, nPer,
     1      period_all, nPer_all) 
      ! interpolate rho and sigma using log-lin interpolation
      call Interp_loglin_rho ( nLevel, nPer_UHS, rho, rho_all, MAXPER, period_UHS, period, nPer,
     1      period_all, nPer_all, Per0) 
      call Interp_loglin ( nLevel, nPer_UHS, sigma, sigma_all, MAXPER, period_UHS, period, nPer,
     1      period_all, nPer_all)      
     
      if ( HVFlag == 0 ) then
            call Interp_loglog ( nLevel, nPer_UHS, UHSv, UHSv_all, MAXPER, period_UHS, period, nPer,
     1          period_all, nPer_all) 
            call Interp_loglog ( nLevel, nPer_UHS, CMSv, CMSv_all, MAXPER, period_UHS, period, nPer,
     1          period_all, nPer_all)                
            call Interp_loglin_rho ( nLevel, nPer_UHS, rhoV, rhoV_all, MAXPER, period_UHS, period, nPer,
     1          period_all, nPer_all, Per0) 
            call Interp_loglin ( nLevel, nPer_UHS, sigmaV, sigmaV_all, MAXPER, period_UHS, period, nPer,
     1          period_all, nPer_all)       
      endif
!      pause 'end of interpolations'
    
c     select the recordings that fall within the +- 2 sigma of the CMS for each level
c     Loop over each level
      write(*,*) 'Selecting candidate TH...'  
      write(*,*) nTH
      write(11,*) 'Total number of candidate TH:', nTH
      write(11,*)
      write(*,*) ' Haz. Level, Nb. of selected TH'
      write(11,*) 'Selected TH per hazard level:'
      write(11,*) ' Haz. Level, Nb. of selected TH'
      do iLevel=1,nLevel-1        
        call Select_TH ( iPer0, nTH, nPer, sa, SaV, UHS, UHSv, cms, cmsV, sigma, sigmaV, 
     1      rho, rhoV, iSelect, nSelect, iLevel, MAXLEVEL, MAXPER, HVFlag, PGV, PGVmin, PGVmax )    
        write(*,*) iLevel, nSelect(iLevel)
        write(11,*) iLevel, nSelect(iLevel)             
      enddo
      pause 'ok_selectTH'
        
c     Select the best N1 time histories for each level
      write(*,*) 'Running Monte Carlo...'
      write(11,*)
      write(11,*) 'Subselected TH per hazard level (MonteCarlo):'
      write(11,*) ' Haz. Level, Nb. of selected TH'
      do iLevel=1,nLevel-1
        n1(iLevel) = n
        call selectSubset_v1 ( iPer0, iLevel, n1, nMonte, nPer, sa, SaV, UHS, UHSv, cms, cmsV, 
     1      sigma, sigmaV, rho, rhoV, iset_save, MAXLEVEL, MAXPER, period, iSelect, nSelect, nTH,
     2      HVFlag )
        write(*,*) iLevel, n1(iLevel)
        write(11,*) iLevel, n1(iLevel)
      enddo
      pause 'monte carlo'
      write(*,*)              

c     Find rate to best represent the hazard
      call Findrate ( n1, nPer, period_all, sa_all, uhs_all, iset_save, MAXLEVEL, MAXPER, 
     1      rate, nLevel, hazLevel, index_final, nSet, scale_final, level, 
     1      nfit, delta, nPer_all, iPer1_loose, iPer2_loose, iPer0,  
     2      SaV_all, UHSv_all, scale_final_v, rate0, maxAdj, iPer1_strict, iPer2_strict,
     2      HVFlag, haz_outfile, hazV_outfile )      
     
c     Calculate horiz and vert hazard curves over all period range of UHS
      nZ = nLevel + 20
      do iPer = 1, nPer_all     
          do iLevel = 1, nLevel 
          	Z_v(iPer,iLevel) = UHSv_all (iPer,iLevel)
          	Z(iPer,iLevel) = UHS_all (iPer,iLevel)
          enddo  
          do iZ = nLevel+1, nZ
            Z(iPer,iZ) = Z(iPer,iZ-1) + 0.2
            Z_v(iPer,iZ) = Z_v(iPer,iZ-1) + 0.2
          enddo   
      enddo               
      Allocate ( Sa1_all(nPer_all,nSet), SaV2_all(nPer_all,nSet) )
      Allocate ( haz(nPer_all,nZ), haz_V(nPer_all,nZ) )
      
      do i = 1, nSet
        do iPer = 1, nPer_all
            Sa1_all(iPer,i) = (sa_all(iPer,index_final(i)) * scale_final(i))
            if ( HVFlag == 0 )  saV2_all(iPer,i) = (SaV_all(iPer,index_final(i)) * scale_final_v(i))
        enddo
      enddo
      
c     Calculate horizontal and vertical hazard curves      
      call CalcHaz ( nPer_all, nZ, nSet, sa1_all, Z, rate, haz, MAXPER, MAXLEVEL ) 
      if ( HVFlag == 0 ) then
        call CalcHaz ( nPer_all, nZ, nSet, saV2_all, Z_v, rate, haz_V, MAXPER, MAXLEVEL )
      endif  

c     Calculate horizontal and vertical UHS curves 
      if ( HVFlag == 0 ) call Calc_UHS ( nLevel, nZ, Z_v, hazlevel, nPer_all, haz_V, UHS_v_comp, MAXPER ) 
      call Calc_UHS ( nLevel, nZ, Z, hazlevel, nPer_all, haz, UHS_comp, MAXPER )                  
     
c     Find the number to unique time series used      
      Nunique = 0
      do iTH = 1, nTH
       do i = 1, nSet
        if ( index_final(i) == iTH ) then
          nUnique = nUnique + 1
          index_final_unique(nUnique) = index_final(i)
          GOTO 23
        ENDIF
       ENDDO
 23   continue      
      enddo
      write(*,*)
      write (*,'(3i5)') nUnique, nSet 

c     Calculate average and max misfit between the computed and the target UHS 
c     curves at all haz levels 
      call calcMisfit_UHS ( UHS_all, UHS_comp, misfit, avemisfit_level, maxmisfit_level, iPerMax_level,
     1      avemisfit_level_main, avemisfit_level_second, nPer_all, nLevel, iPer1_loose, iPer2_loose, 
     1      iPer1_strict, iPer2_strict, hazlevel, MAXPER )       
      if (HVFlag.eq.0 ) then
            call calcMisfit_UHS ( UHSV_all, UHS_V_comp, misfit_v, avemisfit_v_level, maxmisfit_v_level, iPerMax_v_level,
     1          avemisfit_v_level_main, avemisfit_v_level_second, nPer_all, nLevel, iPer1_loose, iPer2_loose, 
     1          iPer1_strict, iPer2_strict, hazlevel, MAXPER )      
      endif
           
c     write output to summary file
      call wrt_summary ( HVFlag, nSet, nUnique, misfit, misfit_v, avemisfit_level, 
     1      avemisfit_v_level, maxmisfit_level, maxmisfit_v_level, avemisfit_level_main, 
     1      avemisfit_level_second, avemisfit_v_level_main, avemisfit_v_level_second, 
     1      period_all, iPerMax_level, iPerMax_v_level, hazlevel, nlevel, MAXPER ) 
     
c     Write output files
      call wrt_tblOut ( nSet, index_final, rate, scale_final, scale_final_v, mag, rRup, 
     1       rJB, Vs30, recSeq, eqid, level, nlevel-1, nPer, nUnique, Per0, rate0,
     1       dur, AccfilenameH1, AccfilenameH2, AccfilenameV, CS_outfile, THnames_outfile,
     1       index_final_unique )  
c     Write output UHS file
      call wrt_UHS ( nLevel, hazlevel, nPer_all, Period_all, UHS_comp, UHS_all, 
     1      MAXPER, Per0, UHS_outfile )  
      if (HVFlag == 0 ) then
        call wrt_UHS ( nLevel, hazlevel, nPer_all, Period_all, UHS_V_comp, UHSv_all, 
     1      MAXPER, Per0, UHSV_outfile )          
      endif 
!c     Write output hazard file        
!      call wrt_haz( nLevel, hazlevel, nPer_all, Period_all, haz, Z, 
!     1      MAXPER, haz_outfile, Per0 )  
!      if (HVFlag == 0 ) then
!        call wrt_haz ( nLevel, hazlevel, nPer_all, Period_all, haz_V, Z_V, 
!     1      MAXPER, hazV_outfile, Per0 )         
!      endif  
c     Write output scenarios spectra file  
      call wrt_scenario( nPer_all, Period_all, nSet, rate, sa1_all, 
     1       recSeq, index_final, MAXPER, SaH_outfile )    
      if (HVFlag == 0 ) then
        call wrt_scenario( nPer_all, Period_all, nSet, rate, saV2_all, 
     1       recSeq, index_final, MAXPER, SaV_outfile )      
      endif


      stop
      end




            