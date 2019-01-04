      subroutine Read_input ( HVFlag, Per0, Per1_loose, Per2_loose, Per1_strict, Per2_strict, 
     1      Mmin, Mmax, distflag, Rmin, Rmax, Vs30min, Vs30max, 
     1      durmin, durmax, PGVmin, PGVmax, AImin, AImax, n, nMonte,  
     1      nFit, delta, maxAdj, UHSh_file, UHSv_file, CMSh_file, CMSv_file, 
     1      Horiz_flatfile, Vert_flatfile, sum_outfile, CS_outfile, SaH_outfile,
     1      SaV_outfile, Haz_outfile, HazV_outfile, UHS_outfile, UHSV_outfile, 
     1      THnames_outfile )
     
      implicit none     
c     Passed Variables   
      character*48 UHSh_file, UHSv_file, CMSh_file, CMSv_file, 
     1      Horiz_flatfile, Vert_flatfile, sum_outfile, CS_outfile, SaH_outfile,
     1      SaV_outfile, Haz_outfile, UHS_outfile, THnames_outfile,
     1      HazV_outfile, UHSV_outfile 
      real Per0, Per1_loose, Per2_loose, delta, maxAdj, Per1_strict, Per2_strict
      real Mmin, Mmax, Rmin, Rmax, Vs30min, Vs30max, durmin, durmax, AImin, AImax
      real PGVmin, PGVmax
      integer n, nMonte, nfit, HVFlag, distflag
      
c     Local Variables  
      character*28 Input_file
      integer k
      character*80 title, dummy
      
      
      write(*,*) 'Enter Input Filename:'
      read(*,*) Input_file
      open (1, file = Input_file, status = 'old' )
      
      read(1,'(a50)') title             ! Title line
      read(1,*) HVFlag                  ! Flag for horiz and/or vertical spectra run ( 0 = both cases, 1 = horiz only, 2 = vert only)
       
      read(1,*) Per0
      read(1,*) Per1_loose, Per2_loose
      read(1,*) Per1_strict, Per2_strict
      read(1,*) Mmin, Mmax
      read(1,*) distflag
      read(1,*) Rmin, Rmax
      read(1,*) Vs30min, Vs30max  
      read(1,*) durmin, durmax
      read(1,*) PGVmin, PGVmax
      read(1,*) AImin, AImax
      
      read(1,*) n
      read(1,*) nMonte
      read(1,*) nFit, delta
      read(1,*) maxAdj
      
c     Read input filenames      
      read(1,*) dummy
      read(1,*) UHSh_file
      if ( HVFlag == 0 )   read(1,*) UHSv_file
      read(1,*) CMSh_file
      if ( HVFlag == 0 )   read(1,*) CMSv_file
      read(1,*) Horiz_flatfile
      if ( HVFlag == 0 )   read(1,*) Vert_flatfile
      
c     Read output filenames          
      read(1,*) dummy
      read(1,*) sum_outfile
      read(1,*) CS_outfile
      read(1,*) SaH_outfile
      if ( HVFlag == 0 )  read(1,*) SaV_outfile
      read(1,*) Haz_outfile
      if ( HVFlag == 0 )  read(1,*) HazV_outfile
      read(1,*) UHS_outfile
      if ( HVFlag == 0 )  read(1,*) UHSV_outfile
      read(1,*) THnames_outfile
           
      close (1)
      
c     Write input on the screen
      write(*,*)
      write(*,'(a50)') title
      write(*,*)
      write(*,'(a59,2x,i2)') ' Run flag (1 = horiz only, 2 = vert only, 0 = both cases):', HVFlag                  
      write(*,'(a22,2x,f6.3)') ' Conditioning period:', Per0
      write(*,'(a48,2x,f6.3,2x,f6.3)') ' Period range for selection and loose matching:', Per1_loose, Per2_loose
      write(*,'(a35,2x,f6.3,2x,f6.3)') ' Period range for strict matching:', Per1_strict, Per2_strict
      write(*,*)
      write(*,'(a53,2x,f6.3,2x,f6.3)') ' Magnitude range for selection of candidate spectra:', Mmin, Mmax
      if ( Distflag == 1 ) then
        write(*,'(a40)') ' Rrup is used to set the distance range'
      else
        write(*,'(a39)') ' Rjb is used to set the distance range'
      endif      
      write(*,'(a52,2x,f8.3,2x,f8.3)') ' Distance range for selection of candidate spectra:', Rmin, Rmax
      write(*,'(a13,2x,f8.3,2x,f8.3)') ' Vs30 range:', Vs30min, Vs30max
      write(*,'(a52,2x,f8.3,2x,f8.3)') ' Duration range for selection of candidate spectra:', Durmin, Durmax
      write(*,'(a47,2x,f8.3,2x,f8.3)') ' PGV range for selection of candidate spectra:', PGVmin, PGVmax    
      write(*,'(a59,2x,f8.3,2x,f8.3)') ' Arias intensity range for selection of candidate spectra:', AImin, AImax     
      
      write(*,*)
      write(*,'(a42,2x,I5)') ' Nb of scenario spectra per hazard level:', n
      write(*,'(a33,2x,I5)') ' Nb of Monte Carlo realizations:', nMonte
      write(*,'(a64,2x,I5,f8.4)') ' Nb of iterations on the rates of scenario spectra, delta rate:', 
     1      nFit, delta 
     
      write(*,*)      
      write(*,*) 'Input files:'
      if ( HVFlag == 1 ) then
        write(*,*) ' Horizontal UHS file: ',  UHSh_file
        write(*,*) ' Horizontal CMS file',  CMSh_file
        write(*,*) ' Horizontal flatfile:',   Horiz_flatfile
      elseif ( HVFlag == 2 ) then
        write(*,*) ' Vertical UHS file: ',  UHSv_file
        write(*,*) ' Vertical CMS file',  CMSv_file
        write(*,*) ' Vertical flatfile:',   Vert_flatfile
      elseif ( HVFlag == 0 ) then
        write(*,*) ' Horizontal UHS file: ',  UHSh_file
        write(*,*) ' Vertical UHS file: ',  UHSv_file
        write(*,*) ' Horizontal CMS file',  CMSh_file
        write(*,*) ' Vertical CMS file',  CMSv_file
        write(*,*) ' Horizontal flatfile:',   Horiz_flatfile
        write(*,*) ' Vertical flatfile:',   Vert_flatfile      
      endif
      
      write(*,*) 
      write(*,*) 'Output files:'
      write(*,*) ' Summary UHS file: ',  sum_outfile
      write(*,*) ' Scenario spectra file: ',  CS_outfile
      if ( HVFlag == 1 ) then
        write(*,*) ' Horizontal Sa file: ',  SaH_outfile
      elseif ( HVFlag == 2 ) then
        write(*,*) ' Vertical Sa file: ',  SaV_outfile
      elseif ( HVFlag == 0 ) then
        write(*,*) ' Horizontal Sa file: ',  SaH_outfile
        write(*,*) ' Vertical Sa file: ',  SaV_outfile   
      endif
      
      if ( HVFlag == 1 ) then
        write(*,*) ' Horizontal Hazard output file: ',  Haz_outfile
      elseif ( HVFlag == 2 ) then
        write(*,*) ' Vertical Hazard output file: ',  HazV_outfile
      elseif ( HVFlag == 0 ) then
        write(*,*) ' Horizontal Hazard output file: ',  Haz_outfile
        write(*,*) ' Vertical Hazard output file: ',  HazV_outfile
      endif
      
      if ( HVFlag == 1 ) then
        write(*,*) ' Horizontal UHS output file: ',  UHS_outfile
      elseif ( HVFlag == 2 ) then
        write(*,*) ' Vertical UHS output file: ',  UHSV_outfile
      elseif ( HVFlag == 0 ) then
        write(*,*) ' Horizontal UHS output file: ',  UHS_outfile
        write(*,*) ' Vertical UHS output file: ',  UHSV_outfile
      endif      
      
      write(*,*) ' Unique TH name: ', THnames_outfile
      write(*,*)            
     
      
c     Write input parameters to the summary output file
      open (11, file = sum_outfile, status = 'replace' ) 
      write(11,*) 'Input Parameters: '
      write(11,*)
      write(11,'(2x,a50)') title
      write(11,*)
      write(11,'(a59,2x,i2)') ' Run flag (1 = horiz only, 2 = vert only, 0 = both cases):', HVFlag                  
      write(11,'(a22,2x,f6.3)') ' Conditioning period:', Per0
      write(11,'(a48,2x,f6.3,2x,f6.3)') ' Period range for selection and loose matching:', Per1_loose, Per2_loose
      write(11,'(a35,2x,f6.3,2x,f6.3)') ' Period range for strict matching:', Per1_strict, Per2_strict
      write(11,*)
      write(11,'(a53,2x,f6.3,2x,f6.3)') ' Magnitude range for selection of candidate spectra:', Mmin, Mmax
      if ( Distflag == 1 ) then
        write(11,'(a40)') ' Rrup is used to set the distance range'
      else
        write(11,'(a39)') ' Rjb is used to set the distance range'
      endif      
      write(11,'(a52,2x,f8.3,2x,f8.3)') ' Distance range for selection of candidate spectra:', Rmin, Rmax
      write(11,'(a13,2x,f8.3,2x,f8.3)') ' Vs30 range:', Vs30min, Vs30max
      write(11,'(a52,2x,f8.3,2x,f8.3)') ' Duration range for selection of candidate spectra:', Durmin, Durmax
      write(11,'(a47,2x,f8.3,2x,f8.3)') ' PGV range for selection of candidate spectra:', PGVmin, PGVmax    
      write(11,'(a59,2x,f8.3,2x,f8.3)') ' Arias intensity range for selection of candidate spectra:', AImin, AImax     
      
      write(11,*)
      write(11,'(a42,2x,I5)') ' Nb of scenario spectra per hazard level:', n
      write(11,'(a33,2x,I5)') ' Nb of Monte Carlo realizations:', nMonte
      write(11,'(a64,2x,I5,f8.4)') ' Nb of iterations on the rates of scenario spectra, delta rate:', 
     1      nFit, delta 
     
      write(11,*)      
      write(11,*) 'Input files:'
      if ( HVFlag == 1 ) then
        write(11,*) ' Horizontal UHS file: ',  UHSh_file
        write(11,*) ' Horizontal CMS file',  CMSh_file
        write(11,*) ' Horizontal flatfile:',   Horiz_flatfile
      elseif ( HVFlag == 2 ) then
        write(11,*) ' Vertical UHS file: ',  UHSv_file
        write(11,*) ' Vertical CMS file',  CMSv_file
        write(11,*) ' Vertical flatfile:',   Vert_flatfile
      elseif ( HVFlag == 0 ) then
        write(11,*) ' Horizontal UHS file: ',  UHSh_file
        write(11,*) ' Vertical UHS file: ',  UHSv_file
        write(11,*) ' Horizontal CMS file: ',  CMSh_file
        write(11,*) ' Vertical CMS file: ',  CMSv_file
        write(11,*) ' Horizontal flatfile: ',   Horiz_flatfile
        write(11,*) ' Vertical flatfile: ',   Vert_flatfile      
      endif
      
      write(11,*) 
      write(11,*) 'Output files:'
      write(11,*) ' Summary UHS file: ',  sum_outfile
      write(11,*) ' Scenario spectra file: ',  CS_outfile
      if ( HVFlag == 1 ) then
        write(11,*) ' Horizontal Sa file: ',  SaH_outfile
      elseif ( HVFlag == 2 ) then
        write(11,*) ' Vertical Sa file: ',  SaV_outfile
      elseif ( HVFlag == 0 ) then
        write(11,*) ' Horizontal Sa file: ',  SaH_outfile
        write(11,*) ' Vertical Sa file: ',  SaV_outfile   
      endif
      if ( HVFlag == 1 ) then
        write(11,*) ' Horizontal Hazard output file: ',  Haz_outfile
      elseif ( HVFlag == 2 ) then
        write(11,*) ' Vertical Hazard output file: ',  HazV_outfile
      elseif ( HVFlag == 0 ) then
        write(11,*) ' Horizontal Hazard output file: ',  Haz_outfile
        write(11,*) ' Vertical Hazard output file: ',  HazV_outfile
      endif
      if ( HVFlag == 1 ) then
        write(11,*) ' Horizontal UHS output file: ',  UHS_outfile
      elseif ( HVFlag == 2 ) then
        write(11,*) ' Vertical UHS output file: ',  UHSV_outfile
      elseif ( HVFlag == 0 ) then
        write(11,*) ' Horizontal UHS output file: ',  UHS_outfile
        write(11,*) ' Vertical UHS output file: ',  UHSV_outfile
      endif  
      write(11,*) ' Unique TH name: ', THnames_outfile
      write(11,*)   
      
      write(11,*)      
      write(11,*) 'Intermediate Parameters:'
      write(11,*)  
      
!      pause 'end of read input'     
                     
      
      return
      end

c -----------------------------------------------------------------
     
      subroutine Rd_UHS ( UHS_file, nLevel, nPer_UHS, UHS, MAXPER, 
     1      period_UHS, hazLevel )
     
      implicit none
      character*80 dummy 
      character*48 UHS_file 
      integer  MAXPER, nPer_UHS 
      real hazLevel(100), UHS(MAXPER,1)
      real period_UHS(1), per1
      integer nLevel, ipos, ii, iPer, i, iLevel

      open (2,file=UHS_file,status='old')

c     Read hazard levels
      do i=1,5
        read (2,'(a1)') dummy 
      enddo
      read (2,'(a)') dummy
      ipos = scan(dummy,"=",back=.true.)
      read (dummy(1+ipos:),*) nLevel     

      do i=1,3
        read (2,'(a)') dummy 
      enddo   
      do iLevel=1,nLevel
        read (2,*) hazLevel(iLevel)        
      enddo     

c     Read Periods
      read (2,'(a)') dummy 
      read (2,'(a)') dummy 
      ipos = scan(dummy,"=",back=.true.)
      read (dummy(1+ipos:),*) nPer_UHS
                  
      do i=1,2
        read (2,'( a1)') dummy 
      enddo   
      
      do iPer=1,nPer_UHS
        read (2,*) period_UHS(iPer)
      enddo

c     Read UHS
      do i=1,4
        read (2,'( a1)') dummy 
      enddo
      do iPer=1,nPer_UHS
        read (2,*) ii, per1, (UHS(iPer,iLevel), iLevel=1,nLevel)
      enddo
      
!      pause 'end of read UHS' 
      
      close (2)
      return
      end

c -----------------------------------------------------------------------------

      subroutine Rd_CMS ( CMS_file, nLevel, nPer_UHS, CMS, MAXPER, period_UHS, 
     1      hazLevel, sigma, rho ) 
     
      implicit none
      character*80 dummy 
      character*48 CMS_file 
      integer  MAXPER, nPer_UHS 
      real hazLevel(100), CMS(MAXPER,1), sigma(MAXPER,1), rho(MAXPER,1)
      real period_UHS(1), per1
      integer nLevel, ipos, ii, iPer, i, iLevel
      integer nLevel_temp, nPer_temp
      real period_temp, hazLevel_temp

      open (3,file=CMS_file,status='old')

c     Read hazard levels
      do i=1,5
        read (3,'(a1)') dummy 
      enddo
      read (3,'(a)') dummy
      ipos = scan(dummy,"=",back=.true.)
      read (dummy(1+ipos:),*) nLevel_temp
      If ( nLevel_temp .ne. nlevel) then
        stop 'CMS should be defined at the same number of hazard levels as UHS'
      endif     

      do i=1,3
        read (3,'(a)') dummy 
      enddo   
      do iLevel=1,nLevel
        read (3,*) hazLevel_temp  
        If ( hazLevel_temp .ne. hazLevel(ilevel) ) then
            stop 'CMS should be defined at the same hazard levels as UHS'
        endif            
      enddo     

c     Read Periods
      read (3,'(a)') dummy 
      read (3,'(a)') dummy 
      ipos = scan(dummy,"=",back=.true.)
      read (dummy(1+ipos:),*) nPer_temp     
      If ( nPer_temp .ne. nPer_UHS ) then
        stop 'CMS should be defined for the same number of periods as UHS'
      endif        
                  
      do i=1,2
        read (3,'( a1)') dummy 
      enddo   
      
      do iPer=1,nPer_UHS
        read (3,*) period_temp
        If ( period_temp .ne. period_UHS(iPer) ) then
            stop 'CMS should be defined for the same periods as UHS'
        endif          
      enddo

c     Read CMS
      do i=1,4
        read (3,'( a)') dummy 
      enddo       
      do iPer=1,nPer_UHS
        read (3,*) ii, per1, ( CMS(iPer,iLevel), iLevel=1,nLevel )
      enddo
      
c     Read median   
      do i=1,2
        read (3,'( a)') dummy 
      enddo
      do iPer=1,nPer_UHS
        read (3,'( a)') dummy
      enddo         
      
c     Read sigma   
      do i=1,2
        read (3,'( a)') dummy          
      enddo        
      do iPer=1,nPer_UHS
        read (3,*) ii, per1, (sigma(iPer,iLevel), iLevel=1,nLevel)
      enddo  
      
c     Read correlation coefficient   
      do i=1,2
        read (3,'( a1)') dummy 
      enddo
      do iPer=1,nPer_UHS
        read (3,*) ii, per1, (rho(iPer,iLevel), iLevel=1,nLevel)
      enddo                 

  
!      pause 'end of read CMS' 
      
      close (3)
      return
      end

c -----------------------------------------------------------------------------
     
      subroutine Read_flatfile ( flatfile, period_UHS, nPer_UHS, nRecs, recSeq, eqid, staSN, 
     1      mag, dip, rake, rRup, rjb, Repi, rhypo, Rx, AZ, Vs30, dur, GMX_C1, GMX_C2, 
     1      GMX_C3, AI, minfreq, PGA, PGV, PGD, sa, MAXPER, period, nPer, 
     1      AccfilenameH1, AccfilenameH2, AccfilenameV, flag )  

      implicit none
      real mag(1), rRup(1), rJB(1), Rx(1), Repi(1), Rhypo(1), Az(1), Vs30(1), dip(1)
      real mag_temp, rRup_temp, rJB_temp, Rx_temp, Repi_temp, Rhypo_temp, Az_temp, 
     1      Vs30_temp, dip_temp
      real rake(1), dur(1), AI(1), PGA(1), PGV(1), PGD(1), period(1)
      real rake_temp, dur_temp, AI_temp, PGA_temp, PGV_temp, PGD_temp, period_temp(MAXPER)
      real minfreq(1), sa(MAXPER,1), period_UHS(1)
      real minfreq_temp, sa_temp(MAXPER)
      integer recSeq(1), eqid(1), StaSN(1), MAXPER
      integer recSeq_temp, eqid_temp, StaSN_temp
      character*80 dummy
      character*38 flatfile
      integer nPer, nPer_UHS
      real low_Per, high_Per
      integer flag, nPer_temp
      real Per1, Per2
      character GMX_c1(1), GMX_c2(1), GMX_c3(1)
      character GMX_c1_temp, GMX_c2_temp, GMX_c3_temp
      integer iTH, nRecs, eof, k, j
      character*100 AccfilenameH1(1), AccfilenameH2(1), AccfilenameV(1)
      character*100 AccfilenameH1_temp, AccfilenameH2_temp, AccfilenameV_temp
      
            
      open (10,file=flatfile, status='old')
      read (10,'( a1)') dummy
      if ( flag == 0 ) then
        read (10,*) nPer      
        read (10,*) ( period(k),k=1,nPer )
      elseif ( flag == 1 ) then
        read (10,*) nPer_temp    
        if ( nPer_temp .ne. nPer ) then
            stop 'nPer for the horizontal and vertical flatfiles should be equal'
        endif
        read (10,*) ( period_temp(k),k=1,nPer )  
        do k = 1,  nPer
            if ( period_temp(k) .ne. period(k) ) then
                stop 'Period vector for the horizontal and vertical flatfiles should be equal'
            endif   
        enddo     
      endif
      read (10,'( a1)') dummy
      
      low_Per = period_UHS(1)
      high_Per = period_UHS(nPer_UHS)  
      
      if ( low_Per < period(1) .or. high_Per > period(nPer) ) then
        stop 'TH period vector is smaller than UHS period vector.'
      endif    
      
            
      iTH = 1
      eof = 0
      do while ( eof == 0 )
        read ( 10,*, IOSTAT = eof ) recSeq_temp, eqid_temp, StaSN_temp, mag_temp, dip_temp, 
     1      rake_temp, rrup_temp, rJB_temp, Repi_temp, Rhypo_temp, rx_temp, 
     &      AZ_temp, vs30_temp, GMX_C1_temp, GMX_C2_temp, GMX_C3_temp, dur_temp, 
     1      AI_temp, minfreq_temp, pga_temp, pgv_temp, pgd_temp, 
     1      (sa_temp(j),j=1,nPer), AccfilenameH1_temp, AccfilenameH2_temp, AccfilenameV_temp
        if ( flag == 0 ) then
            recSeq(iTH) = recSeq_temp
            eqid(iTH)   = eqid_temp
            StaSN(iTH)  = StaSN_temp
            mag(iTH)    = mag_temp
            dip(iTH)    = dip_temp
            rake(iTH)   = rake_temp 
            rrup(iTH)   = rrup_temp 
            rJB(iTH)    = rJB_temp 
            Repi(iTH)   = Repi_temp 
            Rhypo(iTH)  = Rhypo_temp 
            rx(iTH)     = rx_temp 
            Az(iTH)     = AZ_temp 
            vs30(iTH)   = vs30_temp
            GMX_C1(iTH) = GMX_C1_temp
            GMX_C2(iTH) = GMX_C2_temp 
            GMX_C3(iTH) = GMX_C3_temp
            dur(iTH)    = dur_temp
            AI(iTH)     = AI_temp
            minfreq(iTH) = minfreq_temp
            pga(iTH)    = pga_temp
            PGV(iTH)    = pgv_temp
            pgd(iTH)    = pgd_temp
            do j = 1, nPer
                sa(j,iTH) = sa_temp(j)
            enddo
            AccfilenameH1(iTH) = AccfilenameH1_temp
            AccfilenameH2(iTH) = AccfilenameH2_temp
            AccfilenameV(iTH)  = AccfilenameV_temp              
        elseif ( flag == 1 ) then   ! horiz and vertical case
            if ( recSeq(iTH) .ne. recSeq_temp ) then
                stop 'recSeq vertical does not agree with recSeq horizontal.'
            endif
            minfreq(iTH) = minfreq_temp
            pga(iTH)    = pga_temp
            PGV(iTH)    = pgv_temp
            pgd(iTH)    = pgd_temp            
            do j = 1, nPer
                sa(j,iTH) = sa_temp(j)               
            enddo
        endif
        if ( eof == 0 ) iTH = iTH + 1         
      enddo
      
      nRecs = iTH - 1

      close (10)
      return
      end

c -----------------------------------------------------------------

      subroutine screen_TH ( HVFlag, Per1_loose, Per2_loose, iPer1_loose, iPer2_loose, 
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
     1      nPer, period, sa, saV, Per0, iPer0)
     
      implicit none
      integer MAXPER, nper_all, nPer_temp, nTH, nTH_temp
      integer recSeq_temp(1), eqid_temp(1), staSN_Temp(1), recSeq(1), 
     1      eqid(1), staSN(1), HVFlag, iPer1_loose, iPer2_loose,
     1      iPer1_strict, iPer2_strict, nPer_UHS, nPer
      real Per1_loose, Per2_loose, Per1_strict, Per2_strict, period_all(1), 
     1      period_temp(1), period_UHS(1), period(1)
      real mag_temp(1), dip_temp(1), rake_temp(1), rRup_temp(1), rjb_temp(1), Repi_temp(1), 
     1      rhypo_temp(1), Rx_temp(1), AZ_temp(1), Vs30_temp(1), dur_temp(1),
     1      AI_temp(1), minfreq_H_temp(1), minfreq_V_temp(1), PGA_temp(1), 
     1      PGV_temp(1), PGD_temp(1), PGA_V_temp(1), PGV_V_temp(1), PGD_V_temp(1),
     1      sa_temp(MAXPER,1), sa_V_temp(MAXPER,1), sa_all(MAXPER,1), saV_all(MAXPER,1),
     1      sa(MAXPER,1), saV(MAXPER,1)
      real  mag(1), dip(1), rake(1), rRup(1), rjb(1), Repi(1), rhypo(1), 
     1      Rx(1), AZ(1), Vs30(1), dur(1), AI(1), minfreq_H(1), minfreq_V(1), PGA(1), 
     1      PGV(1), PGD(1), PGA_V(1), PGV_V(1), PGD_V(1)
      character*100 AccfilenameH1_temp(1), AccfilenameH2_temp(1), AccfilenameV_temp(1),
     1      AccfilenameH1(1), AccfilenameH2(1), AccfilenameV(1)
      character GMX_C1(1), GMX_C2(1), GMX_C3(1), GMX_C1_temp(1), GMX_C2_temp(1), 
     1      GMX_C3_temp(1)
      real Mmin, Mmax, distflag, Rmin, Rmax, Vs30min, Vs30max,  
     1      durmin, durmax, PGVmin, PGVmax, AImin, AImax
      integer i, j, k, iTH, iPer1_all, iPer2_all, iPer0
      real low_per, high_per, Per0
      
c     Define the extent of the given UHS period range and the period ranges for strict 
c     and loose matching with respect to the period vector of the TH corresponding to
c     the UHS
      low_Per = period_UHS(1)
      high_Per = period_UHS(nPer_UHS)
      do i = 1, nPer_temp   
        if ( period_temp(i) .le. low_Per )  iPer1_all = i   ! UHS period range
        if ( period_temp(i) .le. high_Per ) iPer2_all = i 
      enddo
      
      ! Save period vector for UHS corresponding to the TH periods
      k = 0
      do i = iPer1_all, iPer2_all
        k = k + 1
        period_all(k) = period_temp(i)
      enddo
      nPer_all =  k
              
      ! Set the pointers of the loose and strict period vectors with respect
      ! to the period_all vector 
      do i = 1, nPer_all
        if ( period_all(i) .le. Per1_loose )     iPer1_loose = i   ! Period range for loose matching
        if ( period_all(i) .le. Per2_loose )     iPer2_loose = i    
        if ( period_all(i) .le. Per1_strict )    iPer1_strict = i   ! Period range for strict matching
        if ( period_all(i) .le. Per2_strict )    iPer2_strict = i                         
      enddo 
      
      ! Save period vector for loose matching range corresponding to the TH periods
      k = 0
      do i = iPer1_loose, iPer2_loose
        k = k + 1
        period(k) = period_all(i)
      enddo
      nPer =  k     
      
c     Set pointer to reference period with respect to the loose matching period vector  
      do i = 1, nPer
        if ( abs(Per0 - Period(i)) < 0.001 ) then  
            iPer0 = i
!            write (*,'(A54,f6.4,A6,f6.4)') ' Closest matching period of flatfile to cond. period (',Per0,') is: ',Period(i)
            goto 11
        endif 
      enddo       
      write(*,*) 'Conditioning period should be an element of the matching period vector.'
      stop
11    continue           
     
      iTH = 1
      do i = 1, nTH_temp
        if ( mag_temp(i) < Mmin .or. mag_temp(i) > Mmax )   goto 50
        if ( DistFlag == 1 ) then
            if ( rrup_temp(i) < Rmin .or. rrup_temp(i) > Rmax ) goto 50
        elseif ( DistFlag == 2 ) then
            if ( rjb_temp(i) < Rmin .or. rjb_temp(i) > Rmax ) goto 50
        endif            
        if ( vs30_temp(i) < Vs30min .or. vs30_temp(i) > Vs30max ) goto 50
        if ( dur_temp(i) < Durmin .or. dur_temp(i) > Durmax ) goto 50
        if ( AI_temp(i) < AImin .or. AI_temp(i) > AImax )  goto 50

        if ( minfreq_H_temp(i) .ne. -999 .and. minfreq_H_temp(i) > 1./Per2_loose )  goto 50     ! Check minimum usable frequency

!c ----  START OF TEMPORARY BLOCK----------------          
            if ( GMX_c1_temp(i) == 'C' .or. GMX_c1_temp(i) == 'D' .or. GMX_c1_temp(i) == 'E'
     &          .or. GMX_c1_temp(i) == 'F' .or. GMX_c1_temp(i) == 'G' ) goto 50          ! Instrument location check
!     
!        if ( recSeq_temp(i) > 3551 ) then
!            if ( recSeq_temp(i) .ne. 4083 .and. recSeq_temp(i) .ne. 4098 .and. recSeq_temp(i) .ne. 4117
!     1      .and. recSeq_temp(i) .ne. 4132 .and. recSeq_temp(i) .ne. 4138 .and. recSeq_temp(i) .ne. 4139
!     2      .and. recSeq_temp(i) .ne. 4145 .and. recSeq_temp(i) .ne. 6886 .and. recSeq_temp(i) .ne. 6893
!     3      .and. recSeq_temp(i) .ne. 6969 .and. recSeq_temp(i) .ne. 8119 .and. recSeq_temp(i) .ne. 8124
!     4      .and. recSeq_temp(i) .ne. 8142 ) goto 50
!        endif        
!     
!c ----  END OF TEMPORARY BLOCK----------------             
        
        do j = iPer1_all, iPer2_all     !! Save Sa in the UHS period range
            if ( Sa_temp(j,i) <= 0.0 )    goto 50
            if ( HVFlag == 0 ) then
                if ( Sa_V_temp(j,i) <= 0.0 )    goto 50
            endif
            k = j - iPer1_all + 1
            Sa_all(k,iTH) = Sa_temp(j,i)       
            SaV_all(k,iTH) = Sa_V_temp(j,i)
        enddo
        
        do j = iPer1_loose, iPer2_loose     !! Save Sa in the loose matching period range
            k = j - iPer1_loose + 1
            Sa(k,iTH) = Sa_all(j,iTH)               
            SaV(k,iTH) = SaV_all(j,iTH)
        enddo     
        
        recSeq(iTH) = recSeq_temp(i)  
        eqid(iTH) = eqid_temp(i)
        staSN(iTH) = staSN_temp(i)
        mag(iTH) = mag_temp(i)
        dip(iTH) = dip_temp(i)
        rake(iTH) = rake_temp(i)
        rRup(iTH) = rRup_temp(i)
        rjb(iTH) = rjb_temp(i)
        Repi(iTH) = Repi_temp(i)
        rhypo(iTH) = rhypo_temp(i)               
        Rx(iTH) = Rx_temp(i)
        AZ(iTH) = AZ_temp(i)
        Vs30(iTH) = Vs30_temp(i)
        dur(iTH) = dur_temp(i)
        GMX_C1(iTH) = GMX_C1_temp(i)
        GMX_C2(iTH) = GMX_C2_temp(i)
        GMX_C3(iTH) = GMX_C3_temp(i)
        AI(iTH) = AI_temp(i)
        minfreq_H(iTH) = minfreq_H_temp(i)
        AccfilenameH1(iTH) = AccfilenameH1_temp(i)
        AccfilenameH2(iTH) = AccfilenameH2_temp(i)
        AccfilenameV(iTH) = AccfilenameV_temp(i)
        PGA(iTH) = PGA_temp(i)
        PGV(iTH) = PGV_temp(i)
        PGD(iTH) = PGD_temp(i)
        
        if ( HVFlag == 0 ) then
            minfreq_V(iTH) = minfreq_V_temp(i)
            PGA_V(iTH) = PGA_V_temp(i)
            PGV_V(iTH) = PGV_V_temp(i)
            PGD_V(iTH) = PGD_V_temp(i)
        endif       

        iTH = iTH + 1

50      continue
      enddo
      
      nTH = iTH - 1
      
      
     
      end
c -----------------------------------------------------------------     

               