      Module Declare
      
      integer, parameter :: MAX_TH=30000, MAXPER=120, MAXLEVEL=50
      
      real Mmin, Mmax, Rmin, Rmax, Vs30min, Vs30max, durmin, durmax, 
     1  AImin, AImax, PGVmin, PGVmax
      
      real sa(MAXPER,MAX_TH), period(MAXPER),  period_all(MAXPER),
     1  corr(MAXPER), sa_all(MAXPER,MAX_TH), saV_all(MAXPER,MAX_TH), 
     5  saV(MAXPER,MAX_TH)     
     
      integer recSeq(MAX_TH), eqid(MAX_TH), nTH, nPer, nPer_all, 
     1  k, i, nLevel, nPer_UHS, StaSN(MAX_TH)
     
      integer iPer, iLevel, iSelect(MAXLEVEL,MAX_TH), nSelect(MAXLEVEL),
     1  iset_save(MAXLEVEL,MAX_TH), index_final(MAX_TH), N1(MAXLEVEL), 
     1  nMonte, nZ, nFit, iPer0, iPer1_loose, iPer2_loose, index_final_unique(MAX_TH)
     
      real hazLevel(MAXLEVEL), UHS(MAXPER, MAXLEVEL), UHS_all(MAXPER, MAXLEVEL), 
     1  UHSv_all(MAXPER, MAXLEVEL), UHSv(MAXPER, MAXLEVEL) 
    
      real cms(MAXPER, MAXLEVEL), cms_all(MAXPER, MAXLEVEL), CMSv(MAXPER, MAXLEVEL), 
     1  CMSv_all(MAXPER, MAXLEVEL), sigma(MAXPER, MAXLEVEL), rho(MAXPER, MAXLEVEL), 
     1  sigma_all(MAXPER, MAXLEVEL), rho_all(MAXPER, MAXLEVEL), sigma_cms(MAXPER, MAXLEVEL), 
     1  SigmaV(MAXPER, MAXLEVEL), rhoV(MAXPER, MAXLEVEL), SigmaV_all(MAXPER, MAXLEVEL), 
     1  rhoV_all(MAXPER, MAXLEVEL)
     
      real period_UHS(MAXPER), rate(MAX_TH), scale_final(MAX_TH), 
     1  scale_final_v(MAX_TH), rate0(MAX_TH)
     
      integer nUnique, iTH, nSet, level(MAX_TH), high_Per, nlevel_TH, nPer_VH
      
      real UHS_comp(MAXPER, MAXLEVEL), Z(MAXPER, MAXLEVEL),
     1  Z_v(MAXPER, MAXLEVEL), UHS_V_comp(MAXPER, MAXLEVEL)
     
!      real*8 avemisfit_level, maxmisfit_level, avemisfit_v_level, 
!     1  maxmisfit_v_level, 
       real*8 misfit, misfit_v
     
      integer iPer_max, iPer_v_max, iLevel_targ, nPer_cor1   
      
      character*48 Horiz_flatfile, Vert_flatfile, UHSh_file, UHSv_file, 
     1  CMSh_file, CMSv_file, sum_outfile, CS_outfile, SaH_outfile,
     1  SaV_outfile, Haz_outfile, UHS_outfile, THnames_outfile,
     1  HazV_outfile, UHSV_outfile 
     
      real Per0, Per1_loose, Per2_loose, delta, maxAdj, Per1_strict, 
     1  Per2_strict, cor1(MAXPER), period_cor1(MAXPER)
        
      integer n, iPer1_strict, iPer2_strict, nGMPEs
      
      integer HVFlag, iZ, distflag 
      
      real mag(MAX_TH), rRup(MAX_TH), Vs30(MAX_TH), 
     1      dip(MAX_TH), rake(MAX_TH),   
     1      AZ(MAX_TH), AI(MAX_TH), dur(MAX_TH), PGA(MAX_TH),
     1      PGV(MAX_TH), PGD(MAX_TH), minfreq_H(MAX_TH), 
     1      PGA_V(MAX_TH), PGV_V(MAX_TH), PGD_V(MAX_TH),
     1      minfreq_V(MAX_TH), sa_V(MAXPER,MAX_TH), 
     1      Rjb(MAX_TH), Rx(MAX_TH), Repi(MAX_TH), Rhypo(MAX_TH)
     
      character GMX_C1(MAX_TH), GMX_C2(MAX_TH), GMX_C3(MAX_TH)      
      
      character*100 AccfilenameH1(MAX_TH), AccfilenameH2(MAX_TH), 
     1      AccfilenameV(MAX_TH)
      
      integer iPermax_level(MAXLEVEL), iPermax_V_level(MAXLEVEL)
      double precision avemisfit_level(MAXLEVEL), maxmisfit_level(MAXLEVEL), 
     1      avemisfit_V_level(MAXLEVEL), maxmisfit_V_level(MAXLEVEL), 
     2      avemisfit_level_main(MAXLEVEL), avemisfit_level_second(MAXLEVEL), 
     3      avemisfit_V_level_main(MAXLEVEL), avemisfit_V_level_second(MAXLEVEL)      
     
      
      
      
      real mag_temp(MAX_TH), rRup_temp(MAX_TH), Vs30_temp(MAX_TH), 
     1      sa_temp(MAXPER,MAX_TH), dip_temp(MAX_TH), rake_temp(MAX_TH),   
     1      AZ_temp(MAX_TH), AI_temp(MAX_TH), dur_temp(MAX_TH), PGA_temp(MAX_TH),
     1      PGV_temp(MAX_TH), PGD_temp(MAX_TH), minfreq_H_temp(MAX_TH), 
     1      PGA_V_temp(MAX_TH), PGV_V_temp(MAX_TH), PGD_V_temp(MAX_TH),
     1      minfreq_V_temp(MAX_TH), sa_V_temp(MAXPER,MAX_TH), period_temp(MAXPER),
     1      Rjb_temp(MAX_TH), Rx_temp(MAX_TH), Repi_temp(MAX_TH), Rhypo_temp(MAX_TH)       
      integer nTH_temp, recSeq_temp(MAX_TH), eqid_temp(MAX_TH), StaSN_temp(MAX_TH)
      integer  nPer_UHS_temp, nPer_temp, nLevel_temp
      character GMX_C1_temp(MAX_TH), GMX_C2_temp(MAX_TH), GMX_C3_temp(MAX_TH)
      character*100 AccfilenameH1_temp(MAX_TH), AccfilenameH2_temp(MAX_TH), 
     1      AccfilenameV_temp(MAX_TH)
      real hazLevel_temp(MAXLEVEL), period_UHS_temp(MAXPER)
      
      
      end Module Declare