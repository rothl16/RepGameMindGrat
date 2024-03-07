* Encoding: UTF-8.
USE ALL.
COMPUTE filter_$=(filtrCOMP_opuszcz_odp = 0).
VARIABLE LABELS filter_$ 'filtrCOMP_opuszcz_odp = 0 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

*Scales
    
*sexism

COMPUTE sexT_t0=MEAN(sexism3_t0, sexism6_t0, sexism7_t0, sexism8_t0, sexism9_t0, sexism12_t0, sexism1_t0,
   sexism2_t0, sexism4_t0, sexism5_t0, sexism10_t0, sexism11_t0).
Execute.

RELIABILITY
  /VARIABLES= sexism3_t0, sexism6_t0, sexism7_t0, sexism8_t0, sexism9_t0, sexism12_t0, sexism1_t0,
   sexism2_t0, sexism4_t0, sexism5_t0, sexism10_t0, sexism11_t0
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
    /SUMMARY=TOTAL.

COMPUTE sexT_t1=MEAN(sexism3_t1, sexism6_t1, sexism7_t1, sexism8_t1, sexism9_t1, sexism12_t1, sexism1_t1,
   sexism2_t1, sexism4_t1, sexism5_t1, sexism10_t1, sexism11_t1).
Execute.

RELIABILITY
  /VARIABLES= sexism3_t1, sexism6_t1, sexism7_t1, sexism8_t1, sexism9_t1, sexism12_t1, sexism1_t1,
   sexism2_t1, sexism4_t1, sexism5_t1, sexism10_t1, sexism11_t1
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
    /SUMMARY=TOTAL.

*cn

COMPUTE CN_t0 = MEAN(cn1_t0, cn2_t0, cn3_t0, cn4_t0, cn5_t0).
EXECUTE. 

RELIABILITY
  /VARIABLES= cn1_t0, cn2_t0, cn3_t0, cn4_t0, cn5_t0
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
    /SUMMARY=TOTAL.

COMPUTE CN_t1 = MEAN(cn1_t1, cn2_t1, cn3_t1, cn4_t1, cn5_t1).
EXECUTE.

RELIABILITY
  /VARIABLES= cn1_t1, cn2_t1, cn3_t1, cn4_t1, cn5_t1
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
    /SUMMARY=TOTAL.

RECODE ffmq1_t0 ffmq4_t0 ffmq5_t0 ffmq7_t0 ffmq8_t0 ffmq10_t0 ffmq13_t0 ffmq15_t0 ffmq16_t0 ffmq17_t0 ffmq22_t0(1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
    INTO ffmq1_t0r ffmq4_t0r ffmq5_t0r ffmq7_t0r ffmq8_t0r ffmq10_t0r ffmq13_t0r ffmq15_t0r ffmq16_t0r ffmq17_t0r ffmq22_t0r.
    
COMPUTE FFMQ_t0= MEAN(ffmq4_t0r, ffmq13_t0r, ffmq16_t0r, ffmq7_t0r, ffmq5_t0r,
    ffmq8_t0r, ffmq15_t0r, ffmq17_t0r, ffmq22_t0r, ffmq1_t0r, ffmq2_t0, ffmq3_t0, 
    ffmq6_t0, ffmq9_t0, ffmq10_t0r, ffmq11_t0, ffmq12_t0, ffmq14_t0, ffmq18_t0, ffmq19_t0, 
    ffmq20_t0, ffmq21_t0).
EXECUTE.

RELIABILITY
  /VARIABLES= ffmq4_t0r  ffmq13_t0r  ffmq16_t0r  ffmq7_t0r  ffmq5_t0r  
    ffmq8_t0r  ffmq15_t0r  ffmq17_t0r  ffmq22_t0r  ffmq1_t0r  ffmq2_t0  ffmq3_t0  
    ffmq6_t0 ffmq9_t0  ffmq10_t0r  ffmq11_t0  ffmq12_t0  ffmq14_t0   ffmq18_t0  ffmq19_t0  
    ffmq20_t0  ffmq21_t0
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
    /SUMMARY=TOTAL.

RECODE ffmq1_t1 ffmq4_t1 ffmq5_t1 ffmq7_t1 ffmq8_t1 ffmq10_t1 ffmq13_t1 ffmq15_t1 ffmq16_t1 ffmq17_t1 ffmq22_t1(1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
    INTO ffmq1_t1r ffmq4_t1r ffmq5_t1r ffmq7_t1r ffmq8_t1r ffmq10_t1r ffmq13_t1r ffmq15_t1r ffmq16_t1r ffmq17_t1r ffmq22_t1r.
EXECUTE.

COMPUTE FFMQ_t1=MEAN (ffmq4_t1r, ffmq13_t1r, ffmq16_t1r, ffmq7_t1r, ffmq5_t1r, 
    ffmq8_t1r, ffmq15_t1r, ffmq17_t1r, ffmq22_t1r, ffmq1_t1r, ffmq2_t1, ffmq3_t1, 
    ffmq6_t1, ffmq9_t1, ffmq10_t1r, ffmq11_t1, ffmq12_t1, ffmq14_t1, ffmq18_t1, ffmq19_t1, 
    ffmq20_t1, ffmq21_t1).
EXECUTE.

RELIABILITY
  /VARIABLES= ffmq4_t1r  ffmq13_t1r  ffmq16_t1r  ffmq7_t1r  ffmq5_t1r  
    ffmq8_t1r  ffmq15_t1r  ffmq17_t1r  ffmq22_t1r  ffmq1_t1r  ffmq2_t1  ffmq3_t1  
    ffmq6_t1 ffmq9_t1  ffmq10_t1r  ffmq11_t1  ffmq12_t1  ffmq14_t1   ffmq18_t1  ffmq19_t1  
    ffmq20_t1  ffmq21_t1
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
    /SUMMARY=TOTAL.

*Gratitude

COMPUTE grat_t0= MEAN(grat1_t0, grat2_t0, grat3_t0).
    EXECUTE.

RELIABILITY
  /VARIABLES= grat1_t0, grat2_t0, grat3_t0
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
    /SUMMARY=TOTAL.

COMPUTE grat_t1= MEAN(grat1_t1, grat2_t1, grat3_t1).
    EXECUTE.

RELIABILITY
  /VARIABLES= grat1_t1, grat2_t1, grat3_t1
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
    /SUMMARY=TOTAL.

**Ukrainina prejudice

*intergroup threat
    
COMPUTE igth_t0=MEAN(igr_thr1_t0, igr_thr2_t0, igr_thr3_t0, igr_thr4_t0, igr_thr5_t0, igr_thr6_t0, igr_thr7_t0, igr_thr8_t0, igr_thr9_t0, igr_thr10_t0).
EXECUTE.

RELIABILITY
  /VARIABLES= igr_thr1_t0, igr_thr2_t0, igr_thr3_t0, igr_thr4_t0, igr_thr5_t0, igr_thr6_t0, igr_thr7_t0, igr_thr8_t0, igr_thr9_t0, igr_thr10_t0
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
    /SUMMARY=TOTAL.

COMPUTE igth_t1=MEAN(igr_thr1_t1, igr_thr2_t1, igr_thr3_t1, igr_thr4_t1, igr_thr5_t1, igr_thr6_t1, igr_thr7_t1, igr_thr8_t1, igr_thr9_t1, igr_thr10_t1).
EXECUTE.

RELIABILITY
  /VARIABLES= igr_thr1_t1, igr_thr2_t1, igr_thr3_t1, igr_thr4_t1, igr_thr5_t1, igr_thr6_t1, igr_thr7_t1, igr_thr8_t1, igr_thr9_t1, igr_thr10_t1
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
    /SUMMARY=TOTAL.


*social distance

RECODE socd1_t0 socd2_t0 socd3_t0 socd4_t0(1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO socd1r_t0 socd2r_t0 socd3r_t0 socd4r_t0.
EXECUTE.

COMPUTE socd_t0= MEAN(socd1r_t0, socd2r_t0, socd3r_t0, socd4r_t0, socd5_t0, socd6_t0).
EXECUTE.

RELIABILITY
  /VARIABLES= socd1r_t0, socd2r_t0, socd3r_t0, socd4r_t0, socd5_t0, socd6_t0
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
    /SUMMARY=TOTAL.

RECODE socd1_t1 socd2_t1 socd3_t1 socd4_t1 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO socd1r_t1 socd2r_t1 socd3r_t1 socd4r_t1.
EXECUTE.
    
COMPUTE socd_t1= MEAN(socd1r_t1, socd2r_t1, socd3r_t1, socd4r_t1, socd5_t1, socd6_t1).
EXECUTE.

RELIABILITY
  /VARIABLES= socd1r_t1, socd2r_t1, socd3r_t1, socd4r_t1, socd5_t1, socd6_t1
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
    /SUMMARY=TOTAL.

*PrejUk
    
COMPUTE PreUK_t0 = MEAN (socd_t0, igth_t0).
    
COMPUTE PreUK_t1 = MEAN (socd_t1, igth_t1).

*anitsemitism
    
COMPUTE ant_t0 = MEAN (ant_sem7_t0, ant_sem8_t0, ant_sem9_t0, ant_sem10_t0, ant_sem11_t0, ant_sem12_t0, ant_sem3_t0, 
  ant_sem4_t0, ant_sem5_t0, ant_sem6_t0, ant_sem1_t0, ant_sem2_t0).

RELIABILITY
  /VARIABLES= ant_sem7_t0, ant_sem8_t0, ant_sem9_t0, ant_sem10_t0, ant_sem11_t0, ant_sem12_t0, ant_sem3_t0, 
  ant_sem4_t0, ant_sem5_t0, ant_sem6_t0, ant_sem1_t0, ant_sem2_t0
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
    /SUMMARY=TOTAL.

COMPUTE ant_t1 = MEAN (ant_sem7_t1, ant_sem8_t1, ant_sem9_t1, ant_sem10_t1, ant_sem11_t1, ant_sem12_t1, ant_sem3_t1, 
  ant_sem4_t1, ant_sem5_t1, ant_sem6_t1, ant_sem1_t1, ant_sem2_t1).

RELIABILITY
  /VARIABLES= ant_sem7_t1, ant_sem8_t1, ant_sem9_t1, ant_sem10_t1, ant_sem11_t1, ant_sem12_t1, ant_sem3_t1, 
  ant_sem4_t1, ant_sem5_t1, ant_sem6_t1, ant_sem1_t1, ant_sem2_t1
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
    /SUMMARY=TOTAL.

*homophobia
    
COMPUTE homo_t0=MEAN(homo1_t0, homo2_t0, homo3_t0).
EXECUTE.
    
RELIABILITY
/VARIABLES= homo1_t0, homo2_t0, homo3_t0
/SCALE('ALL VARIABLES') ALL
/MODEL=ALPHA
/SUMMARY=TOTAL.
    
COMPUTE homo_t1=MEAN(homo1_t1, homo2_t1, homo3_t1).
EXECUTE.
    
RELIABILITY
  /VARIABLES= homo1_t1, homo2_t1, homo3_t1
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
    /SUMMARY=TOTAL.
    
*Ingroup satisfaction
    
COMPUTE IS_t0 = MEAN(is1_t0, is2_t0, is3_t0, is4_t0).
    EXECUTE.
    
RELIABILITY
  /VARIABLES= is1_t0, is2_t0, is3_t0, is4_t0
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
    /SUMMARY=TOTAL.

COMPUTE IS_t1 = MEAN(is1_t1, is2_t1, is3_t1, is4_t1).
    EXECUTE.

RELIABILITY
  /VARIABLES= is1_t1, is2_t1, is3_t1, is4_t1
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
    /SUMMARY=TOTAL.

*Right-wing authoritarianism
    
 RECODE rwa5_t0 rwa8_t0 rwa9_t0 rwa10_t0 rwa11_t0 rwa12_t0 rwa13_t0 rwa14_t0 rwa15_t0 rwa18_t0 rwa20_t0 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
    INTO rwa5r_t0 rwa8r_t0 rwa9r_t0 rwa10r_t0 rwa11r_t0 rwa12r_t0 rwa13r_t0 rwa14r_t0 rwa15r_t0 rwa18r_t0 rwa20r_t0.
EXECUTE.
    
COMPUTE RWA_t0=MEAN(rwa1_t0, rwa2_t0, rwa3_t0, rwa4_t0, rwa5r_t0, rwa6_t0, rwa7_t0, rwa8r_t0, rwa9r_t0, rwa10r_t0, rwa11r_t0, rwa12r_t0, rwa13r_t0, rwa14r_t0, rwa15r_t0, rwa16_t0, rwa17_t0, rwa18r_t0, rwa19_t0, rwa20r_t0).
EXECUTE.

RELIABILITY
  /VARIABLES= rwa1_t0, rwa2_t0, rwa3_t0, rwa4_t0, rwa5r_t0, rwa6_t0, rwa7_t0, rwa8r_t0, rwa9r_t0, rwa10r_t0, rwa11r_t0, rwa12r_t0, rwa13r_t0, rwa14r_t0, rwa15r_t0, rwa16_t0, rwa17_t0, rwa18r_t0, rwa19_t0, rwa20r_t0
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
    /SUMMARY=TOTAL.
 
 RECODE rwa5_t1 rwa8_t1 rwa9_t1 rwa10_t1 rwa11_t1 rwa12_t1 rwa13_t1 rwa14_t1 rwa15_t1 rwa18_t1 rwa20_t1(1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) 
    INTO rwa5r_t1 rwa8r_t1 rwa9r_t1 rwa10r_t1 rwa11r_t1 rwa12r_t1 rwa13r_t1 rwa14r_t1 rwa15r_t1 rwa18r_t1 rwa20r_t1. 
EXECUTE.

COMPUTE RWA_t1=MEAN(rwa1_t1, rwa2_t1, rwa3_t1, rwa4_t1, rwa5r_t1, rwa6_t1, rwa7_t1, rwa8r_t1, rwa9r_t1, rwa10r_t1, rwa11r_t1, rwa12r_t1, rwa13r_t1, rwa14r_t1, rwa15r_t1, rwa16_t1, rwa17_t1, rwa18r_t1, rwa19_t1, rwa20r_t1).
EXECUTE.
 
RELIABILITY
  /VARIABLES= rwa1_t1, rwa2_t1, rwa3_t1, rwa4_t1, rwa5r_t1, rwa6_t1, rwa7_t1, rwa8r_t1, rwa9r_t1, rwa10r_t1, rwa11r_t1, rwa12r_t1, rwa13r_t1, rwa14r_t1, rwa15r_t1, rwa16_t1, rwa17_t1, rwa18r_t1, rwa19_t1, rwa20r_t1
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
    /SUMMARY=TOTAL.


    
*Social-dominance orientation
    
RECODE sdo1_t0 sdo3_t0 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO sdo1r_t0 sdo3r_t0.
EXECUTE.

COMPUTE sdo_t0=MEAN(sdo1r_t0, sdo2_t0, sdo3r_t0, sdo4_t0).
EXECUTE.

RELIABILITY
  /VARIABLES= sdo1r_t0, sdo2_t0, sdo3r_t0, sdo4_t0
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
    /SUMMARY=TOTAL.

RECODE sdo1_t1 sdo3_t1 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) INTO sdo1r_t1 sdo3r_t1.
EXECUTE.

COMPUTE sdo_t1=MEAN(sdo1r_t1, sdo2_t1, sdo3r_t1, sdo4_t1).
EXECUTE.

RELIABILITY
  /VARIABLES= sdo1r_t1, sdo2_t1, sdo3r_t1, sdo4_t1
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
    /SUMMARY=TOTAL.
 
*Individual Narcissism
    
COMPUTE NPI_t0=MEAN(NPI1_t0, NPI2_t0, NPI3_t0, NPI4_t0, NPI5_t0, NPI6_t0, NPI7_t0, NPI8_t0, NPI9_t0, NPI10_t0, NPI11_t0, NPI12_t0, NPI13_t0).
EXECUTE.

RELIABILITY
  /VARIABLES= NPI1_t0, NPI2_t0, NPI3_t0, NPI4_t0, NPI5_t0, NPI6_t0, NPI7_t0, NPI8_t0, NPI9_t0, NPI10_t0, NPI11_t0, NPI12_t0, NPI13_t0
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
    /SUMMARY=TOTAL.
    
COMPUTE NPI_t1=MEAN(NPI1_t1, NPI2_t1, NPI3_t1, NPI4_t1, NPI5_t1, NPI6_t1, NPI7_t1, NPI8_t1, NPI9_t1, NPI10_t1, NPI11_t1, NPI12_t1, NPI13_t1).
EXECUTE.
    
RELIABILITY
  /VARIABLES= NPI1_t1, NPI2_t1, NPI3_t1, NPI4_t1, NPI5_t1, NPI6_t1, NPI7_t1, NPI8_t1, NPI9_t1, NPI10_t1, NPI11_t1, NPI12_t1, NPI13_t1
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
    /SUMMARY=TOTAL.

*general prejudice
  
COMPUTE genPr_t0 = MEAN (sext_t0, preuk_t0, ant_t0, homo_t0).

COMPUTE genPr_t1 = MEAN (sext_t1, preuk_t1, ant_t1, homo_t1).

*Main text

DESCRIPTIVES VARIABLES=age 
  /STATISTICS=MEAN STDDEV.

FREQUENCIES VARIABLES=gender
  /ORDER=ANALYSIS.

CROSSTABS
  /TABLES=pract0 gender BY group
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ PHI 
  /CELLS=COUNT COLUMN
  /COUNT ROUND CELL.

T-TEST GROUPS=group(0 1)
  /MISSING=ANALYSIS
  /VARIABLES=age cn_t0 sext_t0 preUK_t0 ant_t0 homo_t0
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

DESCRIPTIVES VARIABLES=cn_t0 sext_t0 preUK_t0 ant_t0 homo_t0 cn_t1 sext_t1 preUK_t1 ant_t1 homo_t1
  /STATISTICS=MEAN STDDEV.


CORRELATIONS
  /VARIABLES=CN_t0 sexT_t0 homo_t0 ant_t0 PreUK_t0 PrejT0 FFMQ_t0 grat_t0 NPI_t0 SI_t0 sdo_t0 RWA_t0 
  /PRINT=TWOTAIL NOSIG FULL
    /CI CILEVEL(95)
  /MISSING=PAIRWISE.


CORRELATIONS
  /VARIABLES=CN_t1 sexT_t1 homo_t1 ant_t1 PreUK_t1 PrejT1 FFMQ_t1 grat_t1 NPI_t1 SI_t1 sdo_t1 RWA_t1 
  /PRINT=TWOTAIL NOSIG FULL
    /CI CILEVEL(95)
  /MISSING=PAIRWISE.





*SM
    

T-TEST GROUPS=group(0 1)
  /MISSING=ANALYSIS
  /VARIABLES=ffmq_t1 grat_t1 IS_t1 RWA_t1 SDO_t1 NPI_t1
  /ES DISPLAY(TRUE)
  /CRITERIA=CI(.95).

GLM CN_t0 CN_t1 BY group
  /WSFACTOR=czas 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(czas*group) TYPE=BAR ERRORBAR=CI MEANREFERENCE=NO
  /EMMEANS=TABLES(group) COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(czas) COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(group*czas) COMPARE(group) ADJ(BONFERRONI)
  /EMMEANS=TABLES(group*czas) COMPARE(czas) ADJ(BONFERRONI)
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=czas 
  /DESIGN=group.

GLM FFMQ_t0 FFMQ_t1 BY group
  /WSFACTOR=czas 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(czas*group) TYPE=BAR ERRORBAR=CI MEANREFERENCE=NO
  /EMMEANS=TABLES(group) COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(czas) COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(group*czas) COMPARE(group) ADJ(BONFERRONI)
  /EMMEANS=TABLES(group*czas) COMPARE(czas) ADJ(BONFERRONI)
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=czas 
  /DESIGN=group.

GLM  grat_t0 grat_t1 BY group
  /WSFACTOR=czas 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(czas*group) TYPE=BAR ERRORBAR=CI MEANREFERENCE=NO
  /EMMEANS=TABLES(group) COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(czas) COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(group*czas) COMPARE(group) ADJ(BONFERRONI)
  /EMMEANS=TABLES(group*czas) COMPARE(czas) ADJ(BONFERRONI)
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=czas 
  /DESIGN=group.

GLM IS_t0 IS_t1 BY group
  /WSFACTOR=czas 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(czas*group) TYPE=BAR ERRORBAR=CI MEANREFERENCE=NO
  /EMMEANS=TABLES(group) COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(czas) COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(group*czas) COMPARE(group) ADJ(BONFERRONI)
  /EMMEANS=TABLES(group*czas) COMPARE(czas) ADJ(BONFERRONI)
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=czas 
  /DESIGN=group.

GLM RWA_t0 RWA_t1 BY group
  /WSFACTOR=czas 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(czas*group) TYPE=BAR ERRORBAR=CI MEANREFERENCE=NO
  /EMMEANS=TABLES(group) COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(czas) COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(group*czas) COMPARE(group) ADJ(BONFERRONI)
  /EMMEANS=TABLES(group*czas) COMPARE(czas) ADJ(BONFERRONI)
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=czas 
  /DESIGN=group.

GLM SDO_t0 SDO_t1 BY group
  /WSFACTOR=czas 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(czas*group) TYPE=BAR ERRORBAR=CI MEANREFERENCE=NO
  /EMMEANS=TABLES(group) COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(czas) COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(group*czas) COMPARE(group) ADJ(BONFERRONI)
  /EMMEANS=TABLES(group*czas) COMPARE(czas) ADJ(BONFERRONI)
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=czas 
  /DESIGN=group.

GLM NPI_t0 NPI_t1 BY group
  /WSFACTOR=czas 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(czas*group) TYPE=BAR ERRORBAR=CI MEANREFERENCE=NO
  /EMMEANS=TABLES(group) COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(czas) COMPARE ADJ(BONFERRONI)
  /EMMEANS=TABLES(group*czas) COMPARE(group) ADJ(BONFERRONI)
  /EMMEANS=TABLES(group*czas) COMPARE(czas) ADJ(BONFERRONI)
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=czas 
  /DESIGN=group.




