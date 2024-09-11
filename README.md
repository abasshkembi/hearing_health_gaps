## hearing_health_gaps
multilevel regression and post-stratification analysis to document the gaps in the primary, secondary, and tertiary public health prevention of hearing loss in the US

--------------------------------------------------------------------------------

*Code for*
**Shkembi A**, Smith LM, Zhang X, DePalma G, Green G, **Neitzel RL**. The Unmet Needs of Hearing Health across America: small area estimation modelling of noise pollution, hearing screening, and hearing aid use from the Apple Hearing Study, 2019-2022.

I (Abas Shkembi) conducted the analysis. For questions about this analysis, you can reach me at ashkembi@umich.edu. Richard Neitzel is the PI of the Apple Hearing Study. For questions about the Apple Hearing Study as a whole, you can reach him at rneitzel@umich.edu.

This work was supported by Apple Inc.

### R code

The R scripts in this repository cannot be used to replicate our study findings, as the data is proprietary. However, the scripts document the steps for using for processing the data, running the MRP models, and analyzing the results.

  * **01A_data_preprocessing.R** -  Cleans participant demographics, survey responses, and environmental noise levels
  * **01_data_preprocessing.R** - ACS/BLS data on county demographics, income, and audiologist concentration for post-stratification after multilevel regression
  * **02A_noise_model.R** - Model construction for Small Area Estimation for noise overexposure in Cohort One
  * **02B_hearingAbility_model.R** - Model construction for Small Area Estimation for poor/fair hearing ability in Cohort One
  * **02C_hearingLossDx_model.R** - Model construction for Small Area Estimation for diagnosed hearing loss in Cohort One
  * **02D_hearingAidUse_model.R** - Model construction for Small Area Estimation for using hearing aids/cochlear implants in Cohort One
  * **02E_allOtherModels_model.R** - Model construction for (1) no hearing test and (2) no hearing aid use in Cohort One
  * **03_table1.R** - Nationwide estimation of prevalence and cases of four main outcomes for the construction of Table 1
  * **04_figures.R** - County-level estimation of four main outcomes for paper figures
  * **05_supplement_countyStateEstimates.R** - county and state-level breakdowns of main outcomes for Appendix D
