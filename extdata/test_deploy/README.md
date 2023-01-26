# dfEDSobfuscated:
---

This dataset contains data from the University Hospital of Bordeaux (emergency units, RT-PCR, hospitalisations related to COVID-19). Data are obfuscated, values below 10 are set to 0 for confidentiality purpose. Those data are used to forecast COVID-19 hospitalisations.

## Description of the Data and file structure

Data are presented in a tabular format where each row is a day and each column a feature.

*Time*

- START_DATE : the date of the day
- WEEKDAY : the day of the week

*Hospitalisations*

- hosp : Number of SARS-CoV-2 hospitalisations in Bordeaux hospital
- IN_HOSP_in_COUNT : Number of new hospitalisations
- OUT_HOSP_out_COUNT : Number of hospitalisations leave
- OUT_ICU_out_COUNT : Number of ICU leave

*RT-PCR*

- P_0_19_ANS : Number of positive RT-PCR among the 0-19 yo
- P_20_59_ANS : Number of positive RT-PCR among the 20-59 yo
- P_60_90_PLUS_ANS : Number of positive RT-PCR among the 60+ yo
- P_TOUS_AGES : Number of positive RT-PCR
- TESTED_0_19_ANS : Number of tested RT-PCR among the 0-19 yo
- TESTED_20_59_ANS : Number of tested RT-PCR among the 20-59 yo
- TESTED_60_90_PLUS_ANS : Number of tested RT-PCR among the 60+ yo
- TESTED_TOUS_AGES : Number of tested RT-PCR
- FRACP_0_19_ANS : P_0_19_ANS divided by TESTED_0_19_ANS
- FRACP_20_59_ANS : P_20_59_ANS divided by TESTED_20_59_ANS
- FRACP_60_90_PLUS_ANS : P_60_90_PLUS_ANS divided by TESTED_60_90_PLUS_ANS
- FRACP_TOUS_AGES : P_TOUS_AGES divided by TESTED_TOUS_AGES
- PCR_INTRA_EXTRA_prelevements_intra_hospitaliers_COUNT : Number of positive RT-PCR inside the hospital
- PCR_INTRA_EXTRA_prelevements_extra_hospitaliers_COUNT : Number of positive RT-PCR outside the hospital

*Emergency service call*

- SAMU_symptomes_lies_au_covid_COUNT : Number of emergency medical service call mentioning symptoms associated with covid-19.
- SAMU_symptomes_lies_au_covid_PERCENT : Proportion of emergency medical service call mentioning symptoms associated with covid-19.
- SAMU_tous_les_termes_COUNT : Number of emergency medical service call mentioning one concept related to covid-19 infection (cought, diarrhea, fever, anosmia, cephalea, agueusia, symptoms related to covid-19).
- SAMU_tous_les_termes_PERCENT : Proportion of emergency medical service call mentioning one concept related to covid-19 infection (cough, diarrhea, fever, anosmia, cephalea, agueusia, symptoms related to covid-19).
- SAMU_agueusie_COUNT : Number of emergency medical service call mentioning agueusia.
- SAMU_toux_COUNT : Number of emergency medical service call mentioning cough.
- SAMU_toux_PERCENT : Proportion of emergency medical service call mentioning cough.
- SAMU_hyperthermie_COUNT : Number of emergency medical service call mentioning hyperthermia.
- SAMU_diarrhee_COUNT : Number of emergency medical service call mentioning diarrhea.
- SAMU_diarrhee_PERCENT : Proportion of emergency medical service call mentioning diarrhea.
- SAMU_dyspnee_COUNT : Number of emergency medical service call mentioning dyspnea.
- SAMU_dyspnee_PERCENT : Proportion of emergency medical service call mentioning dyspnea.
- SAMU_anosmie_COUNT : Number of emergency medical service call mentioning anosmia.
- SAMU_cephalee_2_COUNT : Number of emergency medical service call mentioning cephalea.
- SAMU_cephalee_2_PERCENT : Proportion of emergency medical service call mentioning cephalea.
- SAMU_fievre_COUNT : Number of emergency medical service call mentioning fever.
- SAMU_fievre_PERCENT : Proportion of emergency medical service call mentioning fever.
- SAMU_covid_19_COUNT : Number of emergency medical service call mentioning covid-19.
- SAMU_covid_19_PERCENT : Proportion of emergency medical service call mentioning covid-19.

*Emergency service EHR (all hospitals)*

- URG_covid_19_COUNT : Number of emergency medical service EHR mentioning covid-19.
- URG_covid_19_PERCENT : Proportion of emergency medical service EHR mentioning covid-19.
- URG_tous_les_termes_COUNT : Number of emergency medical service EHR mentioning one concept related to covid-19 infection (cought, diarrhea, fever, anosmia, cephalea, agueusia, symptoms related to covid-19).
- URG_tous_les_termes_PERCENT : Proportion of emergency medical service EHR mentioning one concept related to covid-19 infection (cought, diarrhea, fever, anosmia, cephalea, agueusia, symptoms related to covid-19).
- URG_cephalee_COUNT : Number of emergency medical service EHR mentioning cephalea.
- URG_cephalee_PERCENT : Proportion of emergency medical service EHR mentioning cephalea.
- URG_diarrhee_COUNT : Number of emergency medical service EHR mentioning diarrhea.
- URG_diarrhee_PERCENT : Proportion of emergency medical service EHR mentioning diarrhea.
- URG_agueusie_COUNT : Number of emergency medical service EHR mentioning agueusia.
- URG_anosmie_COUNT : Number of emergency medical service EHR mentioning anosmia.
- URG_hyperthermie_COUNT : Number of emergency medical service EHR mentioning hyperthermia.
- URG_hyperthermie_PERCENT : Proportion of emergency medical service EHR mentioning hyperthermia.
- URG_symptomes_lies_au_covid_COUNT : Number of emergency medical service EHR mentioning symptoms associated with covid-19.
- URG_symptomes_lies_au_covid_PERCENT : Proportion of emergency medical service EHR mentioning symptoms associated with covid-19.
- URG_dyspnee_COUNT : Number of emergency medical service EHR mentioning dyspnea.
- URG_dyspnee_PERCENT : Proportion of emergency medical service EHR mentioning dyspnea.
- URG_toux_COUNT : Number of emergency medical service EHR mentioning cough.
- URG_toux_PERCENT : Proportion of emergency medical service EHR mentioning cough.
- URG_fievre_COUNT : Number of emergency medical service EHR mentioning fever.
- URG_fievre_PERCENT : Proportion of emergency medical service EHR mentioning fever.

*Emergency service EHR (St André hospital)*

- URG_SA_fievre_COUNT : same as URG_fievre_COUNT but in St Andre hospital 
- URG_SA_fievre_PERCENT : same as URG_fievre_PERCENT but in St Andre hospital 
- URG_SA_tous_les_termes_COUNT : same as URG_tous_les_termes_COUNT but in St Andre hospital 
- URG_SA_tous_les_termes_PERCENT : same as URG_tous_les_termes_PERCENT but in St Andre hospital 
- URG_SA_anosmie_COUNT : same as URG_anosmie_COUNT but in St Andre hospital 
- URG_SA_anosmie_PERCENT : same as URG_anosmie_PERCENT but in St Andre hospital 
- URG_SA_covid_19_COUNT : same as URG_covid_19_COUNT but in St Andre hospital 
- URG_SA_covid_19_PERCENT : same as URG_covid_19_PERCENT but in St Andre hospital 
- URG_SA_diarrhee_PERCENT : same as URG_diarrhee_PERCENT but in St Andre hospital 
- URG_SA_agueusie_COUNT : same as URG_agueusie_COUNT but in St Andre hospital 
- URG_SA_agueusie_PERCENT : same as URG_agueusie_PERCENT but in St Andre hospital 
- URG_SA_dyspnee_COUNT : same as URG_dyspnee_COUNT but in St Andre hospital 
- URG_SA_dyspnee_PERCENT : same as URG_dyspnee_PERCENT but in St Andre hospital 
- URG_SA_cephalee_COUNT : same as URG_cephalee_COUNT but in St Andre hospital 
- URG_SA_cephalee_PERCENT : same as URG_cephalee_PERCENT but in St Andre hospital 
- URG_SA_toux_COUNT : same as URG_toux_COUNT but in St Andre hospital 
- URG_SA_toux_PERCENT : same as URG_toux_PERCENT but in St Andre hospital 
- URG_SA_symptomes_lies_au_covid_COUNT : same as URG_symptomes_lies_au_covid_COUNT but in St Andre hospital 
- URG_SA_symptomes_lies_au_covid_PERCENT : same as URG_symptomes_lies_au_covid_PERCENT but in St Andre hospital 
- URG_SA_hyperthermie_COUNT : same as URG_hyperthermie_COUNT but in St Andre hospital 
- URG_SA_hyperthermie_PERCENT : same as URG_hyperthermie_PERCENT but in St Andre hospital 

*Emergency service EHR (Pellegrin hospital)*

- URG_PEL_symptomes_lies_au_covid_COUNT : same as URG_symptomes_lies_au_covid_COUNT but in Pellegrin hospital 
- URG_PEL_symptomes_lies_au_covid_PERCENT : same as URG_symptomes_lies_au_covid_PERCENT but in Pellegrin hospital 
- URG_PEL_covid_19_COUNT : same as URG_covid_19_COUNT but in Pellegrin hospital 
- URG_PEL_covid_19_PERCENT : same as URG_covid_19_PERCENT but in Pellegrin hospital 
- URG_PEL_dyspnee_COUNT : same as URG_dyspnee_COUNT but in Pellegrin hospital 
- URG_PEL_dyspnee_PERCENT : same as URG_dyspnee_PERCENT but in Pellegrin hospital 
- URG_PEL_fievre_COUNT : same as URG_fievre_COUNT but in Pellegrin hospital 
- URG_PEL_fievre_PERCENT : same as URG_fievre_PERCENT but in Pellegrin hospital 
- URG_PEL_hyperthermie_COUNT : same as URG_hyperthermie_COUNT but in Pellegrin hospital 
- URG_PEL_cephalee_COUNT : same as URG_cephalee_COUNT but in Pellegrin hospital 
- URG_PEL_cephalee_PERCENT : same as URG_cephalee_PERCENT but in Pellegrin hospital 
- URG_PEL_diarrhee_COUNT : same as URG_diarrhee_COUNT but in Pellegrin hospital 
- URG_PEL_tous_les_termes_COUNT : same as URG_tous_les_termes_COUNT but in Pellegrin hospital 
- URG_PEL_tous_les_termes_PERCENT : same as URG_tous_les_termes_PERCENT but in Pellegrin hospital 
- URG_PEL_anosmie_COUNT : same as URG_anosmie_COUNT but in Pellegrin hospital 
- URG_PEL_toux_COUNT : same as URG_toux_COUNT but in Pellegrin hospital 
- URG_PEL_toux_PERCENT : same as URG_toux_PERCENT but in Pellegrin hospital 

*Emergency service EHR (Pediatric hospital)*

- URG_PED_fievre_COUNT : same as URG_fievre_COUNT but in Pediatric hospital 
- URG_PED_fievre_PERCENT : same as URG_fievre_PERCENT but in Pediatric hospital 
- URG_PED_symptomes_lies_au_covid_COUNT : same as URG_symptomes_lies_au_covid_COUNT but in Pediatric hospital 
- URG_PED_symptomes_lies_au_covid_PERCENT : same as URG_symptomes_lies_au_covid_PERCENT but in Pediatric hospital 
- URG_PED_toux_COUNT : same as URG_toux_COUNT but in Pediatric hospital 
- URG_PED_toux_PERCENT : same as URG_toux_PERCENT but in Pediatric hospital 
- URG_PED_hyperthermie_COUNT : same as URG_hyperthermie_COUNT but in Pediatric hospital 
- URG_PED_hyperthermie_PERCENT : same as URG_hyperthermie_PERCENT but in Pediatric hospital 
- URG_PED_dyspnee_COUNT : same as URG_dyspnee_COUNT but in Pediatric hospital 
- URG_PED_dyspnee_PERCENT : same as URG_dyspnee_PERCENT but in Pediatric hospital 
- URG_PED_cephalee_COUNT : same as URG_cephalee_COUNT but in Pediatric hospital 
- URG_PED_cephalee_PERCENT : same as URG_cephalee_PERCENT but in Pediatric hospital 
- URG_PED_covid_19_COUNT : same as URG_covid_19_COUNT but in Pediatric hospital 
- URG_PED_covid_19_PERCENT : same as URG_covid_19_PERCENT but in Pediatric hospital 
- URG_PED_diarrhee_COUNT : same as URG_diarrhee_COUNT but in Pediatric hospital 
- URG_PED_diarrhee_PERCENT : same as URG_diarrhee_PERCENT but in Pediatric hospital 
- URG_PED_tous_les_termes_COUNT : same as URG_tous_les_termes_COUNT but in Pediatric hospital 
- URG_PED_tous_les_termes_PERCENT : same as URG_tous_les_termes_PERCENT but in Pediatric hospital 

## Sharing/access Information

Links to other publicly accessible locations of the data:

- Data related to Gironde Covid-19 epidemic are available at : Etalab. Les données relatives au COVID-19 en France - data.gouv.fr.
2020.https://www.data.gouv.fr/fr/pages/donnees-coronavirus/ (accessed 29 Sep 2021).
- Data related to weather are available at :  Smith A, Lott N, Vose R. The Integrated Surface Database: Recent Developments and Partnerships. Bull Am
Meteorol Soc 2011;92:704–8. doi:10.1175/2011BAMS3015.1
