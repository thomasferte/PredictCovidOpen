# dfEDSobfuscated:
---

This dataset contains French open data (public covid data, weather data) and data from the University Hospital of Bordeaux (emergency units, RT-PCR, hospitalisations related to COVID-19). Data from the hospital are obfuscated, values below 10 are set to 0 for confidentiality purpose. Those data are used to forecast COVID-19 hospitalisations.

## Description of the Data and file structure

Data are presented in a tabular format where each row is a day and each column a feature.

- START_DATE : the date of the day
- GIRONDE_HOSP : SARS-CoV-2 hospitalisations in Gironde department
- hosp : SARS-CoV-2 hospitalisations in Bordeaux hospital
- WEEKDAY : the day of the week
- SAMU_... : Information from the emergency medical service retrieved by natural language processing using a dictionnary based approach.
- URG_... : Information from the hospital emergency units retrieved by natural language processing using a dictionnary based approach. URG_PEL is the main hospital, URG_SA is another adult hospital and URG_PED is the pediatric hospital.
- PCR_INTRA_EXTRA_... : are hospital RT-PCR separated between inside (intra_hospitaliers) and outside (extra_hospitaliers) of the hospital.
- IN/OUT_HOSP/ICU : daily new or leave of the hospital and the ICU.
- P_... : positive RT-PCR (in Gironde if GRP_GIRONDE)
- TESTED_... : tested RT-PCR (in Gironde if GRP_GIRONDE)
- Majority_variant : the main current variant
- Vaccin_1dose : number of people vaccinated with 1 dose of vaccin.
- t.mean : temperature
- precip : precipitation
- RH/AH : relative and absolute humidity
- IPTCC : weather index transmissibility (see: https://doi.org/10.1016/j.pxur.2021.01.002)
- ws : wind speed
- dewpoint : dewpoint
- FRACP_... : proportion of positvie RT-PCR among tested RT-PCR (in Gironde if GRP_GIRONDE)
- INTERVARIANT : interaction between variant and RT-PCR data
- INTERVACCIN : interaction between vaccine and RT-PCR data

## Sharing/access Information

Links to other publicly accessible locations of the data:

- Etalab. Les données relatives au COVID-19 en France - data.gouv.fr.
2020.https://www.data.gouv.fr/fr/pages/donnees-coronavirus/ (accessed 29 Sep 2021).
- Smith A, Lott N, Vose R. The Integrated Surface Database: Recent Developments and Partnerships. Bull Am
Meteorol Soc 2011;92:704–8. doi:10.1175/2011BAMS3015.1
