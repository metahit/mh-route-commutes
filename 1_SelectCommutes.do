clear
clear matrix
cd "C:\Users\Anna Goodman\Dropbox\GitHub"
	
	/** PREPARE RUC2011
		import delimited "mh-route-commutes\01_DataInput\ruc2011\Rural_Urban_Classification_2011_of_Lower_Layer_Super_Output_Areas_in_England_and_Wales.csv", varnames(1) clear 
			gen home_lsoa= ïlsoa11cd
			gen urban_lsoa = 0
			recode urban_lsoa 0=1 if ruc11cd=="A1" | ruc11cd=="B1" | ruc11cd=="C1" | ruc11cd=="C2"  
			keep home_lsoa urban_lsoa
		save "mh-route-commutes\02_DataCreated\temp\ruc2011_lsoa.dta", replace
	
	** ESTIMATE MAX TYPICAL DISTANCES BY COMMUTE MODES
		use "C:\Users\Anna Goodman\Dropbox\1 - Phys Act_1-PA main\2017_MetaHIT_analysis\1b_datacreated\NTSEng_20102016.dta", clear
			keep if trip_purpose==1
			recode trip_mainmode 1=2 2=1 3/5=3 6=. 7/9=4 10/max=., gen(mode4)
			bysort mode4: egen topp = pctile(trip_distraw_km), p(98)
			tab mode4, sum(topp)
			
	** ESTIMATE WITHIN-ZONE TRAVEL DISTANCE
		use "C:\Users\Anna Goodman\Dropbox\GitHub\pct-inputs\02_intermediate\x_temporary_files\scenario_building\commute\CommuteSP_individual.dta" 
			keep if home_lsoa== work_lsoa & home_gor<=9 // within-zone in England
			sum rf_dist_km
			keep home_lsoa rf_dist_km
			duplicates drop
		save "mh-route-commutes\02_DataCreated\temp\withinlsoa_dist.dta", replace
		
	*/
		
	*****************
	** SELECT ROUTE SAMPLE FROM CENSUS DATA
	*****************
		use "..\1 - Phys Act_1-PA main\2017_MetaHIT_analysis\1b_datacreated\SPindivid_CensusNTSAPS_Eng.dta", clear
			keep home_lsoa home_lad14cd home_laname home_gor work_lsoa commute_mainmode9
			
		* MERGE IN URBAN/RURAL STATUS OF LSOA
			merge m:1 home_lsoa using "mh-route-commutes\02_DataCreated\temp\ruc2011_lsoa.dta"
			drop if _m!=3
			drop _m
			
		* MERGE IN ESTIMATED WITHIN-ZONE TRAVEL
			merge m:1 home_lsoa using "mh-route-commutes\02_DataCreated\temp\withinlsoa_dist.dta"
			drop if _m!=3
			drop _m
			rename rf_dist_km within_lsoa_dist
						
		* DROP OD PAIRS NOT IN SCOPE
			drop if work_lsoa=="OD0000001" | work_lsoa=="" 			// Work from home and non-commuters
			drop if work_lsoa=="OD0000002" | work_lsoa=="OD0000003" | work_lsoa=="OD0000004" | work_lsoa=="S92000003" | work_lsoa=="N92000002"	// No fixed workplace or overseas
			drop if home_gor==10 									// Wales
			drop if commute_mainmode9==6 | commute_mainmode9==8 	// modes not routing in Metahit
			
		* COMBINE CITY OF LONDON AND ISLES SCILLY
			replace home_lad14cd="E09000033" if home_laname=="City of London"
			replace home_lad14cd="E06000052" if home_laname=="Isles of Scilly"
			
		* RANDOMLY SELECT BY LA BY MODE
			recode commute_mainmode9 4/5=3 7=4, gen(mode4)
			set seed 180426
			gen random=uniform()
			by home_lad14cd mode4 (random), sort: gen littlen=_n
			keep if littlen<=1000
			drop commute_mainmode9 home_gor random littlen
			
		* MAKE 1-WAY IDs
			rename home_lsoa geo_code_o
			rename work_lsoa geo_code_d
			gen id=geo_code_o+" "+geo_code_d
		
		* GEN MAX DISTANCE
			recode mode4 1=19.3 2=4.8 3=72.4 4=27.4 , gen(maxdist_mode)
		
		* SAVE
			order id geo_code_o geo_code_d home_lad14cd home_laname urban_lsoa within_lsoa_dist mode4 maxdist_mode //freq
			keep id-maxdist_mode //freq
			*duplicates drop
			sort id
			export delimited using "mh-route-commutes\02_DataCreated\1_sampleroutes.csv", replace
