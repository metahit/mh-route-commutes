clear
clear matrix
cd "C:\Users\Anna Goodman\Dropbox\GitHub"
	
	/** ESTIMATE MAX TYPICAL DISTANCES BY COMMUTE MODES
		use "C:\Users\Anna Goodman\Dropbox\1 - Phys Act_1-PA main\2017_MetaHIT_analysis\1b_datacreated\NTSEng_20102016.dta", clear
			keep if trip_purpose==1
			recode trip_mainmode 1=2 2=1 3/5=3 6=. 7/9=4 10/max=., gen(mode4)
			bysort mode4: egen topp = pctile(trip_distraw_km), p(98)
			tab mode4, sum(topp)
			
	*/
		
	*****************
	** SELECT ROUTE SAMPLE FROM CENSUS DATA
	*****************
		use "..\1 - Phys Act_1-PA main\2017_MetaHIT_analysis\1b_datacreated\SPindivid_CensusNTSAPS_Eng.dta", clear
			keep home_lsoa home_postcode home_lad14cd home_laname home_gor work_lsoa urban commute_mainmode9
			
		* DROP OD PAIRS NOT IN SCOPE
			drop if work_lsoa=="OD0000001" | work_lsoa=="" 			// Work from home and non-commuters
			drop if work_lsoa=="OD0000002" | work_lsoa=="OD0000003" | work_lsoa=="OD0000004" | work_lsoa=="S92000003" | work_lsoa=="N92000002"	// No fixed workplace or overseas
			drop if home_gor==10 									// Wales
			drop if commute_mainmode9==6 | commute_mainmode9==8 	// modes not routing in Metahit
			
		* RANDOMLY SELECT BY LA BY MODE, AND GENERATE WEIGHTS [how many people does each commuter stand for?]
			recode commute_mainmode9 4/5=3 7=4, gen(mode4)
			set seed 180426
			gen random=uniform()
			by home_lad14cd mode4 (random), sort: gen littlen=_n
			bysort home_lad14cd mode4: gen bign=_N
			gen threshold = 50 // 1000
			gen lahome_weight = bign / threshold
			recode lahome_weight min / 1 = 1 // if <1000, everyone included and counts for selves
			keep if littlen<=threshold
			drop commute_mainmode9 home_gor random littlen bign threshold
			
		* MAKE 1-WAY IDs
			rename home_postcode geo_code_o
			rename work_lsoa geo_code_d
			gen id=geo_code_o+" "+geo_code_d	
			
		* GEN MAX DISTANCE 
			recode mode4 1=19.3 2=4.8 3=72.4 4=27.4 , gen(maxdist_mode)	
					
		* SAVE
			order id geo_code_o geo_code_d lahome_weight home_lad14cd home_laname urban mode4 maxdist_mode
			keep id-maxdist_mode 
			sort id
		*	export delimited using "mh-route-commutes\02_DataCreated\1_sampleroutes.csv", replace
			export delimited using "mh-route-commutes\02_DataCreated\1_sampleroutes_small.csv", replace
