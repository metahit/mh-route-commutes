clear
clear matrix
cd "C:\Users\Anna Goodman\Dropbox\GitHub"
	
	/** ESTIMATE MAX TYPICAL DISTANCES BY COMMUTE MODES
		use "C:\Users\Anna Goodman\Dropbox\1 - Phys Act_1-PA main\2017_MetaHIT_analysis\1b_datacreated\NTSEng_20102016.dta", clear
			keep if trip_purpose==1
			recode trip_mainmode 1=2 2=1 3/4=3 5=4 6=. 7/9=5 10/max=., gen(mode5)
			bysort mode5: egen topp = pctile(trip_distraw_km), p(98)
			tab mode5, sum(topp)
	*/
		
	*****************
	** SELECT ROUTE SAMPLE FROM CENSUS DATA
	*****************
		use "..\1 - Phys Act_1-PA main\2017_MetaHIT_analysis\1b_datacreated\SPindivid_CensusNTSAPS_Eng.dta", clear
			keep home_lsoa home_postcode home_lad14cd home_laname home_gor work_lsoa urban urbanmatch commute_mainmode9
			
		* DROP OD PAIRS NOT IN SCOPE
			drop if work_lsoa=="OD0000001" | work_lsoa=="" 			// Work from home and non-commuters
			drop if work_lsoa=="OD0000002" | work_lsoa=="OD0000003" | work_lsoa=="OD0000004" | work_lsoa=="S92000003" | work_lsoa=="N92000002"	// No fixed workplace or overseas
			drop if home_gor==10 									// home in Wales
			gen workcountry=substr(work_lsoa,1,1)
			drop if workcountry=="W"								// work in Wales
			drop if commute_mainmode9==6 | commute_mainmode9==8 	// modes not routing in Metahit

		* RANDOMLY SELECT BY LA BY MODE, AND GENERATE WEIGHTS [how many people does each commuter stand for?]
			recode commute_mainmode9 4=3 5=4 7=5, gen(mode5)
			set seed 180426
			gen random=uniform()
			by home_lad14cd mode5 (random), sort: gen littlen=_n
			bysort home_lad14cd mode5: gen bign=_N
			gen threshold = 1000 // 50			
			gen lahome_weight = bign / threshold
			recode lahome_weight min / 1 = 1 // if <1000, everyone included and counts for selves
			keep if littlen<=threshold			
			*bysort home_lad14cd mode5 urbanmatch: gen bign2=_N
			*ta bign2  // Examine how many people per urbanmatch type: always at least 40
				
		* MERGE IN WORK LA
			rename work_lsoa lsoa11cd
			merge m:1 lsoa11cd using "..\1 - Phys Act_1-PA main\2017_MetaHIT_analysis\1a_dataoriginal\Census2011_MergeInGeography\LSOA_MSOA_LAlookup\EngWales_LSOA11_MSOA11_LAD11_EW_LUv2.dta" , keepus(lad11cd)
				drop if _m==2 // wales
				drop _m	
			rename lsoa11cd work_lsoa
			rename lad11cd home_lad11cd
			merge m:1 home_lad11cd using "..\1 - Phys Act_1-PA main\2017_MetaHIT_analysis\1b_datacreated\0temp\LAcode_20112014.dta"
				drop if _m==2 // should be no one
				drop _m	
			rename lad2014_code work_lad14cd
	
		* MAKE 1-WAY IDs
			rename home_postcode geo_code_o
			rename work_lsoa geo_code_d
			gen id=geo_code_o+" "+geo_code_d	
			
		* GEN MAX DISTANCE 
			recode mode5 1=19.3 2=4.8 3=72.4 4=59.5 5=27.4 , gen(maxdist_mode)	
					
		* SAVE
			order id geo_code_o geo_code_d home_lad14cd home_laname work_lad14cd lahome_weight urban urbanmatch mode5 maxdist_mode
			keep id-maxdist_mode 
			sort id
			export delimited using "mh-route-commutes\02_DataCreated\1_sampleroutes.csv", replace
		*	export delimited using "mh-route-commutes\02_DataCreated\1_sampleroutes_small.csv", replace
