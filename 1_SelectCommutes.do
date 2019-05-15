clear
clear matrix
cd "C:\Users\Anna Goodman\Dropbox\GitHub"

	*****************
	** SELECT ROUTE SAMPLE FROM CENSUS DATA
	*****************
		use "..\1 - Phys Act_1-PA main\2017_MetaHIT_analysis\1b_datacreated\SPindivid_CensusNTSAPS_Eng.dta", clear
			keep home_lsoa home_lad14cd home_laname home_gor work_lsoa commute_mainmode9
			
		* DROP OD PAIRS NOT IN SCOPE
			drop if work_lsoa=="OD0000001" | work_lsoa=="" 			// Work from home and non-commuters
			drop if work_lsoa=="OD0000002" | work_lsoa=="OD0000003" | work_lsoa=="OD0000004" | work_lsoa=="S92000003" | work_lsoa=="N92000002"	// No fixed workplace or overseas
			drop if home_gor==10 									// Wales
			drop if commute_mainmode9==6 | commute_mainmode9==8 	// modes not routing
			
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
			
		/* MAKE 2-WAY IDs
			rename home_lsoa geo_code_o
			rename work_lsoa geo_code_d
			gen osub1=substr(geo_code_o,1,1)
			gen dsub1=substr(geo_code_d,1,1)
			gen osub2=substr(geo_code_o,2,.)
			gen dsub2=substr(geo_code_d,2,.)
			destring osub2 dsub2, replace force
			gen homefirst=.
			recode homefirst .=1 if (osub1=="E" & dsub1!="E")
			recode homefirst .=1 if (osub1=="W" & dsub1!="E" & dsub1!="W")
			recode homefirst .=0 if (osub1=="W" & dsub1=="E")
			recode homefirst .=1 if (osub1=="E" & dsub1=="E" & osub2<= dsub2)
			recode homefirst .=1 if (osub1=="W" & dsub1=="W" & osub2<= dsub2)
			recode homefirst .=0 if (osub1=="E" & dsub1=="E" & osub2>dsub2)
			recode homefirst .=0 if (osub1=="W" & dsub1=="W" & osub2>dsub2)
			gen geo_code1=geo_code_o
			replace geo_code1=geo_code_d if homefirst==0
			gen geo_code2=geo_code_d
			replace geo_code2=geo_code_o if homefirst==0
			drop osub1 dsub1 osub2 dsub2 homefirst
			gen id=geo_code1+" "+geo_code2
		*/
			rename home_lsoa geo_code_o
			rename work_lsoa geo_code_d
			gen id=geo_code_o+" "+geo_code_d
		
		
		 * COUNT NUMBER OF PEOPLE IN EACH ROUTE
		 *	bysort id mode4: gen freq=_N
		
		* SAVE
			order id geo_code_o geo_code_d home_lad14cd home_laname mode4 //freq
			keep id-mode4 //freq
			*duplicates drop
			sort id
			export delimited using "mh-route-commutes\1_DataCreated\1_sampleroutes.csv", replace
