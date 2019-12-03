clear
clear matrix
cd "F:\Github_Maxtor"
	
	*****************
	** PREPARE ROB DATA
	*****************
		* LOAD
			import excel "mh-route-commutes\01_DataInput\RTS\FromRob\191114_mode_road_CityRegionLA_agedit.xls", sheet("Data") firstrow case(lower) clear
			rename a latravel
			rename b modename
			rename total allroads
			keep latravel - allroads

		* DEFINE MODE 5, DROP HGV
			drop if modename=="hgv" | modename=="bicycle"
			gen mode5=.
			recode mode5 .=3 if modename=="car" | modename=="lgv"
			recode mode5 .=4 if modename=="motorcycle"
			recode mode5 .=5 if modename=="bus"
			
		* Define city regions
				gen cityregion=""
			* Bristol			
				foreach x in /*
				*/ E06000022 E06000023 E06000024 E06000025 /*
				*/ {
				replace cityregion="bristol" if latravel=="`x'"
				}
			* greatermanchester			
				foreach x in /*
				*/ E08000001 E08000002 E08000003 E08000004 E08000005 E08000006 E08000007 E08000008 E08000009 E08000010 /*
				*/ {
				replace cityregion="greatermanchester" if latravel=="`x'"
				}
			* leeds			
				foreach x in /*
				*/ E08000032 E08000033 E08000034 E08000035 E08000036 /*
				*/ {
				replace cityregion="leeds" if latravel=="`x'"
				}
			* liverpool			
				foreach x in /*
				*/ E06000006 E08000011 E08000012 E08000013 E08000014 E08000015 /*
				*/ {
				replace cityregion="liverpool" if latravel=="`x'"
				}
			* london
				foreach x in /*
				*/ E09000001 E09000002 E09000003 E09000004 E09000005 E09000006 E09000007 E09000008 E09000009 E09000010 E09000011 E09000012 E09000013  E09000014 E09000015 E09000016 E09000017 E09000018 E09000019 E09000020 E09000021 E09000022 E09000023 E09000024 E09000025 E09000026 E09000027 E09000028 E09000029 E09000030 E09000031 E09000032 E09000033 /*
				*/ {
				replace cityregion="london" if latravel=="`x'"
				}
			* northeast
				foreach x in /*
				*/ E06000047 E06000057 E08000021 E08000022 E08000023 E08000024 E08000037 /*
				*/ E06000048 E08000020/*
				*/ {
				replace cityregion="northeast" if latravel=="`x'"
				}
			* nottingham
				foreach x in /*
				*/ E06000018 E07000170 E07000171 E07000172 E07000173 E07000174 E07000175  E07000176 /*
				*/ E10000024 /*
				*/ {
				replace cityregion="nottingham" if latravel=="`x'"
				}
			* sheffield
				foreach x in /*
				*/ E08000016 E08000017 E08000018 E08000019 /*
				*/ {
				replace cityregion="sheffield" if latravel=="`x'"
				}
			* westmidlands
				foreach x in /*
				*/ E08000025 E08000026 E08000027 E08000028 E08000029 E08000030 E08000031 /*
				*/ {
				replace cityregion="westmidlands" if latravel=="`x'"
				}	

		* SUM BY MODE
			bysort cityregion mode5: egen rts_all=sum(allroads)
			bysort cityregion mode5: egen rts_allm=sum(motorway)
				gen rts_motorway = rts_allm/rts_all

		* SAVE
			order cityregion mode5 rts_motorway
			keep cityregion - rts_motorway
			duplicates drop
		export delimited using "mh-route-commutes\01_DataInput\RTS\rts_motorway.csv", replace
