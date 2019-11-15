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
			drop if modename=="hgv"
			gen mode5=1
			recode mode5 1=3 if modename=="car" | modename=="lgv"
			recode mode5 1=4 if modename=="motorcycle"
			recode mode5 1=5 if modename=="bus"

		* SUM BY MODE
			bysort latravel mode5: egen rts_all=sum(allroads)
			gen rts_mot_prim = motorway+rurala+urbana
			bysort latravel mode5: egen rts_allam=sum(rts_mot_prim)
				gen rts_am = rts_allam/rts_all

			bysort latravel mode5: egen rts_motorway=sum(motorway)
				replace rts_motorway = rts_motorway/rts_allam
			bysort latravel mode5: egen rts_rural_primary=sum(rurala)
				replace rts_rural_primary = rts_rural_primary/rts_allam
			bysort latravel mode5: egen rts_urban_primary=sum(urbana)
				replace rts_urban_primary = rts_urban_primary/rts_allam

		* SAVE
			order latravel mode5 rts_am rts_motorway rts_rural_primary rts_urban_primary
			keep latravel - rts_urban_primary
			duplicates drop
		export delimited using "mh-route-commutes\01_DataInput\RTS\rts_motorway_primary.csv", replace
