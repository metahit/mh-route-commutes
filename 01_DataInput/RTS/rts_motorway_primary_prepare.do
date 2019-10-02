clear
clear matrix
cd "C:\Users\Anna Goodman\Dropbox\GitHub"
	
	*****************
	** PREPARE ROB DATA
	*****************
		* LOAD
			import delimited "mh-route-commutes\01_DataInput\RTS\FromRob\191001_mode_road_BristolLA.csv", delimiter(comma) varnames(1) 
			rename v1 latravel
			rename v2 modename
		
		* DEFINE MODE 5, DROP HGV
			drop if modename=="hgv"
			gen mode5=1
			recode mode5 1=3 if modename=="car" | modename=="lgv"
			recode mode5 1=4 if modename=="motorcycle"
			recode mode5 1=5 if modename=="bus"
			
		* SUM BY MODE
			gen rts_mot_prim = motorway+rurala+urbana
			bysort latravel mode5: egen rts_all=sum(rts_mot_prim)
			
			bysort latravel mode5: egen rts_motorway=sum(motorway)
				replace rts_motorway = rts_motorway/rts_all
			bysort latravel mode5: egen rts_rural_primary=sum(rurala)
				replace rts_rural_primary = rts_rural_primary/rts_all
			bysort latravel mode5: egen rts_urban_primary=sum(urbana)
				replace rts_urban_primary = rts_urban_primary/rts_all
			
		* SAVE
			order latravel mode5 rts_motorway rts_rural_primary rts_urban_primary
			keep latravel - rts_urban_primary
			duplicates drop
		export delimited using "mh-route-commutes\01_DataInput\RTS\rts_motorway_primary.csv", replace
