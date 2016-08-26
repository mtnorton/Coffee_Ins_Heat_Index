use "/Users/mtnorton/Coffee_Ins_Heat_Index/Base_Pron.dta", clear

keep ao semestre cod_lote y_lote ecotopo_lote densidad variedad luminosid
gen log_y_lote = ln(y_lote)
tabulate ao, generate(ao_dum)
tab semestre, gen(sem_dum)
tab ecotopo_lote, gen(eco_dum)
tab variedad, gen(var_dum)
tab luminosid, gen(lum_dum)

regress log_y_lote ao_dum2 ao_dum3 ao_dum4 ao_dum5 ao_dum6 ao_dum7 ao_dum8 ao_dum9 ao_dum10
regress log_y_lote eco_dum2 eco_dum3 eco_dum4 eco_dum5 eco_dum6 eco_dum7 eco_dum8 eco_dum9 eco_dum10 eco_dum11 eco_dum12 eco_dum13 eco_dum14 eco_dum15 eco_dum16 eco_dum17 eco_dum18 eco_dum19 eco_dum20 eco_dum21 eco_dum22 eco_dum23 eco_dum24 eco_dum25 eco_dum26 eco_dum27 eco_dum28 eco_dum29 eco_dum30 eco_dum31 eco_dum32 eco_dum33 eco_dum34 eco_dum35 eco_dum36 eco_dum37 eco_dum38 eco_dum39 eco_dum40 eco_dum41 eco_dum42 eco_dum43 eco_dum44 eco_dum45 eco_dum46 eco_dum47 eco_dum48 eco_dum49 eco_dum50 eco_dum51 eco_dum52 eco_dum53 eco_dum54 eco_dum55 eco_dum56 eco_dum57 eco_dum58 eco_dum59 eco_dum60 eco_dum61 eco_dum62 eco_dum63 eco_dum64 eco_dum65 eco_dum66 eco_dum67 eco_dum68 eco_dum69 eco_dum70 eco_dum71 eco_dum72 eco_dum73 eco_dum74 eco_dum75 eco_dum76 eco_dum77 eco_dum78 eco_dum79 eco_dum80 eco_dum81

gen region="."
* No a/b subsets in region 4
replace region="4" if substr(ecotopo_lote,1,1)=="4"

replace region="1A" if (substr(ecotopo_lote,1,1)=="1" && substr(ecotopo_lote,4,4)=="A")
replace region="1B" if (substr(ecotopo_lote,1,1)=="1" && substr(ecotopo_lote,4,4)=="B")
replace region="2A" if (substr(ecotopo_lote,1,1)=="2" && substr(ecotopo_lote,4,4)=="A")
replace region="2B" if (substr(ecotopo_lote,1,1)=="2" && substr(ecotopo_lote,4,4)=="B")
replace region="3A" if (substr(ecotopo_lote,1,1)=="3" && substr(ecotopo_lote,4,4)=="A")
replace region="3B" if (substr(ecotopo_lote,1,1)=="3" && substr(ecotopo_lote,4,4)=="B")

tab reg, generate(reg_dum)
regress log_y_lote reg_dum2 reg_dum3 reg_dum4 reg_dum5 reg_dum6 reg_dum7 reg_dum8

save "/Users/mtnorton/Coffee_Ins_Heat_Index/Base_Pron.dta", replace


collapse(mean) log_y_lote if dist_est_mas_cercana<=3000, by(estacion_mas_cercana_govt_only ao semestre) 
tab estacion_mas_cercana_govt_only, gen(dum_est)
tab ao, gen(dum_ao)
tab semestre, gen(dum_sem)

drop dum_est1 dum_ao1
regress log_y_lote dum_est*
regress log_y_lote dum_est* dum_sem1
regress log_y_lote dum_est* dum_ao* dum_sem1

