****************************************************************
* This file: Merge production files to look for multiples
****************************************************************

* destring character data in COD_LOTE, remove obvious bad codes (< 1000)

use "/Users/mtnorton/Coffee_Ins_Heat_Index/pron_20011_.dta", clear
destring(COD_LOTE), replace
drop if COD_LOTE<1000
duplicates report COD_LOTE
duplicates drop COD_LOTE, force
keep COD_LOTE
gen x20011=1
sort COD_LOTE
save "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20011_.dta", replace

use "/Users/mtnorton/Coffee_Ins_Heat_Index/pron_20012_.dta", clear
destring(COD_LOTE), replace
drop if COD_LOTE<1000
duplicates report COD_LOTE
duplicates drop COD_LOTE, force
keep COD_LOTE
gen x20012=1
sort COD_LOTE
save "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20012_.dta", replace

use "/Users/mtnorton/Coffee_Ins_Heat_Index/pron_20021_.dta", clear
destring(COD_LOTE), replace
drop if COD_LOTE<1000
duplicates report COD_LOTE
duplicates drop COD_LOTE, force
keep COD_LOTE
gen x20021=1
sort COD_LOTE
save "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20021_.dta", replace

use "/Users/mtnorton/Coffee_Ins_Heat_Index/pron_20022_.dta", clear
destring(COD_LOTE), replace
drop if COD_LOTE<1000
duplicates report COD_LOTE
duplicates drop COD_LOTE, force
keep COD_LOTE
gen x20022=1
sort COD_LOTE
save "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20022_.dta", replace

use "/Users/mtnorton/Coffee_Ins_Heat_Index/pron_20031_.dta", clear
destring(COD_LOTE), replace
drop if COD_LOTE<1000
duplicates report COD_LOTE
duplicates drop COD_LOTE, force
keep COD_LOTE
gen x20031=1
sort COD_LOTE
save "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20031_.dta", replace

use "/Users/mtnorton/Coffee_Ins_Heat_Index/pron_20032_.dta", clear
destring(COD_LOTE), replace
drop if COD_LOTE<1000
duplicates report COD_LOTE
duplicates drop COD_LOTE, force
keep COD_LOTE
gen x20032=1
sort COD_LOTE
save "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20032_.dta", replace

use "/Users/mtnorton/Coffee_Ins_Heat_Index/pron_20041_.dta", clear
destring(COD_LOTE), replace
drop if COD_LOTE<1000
duplicates report COD_LOTE
duplicates drop COD_LOTE, force
keep COD_LOTE
gen x20041=1
sort COD_LOTE
save "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20041_.dta", replace

use "/Users/mtnorton/Coffee_Ins_Heat_Index/pron_20042_.dta", clear
destring(COD_LOTE), replace
drop if COD_LOTE<1000
duplicates report COD_LOTE
duplicates drop COD_LOTE, force
keep COD_LOTE
gen x20042=1
sort COD_LOTE
save "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20042_.dta", replace

use "/Users/mtnorton/Coffee_Ins_Heat_Index/pron_20051_.dta", clear
destring(COD_LOTE), replace
drop if COD_LOTE<1000
duplicates report COD_LOTE
duplicates drop COD_LOTE, force
keep COD_LOTE
gen x20051=1
sort COD_LOTE
save "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20051_.dta", replace

use "/Users/mtnorton/Coffee_Ins_Heat_Index/pron_20052_.dta", clear
destring(COD_LOTE), replace
drop if COD_LOTE<1000
duplicates report COD_LOTE
duplicates drop COD_LOTE, force
keep COD_LOTE
gen x20052=1
sort COD_LOTE
save "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20052_.dta", replace

use "/Users/mtnorton/Coffee_Ins_Heat_Index/pron_20061_.dta", clear
destring(COD_LOTE), replace
drop if COD_LOTE<1000
duplicates report COD_LOTE
duplicates drop COD_LOTE, force
keep COD_LOTE
gen x20061=1
sort COD_LOTE
save "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20061_.dta", replace

use "/Users/mtnorton/Coffee_Ins_Heat_Index/pron_20062_.dta", clear
destring(COD_LOTE), replace
drop if COD_LOTE<1000
duplicates report COD_LOTE
duplicates drop COD_LOTE, force
keep COD_LOTE
gen x20062=1
sort COD_LOTE
save "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20062_.dta", replace

use "/Users/mtnorton/Coffee_Ins_Heat_Index/pron_20071_.dta", clear
destring(COD_LOTE), replace
drop if COD_LOTE<1000
duplicates report COD_LOTE
duplicates drop COD_LOTE, force
keep COD_LOTE
gen x20071=1
sort COD_LOTE
save "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20071_.dta", replace

use "/Users/mtnorton/Coffee_Ins_Heat_Index/pron_20072_.dta", clear
destring(COD_LOTE), replace
drop if COD_LOTE<1000
duplicates report COD_LOTE
duplicates drop COD_LOTE, force
keep COD_LOTE
gen x20072=1
sort COD_LOTE
save "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20072_.dta", replace

use "/Users/mtnorton/Coffee_Ins_Heat_Index/pron_20081_.dta", clear
destring(COD_LOTE), replace
drop if COD_LOTE<1000
duplicates report COD_LOTE
duplicates drop COD_LOTE, force
keep COD_LOTE
gen x20081=1
sort COD_LOTE
save "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20081_.dta", replace

use "/Users/mtnorton/Coffee_Ins_Heat_Index/pron_20082_.dta", clear
destring(COD_LOTE), replace
drop if COD_LOTE<1000
duplicates report COD_LOTE
duplicates drop COD_LOTE, force
keep COD_LOTE
gen x20082=1
sort COD_LOTE
save "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20082_.dta", replace

use "/Users/mtnorton/Coffee_Ins_Heat_Index/pron_20091_.dta", clear
destring(COD_LOTE), replace
drop if COD_LOTE<1000
duplicates report COD_LOTE
duplicates drop COD_LOTE, force
keep COD_LOTE
gen x20091=1
sort COD_LOTE
save "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20091_.dta", replace

use "/Users/mtnorton/Coffee_Ins_Heat_Index/pron_20092_.dta", clear
destring(COD_LOTE), replace
drop if COD_LOTE<1000
duplicates report COD_LOTE
duplicates drop COD_LOTE, force
keep COD_LOTE
gen x20092=1
sort COD_LOTE
save "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20092_.dta", replace

use "/Users/mtnorton/Coffee_Ins_Heat_Index/pron_20101_.dta", clear
destring(COD_LOTE), replace
drop if COD_LOTE<1000
duplicates report COD_LOTE
duplicates drop COD_LOTE, force
keep COD_LOTE
gen x20101=1
sort COD_LOTE
save "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20101_.dta", replace

use "/Users/mtnorton/Coffee_Ins_Heat_Index/pron_20102_.dta", clear
destring(COD_LOTE), replace
drop if COD_LOTE<1000
duplicates report COD_LOTE
duplicates drop COD_LOTE, force
keep COD_LOTE
gen x20102=1
sort COD_LOTE
save "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20102_.dta", replace

****************************************************************
* Now: merge files on COD_LOTE
****************************************************************

use "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20011_.dta", clear
merge 1:1 COD_LOTE using "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20012_.dta", nogen
merge 1:1 COD_LOTE using "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20021_.dta", nogen
merge 1:1 COD_LOTE using "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20022_.dta", nogen
merge 1:1 COD_LOTE using "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20031_.dta", nogen
merge 1:1 COD_LOTE using "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20032_.dta", nogen
merge 1:1 COD_LOTE using "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20041_.dta", nogen
merge 1:1 COD_LOTE using "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20042_.dta", nogen
merge 1:1 COD_LOTE using "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20051_.dta", nogen
merge 1:1 COD_LOTE using "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20052_.dta", nogen
merge 1:1 COD_LOTE using "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20061_.dta", nogen
merge 1:1 COD_LOTE using "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20062_.dta", nogen
merge 1:1 COD_LOTE using "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20071_.dta", nogen
merge 1:1 COD_LOTE using "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20072_.dta", nogen
merge 1:1 COD_LOTE using "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20081_.dta", nogen
merge 1:1 COD_LOTE using "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20082_.dta", nogen
merge 1:1 COD_LOTE using "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20091_.dta", nogen
merge 1:1 COD_LOTE using "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20092_.dta", nogen
merge 1:1 COD_LOTE using "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20101_.dta", nogen
merge 1:1 COD_LOTE using "/Users/mtnorton/Coffee_Ins_Heat_Index/merging/pron_20102_.dta", nogen

egen num_prod_records = rowtotal(x*) 
