*August 14, 2016 by Mike
*Add regions to sica files based on Ecotopo codes

* version for raw sica files with lower case variable names

gen region="."
* No a/b subsets in region 4
replace region="4" if substr(ccod_ecoto,1,1)=="4"

replace region="1A" if (substr(ccod_ecoto,1,1)=="1" && substr(ccod_ecoto,4,4)=="A")
replace region="1B" if (substr(ccod_ecoto,1,1)=="1" && substr(ccod_ecoto,4,4)=="B")
replace region="2A" if (substr(ccod_ecoto,1,1)=="2" && substr(ccod_ecoto,4,4)=="A")
replace region="2B" if (substr(ccod_ecoto,1,1)=="2" && substr(ccod_ecoto,4,4)=="B")
replace region="3A" if (substr(ccod_ecoto,1,1)=="3" && substr(ccod_ecoto,4,4)=="A")
replace region="3B" if (substr(ccod_ecoto,1,1)=="3" && substr(ccod_ecoto,4,4)=="B")

tab luminosid if region=="1A"
tab luminosid if region=="1B"
tab luminosid if region=="2A"
tab luminosid if region=="2B"
tab luminosid if region=="3A"
tab luminosid if region=="3B"
tab luminosid if region=="4"

tab tipo if region=="1A"
tab tipo if region=="1B"
tab tipo if region=="2A"
tab tipo if region=="2B"
tab tipo if region=="3A"
tab tipo if region=="3B"
tab tipo if region=="4"

tab variedad if region=="1A"
tab variedad if region=="1B"
tab variedad if region=="2A"
tab variedad if region=="2B"
tab variedad if region=="3A"
tab variedad if region=="3B"
tab variedad if region=="4"
tab variedad
