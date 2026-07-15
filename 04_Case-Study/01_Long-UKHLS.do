/*****************************************************************************************
* MERGING INDIVIDUAL FILES FROM HARMONISED BHPS AND UKHLS IN LONG FORMAT                 *
* To match individual level files from the harmonised BHPS and Understanding Society     *
* in long format, you need to remove the wave prefixes in the two sets of files and      *
* generate a wave identifier that works across both sets of files. The pidp will         *
* work as the unique cross-wave identifier across both sets of files. This code only     *
* keeps individuals who took part in BHPS and drops those who joined as part of          *
* Understanding Society.                                                                 *
*****************************************************************************************/

// change current file location
cd "C:\work\Forschung\Daten\Understanding Society UK\02_Data"

// assign global macro to refer to Understanding Society data
global ukhls "C:\work\Forschung\Daten\Understanding Society UK\UKDA-6931-stata\stata\stata13_se"


// assign global macros for the lists of waves
global BHPSwaves "a b c d e f g h i j k l m n o p q r"
global UKHLSwaves_bh "b c d e f g h i j k l m" // since BHPS respondents did not take 
									 // part in Wave 1, begin at Wave 2
									 // - update this to include 
									 // new waves as they are released
global UKHLSwaves "a b c d e f g h i j k l m" // use all UKHLS waves
global UKHLSno 10	// number of waves of UKHLS data	



*-------------------------------------------------------------
*------------------- Individual level ------------------------			
*-------------------------------------------------------------				 



// loop through the waves of bhps for feed-forward variables
foreach w of global BHPSwaves {
	// find the wave number
	local waveno=strpos("abcdefghijklmnopqrstuvwxyz","`w'")

	// open the individual file for that wave
	// use pidp b`w'_age_dv b`w'_paygu_dv using "$ukhls\bhps_w`waveno'\b`w'_indresp_protext", clear
	use "$ukhls\bhps\b`w'_indresp_protect", clear
	
	// remove the wave prefix
	rename b`w'_* *
	
	// keep relevant variables
	
	* identifiers
	global vars_id pidp 
	
	* moving vars
	global vars_loc mvever plnowm plnowy4
	
		* keep variables (continue if not available)
	// keep $vars_id $vars_dem $vars_loc vars_educ $vars_socec $vars_he $vars_bkg $vars_hh $vars_att $var_met
	global varlist1 $vars_id
	global varlist $vars_loc
	foreach v of global varlist {
		capture confirm var `v'							//Var exists?
		if !_rc {
			global varlist1 ${varlist1} `v'	//only existing vars in varlist1
		}
	}		
	keep $varlist1
	
	// generate a variable which records the wave number
	gen wave=`waveno'
	
	// gen year
	gen year=1990+`waveno'
	
		// save the file for future use
	save tmp_b`w'_indresp, replace
}





// loop through the relevant waves of Understanding Society

* identifiers
global vars_id pidp hidp pno memorig 

* meta data 
global var_met buno buno_dv sampst ivfio istrtdatd istrtdatm istrtdaty intdatd_dv intdatm_dv intdaty_dv hhmem indin01_lw lrwtuk1 indinus_xw indinub_xw xrwtuk1

* locality
global vars_loc gor_dv mvever mvmnth mvyr plnew movest origadd adcts addrmov_dv movdir lkmove xpmove xpmvmnth xpmvyr plnowm plnowy4 urban_dv

* demographics
// global vars_dem age_dv ukborn bornuk_dv plbornc_cc sex sex_dv mastat marstat jbstat race racel_dv nchild_dv ethn_dv
global vars_dem age age_dv sex plbornc plbornc_all yr2uk4 mastat marstat_dv race racel_dv depchl_dv ohch16 lprnt lnprnt ladopt lnadopt fnspno mnspno mnspid fnspid nchunder16 father nchild nnewborn hood15

* socec
// global vars_socec hiqual_dv jbsoc90_cc jbsoc00_cc jbnssec_dv jbnssec8_dv jbnssec5_dv jbnssec3_dv fimnnet_dv fimnlabnet_dv a_fimnlabgrs_dv paygu_dv
global vars_socec jbstat jbsemp jbhrs jbot jbttwt basnsa basrate basrest paynu_dv paygu_dv fimnlabgrs_dv fimngrs_dv jbnssec_dv jbnssec8_dv jbnssec5_dv finnow save fiyrinvinc_dv fiyrdia

* educ
global vars_educ edasp edtype feend hiqual_dv nhiqual_dv qfhigh_dv scend school

* health and satisfaction
//global vars_he sf12mcs_dv sf12pcs_dv health scghq1_dv scghq2_dv
global vars_he sf1 scsf1 sf12mcs_dv sf12pcs_dv health scghq1_dv scghq2_dv sclfsat1 sclfsat2 sclfsato lfsat1 lfsat2 lfsato scwemwb scwemwbb scghqa scghqb scghqc scghqd scghqe scghqf scghqg scghqh scghqi scghqj scghqk scghql slp_qual hrs_slph hrs_slpm med_slp scslp_qual scmed_slp scsf2a scsf2b scsf3a scsf3b scsf4a scsf4b scsf5 scsf6a scsf6b scsf6c scsf7

// variables efficacy and trust
global vars_eff scwemwb scwemwbb scghqb scghqf scghqd scghqh se1 se2 se3 se4 se5 se6 se7 se8 se9 se10 riska scriska riskb scriskb sctrust ivcoop iv4 volun volfreq chargv charfreq

* individual and family background
// global vars_bkg scend_dv j1soc00_cc maid macob maedqf masoc90_cc masoc00_cc masoc10_cc paid pacob paedqf pasoc90_cc pasoc00_cc pasoc10_cc
global vars_bkg manssec_dv panssec_dv manssec8_dv panssec8_dv paju maju pacob pacob_all macob macob_all maid maedqf masoc90_cc masoc00_cc masoc10_cc paid paedqf pasoc90_cc pasoc00_cc pasoc10_cc

* household
global vars_hh fihhmn nchild_dv husits huboss hoh howlng livesp livewith livpar jnyear ynew

* attitudes
global vars_att scopfama scopfamb scopfamd scopfamf scopfamh oprlg1 oprlg2 oprlg3 vote1 vote2 vote3 vote4 vote5 vote6 vote7 vote8 vote3_all poleff1 poleff2 poleff3 poleff4 oppola oppolb oppolc oppold 

* environment variables
global vars_env1 scenv_bccc scenv_bcon scenv_brit scenv_canc scenv_ccls scenv_cfit scenv_chwo scenv_crex scenv_crlf scenv_dstr scenv_exag scenv_fitl scenv_ftst scenv_futr scenv_grn scenv_meds scenv_noot scenv_nowo scenv_pmep scenv_pmre scenv_tlat orgm3 orga3
global vars_env2 openv1 openv2 openv3 openv4 openva openvb openvc

* Climate change variables
global vars_clim opcca opccb opccc opccd opcce opccf scopecl200 scopecl30

* environmental behaviour variables
global vars_envb1 envhabit1 envhabit2 envhabit3 envhabit4 envhabit5 envhabit6 envhabit7 envhabit8 envhabit9 envhabit10 envhabit11
global vars_envb2 grnlfa grnlfb grnlfc grnlfd grnlfe grnlff grnlfg grnlfh trcarfq trbikefq

* Neighbourhood
global vars_nb simarea nbrcoh1 nbrcoh2 nbrcoh3 nbrcoh4 nbrcoh_dv nbrcohdk_dv nbrsnci_dv crdark crburg crcar crdrnk crgraf crmugg crrace crteen crvand lknbrd llknbrd locchd locsera locserap locseras locserb locserc locserd locsere opngbha opngbhb opngbhc opngbhd opngbhe opngbhf opngbhg opngbhh scopngbha scopngbhb scopngbhc scopngbhd scopngbhe scopngbhf scopngbhg scopngbhh

* Accomodation
global vars_acc lfsat3 lkmovy netuse netpuse

* Commuting
global vars_com jsttwtb jbttwt jbpl twkdiff1 twkdiff2 twkdiff3 twkdiff4 twkdiff5 twkdiff6 twkdiff7 twkdiff8 twkdiff9

* Distances
global vars_dis distmov distmov_dv jsworkdis workdis mafar pafar chfar mlivedistf mlivedist

* Language
global vars_lan iv6d englang natidb engspk engtel engform readdif formdif teldif spkdif

* Fertility
	global vars_fert futrk futrl lchmor lchmorn

foreach w of global UKHLSwaves {

	// find the wave number
	local waveno=strpos("abcdefghijklmnopqrstuvwxyz","`w'")
	
	// open the individual level file for that wave
	// use pidp pid `w'_age_dv `w'_paygu_dv using "$ukhls/ukhls_w`waveno'/`w'_indresp_protect", clear
	use "$ukhls/ukhls/`w'_indresp_protect", clear
	
	/*
	// keep the individual if they have a pid - ie were part of BHPS
	// individuals have pid==-8 (inapplicable) if they were not part of BHPS
	keep if pid>0
	
	// drop the pid variable
	drop pid
	*/
	
	// remove the wave prefix
	rename `w'_* *
	
	// keep relevant variables
	* keep variables (continue if not available)
	// keep $vars_id $vars_loc $vars_dem $vars_socec $vars_he $vars_bkg $vars_hh
	global varlist1 $vars_id
	global varlist $vars_dem $vars_loc $vars_educ $vars_socec $vars_he $vars_eff $vars_bkg $vars_hh $vars_att $var_met $vars_env1 $vars_env2 $vars_clim $vars_envb1 $vars_envb2 $vars_nb $vars_acc $vars_com $vars_dis $vars_lan $vars_fert
	foreach v of global varlist {
		capture confirm var `v'							//Var exists?
		if !_rc {
			global varlist1 ${varlist1} `v'	//only existing vars in varlist1
		}
	}		
	keep $varlist1

	// generate a variable which records the wave number + 17 
	// - treating wave 2 ukhls as wave 19 of bhps --> TR: changed to 18!
	gen wave=`waveno'+18
	
	// gen year
	gen year=1990+`waveno'+18
	
	// save the file for future use
	save tmp_`w'_indresp, replace
}


// loop through the waves of bhps
foreach w of global BHPSwaves {
	
	// first time through the loop
	if "`w'"=="a" {
	
		// reopen the first file created
		use tmp_ba_indresp, clear
		
	// following times through the loop	
	} 
	else {	
		
		// append each file in turn
		append using tmp_b`w'_indresp
	}
}



// Feed forward last information of BHPS

recode plnowm (-99/0 = .)
recode plnowy4 (-99/0 = .)

sort pidp wave

clonevar ff_plnowm = plnowm if !missing(plnowm)
bysort pidp (wave) : replace ff_plnowm = ff_plnowm[_n-1] if missing(plnowm)

clonevar ff_plnowy4 = plnowy4 if !missing(plnowy4)
bysort pidp (wave) : replace ff_plnowy4 = ff_plnowy4[_n-1] if missing(plnowy4)

// reduce to last obs per person
bysort pidp (wave) : gen pynr = _n
bysort pidp (wave) : gen pyN = _N
keep if pynr == pyN
gen ff_plnowy4_year = year

// save relevant vars
keep pidp ff_plnowm ff_plnowy4 ff_plnowy4_year

save tmp_feedforward, replace



// loop through the waves of ukhls from Wave 1
foreach w of global UKHLSwaves {
		// first time through the loop
	if "`w'"=="a" {
	
		// reopen the first file created
		use tmp_a_indresp, clear
		
	// following times through the loop	
	} 
	else {
	// append each file in turn
	append using tmp_`w'_indresp
	}
}


// Merge feed-forwarded vars
merge m:1 pidp using tmp_feedforward.dta
drop if _merge==2
drop _merge

/*
// create labels for the wave variable
// loop through the waves of bhps
foreach n of numlist 1/18 {

	// add a label for each wave number in turn
	lab def wave `n' "BHPS Wave `n'", modify
}
*/

// loop through the waves of ukhls 
// (using the global macro UKHLSno to define the last wave)
foreach n of numlist 1/$UKHLSno {
	
	// calculate which label value this label will apply to
	local waveref=`n'+18
	
	// add a label for each wave in turn
	lab def wave `waveref' "UKHLS Wave `n'", modify
}

// apply the label to the wave variable
lab val wave wave

// check how many observations are available from each wave
tab wave

// order
*order $vars_id $varlist
order pidp hidp pno year wave memorig

// Sort by id year
sort pidp year



// Use xwavedat to update person-constant variables
global var_con birthm birthy ukborn bornuk_dv plbornc plbornc_all scend_dv feend_dv school_dv racel_dv ethn_dv generation yr2uk4 evermar_dv anychild_dv paju maju pacob pacob_all macob macob_all maid maedqf masoc90_cc masoc00_cc masoc10_cc paid paedqf pasoc90_cc pasoc00_cc pasoc10_cc psnenub_xd

/* // not neccessary with update replace option
foreach v of global var_con {
	capture confirm var `v'							//Var exists?
	if !_rc {
		drop `v'	//drop current var
	}
}	
*/

merge m:1 pidp using "$ukhls\ukhls\xwavedat_protect", keepusing(pidp $var_con)  update replace
drop if _merge<=2
drop _merge


// order
* order $vars_id $varlist
order pidp hidp pno year wave memorig gor_dv urban_dv sex age age_dv plbornc yr2uk4 racel_dv mastat marstat_dv hiqual_dv nhiqual_dv qfhigh_dv origadd movdir mvmnth mvyr plnowm plnowy4 lkmove xpmove xpmvmnth xpmvyr 

// Sort by id year
sort pidp year

// Code negatives as missing values
mvdecode _all, mv(-21/-10=.a\-9=.\-8/-7=.b\-2/-1=.)



// save the file containing all waves
save all_indresp, replace

// erase each temporary file using loops

foreach w of global BHPSwaves {
	erase tmp_b`w'_indresp.dta
}
erase tmp_feedforward.dta

foreach w of global UKHLSwaves {
	erase tmp_`w'_indresp.dta
}


*------------------------------------------------------------
*------------------- Household level ------------------------
*------------------------------------------------------------



// loop through the relevant waves of Understanding Society

* identifiers
global vars_hid hidp

* demographics 
global var_hdem hhsize hhtype hhtype_dv agechy_dv nkids_dv nch02_dv nch34_dv nch511_dv nch1215_dv nemp_dv ncouple_dv fihhmngrs_dv fihhmnnet1_dv ieqmoecd_dv carown carval region

* locaility 
global var_hloc hhmove hsivlw hhorigadd crburg crcar crdrnk crgraf crmugg crrace crrubsh crteen crvand

* accomodation
global var_haccom tenure_dv hsownd hsownd_bh hsval hscost mglife mgold mgnew hsroom hsprbg hsprbh hsprbi hsprbj hsprbp hsprbq rent rent_dv rentgrs_dv rent1 rent2 rent3 rent4 rent5 rent6 rent7 xphsdb
global var_haccom2 mgynot mgynot_bh mgextra houscost1_dv houscost2_dv 

* environmental
global var_henv grimyn noisyn

* Electricity and gas
global var_elic fuelhave1 fuelhave2 fuelhave3 xpgasy gaspay xpelecy elecpay xpduely fuelduel duelpay heatch heatyp hheat xpoily

* reference person
global var_head hrpid hrpno

foreach w of global UKHLSwaves {

	// find the wave number
	local waveno=strpos("abcdefghijklmnopqrstuvwxyz","`w'")
	
	// open the individual level file for that wave
	use "$ukhls/ukhls/`w'_hhresp_protect", clear
	
	// remove the wave prefix
	rename `w'_* *
	capture confirm var origadd						//Var exists?
	if !_rc {
		rename origadd hhorigadd	//rename (same var in indresp
	}
	
	
	// keep relevant variables
	* keep variables (continue if not available)
	global varlist1 $vars_hid
	global varlist $var_hdem $var_hloc $var_haccom $var_haccom2 $var_henv $var_elic $var_head 
	foreach v of global varlist {
		capture confirm var `v'							//Var exists?
		if !_rc {
			global varlist1 ${varlist1} `v'	//only existing vars in varlist1
		}
	}		
	keep $varlist1

	// generate a variable which records the wave number + 17 
	// - treating wave 2 ukhls as wave 19 of bhps --> TR: changed to 18!
	gen wave=`waveno'+18
	
	// gen year
	gen year=1990+`waveno'+18
	
	// save the file for future use
	save tmp_`w'_hhresp, replace
}


// loop through the waves of ukhls from Wave 1
foreach w of global UKHLSwaves {
		// first time through the loop
	if "`w'"=="a" {
	
		// reopen the first file created
		use tmp_a_hhresp, clear
		
	// following times through the loop	
	} 
	else {
	// append each file in turn
	append using tmp_`w'_hhresp
	}
}

/*
// create labels for the wave variable
// loop through the waves of bhps
foreach n of numlist 1/18 {

	// add a label for each wave number in turn
	lab def wave `n' "BHPS Wave `n'", modify
}
*/

// loop through the waves of ukhls 
// (using the global macro UKHLSno to define the last wave)
foreach n of numlist 1/$UKHLSno {
	
	// calculate which label value this label will apply to
	local waveref=`n'+18
	
	// add a label for each wave in turn
	lab def wave `waveref' "UKHLS Wave `n'", modify
}

// apply the label to the wave variable
lab val wave wave

// check how many observations are available from each wave
tab wave

// order
*order $vars_hid $varlist
order hidp year wave

// Sort by id year
sort hidp year


// Code negatives as missing values
mvdecode _all, mv(-21/-10=.a\-9=.\-8/-7=.b\-2/-1=.)


// save the file containing all waves
save all_hhresp, replace

// erase each temporary file using loops

/*
foreach w of global BHPSwaves {
	erase tmp_b`w'_hhresp.dta
}
*/


foreach w of global UKHLSwaves {
	erase tmp_`w'_hhresp.dta
}





*-------------------------------------------------------
*------------------- Merge data ------------------------
*-------------------------------------------------------

// Load ind
use all_indresp, clear

// merge hh data
merge m:1 hidp year wave using all_hhresp
drop if _merge==2
drop _merge


// save the file containing all waves
save all_ukhls_11_stata, replace 

// save the file containing all waves
saveold all_ukhls_11, replace nolabel version(12) 

