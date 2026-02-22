*Williamson - A Global Analysis of Transgender Rights: Introducing TRIP
**using "replication_data"

sort country_id year
xtset country_id year

*Figure 1. Mean Global and Regional TRIP Scores, 2000-2021
egen trip_global = mean(trip_percent), by(year)
egen trip_eeca = mean(trip_percent) if e_regionpol_6C==1, by(year)
egen trip_lac = mean(trip_percent) if e_regionpol_6C==2, by(year)
egen trip_mena = mean(trip_percent) if e_regionpol_6C==3, by(year)
egen trip_ssa = mean(trip_percent) if e_regionpol_6C==4, by(year)
egen trip_wena = mean(trip_percent) if e_regionpol_6C==5, by(year)
egen trip_ap = mean(trip_percent) if e_regionpol_6C==6, by(year)

lab var trip_global "Global"
lab var trip_eeca "E. Europe & C. Asia"
lab var trip_lac "L. Amer. & Caribbean"
lab var trip_mena "M. East & N. Africa"
lab var trip_ssa "Sub-Saharan Africa"
lab var trip_ap "Asia & Pacific"
lab var trip_wena "W. Europe & N. Amer."

twoway line trip_global trip_eeca trip_lac trip_mena trip_ssa ///
 trip_ap trip_wena year if year>1999, sort  scheme(s2mono) ///
  legend(col(3) rows(3) size(small) span) yti("TRIP Score") ///
  graphregion(color(white))

*Figure 2. TRIP Score Indicators in 2000 versus 2021

///start -->

preserve
collapse (mean) no_dircrim no_indcrim gmc nophys nopsych nodiv nb3g ///
	adp_general adp_constitution adp_employment adp_education ///
	adp_healthcare adp_housing, by(year)
	
rename (no_dircrim-adp_housing) var=
qui reshape long var, i(year) j(id) string

gen new_id = 1 if id== "no_dircrim"
replace new_id = 2 if id== "no_indcrim"
replace new_id = 3 if id=="gmc"
replace new_id = 4 if id=="nophys" 
replace new_id = 5 if id=="nopsych"
replace new_id = 6 if id=="nodiv"
replace new_id = 7 if id=="nb3g"
replace new_id = 8 if id=="adp_general"
replace new_id = 9 if id=="adp_constitution"
replace new_id = 10 if id=="adp_employment" 
replace new_id = 11 if id=="adp_education"
replace new_id = 12 if id=="adp_healthcare"
replace new_id = 13 if id== "adp_housing"

label define new_id 1 "No direct crim." 2 "No indirect crim." ///
	3 "GMC possible" 4 "No physiological req." ///
	5 "No psychological req." 6 "No divorce req." 7 "Nonbinary marker" ///
	8 "ADP - General" 9 "ADP - Constitution" 10 "ADP - Employment" ///
	11 "ADP - Education" 12 "ADP - Healthcare" 13 "ADP - Housing"
	
label values new_id new_id

twoway (scatter new_id var if year==2000, m(O) mcolor(black) msize(small)) ///
	(scatter new_id var if year==2021, m(D) mcolor(black) msize(small)), ///
	scheme(cleanplots) ylab(1(1)13, val labsize(small)) ///
	legend(order(1 "2000" 2 "2021") pos(6) rows(1) size(small)) yscale(reverse) yti("") ///
	xti("Proportion of Countries", size(small)) xsize(6) ysize(6)
	
restore

///end


*Figure 5. TRIP and LGB Index Averages by Region, 2020
graph hbar trip_percent lgb_percent if year==2020, over(e_regionpol_6C) ///
 legend(order(1 "TRIP Score" 2 "LGB Score")) scheme(s2mono) ///
 graphregion(color(white))


*Figure 6. TRIP versus LGB Index Scores, 2020
twoway scatter trip_percent lgb_percent if year==2020, ///
 jitter (5) color(%50) yti("TRIP Score") xti("LGB Score") legend(off)|| lfitci ///
 trip_percent lgb_percent if year==2020, level(90) color(%50) ///
 graphregion(color(white)) scheme(s2mono)

reg trip_percent lgb_percent
predict res, residual
sort res
list country_name res if year==2020


*Table 4. Descriptive Statistics

	**rescaling polity2 (0-1.0)
	su e_polity2
	gen polity2_rescaled = (e_polity2 - r(min)) / (r(max) - r(min))
	su polity2_rescaled
	lab var polity2_rescaled "Polity2 (rescaled)"
	
xtsum trip_percent v2x_polyarchy polity2_rescaled ///
	lgdppc_wb religiosity_percent


*Table 5. Regressions of TRIP Scores 

sort country_id year

*Pooled OLS with clustered standard errors - EDI
eststo: reg F.trip_percent v2x_polyarchy lgdppc_wb religiosity_percent ///
	ib(5).e_regionpol_6C trip_percent, cluster(country_id)

*OLS with panel corrected standard errors - EDI 
eststo: xtpcse F.trip_percent v2x_polyarchy lgdppc_wb religiosity_percent ///
	ib(5).e_regionpol_6C trip_percent, pairwise

*Pooled OLS with clustered standard errors - POLITY2
eststo: reg F.trip_percent polity2_rescaled lgdppc_wb religiosity_percent ///
	ib(5).e_regionpol_6C trip_percent, cluster(country_id)

*OLS with PCSE - POLITY2
eststo: xtpcse F.trip_percent polity2_rescaled lgdppc_wb religiosity_percent ///
	ib(5).e_regionpol_6C trip_percent, pairwise

esttab _all using results.rtf, se star(* 0.1 ** 0.05 *** 0.01) ///
 stats (N N_clust N_g r2) label


*Figure 7. Predicted Margins with 95% Confidence Intervals
 eststo clear

	*Pooled OLS with clustered standard errors - EDI
	eststo: reg F.trip_percent v2x_polyarchy lgdppc_wb religiosity_percent ///
		ib(5).e_regionpol_6C trip_percent, cluster(country_id)
	
	*OLS with panel corrected standard errors - EDI 
	eststo: xtpcse F.trip_percent v2x_polyarchy lgdppc_wb religiosity_percent ///
		ib(5).e_regionpol_6C trip_percent, pairwise
		
	est restore est1
	margins, at(v2x_polyarchy=(0(0.1)1))
	marginsplot, name(M2, replace) ti("Pooled OLS - EDI") ///
	yti("TRIP Score") scheme(s2mono) graphregion(color(white))
	
	est restore est2
	margins, at(v2x_polyarchy=(0(0.1)1))
	marginsplot, name(M3, replace) ti("Panel Corrected SE - EDI") ///
	yti("TRIP Score") scheme(s2mono) graphregion(color(white))
	
	est restore est1
	margins, at(lgdppc_wb=(0(1)12))
	marginsplot, name(M4, replace) ti("Pooled OLS - GDP per capita") ///
	yti("TRIP Score") scheme(s2mono) graphregion(color(white))
	
	est restore est2
	margins, at(lgdppc_wb=(0(1)12))
	marginsplot, name(M5, replace) ti("Panel Corrected SE - GDP per capita") ///
	yti("TRIP Score") scheme(s2mono) graphregion(color(white))
	
	graph combine M2 M3 M4 M5, ycommon graphregion(color(white))

***Appendix

 *Table A1. Testing for Serial Correlation
 eststo clear
 drop res
 
 
 eststo: reg trip_percent L.trip_percent v2x_polyarchy lgdppc_wb religiosity_percent ///
	ib(5).e_regionpol_6C, cluster(country_id)

predict res, residuals

eststo: reg res L.res L.trip_percent v2x_polyarchy lgdppc_wb religiosity_percent ///
	ib(5).e_regionpol_6C, cluster(country_id)

esttab _all using serialcorr.rtf, se star(* 0.1 ** 0.05 *** 0.01) ///
		stats (N N_clust r2) label
	
	
*Table A2. Testing for Multicollinearity 
eststo clear
	eststo: reg F.trip_percent v2x_polyarchy ///
		lgdppc_wb religiosity_percent ib(5).e_regionpol_6C, ///
			cluster(country_id)
		
	vif
	
	esttab using multicollinearity.rtf, se star(* 0.1 ** 0.05 *** 0.01) ///
		stats (N N_clust r2) label


*Table A3. Regressions with Alternative Measures of Regime Type
eststo clear

**Democracy Measures
*Pooled OLS with clustered standard errors - RoW
eststo: reg F.trip_percent ib(3).v2x_regime lgdppc_wb religiosity_percent ///
	ib(5).e_regionpol_6C trip_percent, cluster(country_id)

*OLS with PCSE - RoW
eststo: xtpcse F.trip_percent ib(3).v2x_regime lgdppc_wb religiosity_percent ///
	ib(5).e_regionpol_6C trip_percent, pairwise
	
*Pooled OLS with clustered standard errors - BMR
eststo: reg F.trip_percent e_boix_regime lgdppc_wb religiosity_percent ///
	ib(5).e_regionpol_6C trip_percent, cluster(country_id)

*OLS with PCSE - BMR 
eststo: xtpcse F.trip_percent e_boix_regime lgdppc_wb religiosity_percent ///
	ib(5).e_regionpol_6C trip_percent, pairwise

esttab _all using robustdem1.rtf, se star(* 0.1 ** 0.05 *** 0.01) ///
 stats (N N_clust N_g r2) label

eststo clear



*Table A4. Regressions with an Alternative Measure of Religion
*Pooled OLS with clustered standard errors - RCS (% religious)
eststo: reg F.trip_percent v2x_polyarchy lgdppc_wb relpc_rcs ///
		ib(5).e_regionpol_6C trip_percent, cluster(country_id)

*OLS with PCSE - RCS (% religious)
eststo: xtpcse F.trip_percent v2x_polyarchy lgdppc_wb relpc_rcs ///
		ib(5).e_regionpol_6C trip_percent, pairwise
		

esttab _all using rcspercent.rtf, se star(* 0.1 ** 0.05 *** 0.01) ///
 stats (N N_clust N_g r2) label
 
eststo clear


*Table A5. Regressions with Measures for Specific Religions

*Pooled OLS with clustered standard errors - RCS % Christian, Muslim, Non	
eststo: reg F.trip_percent chrpc_rcs muspc_rcs nrepc_rcs ///
	v2x_polyarchy lgdppc_wb ib(5).e_regionpol_6C ///
		trip_percent, cluster(country_id)

*OLS with PCSE - RCS % Christian, Muslim, Non
eststo: xtpcse F.trip_percent chrpc_rcs muspc_rcs nrepc_rcs ///
	v2x_polyarchy lgdppc_wb ib(5).e_regionpol_6C ///
		trip_percent, pairwise

esttab _all using rcsreligions.rtf, se star(* 0.1 ** 0.05 *** 0.01) ///
	stats (N N_clust N_g r2) label

