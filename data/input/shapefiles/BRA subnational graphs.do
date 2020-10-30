////////////////////////////////////////////////////////////////////////////////
////////																////////
//////// OxCGRT Brazil Subnational COVID-19 Government Response Tracker ////////
////////																////////
////////	This is a .do file to clean and process data exported 		////////
////////	from the OxCGRT Brazil subnational database.				////////
////////																////////
////////	You will need to download the CSV file from the data base   ////////
////////	before you run the .do file.								////////
////////																////////
//////////////////////////////////////////////////////////////////////////////// 

** CLEANING THE DATA 

** setting up
clear all
capture log close
set more off

** import the data
cd "/Users/wolf5258/Nexus365/BSG - Covid-19 Tracker - Documents/General/Personal analysis/Bea"
import delim "/Users/wolf5258/Nexus365/BSG - Covid-19 Tracker - Documents/General/Personal analysis/Bea/OxCGRT_Download_100920_061634.csv" , clear
drop *notes

rename date dateSTR
tostring dateSTR, replace
gen date = date(dateSTR,"YMD") , before(dateSTR)
format date %td
sort regioncode date
drop dateSTR

** set the date
global data_date : display %tdCCYYNNDD td($S_DATE)
global yesterday : display %tdDD_Mon_CCYY td($S_DATE)-1
global fignote "Data from $yesterday. Individual countries may be several days older."
global caption "Source: OxCGRT Brazilian Sub-National Covid-19 Policy Responses. More at: https://github.com/OxCGRT/Brazil-covid-policy"

** create regional labels
global Centro_Oeste "BR_GO BR_MT BR_MS BR_DF"
global Nordeste "BR_AL BR_BA BR_CE BR_MA BR_PB BR_PE BR_PI BR_RN BR_SE"
global Norte "BR_AC BR_AP BR_AM BR_PA BR_RR BR_RO BR_TO"
global Sudeste "BR_ES BR_MG BR_RJ BR_SP"
global Sul "BR_PR BR_RS BR_SC"
global region_bra_list "Centro_Oeste Nordeste Norte Sudeste Sul"

gen regionarea = "NORDESTE", after(regioncode)
replace regionarea = "NORTE" if regioncode =="BR_AC"
replace regionarea = "NORTE" if regioncode =="BR_AP"
replace regionarea = "NORTE" if regioncode =="BR_AM"
replace regionarea = "NORTE" if regioncode =="BR_PA"
replace regionarea = "NORTE" if regioncode =="BR_RR"
replace regionarea = "NORTE" if regioncode =="BR_RO"
replace regionarea = "NORTE" if regioncode =="BR_TO"
replace regionarea = "CENTRO-OESTE" if regioncode =="BR_GO"
replace regionarea = "CENTRO-OESTE" if regioncode =="BR_MT"
replace regionarea = "CENTRO-OESTE" if regioncode =="BR_MS"
replace regionarea = "CENTRO-OESTE" if regioncode =="BR_DF"
replace regionarea = "SUDESTE" if regioncode =="BR_ES"
replace regionarea = "SUDESTE" if regioncode =="BR_SP"
replace regionarea = "SUDESTE" if regioncode =="BR_RJ"
replace regionarea = "SUDESTE" if regioncode =="BR_MG"
replace regionarea = "SUL" if regioncode =="BR_PR"
replace regionarea = "SUL" if regioncode =="BR_RS"
replace regionarea = "SUL" if regioncode =="BR_SC"

gen citytype = "Second_city", after(cityname)
replace citytype = "State_level" if cityname =="State government"
replace citytype = "Capital_city" if cityname =="Rio Branco"
replace citytype = "Capital_city" if cityname =="Maceio"
replace citytype = "Capital_city" if cityname =="Macapa"
replace citytype = "Capital_city" if cityname =="Manaus"
replace citytype = "Capital_city" if cityname =="Salvador"
replace citytype = "Capital_city" if cityname =="Fortaleza"
replace citytype = "Capital_city" if cityname =="Brasilia"
replace citytype = "Capital_city" if cityname =="Vitoria"
replace citytype = "Capital_city" if cityname =="Goiania"
replace citytype = "Capital_city" if cityname =="Sao Luis"
replace citytype = "Capital_city" if cityname =="Cuiaba"
replace citytype = "Capital_city" if cityname =="Campo Grande"
replace citytype = "Capital_city" if cityname =="Belo Horizonte"
replace citytype = "Capital_city" if cityname =="Belem"
replace citytype = "Capital_city" if cityname =="Joao Pessoa"
replace citytype = "Capital_city" if cityname =="Curitiba"
replace citytype = "Capital_city" if cityname =="Recife"
replace citytype = "Capital_city" if cityname =="Teresina"
replace citytype = "Capital_city" if cityname =="Natal"
replace citytype = "Capital_city" if cityname =="Porto Alegre"
replace citytype = "Capital_city" if cityname =="Rio de Janeiro"
replace citytype = "Capital_city" if cityname =="Porto Velho"
replace citytype = "Capital_city" if cityname =="Boa Vista"
replace citytype = "Capital_city" if cityname =="Florianopolis"
replace citytype = "Capital_city" if cityname =="Sao Paulo"
replace citytype = "Capital_city" if cityname =="Aracaju"
replace citytype = "Capital_city" if cityname =="Palmas"

gen statename = "City government", after(regionarea)
replace statename = "Acre" if cityname =="State government" & regioncode=="BR_AC"
replace statename = "Alagoas" if cityname =="State government" & regioncode=="BR_AL"
replace statename = "Amapá" if cityname =="State government" & regioncode=="BR_AP"
replace statename = "Amazonas" if cityname =="State government" & regioncode=="BR_AM"
replace statename = "Bahia" if cityname =="State government" & regioncode=="BR_BA"
replace statename = "Ceará" if cityname =="State government" & regioncode=="BR_CE"
replace statename = "Distrito Federal" if cityname =="State government" & regioncode=="BR_DF"
replace statename = "Espírito Santo" if cityname =="State government" & regioncode=="BR_ES"
replace statename = "Goiás" if cityname =="State government" & regioncode=="BR_GO"
replace statename = "Maranhão" if cityname =="State government" & regioncode=="BR_MA"
replace statename = "Mato Grosso" if cityname =="State government" & regioncode=="BR_MT"
replace statename = "Mato Grosso do Sul" if cityname =="State government" & regioncode=="BR_MS"
replace statename = "Minas Gerais" if cityname =="State government" & regioncode=="BR_MG"
replace statename = "Pará" if cityname =="State government" & regioncode=="BR_PA"
replace statename = "Paraíba" if cityname =="State government" & regioncode=="BR_PB"
replace statename = "Paraná" if cityname =="State government" & regioncode=="BR_PR"
replace statename = "Pernambuco" if cityname =="State government" & regioncode=="BR_PE"
replace statename = "Piauí" if cityname =="State government" & regioncode=="BR_PI"
replace statename = "Rio Grande do Norte" if cityname =="State government" & regioncode=="BR_RN"
replace statename = "Rio Grande do Sul" if cityname =="State government" & regioncode=="BR_RS"
replace statename = "Rio de Janeiro" if cityname =="State government" & regioncode=="BR_RJ"
replace statename = "Rondônia" if cityname =="State government" & regioncode=="BR_RO"
replace statename = "Roraima" if cityname =="State government" & regioncode=="BR_RR"
replace statename = "Santa Catarina" if cityname =="State government" & regioncode=="BR_SC"
replace statename = "São Paulo" if cityname =="State government" & regioncode=="BR_SP"
replace statename = "Sergipe" if cityname =="State government" & regioncode=="BR_SE"
replace statename = "Tocantins" if cityname =="State government" & regioncode=="BR_TO"

** interpolate missing case data
sort countrycode regioncode date
bysort countrycode regioncode : ipolate confirmedcases date , g(cases_ipo)
bysort countrycode regioncode : ipolate confirmeddeaths date , g(deaths_ipo)
replace confirmedcases = cases_ipo
replace confirmeddeaths = deaths_ipo
bysort countrycode regioncode (date): carryforward confirmedcases, replace
bysort countrycode regioncode (date): carryforward confirmeddeaths, replace
drop cases_ipo deaths_ipo

** carryforward indices
sort countrycode regioncode date
by countrycode regioncode (date): carryforward stringencyindexfordisplay , replace
by countrycode regioncode (date): carryforward governmentresponseindexfordispla , replace
by countrycode regioncode (date): carryforward containmenthealthindexfordisplay , replace
by countrycode regioncode (date): carryforward economicsupportindexfordisplay , replace

** generate log values of cases and deaths
gen cases_log10=log10(confirmedcases+1) , after(confirmedcases)
label define log10label -8 "0.00000001" -7 "0.0000001" -6 "0.000001" -5 "0.00001" -4 "0.0001" -3 "0.001" -2 "0.01" -1 "0.1" 0 "1" 1 "10" 2 "100" 3 "1,000" 4 "10,000" 5 "100,000" 6 "1,000,000"
label values cases_log10 log10label

gen deaths_log10=log10(confirmeddeaths+1) , after(confirmeddeaths)
label values deaths_log10 log10label

gen cases_ln=log(confirmedcases+1) , after(cases_log10)
gen deaths_ln=log(confirmeddeaths+1) , after(deaths_log10)

** gen date of first case
gen date_firstcase=. , after(confirmedcases)
sort regionname date 
by regionname: replace date_firstcase=date if (confirmedcases[_n-1]==0 | confirmedcases[_n-1]==.) & confirmedcases>0
by regionname: replace date_firstcase=date if [_n]==1 & confirmedcases>0
by regionname : egen date_firstcaseALL = min(date_firstcase)
format date_firstcaseALL %td
gen daysafterfirst = date-date_firstcaseALL /*if date_firstcaseALL<date*/ , after(confirmedcases)
drop date_firstcase date_firstcaseALL

** drop if date after 1 August
drop if date>22128

** generate latest SI

global indexlist "stringencyindexfordisplay"

foreach index in $indexlist {
	sort regionname date
	
	local title `: di substr("`index'", 1, length("`index'")-5)'
	
	** generate the current (and previous maximum) stringency level in each region
	sort regionname date 
	gen `title'_date = date if `index'!=.
	by regionname: egen last_`title'_date = max(`title'_date)
	by regionname: gen last_`title'_temp = `index' if date==last_`title'_date
	by regionname: egen latest_`title' = max(last_`title'_temp)
	
	drop `title'_date last_`title'_date last_`title'_temp
}




/// TOBY's GRAPH ///

** set sort order and assign each state to a page
*drop if citycode != "STATE_GOV" [para evitar o zig zag - enable so na hora de criar o gráfico]
bysort regionname: gen nvals = _n == 1
sort regionname date

egen order_temp = group(regionname)
	gen page=.
	replace page = 1 if order_temp<=28
	replace page = 2 if order_temp>28
	drop order_temp

** multiple state graphs in one page (cases and stringency)
forvalues page = 1/2 {
	preserve
	keep if page == `page'
	egen regionorder = group(regionname)
	count if nvals
	local num = 28 - `r(N)'
	local holes = ""
	local name = ""
	forvalues x = 1/`num' {
		insobs 1 , after(_N)
		local name = "`name'" + " "
		replace regionname = "`name'" if _n==_N
		replace regionorder = 28 - `num' +`x' if _n==_N
		}
	labmask regionorder, values(regionname)
	//NOTE! The following line function produces a line graph, but it duplicates the left y-axis. I have recorded a local graph macro on my computer (the "play" option), but this won't work for you. Happy to explain if needed.
	line cases_log10 daysafterfirst , yaxis(1) ylabel(0 1 2 3 4, valuelabel) lpatter(solid)  ytitle("Reported cases", axis(1) size(small)) ///
		|| line stringencyindexfordisplay daysafterfirst , yaxis(2) yscale(axis(2) range(0 100)) lcolor(red) lpatter(solid) ylabel(#5 , axis(2) labc(red)) ytitle("Stringency index", axis(2) size(small) color(red)) ///
		, by(regionorder , iytitle rows(8) legend(off) note("") noedgelabel imargin(tiny)) xtitle("Days after first recorded case", size(vsmall)) play(Get rid of left axis in BY chart) ysize(8.1) xsize(6.1)
	restore
	}


/// BEA's GRAPH ///

** generate line graphs of SI and cases in all states
set scheme plotplain
preserve
drop if statename =="City government"
line cases_log10 date , by(statename, note("") legend(off)) yaxis(1) ylabel(0 1 2 3 4 5, valuelabel) lpatter(solid)  ytitle("Reported cases", axis(1) size(small)) ///
			|| line stringencyindexfordisplay date, by(statename, note("")) yaxis(2) ytitle("Stringency index", axis(2) size(small) color(red)) yscale(axis(2) range(0 100)) lcolor(red) lpatter(solid) ylabel(#5 , axis(2) labc(red)) ///
			, xtitle("Date", size(small)) xlabel(, format(%td_d_m))
restore

*** Plotting graphs state + city + second city SI [in English]
set scheme plotplain

preserve
line stringencyindexfordisplay date if regioncode == "BR_GO" & citycode == "STATE_GOV" /// 
		,yscale(range(0 100)) lpatter(solid) ///
		ylabel(#5 ,) ytitle("Stringency index", size(small)) ///
		, xtitle("Date", size(small)) xlabel(, format(%td_d_m)) title("Goiás Covid-19 trajectory") ///
		caption($caption, span size(vsmall))  ///
	|| line stringencyindexfordisplay date if regioncode == "BR_GO" & citycode == "BR_5208707" ///
	, lpatter(longdash) ///
	|| line stringencyindexfordisplay date if regioncode == "BR_GO" & citycode == "BR_5201405" ///
	, lpatter(shortdash_dot)	legend(label (1 "Goiás state") label (2 "Goiânia") label (3 "Aparecida de Goiânia") size(vsmall)) 
restore

*** Plotting graphs state + city + second city [em português]
set scheme plotplain

preserve
line stringencyindexfordisplay date if regioncode == "BR_MG" & citycode == "STATE_GOV" /// 
		,yscale(range(0 100)) lpatter(solid) ///
		ylabel(#5 ,) ytitle("Índice de rigidez", size(small)) ///
		, xtitle("Data", size(small)) xlabel(, format(%tddd/nn/yyyy)) title("Minas Gerais Covid-19 trajectory") ///
		caption("Fonte: OxCGRT Políticas Brasileiras Subnacionais de Resposta à Covid-19. Mais em: https://github.com/OxCGRT/Brazil-covid-policy", span size(vsmall))  ///
	|| line stringencyindexfordisplay date if regioncode == "BR_MG" & citycode == "BR_3106200" ///
	, lpatter(longdash) ///
	|| line stringencyindexfordisplay date if regioncode == "BR_MG" & citycode == "BR_3170206" ///
	, lpatter(shortdash_dot)	legend(label (1 "Minas Gerais") label (2 "Belo Horizonte") label (3 "Uberlândia") size(vsmall))
restore

*** Plotting graphs state + city + second city [em português com cores]
preserve
line stringencyindexfordisplay date if regioncode == "BR_MG" & citycode == "STATE_GOV" /// 
		,yscale(range(0 100)) lpatter(solid) lcolor(dkorange) ///
		ylabel(#5 ,) ytitle("Índice de rigidez", size(small)) ///
		, xtitle("", size(small)) xlabel(, labsize(*0.8) format(%tddd/nn/yyyy)) title("Minas Gerais") subtitle("Evolução das Políticas de Resposta à Covid-19", size(small)) ///
		caption("Fonte: OxCGRT Políticas Brasileiras Subnacionais de Resposta à Covid-19. Mais em: https://github.com/OxCGRT/Brazil-covid-policy", span size(tiny))  ///
	|| line stringencyindexfordisplay date if regioncode == "BR_MG" & citycode == "BR_3106200" ///
	, lpatter(solid) lcolor(emerald) ///
	|| line stringencyindexfordisplay date if regioncode == "BR_MG" & citycode == "BR_3170206" ///
	, lpatter(solid) lcolor(lavender) legend(label (1 "Minas Gerais") label (2 "Belo Horizonte") label (3 "Uberlândia") size(vsmall))
restore


*** Plotting individual graphs of SI for each unit
set scheme plotplain

preserve
drop if regioncode != "BR_SP"
drop if citycode != "STATE_GOV"
line stringencyindexfordisplay date, yscale(range(0 100)) lcolor(red) lpatter(solid) ylabel(#5 , labc(red)) ytitle("Stringency index", size(small) color(red)) ///
	, xtitle("Date", size(small)) xlabel(, format(%td_d_m)) ///
	title("São Paulo Covid-19 trajectory") caption($caption, span size(vsmall)) note($fignote, span size(vsmall)) legend(off) ///
	name("graphSPstate", replace)
restore

preserve
drop if regioncode != "BR_SP"
drop if citycode != "BR_3550308"
line stringencyindexfordisplay date, yscale(range(0 100)) lcolor(red) lpatter(solid) ylabel(#5 , labc(red)) ytitle("Stringency index", size(small) color(red)) ///
	, xtitle("Date", size(small)) xlabel(, format(%td_d_m)) ///
	title("São Paulo City Covid-19 trajectory") caption($caption, span size(vsmall)) note($fignote, span size(vsmall)) legend(off) ///
	name("graphSPcity", replace)
restore

** generate some state graphs BRA (deaths and stringency)

set scheme plotplain

foreach region in BR_SP {
	preserve
	keep if regioncode=="`region'"
	if [_N]!=0 {
		local titlename = regionname[1]
		line deaths_log10 date , yaxis(1) ylabel(0 1 2 3 4 5, valuelabel) lpatter(solid)  ytitle("Reported deaths", axis(1) size(small)) ///
			|| line stringencyindexfordisplay date , yaxis(2) yscale(axis(2) range(0 100)) lcolor(red) lpatter(solid) ylabel(#5 , axis(2) labc(red)) ytitle("Stringency index", axis(2) size(small) color(red)) ///
			, xtitle("Date", size(small)) xlabel(, format(%td_d_m)) title("`titlename''s Covid-19 trajectory") caption($caption, span size(vsmall)) note($fignote, span size(vsmall)) legend(off)
		graph export "images/`titlename'.png" , replace
		}
	restore
	}




/** MAPS BRAZIL SUBNATIONAL

** installing packages
ssc install spmap
ssc install tmap
ssc install shp2dta
ssc install mif2dta

** creating data shape files
// shp2dta using BRUFE250GC_SIR, database(bradb) coordinates(bracoord) genid(id)


** preparing the data
gen NM_ESTADO = regionname
replace NM_ESTADO = "AMAPÁ" if regionname=="Amapa"
replace NM_ESTADO = "ACRE" if regionname=="Acre"
replace NM_ESTADO = "ALAGOAS" if regionname=="Alagoas"
replace NM_ESTADO = "BAHIA" if regionname=="Bahia"
replace NM_ESTADO = "CEARÁ" if regionname=="Ceara"
replace NM_ESTADO = "DISTRITO FEDERAL" if regionname=="Distrito Federal"
replace NM_ESTADO = "ESPÍRITO SANTO" if regionname=="Espirito Santo"
replace NM_ESTADO = "GOIÁS" if regionname=="Goias"
replace NM_ESTADO = "MARANHÃO" if regionname=="Maranhao"
replace NM_ESTADO = "MATO GROSSO" if regionname=="Mato Grosso"
replace NM_ESTADO = "MATO GROSSO DO SUL" if regionname=="Mato Grosso do Sul"
replace NM_ESTADO = "MINAS GERAIS" if regionname=="Minas Gerais"
replace NM_ESTADO = "PARÁ" if regionname=="Para"
replace NM_ESTADO = "PARAÍBA" if regionname=="Paraiba"
replace NM_ESTADO = "PERNAMBUCO" if regionname=="Pernambuco"
replace NM_ESTADO = "PIAUÍ" if regionname=="Piaui"
replace NM_ESTADO = "RIO DE JANEIRO" if regionname=="Rio de Janeiro"
replace NM_ESTADO = "RIO GRANDE DO NORTE" if regionname=="Rio Grande do Norte"
replace NM_ESTADO = "RIO GRANDE DO SUL" if regionname=="Rio Grande do Sul"
replace NM_ESTADO = "RORAIMA" if regionname=="Roraima"
replace NM_ESTADO = "SANTA CATARINA" if regionname=="Santa Catarina"
replace NM_ESTADO = "SÃO PAULO" if regionname=="Sao Paulo"
replace NM_ESTADO = "SERGIPE" if regionname=="Sergipe"
replace NM_ESTADO = "TOCANTINS" if regionname=="Tocantins"
replace NM_ESTADO = "PARANÁ" if regionname=="Parana"
replace NM_ESTADO = "RONDÔNIA" if regionname=="Rondonia"
replace NM_ESTADO = "AMAZONAS" if regionname=="Amazonas"


** Map SI 01 March 2020
preserve
drop if citycode !="STATE_GOV"
drop if date !=21975
merge m:1 NM_ESTADO using "/Users/wolf5258/OneDrive - Nexus365/Covid-19 Tracker/Bea's code/bradb"
keep NM_ESTADO id stringencyindexfordisplay
collapse id stringencyindexfordisplay, by(NM_ESTADO)
sort id
spmap stringencyindexfordisplay using "/Users/wolf5258/OneDrive - Nexus365/Covid-19 Tracker/Bea's code/bracoord" , id(id) clmethod(custom) clnumber(5) clbreaks(0 10 20 30 40 50 60 70 80 90) fcolor(Greys) ndfcolor(cranberry*0.1) legtitle("Stringengy Index") caption($caption, size(vsmall)) title("Brazil Subnational Government Responses to COVID-19 - 1 March 2020", size(medsmall) position(11)) xsize(9) ysize(4)
graph export "images/OxCGRT_worldmap_govresponse.png", replace
restore

** Map SI 01 April 2020
preserve
drop if citycode !="STATE_GOV"
drop if date !=22006
merge m:1 NM_ESTADO using "/Users/wolf5258/OneDrive - Nexus365/Covid-19 Tracker/Bea's code/bradb"
keep NM_ESTADO id stringencyindexfordisplay
collapse id stringencyindexfordisplay, by(NM_ESTADO)
sort id
spmap stringencyindexfordisplay using "/Users/wolf5258/OneDrive - Nexus365/Covid-19 Tracker/Bea's code/bracoord" , id(id) clmethod(custom) clnumber(5) clbreaks(0 10 20 30 40 50 60 70 80 90) fcolor(Greys) ndfcolor(cranberry*0.1) legtitle("Stringengy Index") caption($caption, size(vsmall)) title("Brazil Subnational Government Responses to COVID-19 - 1 April 2020", size(medsmall) position(11)) xsize(9) ysize(4)
graph export "images/OxCGRT_worldmap_govresponse.png", replace
restore

** Map SI 01 May 2020
preserve
drop if citycode !="STATE_GOV"
drop if date !=22036
merge m:1 NM_ESTADO using "/Users/wolf5258/OneDrive - Nexus365/Covid-19 Tracker/Bea's code/bradb"
keep NM_ESTADO id stringencyindexfordisplay
collapse id stringencyindexfordisplay, by(NM_ESTADO)
sort id
spmap stringencyindexfordisplay using "/Users/wolf5258/OneDrive - Nexus365/Covid-19 Tracker/Bea's code/bracoord" , id(id) clmethod(custom) clnumber(5) clbreaks(0 10 20 30 40 50 60 70 80 90) fcolor(Greys) ndfcolor(cranberry*0.1) legtitle("Stringengy Index") caption($caption, size(vsmall)) title("Brazil Subnational Government Responses to COVID-19 - 1 May 2020", size(medsmall) position(11)) xsize(9) ysize(4)
graph export "images/OxCGRT_worldmap_govresponse.png", replace
restore

** Map SI 01 June 2020
preserve
drop if citycode !="STATE_GOV"
drop if date !=22067
merge m:1 NM_ESTADO using "/Users/wolf5258/OneDrive - Nexus365/Covid-19 Tracker/Bea's code/bradb"
keep NM_ESTADO id stringencyindexfordisplay
collapse id stringencyindexfordisplay, by(NM_ESTADO)
sort id
spmap stringencyindexfordisplay using "/Users/wolf5258/OneDrive - Nexus365/Covid-19 Tracker/Bea's code/bracoord" , id(id) clmethod(custom) clnumber(5) clbreaks(0 10 20 30 40 50 60 70 80 90) fcolor(Greys) ndfcolor(cranberry*0.1) legtitle("Stringengy Index") caption($caption, size(vsmall)) title("Brazil Subnational Government Responses to COVID-19 - 1 June 2020", size(medsmall) position(11)) xsize(9) ysize(4)
graph export "images/OxCGRT_worldmap_govresponse.png", replace
restore

** Map SI 01 July 2020
preserve
drop if citycode !="STATE_GOV"
drop if date !=22097
merge m:1 NM_ESTADO using "/Users/wolf5258/OneDrive - Nexus365/Covid-19 Tracker/Bea's code/bradb"
keep NM_ESTADO id stringencyindexfordisplay
collapse id stringencyindexfordisplay, by(NM_ESTADO)
sort id
spmap stringencyindexfordisplay using "/Users/wolf5258/OneDrive - Nexus365/Covid-19 Tracker/Bea's code/bracoord" , id(id) clmethod(custom) clnumber(5) clbreaks(0 10 20 30 40 50 60 70 80 90) fcolor(Greys) ndfcolor(cranberry*0.1) legtitle("Stringengy Index") caption($caption, size(vsmall)) title("Brazil Subnational Government Responses to COVID-19 - 1 July 2020", size(medsmall) position(11)) xsize(9) ysize(4)
graph export "images/OxCGRT_worldmap_govresponse.png", replace
restore

** Map SI 01 August 2020
preserve
drop if citycode !="STATE_GOV"
drop if date !=22128
merge m:1 NM_ESTADO using "/Users/wolf5258/OneDrive - Nexus365/Covid-19 Tracker/Bea's code/bradb"
keep NM_ESTADO id stringencyindexfordisplay
collapse id stringencyindexfordisplay, by(NM_ESTADO)
sort id
spmap stringencyindexfordisplay using "/Users/wolf5258/OneDrive - Nexus365/Covid-19 Tracker/Bea's code/bracoord" , id(id) clmethod(custom) clnumber(5) clbreaks(0 10 20 30 40 50 60 70 80 90) fcolor(Greys) ndfcolor(cranberry*0.1) legtitle("Stringengy Index") caption($caption, size(vsmall)) title("Brazil Subnational Government Responses to COVID-19 - 1 August 2020", size(medsmall) position(11)) xsize(9) ysize(4)
graph export "images/OxCGRT_worldmap_govresponse.png", replace
restore

** Map latest SI (considering the data drop)
preserve
drop if citycode !="STATE_GOV"
merge m:1 NM_ESTADO using "/Users/wolf5258/OneDrive - Nexus365/Covid-19 Tracker/Bea's code/bradb"
keep NM_ESTADO id latest_stringencyindexfordi
collapse id latest_stringencyindexfordi, by(NM_ESTADO)
sort id
spmap latest_stringencyindexfordi using "/Users/wolf5258/OneDrive - Nexus365/Covid-19 Tracker/Bea's code/bracoord" , id(id) clmethod(custom) clnumber(5) clbreaks(0 10 20 30 40 50 60 70 80 90) fcolor(Greys) ndfcolor(cranberry*0.1) legtitle("Stringengy Index") caption($caption, size(vsmall)) title("Brazil Subnational Government Responses to COVID-19", size(medsmall) position(11)) xsize(9) ysize(4)
graph export "images/OxCGRT_worldmap_govresponse.png", replace
restore

*/


/** HEAT PLOT BRAZIL **


*** install the baseline graph scheme
net install cleanplots, from("https://tdmize.github.io/data/cleanplots")
set scheme cleanplots, perm

*** install the packages
ssc install heatplot, replace
ssc install colorpalette, replace
ssc install colrspace, replace

** plot heap plot state_gov policies

preserve
drop if citycode !="STATE_GOV"

ren stringencyindexfordisplay Index  

sort regionname
encode regionname , gen(state2)

egen state3 = rank(-state2), by(date)
labmask state3, val(regionname)

summ date 
  local x1 = `r(min)'
  local x2 = `r(max)'

heatplot Index i.state3 date, ///
 yscale(noline) ///
 ylabel(, nogrid labsize(*0.7)) ///
 xlabel(`x1'(5)`x2', labsize(*0.6) angle(vertical) format(%tdDD-Mon) nogrid) ///
 color(viridis, reverse) ///   
 cuts(0(10)100) ///
  ramp(right space(10) label(0(10)100) xlabel("", labsize(*0.7)) ytitle("", size(*0.7)) xtitle("", size(*0.7))) ///
  p(lcolor(gs10) lwidth(*0.05)) ///
 ytitle("") ///
 xtitle("", size(vsmall)) ///
 title("OxCGRT Stringency Index - Brazilian States", size(small)) ///
note("Source: Oxford Covid-19 Government Response Tracker. More at https://github.com/OxCGRT", size(vsmall)) 

restore

** plot heap plot capital policies

preserve
drop if citycode =="STATE_GOV"

ren stringencyindexfordisplay Index  

sort cityname
encode cityname , gen(city2)

egen city3 = rank(-city2), by(date)
labmask city3, val(cityname)

summ date 
  local x1 = `r(min)'
  local x2 = `r(max)'

heatplot Index i.city3 date, ///
 yscale(noline) ///
 ylabel(, nogrid labsize(*0.7)) ///
 xlabel(`x1'(5)`x2', labsize(*0.6) angle(vertical) format(%tdDD-Mon) nogrid) ///
 color(viridis, reverse) ///   
 cuts(0(10)100) ///
  ramp(right space(10) label(0(10)100) xlabel("", labsize(*0.7)) ytitle("", size(*0.7)) xtitle("", size(*0.7))) ///
  p(lcolor(gs10) lwidth(*0.05)) ///
 ytitle("") ///
 xtitle("", size(vsmall)) ///
 title("OxCGRT Stringency Index - Brazilian States", size(small)) ///
note("Source: Oxford Covid-19 Government Response Tracker. More at https://github.com/OxCGRT", size(vsmall)) 

restore

/** gen 7-day moving average cases (notsure why this doesn't work)
sort countrycode regioncode date
by countrycode regioncode: gen moveave_confirmedcases = (confirmedcases+confirmedcases[_n-1]+confirmedcases[_n-2]+confirmedcases[_n-3]+confirmedcases[_n-4]+confirmedcases[_n-5]+confirmedcases[_n-6])/7
bys countrycode regioncode: gen newcases= moveave_confirmedcases-moveave_confirmedcases[_n-1]
gen newcases_log10=log10(newcases+1) , after(newcases)
label values newcases_log10 log10label
by countrycode regioncode: gen moveave_confirmeddeaths = (confirmeddeaths+confirmeddeaths[_n-1]+confirmeddeaths[_n-2]+confirmeddeaths[_n-3]+confirmeddeaths[_n-4]+confirmeddeaths[_n-5]+confirmeddeaths[_n-6])/7
bys countrycode regioncode: gen newdeaths= moveave_confirmeddeaths-moveave_confirmeddeaths[_n-1]
gen newdeaths_log10=log10(newdeaths+1) , after(newdeaths)
label values newdeaths_log10 log10label

** merge with national data

append using "/Users/wolf5258/OneDrive - Nexus365/Covid-19 Tracker/Bea's code/OxCGRT_20200904.dta" , keep(countryname countrycode date confirmedcases confirmeddeaths stringencyindexfordisplay governmentresponseindexfordispla containmenthealthindexfordisplay economicsupportindexfordisplay)
save "allcountriesandBRAstates", replace
use "allcountriesandBRAstates" , clear

** generate 6 country plot
set scheme plotplain
preserve
replace regionname="US - "+regionname if regionname!=""
replace regioncode=countrycode if regioncode==""
replace regionname=countryname if regionname==""
keep if regioncode=="BRA" | regioncode=="MEX" | regioncode=="DEU" | regioncode=="US_FL" | regioncode=="US_NY" | regioncode=="US_OK"
line newdeaths_log10 date , yaxis(1) ylabel(0 1 2 3, valuelabel) lpatter(solid)  ytitle("Deaths per day (7-day avg)", axis(1) size(small)) ///
	|| line containmenthealthindexfordisplay date , yaxis(2) yscale(axis(2) range(0 100)) lcolor(red) lpatter(solid) ylabel(#5 , axis(2) labc(red)) ytitle("Containment and health index", axis(2) size(small) color(red)) ///
	, by(regionname , iytitle note("Data from 04 Aug 2020.", span size(vsmall)) caption($caption, span size(vsmall)) title("Comparison of six government responses to COVID-19 as cases rise" , span position(11)) legend(off)) xtitle("Date", size(small)) xlabel(21946 22036 22128, format(%td_d_m)) xscale(extend) play(Get rid of left axis in BY chart)
graph export "six_charts.png" , replace
restore
