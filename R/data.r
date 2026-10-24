#' States dataset for R Companion to Political Analysis, Fourth Edition
#'
#' A dataset with variables about the 50 states. This dataset is used to demonstrate application of R to political analysis. See book Appendix for variable names and descriptions.
#'
#' @format A data frame with 50 rows and 186 variables.
#' \describe{
#'   \item{abortion_ban}{Total abortion ban according to Guttmacher Institute as of July 1, 2026 (1 = yes, 0 = no)}
#'   \item{abortion_legal}{Percentage saying abortion should be legal in all or most cases (PRRI 2025)}
#'   \item{abortion_rate}{Abortion rate per 1,000 women ages 15-44 (2025)}
#'   \item{abortlaws_7cat}{Level of abortion restriction or protection by state (Guttmacher Institute, as of July 1, 2026)}
#'   \item{adv_or_more}{Percentage age 25 or older with a graduate or professional degree (2024 ACS, table B15003)}
#'   \item{alcohol}{Apparent alcohol consumption in gallons of ethanol per resident age 14 or older (2023)}
#'   \item{assault_ban_support}{Percentage supporting a ban on assault-style rifles (CES 2024)}
#'   \item{attend_weekly}{Percentage attending religious services weekly or more (PRRI 2025)}
#'   \item{ba_or_more}{Percentage age 25 or older with at least a bachelor's degree (2024 ACS, table B15003)}
#'   \item{ballots24_total}{Total ballots counted in the 2024 general election (UF Election Lab)}
#'   \item{battleground2020}{Battleground in 2020? (Cook Political Report)}
#'   \item{battleground2024}{Presidential battleground state in 2024 (Ballotpedia; 1 = yes, 0 = no)}
#'   \item{biden2020}{Biden two-party vote percentage in 2020 election}
#'   \item{black_percent}{Percentage Black alone and not Hispanic or Latino (2024 ACS, table B03002)}
#'   \item{broadband}{Percentage of households with a broadband internet subscription of any type (2024 ACS, table B28002)}
#'   \item{carfatal}{Motor-vehicle traffic fatalities per 100,000 population (2024)}
#'   \item{carry_easier_support}{Percentage supporting making concealed carry easier (CES 2024)}
#'   \item{cellular_only}{Percentage of households with only a cellular data internet subscription (2024 ACS, table B28002)}
#'   \item{charitable_giving_pc}{Tax-reported charitable contributions per resident (2022)}
#'   \item{christian_dominion}{Agreement that Christians should exercise dominion over U.S. society (percent; PRRI 2025)}
#'   \item{christian_foundations}{Agreement that America depends on its Christian foundations (percent; PRRI 2025)}
#'   \item{christian_identity}{Percentage agreeing being Christian is an important part of being truly American (PRRI 2025)}
#'   \item{christian_laws}{Percentage agreeing U.S. laws should be based on Christian values (PRRI 2025)}
#'   \item{christian_nation}{Percentage agreeing the U.S. government should declare America a Christian nation (PRRI 2025)}
#'   \item{cig_tax}{State cigarette excise tax per pack (July 1, 2025)}
#'   \item{cig_tax_3cat}{State cigarette excise tax category based on July 1, 2025 KFF rates}
#'   \item{cigarettes}{Packs smoked bimonthly per adult (2003)}
#'   \item{climate_policies}{Number of state climate policies enacted (State Climate Policy Dashboard snapshot, July 10, 2026)}
#'   \item{cn_support}{Percentage classified as Christian nationalism Adherents or Sympathizers (PRRI 2025)}
#'   \item{cong119_demshare}{Democratic percentage of state's 119th Congress delegation at opening (2025)}
#'   \item{cook_pvi_scores}{Cook PVI score (2025; positive = Republican)}
#'   \item{corporal_punishment_banned}{Corporal punishment prohibited in public schools (November 2024; 1 = yes, 0 = no)}
#'   \item{corrections_incarc_rate}{Incarcerated population per 100,000 residents (2024)}
#'   \item{corrections_total_rate}{Population under correctional control per 100,000 residents (2026)}
#'   \item{covid_cases_per1000}{Cumulative reported COVID-19 cases per 1,000 population through May 10, 2023}
#'   \item{covid_deaths_per1000}{Cumulative reported COVID-19 deaths per 1,000 population through May 10, 2023}
#'   \item{covid_response_max}{Maximum COVID-19 response stringency index (2020-21)}
#'   \item{covid_response_mean}{Mean COVID-19 response stringency index (2020-21)}
#'   \item{covid_vaccinated}{Percentage completing the original COVID-19 primary vaccination series (May 10, 2023)}
#'   \item{crime_rate_burglary}{Burglary offenses per 100,000 residents (FBI via USAFacts, 2024)}
#'   \item{crime_rate_murder}{Murder and non-negligent manslaughter offenses per 100,000 residents (FBI via USAFacts, 2024)}
#'   \item{crime_rate_overall}{Violent plus property crime offenses per 100,000 residents (FBI via USAFacts, 2024)}
#'   \item{crime_rate_property}{Property crime offenses per 100,000 residents (FBI via USAFacts, 2024)}
#'   \item{crime_rate_violent}{Violent crime offenses per 100,000 residents (FBI via USAFacts, 2024)}
#'   \item{deathpen_status}{Does state retain the death penalty? Current as of 2025. From Death Penalty Information Center}
#'   \item{defense_expend}{Federal defense expenditures per capita (FY2024)}
#'   \item{democracy_5cat}{Five-category MAP Democracy Overall Policy classification (December 1, 2025)}
#'   \item{democracy_score}{MAP Democracy Overall Policy Score (December 1, 2025)}
#'   \item{density}{Population per square mile (2024 ACS B01001; 2020 Census TIGER/Line)}
#'   \item{diabetes_death_rate}{Diabetes death rate per 100,000 population (KFF State Health Facts, 2024)}
#'   \item{direct_democracy_tools}{Number of statewide direct-democracy tools available (2025)}
#'   \item{division}{Census division (9 different regions of country)}
#'   \item{drug_death_rate}{Age-adjusted drug overdose deaths per 100,000 residents (2024)}
#'   \item{dv_survivor_protections}{Employment protections for domestic-violence survivors (0-4; 2024)}
#'   \item{earmarks_pcap}{Federal earmarks per capita (FY2026)}
#'   \item{energy_efficiency_score}{ACEEE state energy-efficiency score (2025)}
#'   \item{fairpoor_health}{Adults reporting fair or poor health (percent; 2024)}
#'   \item{felon_ineligible24}{People ineligible to vote because of felony rules (2024)}
#'   \item{firearm_death_rate}{Firearm death rate per 100,000 population (KFF State Health Facts, 2024)}
#'   \item{foreign_born}{Percentage born outside the United States (2024 ACS, table B05012)}
#'   \item{giffords_grade}{GIFFORDS gun law strength letter grade (2025)}
#'   \item{giffords_rank}{GIFFORDS gun law strength rank (2025; 1 = strongest laws)}
#'   \item{gini}{Gini index of income inequality (2024 ACS, table B19083)}
#'   \item{government_workers}{Percentage of employed civilians working for local, state, or federal government (2024 ACS)}
#'   \item{governor_party}{Governor's party: D = Democratic or R = Republican (NCSL 2026)}
#'   \item{gun_dealers}{Active firearm dealers per 100,000 residents (March 2026)}
#'   \item{gun_deaths_100k}{Age-adjusted firearm death rate per 100,000 population (2024)}
#'   \item{gun_law_strength}{Everytown Gun Law Strength score (2026)}
#'   \item{gun_murders}{Age-adjusted firearm homicide death rate per 100,000 population (2024)}
#'   \item{gun_ownership}{Estimated percentage of adults living in a household with a firearm (2016)}
#'   \item{gunsammo_rank}{Ammo.com ranking of states for gun owners (2026; 1 = most gun-friendly)}
#'   \item{harris2024}{Kamala Harris two-party vote percentage in 2024 election}
#'   \item{hh_income}{Median household income in 2024 inflation-adjusted dollars (2024 ACS, table B19013)}
#'   \item{hispanic_percent}{Percentage Hispanic or Latino of any race (2024 ACS, table B03002)}
#'   \item{homeless_per_10k}{People experiencing homelessness per 10,000 residents (January 2024)}
#'   \item{homeownership}{Percentage of occupied housing units that are owner-occupied (2024 ACS, table B25003)}
#'   \item{homeschool_percent}{Percentage of K-12 students homeschooled (2023-24 estimate)}
#'   \item{hs_or_more}{Adults age 25+ with a high school credential (percent; 2024)}
#'   \item{income_percapita}{Per capita income in 2024 inflation-adjusted dollars}
#'   \item{infant_mortality}{Infant deaths per 1,000 live births (KFF State Health Facts, 2023 vital statistics; Vermont 2022)}
#'   \item{judge_selection}{Primary method for selecting justices of the state's court of last resort (2026)}
#'   \item{judges_elected}{State high-court justices selected by contested election (1 = yes)}
#'   \item{labor_force_percent}{Percentage age 16 or older in the labor force (2024 ACS, table B23025)}
#'   \item{land_area}{State land area in square miles (2020 Census TIGER/Line state boundaries)}
#'   \item{leg_dem_seats}{Total Democratic legislative seats across both chambers (NCSL 2026 partisan composition)}
#'   \item{leg_other_seats}{Other-party or nonpartisan legislative seats (2026)}
#'   \item{leg_rep_seats}{Total Republican legislative seats across both chambers (NCSL 2026 partisan composition)}
#'   \item{lgbtq_equality_5cat}{Five-category MAP LGBTQ Equality Overall Policy classification (December 1, 2025)}
#'   \item{lgbtq_equality_score}{MAP LGBTQ Equality Overall Policy Score (December 1, 2025)}
#'   \item{lgbtq_nondiscrim}{Percentage favoring laws protecting LGBTQ people from discrimination (PRRI 2025)}
#'   \item{life_expectancy}{Life expectancy at birth in years (KFF State Health Facts using NCHS state life tables, 2022)}
#'   \item{marijuana_legal_status}{State marijuana legal status (2025-2026)}
#'   \item{median_age}{Median age of the state population (2024 ACS, table B01002)}
#'   \item{median_gross_rent}{Median monthly gross rent in dollars (2024 ACS, table B25064)}
#'   \item{median_home_value}{Median value of owner-occupied housing units in dollars (2024 ACS, table B25077)}
#'   \item{medicaid_expansion}{State adopted ACA Medicaid expansion (1 = adopted, 0 = not adopted; KFF tracker, May 2026)}
#'   \item{minimum_wage}{State minimum wage as of July 1, 2026}
#'   \item{ncaj_attorney_access}{NCAJ Attorney Access Index score (2021)}
#'   \item{ncaj_consumer_debt}{NCAJ Consumer Debt Litigation Index score (2024)}
#'   \item{ncaj_disability_access}{NCAJ Disability Access Index score (2021)}
#'   \item{ncaj_fines_fees}{NCAJ Fines and Fees Index score (2022)}
#'   \item{ncaj_justice_index}{National Center for Access to Justice overall Justice Index score (updated 2024)}
#'   \item{ncaj_language_access}{NCAJ Language Access Index score (2021)}
#'   \item{ncaj_self_representation}{NCAJ Self-Representation Access Index score (2021)}
#'   \item{no_internet}{Percentage of households with no internet access (2024 ACS, table B28002)}
#'   \item{noncitizen24}{Estimated percentage of the voting-age population that is noncitizen in 2024 (UF Election Lab)}
#'   \item{obesity_percent}{Percentage of adults with BMI of 30 or higher (KFF State Health Facts, 2024 BRFSS; Tennessee 2023)}
#'   \item{online_gambling}{Legal online gambling type (July 2026)}
#'   \item{online_gambling_legal}{Percentage saying online gambling should be legal in all or most cases (PRRI 2025)}
#'   \item{opioid_rx_rate}{Retail opioid prescriptions dispensed per 100 persons (2024)}
#'   \item{over64}{Percentage of the population age 65 or older (2024 ACS, table B01001)}
#'   \item{pop_18_24}{Percentage of the population ages 18 through 24 (2024 ACS, table B01001)}
#'   \item{pop_18_44}{Percentage of the population ages 18 through 44 (2024 ACS, table B01001)}
#'   \item{population}{Total state population (2024 ACS, table B01001)}
#'   \item{population_change}{Percentage increase/decrease in population from 2010 to 2020 (not annualized)}
#'   \item{pot_policy}{Marijuana laws (as of June 2021). From the National Conference of State Legislatures}
#'   \item{poverty_rate}{Percentage below the poverty level in the past 12 months (2024 ACS, table B17001)}
#'   \item{preg_uninten_rate}{Unintended pregnancy rate per 1,000 women 15-44 (2010). From Guttmacher Institute}
#'   \item{private_choice_programs}{Number of private-school-choice program types present (2024; 0-3)}
#'   \item{prochoice_percent}{Adults saying abortion should be legal in all or most cases (percent; 2023-24)}
#'   \item{protect_immigrants}{Protection of immigrants: ILRC immigration-enforcement score (2024)}
#'   \item{public_transit}{Workers commuting by public transportation (percent; 2024)}
#'   \item{region}{Census region (4 categories)}
#'   \item{relig_import}{Adults saying religion is very important in their lives (percent; 2023-24)}
#'   \item{religiosity}{Average Pew religiousness scale score (0-8; 2023-24)}
#'   \item{religiosity_3cat}{Three-category Pew religiousness scale (2023-24)}
#'   \item{religious_text_weekly}{Percentage reading religious texts weekly or more (PRRI 2025)}
#'   \item{religious_unaffiliated}{Percentage religiously unaffiliated: atheist, agnostic, or nothing in particular (PRRI 2025)}
#'   \item{rent_burdened}{Renters spending at least 30\% of income on rent (percent; 2024)}
#'   \item{renter_percent}{Percentage of occupied housing units that are renter-occupied (2024 ACS, table B25003)}
#'   \item{right_to_work}{State has a right-to-work law (2026)}
#'   \item{same_sex_marriage}{Percentage favoring legal same-sex marriage (PRRI 2025)}
#'   \item{schools_avg_salary}{Average salary of public school teachers (2016). From the National Education Association}
#'   \item{schools_spend}{Expenditure per student in average daily attendance (2017). From the National Education Association}
#'   \item{schools_st_ratio}{Students enrolled per teacher (2016). From the National Education Association}
#'   \item{smokers}{Age-adjusted percentage of adults who currently smoke cigarettes (2024; Tennessee 2023)}
#'   \item{south}{Southern state?}
#'   \item{speak_english_only}{Percentage age 5 or older speaking only English at home (2024 ACS, table S1601)}
#'   \item{special_ed_funding_supports}{Special-education targeted funding-support index (0-3)}
#'   \item{state}{State name}
#'   \item{state_board_fully_elected}{State board of education fully elected (2026; 1 = yes, 0 = no)}
#'   \item{state_control}{Unified state control: Democrat, Split, or Republican; Nebraska is missing (NCSL 2026)}
#'   \item{state_govt_rank}{U.S. News & World Report 2025 Best States overall rank}
#'   \item{state_preK_4yo_pct}{Percentage of four-year-olds enrolled in state-funded pre-K (2023-24)}
#'   \item{state_preK_quality}{NIEER state pre-K quality standards score (2023-24; 0-10)}
#'   \item{state_preK_spending_per_child}{State pre-K spending per enrolled child (2023-24 dollars)}
#'   \item{stateid}{Two-letter postal abbreviation for state name}
#'   \item{stateleg_black}{Percentage of state legislators identified as Black/African American (NCSL, spring 2023)}
#'   \item{stateleg_dem}{Democratic share of filled state-legislative seats (percent; 2026)}
#'   \item{stateleg_dem_margin}{Democratic seat percentage minus Republican seat percentage (NCSL 2026)}
#'   \item{stateleg_hispanic}{Percentage of state legislators identified as Hispanic/Latino (NCSL, spring 2023)}
#'   \item{stateleg_other}{Other-party share of filled state-legislative seats (percent; 2026)}
#'   \item{stateleg_prof_rank}{Rank of state legislative professionalism based on the 2021 Squire Index}
#'   \item{stateleg_prof_score}{Squire Index of state legislative professionalism (2021)}
#'   \item{stateleg_rep}{Republican share of filled state-legislative seats (percent; 2026)}
#'   \item{stateleg_women}{Percentage of state legislators who are women (NCSL Women in State Legislatures, 2025)}
#'   \item{stateleg_women_count}{Number of women state legislators (NCSL Women in State Legislatures, 2025)}
#'   \item{suicide_rate}{Age-adjusted suicide death rate per 100,000 population (2024)}
#'   \item{tax_competitiveness}{Tax Foundation 2026 State Tax Competitiveness Index score}
#'   \item{tax_source}{Largest source of combined state and local tax collections (FY2023)}
#'   \item{teen_birth_rate}{Teen birth rate per 1,000 females ages 15-19 (2024)}
#'   \item{term_limits}{Does the state currently limit state legislators' terms? (1 = yes, 0 = no; NCSL, 2026)}
#'   \item{trump2020}{Trump two-party vote percentage in 2020 election}
#'   \item{trump2024}{Donald Trump two-party vote percentage in 2024 election}
#'   \item{turnout24_change20}{Percentage-point change in VEP turnout from 2020 to 2024 (UF Election Lab)}
#'   \item{turnout24_rank}{Rank among the 50 states by 2024 VEP turnout; 1 is highest (UF Election Lab)}
#'   \item{under18}{Percentage of the population under age 18 (2024 ACS, table B01001)}
#'   \item{unemployment}{Percentage of the civilian labor force unemployed (2024 ACS, table B23025)}
#'   \item{unified_control}{One party controls both legislative chambers and the governorship (1 = yes, 0 = no; NCSL 2026)}
#'   \item{uninsured}{Population without health insurance (percent; 2024)}
#'   \item{uninsured_0_64}{Population ages 0-64 without health insurance (percent; 2024)}
#'   \item{unionized}{Percentage of employed wage and salary workers who were union members (2025)}
#'   \item{universal_school_meals}{Statewide universal free school breakfast and lunch (2025-26; 1 = yes, 0 = no)}
#'   \item{unsheltered_homeless_pct}{Percentage of people experiencing homelessness who were unsheltered (January 2024)}
#'   \item{urban}{Percentage of the state population living in urban areas (2020 Census, table P2)}
#'   \item{vap24}{Voting-age population in 2024 (UF Election Lab)}
#'   \item{vap24_turnout}{2024 general-election turnout as a percentage of the voting-age population (UF Election Lab)}
#'   \item{vep20_turnout}{Voting-eligible population turnout (percent; 2020)}
#'   \item{vep24}{Voting-eligible population in 2024 (UF Election Lab)}
#'   \item{vep24_turnout}{2024 general-election turnout as a percentage of the voting-eligible population (UF Election Lab)}
#'   \item{veteran_percent}{Percentage of the civilian population age 18 or older who are veterans (2024 ACS, table B21001)}
#'   \item{volunteer_hrs_pc}{Estimated annual formal-volunteer hours per state resident age 16 or older (2023)}
#'   \item{volunteer_rate}{Residents volunteering through an organization (percent; 2023)}
#'   \item{voter_id_law}{Identification-document requirement for in-person voting (NCSL, April 24, 2026)}
#'   \item{white_percent}{Percentage White alone and not Hispanic or Latino (2024 ACS, table B03002)}
#'   \item{work_from_home}{Percentage of workers age 16 or older who usually worked from home (2024 ACS, table B08301)}
#' }
#' @source Data sources vary. See Appendix of printed textbook for further information.
#' @name states
#' @docType data
#' @keywords datasets
NULL


#' World dataset for R Companion to Political Analysis, Fourth Edition
#'
#' A dataset with variables about countries in the world. This dataset is used to demonstrate application of R to political analysis. See book Appendix for variable names and descriptions.
#'
#' @format A data frame with 169 rows and 234 variables.
#' \describe{
#'   \item{arda_code}{Country numerical code. From Association of Religion Data Archives}
#'   \item{bribe_judge}{Proportion of survey respondents from country reporting that they or member of family paid bribe to legal/judicial system in last year. From Global Corruption Barometer}
#'   \item{bribe_police}{Proportion of survey respondents from country reporting that they or member of family paid bribe to police in last year. From Global Corruption Barometer}
#'   \item{broadband}{Broadband subscription per 100 people in 2017. From Our World in Data}
#'   \item{business_starts}{Number of new corporations registered annually. From World Bank's World Development Indicators}
#'   \item{cabrv}{Three-letter abbreviation of country name assigned by International Standards Organization}
#'   \item{ccode}{Numeric country code based on the ISO standard}
#'   \item{ciaedex}{Percent of GDP spent on education (CIA)}
#'   \item{ciagdpag}{Composition of GDP: Agricultural sector (CIA)}
#'   \item{ciagdpin}{Composition of GDP: Industrial sector (CIA)}
#'   \item{ciagdpsv}{Composition of GDP: Service sector (CIA)}
#'   \item{civil_war}{Civil war intensity, 2008}
#'   \item{co2_percap}{Carbon dioxide emissions excluding land-use change and forestry, in metric tonnes of CO2-equivalent per person, 2024.}
#'   \item{co2e_percap}{Total greenhouse gas emissions excluding land-use change and forestry, in metric tonnes of CO2-equivalent per person, 2024.}
#'   \item{energy_renew_percent}{Renewable energy consumption as a percentage of total final energy consumption.}
#'   \item{ocean_health}{Ocean Health Index score for waters associated with a coastal country, ranging from 0 to 100.}
#'   \item{coastal}{Whether the country has an ocean or sea coastline.}
#'   \item{rainfall_mm}{Long-term average annual precipitation over the country's land area, in millimeters per year.}
#'   \item{forest_percent}{Forest area as a percentage of total land area.}
#'   \item{freshwater_percap}{Renewable internal freshwater resources per person, in cubic meters per year.}
#'   \item{unclos_ratified}{Whether the country is a party to the United Nations Convention on the Law of the Sea.}
#'   \item{migratory_species_treaty}{Whether the country is a party to the Convention on Migratory Species.}
#'   \item{nagoya_protocol}{Whether the country is a party to the Nagoya Protocol on access and benefit sharing.}
#'   \item{bbnj_agreement}{Whether the country is a party to the BBNJ Agreement on marine biodiversity beyond national jurisdiction.}
#'   \item{colony}{Colony of what country? (CIA)}
#'   \item{compulsory_voting}{Compulsory voting in national elections in 2025, including whether sanctions are enforced. From V-Dem}
#'   \item{confidence}{Confidence in institutions scale (World Values Survey)}
#'   \item{conflict_index}{Global Peace Index score for 2026, with higher values indicating less peaceful conditions.}
#'   \item{conflict_internal}{Number of internal conflicts between govt and groups in country without intervention from other countries. From UCDP/PRIO Database}
#'   \item{conflict_internat}{Number of internal conflicts between govt and groups in country with intervention from other countries (on one or both sides). From UCDP/PRIO Database}
#'   \item{corp_tax_rate}{Corporate tax rate. From Heritage Foundation's 2021 Index of Economic Freedom}
#'   \item{corrupt_perception}{Corruption perception index. From Transparency International}
#'   \item{country}{Country or territory name used in the RCPA4 World Dataset}
#'   \item{coup_attempts}{Number of attempted coups d'etats since 1950. From Jonathan Powell}
#'   \item{coups}{Number of successful coups d'etats since 1950. From Jonathan Powell}
#'   \item{death_penalty_status}{Legal status of death penalty (1 = Abolished, 2 = Abolished Except for Extraordinary Crimes, 3 = Abolished in Practice, 4 = Uses Death Penalty). From Death Penalty Information Center}
#'   \item{debt_percent_gdp}{Public debt as a percentage of GDP. From Heritage Foundation's 2021 Index of Economic Freedom}
#'   \item{dem_other}{Percentage of other democracies in region}
#'   \item{dem_other5}{Percentage of other democracies in region: 5 categories}
#'   \item{district_size3}{Average number of members per district. From World Values Survey}
#'   \item{dnpp_3}{Effective number of parliamentary parties}
#'   \item{dpi_cemo}{Is chief executive a military officer? 0 = no, 1 = yes. From World Bank's Database of Political Institutions}
#'   \item{dpi_system}{National political system (0 = Presidential, 1 = Assembly-elected President, 2 = Parliamentary). From World Bank's Database of Political Institutions}
#'   \item{durable}{Number of years since the last regime transition as of 2018 (Polity)}
#'   \item{econ_compete}{Measure of global economic competitiveness. From World Economic Forum}
#'   \item{econ_freedom}{Heritage Foundation rating of overall economic freedom. From 2021 Index of Economic Freedom}
#'   \item{econ_freedom_5cat}{Heritage Foundation assessment of overall economic freedom. From 2021 Index of Economic Freedom}
#'   \item{educ_f_avgyrs}{Average schooling years, female (25+). From Educational Attainment Dataset by Barro and Lee}
#'   \item{educ_f_none}{Percentage of females with no schooling (25+). From Educational Attainment Dataset by Barro and Lee}
#'   \item{educ_m_avgyrs}{Average schooling years, male (25+). From Educational Attainment Dataset by Barro and Lee}
#'   \item{educ_m_none}{Percentage of males with no schooling (25+). From Educational Attainment Dataset by Barro and Lee}
#'   \item{educ_quality}{Average rating of quality of educational system, ranges 1-7 (higher values = better quality). From World Economic Forum}
#'   \item{effectiveness}{Government effectiveness scale (Kaufmann, 2002)}
#'   \item{eiu_democ_4cat}{Level of democracy in 2020, 4 ordinal categories. From Economist Intelligence Unit}
#'   \item{eiu_democ_bin}{Is the country classified by the Economist Intelligence Unit as a full or flawed democracy in 2025?}
#'   \item{eiu_democ_score}{Economist Intelligence Unit Democracy Index score for 2025, ranging from 0 to 10; higher values indicate stronger democracy.}
#'   \item{election_integrity}{Average of expert evaluation of integrity of country's electoral system. From Electoral Integrity Project}
#'   \item{election_violence_post}{Were there riots and protest after election? From National Elections Across Democracy and Autocracy}
#'   \item{election_violence_pre}{Were there riots and protest before election? From National Elections Across Democracy and Autocracy}
#'   \item{enpp3_democ}{Effective number of parliamentary parties: 3 categories}
#'   \item{enpp3_democ08}{Effective number of parliamentary parties. From World Values Survey}
#'   \item{enpp_3}{Effective number of parliamentary parties}
#'   \item{eu}{EU member state as of June 2021 (yes/no)}
#'   \item{fdi_inflow}{Inflow of foreign direct investment (in millions of U.S. dollars). From Heritage Foundation's 2021 Index of Economic Freedom}
#'   \item{fertility}{Total fertility rate: average number of births per woman; latest available World Bank value}
#'   \item{fh_democ_3cat}{Freedom House rating of democracy, 3 ordinal categories. From 2021 report}
#'   \item{fh_democ_score}{Freedom House aggregate political-rights and civil-liberties score for 2025, ranging from 0 to 100; higher values indicate greater freedom.}
#'   \item{fh_internet_3cat}{Level of Internet freedom in country, 3 ordinal categories (1 = Not Free, 2 = Partly Free, 3 = Free). From Freedom House, 2021 Report on Internet Freedom}
#'   \item{fh_internet_score}{Measure of Internet freedom in country in 2021; combines measures of obstacles to access, limits on content, and violations of user rights. Ranges 0 to 100 (with 100 = most freedom). From Freedom House}
#'   \item{frac_eth}{Ethnic factionalization (Alesina et al., 2003)}
#'   \item{frac_eth2}{Ethnic factionalization, divided into High and Low categories}
#'   \item{frac_eth3}{Level of ethnic fractionalization: 3 categories (Low, Medium, High)}
#'   \item{frac_lang}{Language factionalization (Alesina et al., 2003)}
#'   \item{frac_relig}{Religious factionalization (Alesina et al., 2003)}
#'   \item{gas_production}{In millions of barrels of oil equivalent. From Michael Ross's Oil and Gas Data, 1932-2014}
#'   \item{gdp_growth}{Annual economic growth rate, 5-year average. From Heritage Foundation's 2021 Index of Economic Freedom}
#'   \item{gdp_percap}{GDP per capita at purchasing power parity in constant 2021 international dollars; latest available World Bank value in July 2026 download}
#'   \item{gdp_percap_5cat}{GDP per capita at purchasing power parity, July 2026 World Bank download, grouped into five ordered RCPA categories}
#'   \item{gender_equal3}{Gender empowerment measure, 3 categories. From World Values Survey}
#'   \item{gender_inequality}{UNDP Gender Inequality Index (0 = least inequality, 1 = greatest inequality), 2023}
#'   \item{gini_index}{GINI index (of income inequality). Most recent estimates from CIA World Factbook (as of June 2021)}
#'   \item{global_social}{Social globalization measured by personal contacts of citizens, information flows, and cultural proximity. From KOF Index of Globalization}
#'   \item{govt_help_cap}{Capacity of state to provide for needy citizens. From Fund for Peace, Fragile States Index}
#'   \item{govt_integrity}{Rating of government integrity. From Heritage Foundation's 2021 Index of Economic Freedom}
#'   \item{govt_per_gdp}{Government spending (all types) as a percentage of GDP}
#'   \item{govt_quality}{Measure of the quality of government. From International Country Risk Guide of Political Risk Group}
#'   \item{gri}{Government Restrictions Index based on 20 indicators of ways that governments restrict religion, including through coercion and force. From Pew Research Center on Global Restrictions of Religion Report, 2016 data}
#'   \item{grp_name}{Name of the religion preferred by the government in 2015 (based on multidimensional measure of government favoritism)}
#'   \item{grp_score}{Measure of Government Religious Preference in 2015, how strongly government favors one religion over others}
#'   \item{happiness}{Measure of how happy citizens are. From World Happiness Report}
#'   \item{hdi}{Human development index (HDI) value (as of June 10, 2021). Human development dimensions include having long and healthy lives, being knowledgeable, and enjoying a decent standard of living. From UNDP Human Development Report}
#'   \item{hiv_percent}{Percentage of adults ages 15-49 living with HIV.}
#'   \item{homicide_rate}{Number of intentional homicides per 100,000 persons. From UNODC Global Study on Homicide 2013 (where 2012 data missing from report, last reported year used, typically 2011)}
#'   \item{hospital_beds}{Hospital beds per 1,000 people.}
#'   \item{human_flight}{Measure of human flight and brain drain from country. From Fund for Peace, Fragile States Index}
#'   \item{icc_treaty_ratified}{Whether the country is currently a state party to the Rome Statute of the International Criminal Court (Yes/No)}
#'   \item{immigrants_percent}{International migrant stock as a percentage of population; latest available World Bank value}
#'   \item{imprisonment_rate}{Number incarcerated per 100,000 persons. From World Prison Brief}
#'   \item{income_tax_rate}{Income tax rate. From Heritage Foundation's 2021 Index of Economic Freedom}
#'   \item{indep_judiciary}{Does country have an independent judiciary? (1 = yes). From Political Constraints Index Dataset}
#'   \item{indy}{Year of independence (CIA)}
#'   \item{infant_mortality}{Infant deaths before age one per 1,000 live births.}
#'   \item{inflation}{Annual inflation rate. From Heritage Foundation's 2021 Index of Economic Freedom}
#'   \item{internet_users}{Percentage of population that uses the Internet (as of 2020 or 2021)}
#'   \item{judicial_effectiveness}{Rating of effectiveness of country's judiciary. From Heritage Foundation's 2021 Index of Economic Freedom}
#'   \item{judicial_indep_wef}{Average rating of judicial independence, ranges 1-7. From World Economic Forum}
#'   \item{laws_protect_prop}{Property rights index in 2025, ranging from 0 (low) to 1 (high). From V-Dem}
#'   \item{legal_origin}{Legal origin of commercial code of country (1 = English Common Law, 2 = French Commercial Code, 3 = Socialist/Communist Laws, 4 = German Commercial Code, 5 = Scandinavian Commercial Code). From LaPorta, Lopez de-Silanes, Shleifer, and Vishny (1998)}
#'   \item{legal_quality}{Measure of quality of country's legal institutions. From the Institutional Quality Dataset}
#'   \item{life_expectancy}{Life expectancy at birth, total years; latest available World Bank value}
#'   \item{lifeex_f}{Life expectancy at birth among females, total years; latest available World Bank value}
#'   \item{lifeex_m}{Life expectancy at birth among males, total years; latest available World Bank value}
#'   \item{literacy}{Literacy rate (CIA)}
#'   \item{media_access_cand}{Does country provide free or subsidized media access for political candidates? (1 = yes) From Institute for Democracy and Electoral Assistance}
#'   \item{media_access_parties}{Does country provide free or subsidized media access for political parties? (1 = yes). From Institute for Democracy and Electoral Assistance}
#'   \item{median_age}{Median age in 2025 from UN World Population Prospects medium projection via Our World in Data}
#'   \item{migration_net}{Net migration, immigrants minus emigrants; latest available World Bank value}
#'   \item{muslim}{Are Muslims the predominant religious group?}
#'   \item{oecd}{OECD member state as of June, 2021? (yes/no)}
#'   \item{oil}{Oil production, in barrels per day (CIA) CIAOIL_P}
#'   \item{oil_production}{In metric tons. From Michael Ross's Oil and Gas Data, 1932-2014}
#'   \item{organized_crime}{Average rating of the impact of organized crime on the economy, ranges 1-7 (lower values = more impact). From World Economic Forum}
#'   \item{peace_5cat}{State of peace in country, 5 ordinal categories. From Visions of Humanity's Global Peace Index 2021 Report}
#'   \item{peace_index}{Reversed Global Peace Index score for 2026, with higher values indicating greater peacefulness.}
#'   \item{pmat12_3}{Post-materialism, 3 categories (World Values Survey)}
#'   \item{pol_terror_scale_ai}{Political terror scale. Ordinal scale from 1-5 of how much government terrorizes country's citizens, with 1 corresponding to very little oppression by government. From Amnesty International}
#'   \item{pol_terror_scale_hrw}{Political terror scale. Ordinal scale from 1 to 5 of how much government terrorizes country's citizens, with 1 corresponding to very little oppression by government. From Human Rights Watch}
#'   \item{polity_score}{Polity5 combined democracy-autocracy score for 2018, ranging from -10 for strongly autocratic regimes to +10 for strongly democratic regimes. This is a historical measure; Polity5 has not been updated beyond 2018. Source: Center for Systemic Peace.}
#'   \item{pop_0_14}{Percentage of population ages 0-14; latest available World Bank value}
#'   \item{pop_15_64}{Percentage of population ages 15-64; latest available World Bank value}
#'   \item{pop_65_older}{Percentage of population ages 65 and older; latest available World Bank value}
#'   \item{pop_growth}{Annual population growth percentage; latest available World Bank value}
#'   \item{pop_urban}{Percentage of population living in urban areas; latest available World Bank value}
#'   \item{population}{Total population; latest available World Bank value}
#'   \item{population_3cat}{Total population grouped into three ordered RCPA categories using the latest available World Bank value}
#'   \item{population_density}{Population density, people per square kilometer of land area; latest available World Bank value}
#'   \item{poverty}{Percentage of the population below the poverty line}
#'   \item{pr_sys}{Proportional representation electoral system? From the Institute for Democracy and Electoral Assistance}
#'   \item{press_freedom_fh}{Measure of freedom of the country's press. Based on Freedom House Freedom of the Press Report 2015, fit to 0-10 range}
#'   \item{press_freedom_rsf}{Measure of freedom of the country's press. From Reporters Sans Frontiers}
#'   \item{protact3}{Protest activity (World Values Survey)}
#'   \item{refugees_from}{Refugees under UNHCR mandate by country or territory of origin; latest available World Bank value}
#'   \item{refugees_impact}{Measure of impact of population displacement on country. From Fund for Peace, Fragile States Index}
#'   \item{refugees_in}{Refugees under UNHCR mandate by country or territory of asylum; latest available World Bank value}
#'   \item{regime_type3}{Regime type. From Cheibub's Democracy Dictatorship dataset}
#'   \item{region}{Region name}
#'   \item{regionun}{United Nations region}
#'   \item{religion}{Largest religion by proportion (UN)}
#'   \item{reserved_seats}{Does country reserve seats in national legislature for any group (e.g., women or young people)? From Inter-parliamentary Union}
#'   \item{rights_assn}{Freedom of assembly and association, ordinal scale 0-2. From CIRI Human Rights Data Project}
#'   \item{rights_dommov}{Freedom of domestic movement, ordinal scale 0-2. From CIRI Human Rights Data Project}
#'   \item{rights_formov}{Freedom of foreign travel and emigration in 2025, recoded into three ordered categories. From V-Dem}
#'   \item{rights_injud}{High-court independence in 2025, recoded into three ordered categories. From V-Dem}
#'   \item{rights_law_index}{Measure of violations of human right and rule of law. From Fund for Peace, Fragile States Index}
#'   \item{rights_relfree}{Freedom of religion in 2025, recoded into three ordered categories. From V-Dem}
#'   \item{rights_speech}{Freedom of speech, ordinal scale 0-2. From CIRI Human Rights Data Project}
#'   \item{rights_treaties}{Number of international human rights treaties ratified (out of 16 possible). From UNHRC}
#'   \item{rights_wecon}{Women's economic rights, ordinal scale 0-2. From CIRI Human Rights Data Project}
#'   \item{rights_wopol}{Women's political rights, ordinal scale 0-2. From CIRI Human Rights Data Project}
#'   \item{rights_worker}{Women's social rights, ordinal scale 0-2. From CIRI Human Rights Data Project}
#'   \item{schools_internet}{Average rating of internet availability in schools, ranges 1-7 (higher values = more extensive). From World Economic Forum}
#'   \item{self_employed}{Percentage of labor force that is self-employed. From World Bank's World Development Indicators}
#'   \item{sexratio}{Sex ratio at birth, male births per 100 female births; latest available World Bank value}
#'   \item{shi}{Social Hostilities Index based on 13 indicators of ways in which private individuals and social groups infringe upon religious beliefs and practices. From Pew Research Center on Global Restrictions of Religion Report, 2016 data}
#'   \item{soldiers_percent}{Percentage of labor force in the military. From World Bank's World Development Indicators}
#'   \item{soldiers_total}{Total number of people in the military. From World Bank's World Development Indicators}
#'   \item{spendeduc}{Public expenditure on education as a percentage of GDP (UN)}
#'   \item{spendhealth}{Domestic general government health expenditure as a percentage of GDP.}
#'   \item{spendmil_wdi}{Military expenditure as a percentage of GDP; latest available World Bank/SIPRI value}
#'   \item{tariff_rate}{Tariff rate on imports. From Heritage Foundation's 2021 Index of Economic Freedom}
#'   \item{taxes_percent_gdp}{Taxes (all forms) as a percentage of gross domestic product. From Heritage Foundation's 2021 Index of Economic Freedom}
#'   \item{terror_index_voh}{Measure of the impact of terrorism on the country. From Visions of Humanity}
#'   \item{trade_percent_gdp}{Exports plus imports of goods and services as a percentage of GDP; latest available World Bank value}
#'   \item{typerel}{Predominant religion}
#'   \item{unemployment}{Percentage of labor force that is unemployed (available/seeking employment but not employed). From CIA World Factbook (most recent estimates available as of June 2021)}
#'   \item{unexp_rd}{Public expenditure on research and development as a percentage of GDP (UN)}
#'   \item{unfempf}{Ratio of female-to-male formal employment rates}
#'   \item{unin_inc}{Inequality-adjusted income index (2010, UN)}
#'   \item{unineduc}{Inequality-adjusted education index (2010)}
#'   \item{unions}{Union density}
#'   \item{unjourn}{Number of verified cases of journalists imprisoned in 2009 (UN)}
#'   \item{unlit}{Adult literacy rate (percentage of population age 15 or older) (UN)}
#'   \item{unmobcov}{Percentage of the population covered by a mobile phone network (2008, UN)}
#'   \item{unmort_f}{Adult mortality rate among females, per 1,000 female adults; latest available World Bank value}
#'   \item{unmort_m}{Adult mortality rate among males, per 1,000 male adults; latest available World Bank value}
#'   \item{unnewsp}{Daily newspapers per thousand people (2004, UN)}
#'   \item{unnoncom}{Age-standardized deaths from noncommunicable diseases per 100,000 people.}
#'   \item{unpop30}{Projected 2030 population in millions, UN medium scenario via Our World in Data}
#'   \item{unremitp}{Per capita remittance inflows in U.S. dollars (2008)}
#'   \item{unremitt}{Remittance inflows as a percentage of GDP (2008)}
#'   \item{unsati}{Overall life satisfaction, most recent measure during 2006-2009 (0-10, higher means more satisfied) (UN)}
#'   \item{unsatif}{Overall life satisfaction among females, most recent measure during 2006-2009 (UN)}
#'   \item{unsatjob}{Percentage of employed who are satisfied with their job (UN)}
#'   \item{unsatliv}{Percentage who are satisfied with their standard of living (UN)}
#'   \item{unseced}{Percentage of the population ages 25 and older with at least secondary education (UN)}
#'   \item{vdem_2cat}{Democracy or autocracy in 2025, derived from V-Dem's Regimes of the World classification}
#'   \item{vdem_4cat}{Regime type in 2025, ordered from liberal democracy to closed autocracy. From V-Dem}
#'   \item{vdem_edi_score}{Electoral Democracy Index in 2025, ranging from 0 (low) to 1 (high). From V-Dem}
#'   \item{vdem_ldi_score}{Liberal Democracy Index in 2025, ranging from 0 (low) to 1 (high). From V-Dem}
#'   \item{vi_rel3}{Percent saying religion very important, 3 categories}
#'   \item{violence_cost}{Economic cost of violence on national economy. Measured as a percentage of GDP. From Visions of Humanity}
#'   \item{votevap10s}{Turnout of voting age population: average of both presidential and parliamentary elections in 2010s. From the Institute for Democracy and Electoral Assistance}
#'   \item{womenleg}{Women as a percentage of the lower or unicameral legislative chamber in 2025. From V-Dem}
#'   \item{womyear}{Year women first enfranchised. From Inter-parliamentary Union}
#'   \item{womyear2}{Women's suffrage. From Inter-parliamentary Union}
#'   \item{youngleg}{Percentage of lower house of legislature aged 40 years or younger, as of June 2021. From the Inter-parliamentary Union}
#'   \item{vdem_participatory_score}{Participatory Democracy Index in 2025, ranging from 0 (low) to 1 (high). From V-Dem}
#'   \item{vdem_deliberative_score}{Deliberative Democracy Index in 2025, ranging from 0 (low) to 1 (high). From V-Dem}
#'   \item{vdem_egalitarian_score}{Egalitarian Democracy Index in 2025, ranging from 0 (low) to 1 (high). From V-Dem}
#'   \item{vdem_judicial_constraints}{Judicial constraints on the executive in 2025, ranging from 0 (low) to 1 (high). From V-Dem}
#'   \item{vdem_rule_of_law}{Rule of Law Index in 2025, ranging from 0 (low) to 1 (high). From V-Dem}
#'   \item{vdem_access_justice}{Secure and effective access to justice in 2025, ranging from 0 (low) to 1 (high). From V-Dem}
#'   \item{vdem_political_corruption}{Political Corruption Index in 2025, ranging from 0 (less corruption) to 1 (more corruption). From V-Dem}
#'   \item{vdem_freedom_expression}{Freedom of expression and alternative information in 2025, ranging from 0 (low) to 1 (high). From V-Dem}
#'   \item{vdem_civil_society}{Civil Society Participation Index in 2025, ranging from 0 (low) to 1 (high). From V-Dem}
#'   \item{vdem_women_empowerment}{Women's Political Empowerment Index in 2025, ranging from 0 (low) to 1 (high). From V-Dem}
#'   \item{vdem_civil_liberties}{Civil Liberties Index in 2025, ranging from 0 (low) to 1 (high). From V-Dem}
#'   \item{vdem_physical_integrity}{Physical Integrity Index in 2025, ranging from 0 (low) to 1 (high). From V-Dem}
#'   \item{vdem_highcourt_independence}{V-Dem model estimate of high-court independence in 2025; higher values indicate greater independence}
#'   \item{vdem_property_rights}{Property Rights Index in 2025, ranging from 0 (low) to 1 (high). From V-Dem}
#'   \item{vdem_freedom_religion}{V-Dem model estimate of freedom of religion in 2025; higher values indicate greater freedom}
#'   \item{vdem_freedom_domestic_movement}{Freedom of domestic movement and residence in 2025, ranging from 0 (low) to 1 (high). From V-Dem}
#'   \item{vdem_freedom_foreign_movement}{V-Dem model estimate of freedom of foreign travel and emigration in 2025; higher values indicate greater freedom}
#'   \item{vdem_freedom_association}{Freedom of association for political parties and civil-society organizations in 2025, ranging from 0 (low) to 1 (high). From V-Dem}
#'   \item{vdem_education_equality}{V-Dem model estimate of equality in access to high-quality basic education in 2025; higher values indicate greater equality}
#'   \item{vdem_health_equality}{V-Dem model estimate of equality in access to high-quality basic health care in 2025; higher values indicate greater equality}
#'   \item{vdem_gender_quota}{National gender quota for the lower or unicameral legislative chamber in 2025. From V-Dem}
#'   \item{vdem_social_consensus}{V-Dem model estimate of social consensus on major political issues in 2025; higher values indicate less polarization}
#'   \item{maternal_mortality}{Maternal deaths per 100,000 live births.}
#'   \item{tb_incidence}{Estimated new and recurrent tuberculosis cases per 100,000 people.}
#'   \item{adult_obesity}{Percentage of adults age 18 and older with a body mass index of 30 or higher, age-standardized.}
#'   \item{dpt_immunization}{Percentage of children ages 12-23 months receiving three doses of DPT vaccine.}
#'   \item{out_of_pocket_health}{Household out-of-pocket spending as a percentage of current health expenditure.}
#'   \item{uhc_service_index}{Universal Health Coverage service coverage index, ranging from 0 to 100; higher values indicate broader essential-service coverage.}
#' }
#' @source Data sources vary. See Appendix of printed textbook for further information.
#' @name world
#' @docType data
#' @keywords datasets
NULL




#' GSS 2024 dataset for R Companion to Political Analysis, Fourth Edition
#'
#' A student-friendly subset of the 2024 General Social Survey. This dataset is used to demonstrate application of R to political analysis. See book Appendix for variable names and descriptions.
#'
#' @format A data frame with 3309 rows and 365 variables.
#' \describe{
#'   \item{id}{Respondent identification number}
#'   \item{abany}{Abortion allowed for any reason}
#'   \item{abdefect}{Abortion allowed for serious birth defect}
#'   \item{abhlth}{Abortion allowed if woman's health is endangered}
#'   \item{abnomore}{Abortion allowed if married woman wants no more children}
#'   \item{abpoor}{Abortion allowed if woman cannot afford child}
#'   \item{abrape}{Abortion allowed if pregnancy resulted from rape}
#'   \item{absingle}{Abortion allowed if woman is not married}
#'   \item{acpttg}{People are generally accepting}
#'   \item{adoptus}{Immigrants should adopt American culture}
#'   \item{adults}{Number of adults age 18 and older in household}
#'   \item{adultsinhh}{Total adults in household, experimental version}
#'   \item{affrmact}{Favor preferential hiring of Black applicants}
#'   \item{afraidof}{Others act afraid of R}
#'   \item{age}{R's age in years}
#'   \item{agekdbrn}{Age at first child's birth}
#'   \item{amancstr}{Importance of American ancestry}
#'   \item{ambetter}{Belief that America is a better country}
#'   \item{ambornin}{Importance of being born in America}
#'   \item{amchrstn}{Importance of being Christian}
#'   \item{amcit}{Importance of US citizenship}
#'   \item{amcitizn}{Preference for being a US citizen}
#'   \item{amfeel}{Importance of feeling American}
#'   \item{amgovt}{Importance of respecting US laws}
#'   \item{amownway}{America should follow its own interests}
#'   \item{amshamed}{Ashamed of some aspects of America}
#'   \item{amtv}{Preference for American films on television}
#'   \item{attend}{Frequency of attending religious services}
#'   \item{babies}{Number of children under age 6 in household}
#'   \item{ballot}{Ballot assignment}
#'   \item{befair}{People try to take advantage}
#'   \item{belikeus}{Better if others were more like Americans}
#'   \item{biasnews}{Political news coverage is biased}
#'   \item{bible}{View of the Bible}
#'   \item{bizmoney}{Business profits come at expense of workers}
#'   \item{born}{Born in the United States}
#'   \item{brneffrt}{Can become American if born elsewhere}
#'   \item{cantrust}{People can be trusted}
#'   \item{cappun}{Favor or oppose death penalty for murder}
#'   \item{childs}{Number of children}
#'   \item{childsinhh}{Total children in household, experimental version}
#'   \item{chldidel}{Ideal number of children}
#'   \item{chngdocs}{Policy on changing official documents}
#'   \item{citizen}{US citizen}
#'   \item{class}{Subjective class identification}
#'   \item{cohort}{R's birth cohort year}
#'   \item{colath}{Allow anti-religionist to teach in college}
#'   \item{colcom}{Fire Communist college teacher}
#'   \item{colmslm}{Allow anti-American Muslim cleric to teach}
#'   \item{colrac}{Allow racist to teach in college}
#'   \item{compuse}{Uses a computer}
#'   \item{conarmy}{Confidence in military}
#'   \item{conbus}{Confidence in major companies}
#'   \item{conclerg}{Confidence in organized religion}
#'   \item{coneduc}{Confidence in education system}
#'   \item{confed}{Confidence in executive branch of federal government}
#'   \item{confinan}{Confidence in banks and financial institutions}
#'   \item{coninc}{Family income in constant dollars}
#'   \item{conjudge}{Confidence in US Supreme Court}
#'   \item{conlabor}{Confidence in organized labor}
#'   \item{conlegis}{Confidence in Congress}
#'   \item{conmedic}{Confidence in medical profession}
#'   \item{conpow}{Conflict between ordinary people and elites}
#'   \item{conpress}{Confidence in press}
#'   \item{conrinc}{R's income, inflation adjusted}
#'   \item{consci}{Confidence in scientific community}
#'   \item{conscrp}{Importance of military service}
#'   \item{contv}{Confidence in television}
#'   \item{corruptn}{Perceived corruption in public service}
#'   \item{courts}{Courts too harsh or too lenient}
#'   \item{degree}{Highest degree earned}
#'   \item{dem10fut}{Democracy will work in ten years}
#'   \item{dem10pst}{Democracy worked ten years ago}
#'   \item{demtoday}{Democracy works today}
#'   \item{denom}{Religious denomination}
#'   \item{denom16}{Denomination raised in at age 16}
#'   \item{dipged}{High school diploma, GED, or equivalent}
#'   \item{discaff}{Belief that whites are hurt by affirmative action}
#'   \item{discaffm}{Men denied job or promotion due to gender discrimination}
#'   \item{discaffw}{Women denied job or promotion due to gender discrimination}
#'   \item{disrspct}{R treated with less courtesy or respect}
#'   \item{divlaw}{Divorce laws too easy or too difficult}
#'   \item{divorce}{Ever divorced or separated}
#'   \item{dsndntpay}{Support for reparations for descendants of enslaved people}
#'   \item{dwelown}{Owns or rents home}
#'   \item{earnrs}{Number of household income earners}
#'   \item{econstat}{Current state of the national economy}
#'   \item{educ}{Years of schooling completed}
#'   \item{eqwlth}{Government should reduce income differences}
#'   \item{evwork}{Ever worked for at least one year}
#'   \item{experts}{Rely on experts versus ordinary people}
#'   \item{fair}{People are fair or take advantage}
#'   \item{famgen}{Number of family generations in household}
#'   \item{fear}{Afraid to walk alone at night}
#'   \item{fechld}{Mother working does not hurt children}
#'   \item{fefam}{Man works and woman cares for home}
#'   \item{fehire}{Hire and promote women equally}
#'   \item{fejobaff}{Preferential hiring of women}
#'   \item{feovrwrk}{Family suffers when women work}
#'   \item{fepol}{Women not suited for politics}
#'   \item{fepresch}{Preschool children suffer if mother works}
#'   \item{finalter}{Change in R's financial situation}
#'   \item{finrela}{Satisfaction with family income}
#'   \item{forland}{Whether foreigners should be allowed to buy land}
#'   \item{form}{Split questionnaire form assignment}
#'   \item{fulldem}{People, not politicians, should decide}
#'   \item{fund}{Religious fundamentalism}
#'   \item{getahead}{How people get ahead in life}
#'   \item{god}{Confidence in existence of God}
#'   \item{goodlife}{Standard of living will improve}
#'   \item{granborn}{Number of grandparents born outside US}
#'   \item{grass}{Favor or oppose legalizing marijuana}
#'   \item{gunlaw}{Favor or oppose gun permits}
#'   \item{hapmar}{Happiness of marriage}
#'   \item{happy}{General happiness}
#'   \item{health}{Self-rated health}
#'   \item{helpblk}{Government should aid Black people}
#'   \item{helpful}{People are helpful or selfish}
#'   \item{helpnot}{Government should do more or less}
#'   \item{helpoth}{Importance for children to help others}
#'   \item{helppoor}{Government should improve living standard of poor}
#'   \item{helpsick}{Government should help pay for medical care}
#'   \item{helpusa}{Support for helping worse off people in America}
#'   \item{hhtype1}{Household type, condensed}
#'   \item{hispanic}{Hispanic origin}
#'   \item{homosex}{Same-sex relations right or wrong}
#'   \item{hompop}{Total number of people in household}
#'   \item{hrs1}{Hours R worked last week}
#'   \item{hunt}{R or spouse hunts}
#'   \item{hunt1}{R's spouse or partner hunts}
#'   \item{if16who}{Vote choice in 2016 counterfactual}
#'   \item{if20who}{Vote choice in 2020 counterfactual}
#'   \item{ifwrong}{People should support their country}
#'   \item{immameco}{Immigrants are good for the economy}
#'   \item{immassim}{Immigrants should assimilate}
#'   \item{immcrime}{Immigrants increase crime}
#'   \item{immfate}{Treatment of undocumented immigrants}
#'   \item{immideas1}{Immigrants bring new ideas and cultures}
#'   \item{immjobs}{Immigrants take jobs from natives}
#'   \item{immwlfare}{Prefer natives over immigrants for benefits}
#'   \item{imports}{Support for limiting imports into the United States}
#'   \item{incgap1}{Government responsibility to reduce inequality}
#'   \item{income}{Total family income}
#'   \item{indus10}{Industry code, 2010 classification}
#'   \item{intlincs}{International companies harm local business}
#'   \item{jobfind}{Could find equally good job}
#'   \item{joblose}{Likely to lose job}
#'   \item{kidssol}{Children's standard of living compared to R}
#'   \item{knwpatnt}{Knows someone seeing mental health professional}
#'   \item{leftrght}{Left-right political self-placement}
#'   \item{letdie1}{Allow incurable patients to die}
#'   \item{letin1}{Number of immigrants allowed into the United States}
#'   \item{letinasn}{Support for increasing immigration from Asia}
#'   \item{letinhsp}{Support for increasing immigration from Latin America}
#'   \item{libath}{Allow anti-religious book in library}
#'   \item{libcom}{Allow Communist book in library}
#'   \item{libmslm}{Allow anti-American Muslim cleric book in library}
#'   \item{librac}{Allow racist book in library}
#'   \item{life}{Life exciting or dull}
#'   \item{liveblks}{Neighborhood is half Black}
#'   \item{lkelyvot}{Likelihood of voting in 2024}
#'   \item{localnum}{Number of employees at R's work site}
#'   \item{major1}{College major}
#'   \item{marasian}{Approve close relative marrying Asian person}
#'   \item{marblk}{Approve close relative marrying Black person}
#'   \item{marcohab}{Cohabitation status}
#'   \item{marhisp}{Approve close relative marrying Hispanic person}
#'   \item{marital}{Current marital status}
#'   \item{marsame}{Same-sex couples' right to marry}
#'   \item{martype}{Marital type classification}
#'   \item{marwht}{Approve close relative marrying White person}
#'   \item{meovrwrk}{Men harm family when focused too much on work}
#'   \item{mode}{Interview mode}
#'   \item{nataid}{Federal spending on foreign aid}
#'   \item{natarms}{Federal spending on military and defense}
#'   \item{natchld}{Federal spending on childcare assistance}
#'   \item{natcity}{Federal spending on solving problems of big cities}
#'   \item{natcrime}{Federal spending on halting rising crime}
#'   \item{natdrug}{Federal spending on dealing with drug addiction}
#'   \item{nateduc}{Federal spending on improving education system}
#'   \item{natenrgy}{Federal spending on developing alternative energy}
#'   \item{natenvir}{Federal spending on protecting the environment}
#'   \item{natfare}{Federal spending on welfare}
#'   \item{natheal}{Federal spending on protecting national health}
#'   \item{natrace}{Federal spending on improving conditions of Black people}
#'   \item{natroad}{Federal spending on highways and bridges}
#'   \item{natsci}{Federal spending on scientific research}
#'   \item{natsoc}{Federal spending on Social Security}
#'   \item{natspac}{Federal spending on space exploration}
#'   \item{news}{Frequency of reading newspaper}
#'   \item{noathome}{Missed family occasion for work}
#'   \item{nonurse}{Unable to care for sick child or relative}
#'   \item{norelgsp}{No religion identification}
#'   \item{notsmart}{R treated as not smart}
#'   \item{obey}{Importance for children to be obedient}
#'   \item{obeylaws}{Importance of always obeying laws}
#'   \item{occ10}{Census occupation code, 2010 classification}
#'   \item{oppsegov}{Importance of civil disobedience}
#'   \item{othlang}{Speaks language other than English or Spanish}
#'   \item{othlang1}{Other language spoken, first response}
#'   \item{owndecns}{Each nation should have its own country}
#'   \item{owngun}{Has gun in home}
#'   \item{parborn}{Whether parents were born in United States}
#'   \item{parsol}{Living standard compared to parents}
#'   \item{partfull}{Works full time or part time}
#'   \item{partyid}{Political party affiliation}
#'   \item{paytaxes}{Importance of never evading taxes}
#'   \item{phone}{Has telephone}
#'   \item{pillok}{Birth control for teenagers age 14 to 16}
#'   \item{pistol}{Pistol or revolver in home}
#'   \item{polabuse}{Approve police verbal abuse}
#'   \item{polattak}{Approve police physical attack}
#'   \item{poleff11}{R has no say in government}
#'   \item{poleff18}{Government does not care what people think}
#'   \item{polescap}{Approve police preventing escape}
#'   \item{polhitok}{Approve police striking suspect}
#'   \item{polintrst}{Interest in election campaigns}
#'   \item{polmurdr}{Approve police questioning murder suspect}
#'   \item{polnews}{Use media for political news}
#'   \item{polopts}{Importance of citizen participation}
#'   \item{poltalk}{Politicians talk too much}
#'   \item{polviews}{Liberal-conservative self-placement}
#'   \item{poorserv}{R received poorer service in stores or restaurants}
#'   \item{popular}{Importance for children to be popular}
#'   \item{pornlaw}{Pornography laws}
#'   \item{posslq}{Has marital partner}
#'   \item{postlife}{Belief in life after death}
#'   \item{pray}{Frequency of prayer}
#'   \item{prayer}{Bible prayer in public schools}
#'   \item{premarsx}{Sex before marriage}
#'   \item{pres16}{Presidential vote recall, 2016}
#'   \item{pres20}{Presidential vote recall, 2020}
#'   \item{prestg10}{Occupational prestige score, 2010}
#'   \item{prestg105plus}{Occupational prestige score, threshold method, 2010}
#'   \item{preteen}{Number of children age 6 to 12 in household}
#'   \item{proudart}{Pride in American arts and literature}
#'   \item{prouddem}{Pride in how American democracy works}
#'   \item{proudeco}{Pride in American economic achievements}
#'   \item{proudhis}{Pride in American history}
#'   \item{proudpol}{Pride in American political influence worldwide}
#'   \item{proudspt}{Pride in American achievements in sports}
#'   \item{racdif1}{Black-white inequality due to discrimination}
#'   \item{racdif2}{Black-white inequality due to inborn ability}
#'   \item{racdif3}{Black-white inequality due to lack of education}
#'   \item{racdif4}{Black-white inequality due to lack of effort}
#'   \item{racdif5}{Black-white inequality due to upbringing}
#'   \item{race}{R's race}
#'   \item{raclive}{Opposite race in neighborhood}
#'   \item{racopen}{Support for open housing law}
#'   \item{rank}{Self-ranking in society}
#'   \item{ratechall124}{Donald Trump feeling rating}
#'   \item{ratechall1ts}{Challenger one time-series rating}
#'   \item{ratechall224}{Kamala Harris feeling rating}
#'   \item{ratechall2ts}{Challenger two time-series rating}
#'   \item{ratechall324}{Nikki Haley feeling rating}
#'   \item{ratechall3ts}{Challenger three time-series rating}
#'   \item{ratedemp}{Democratic Party feeling rating}
#'   \item{rateincumb24}{Joe Biden feeling rating}
#'   \item{rateincumbts}{Incumbent time-series rating}
#'   \item{ratemaga}{MAGA Republicans feeling rating}
#'   \item{raterepp}{Republican Party feeling rating}
#'   \item{realinc}{Family income in constant dollars}
#'   \item{realrinc}{R's income in constant dollars}
#'   \item{reborn}{Born again experience}
#'   \item{refmorwk}{Would take additional paid work}
#'   \item{refpromo}{Would refuse job promotion}
#'   \item{region}{Region of interview}
#'   \item{relactiv}{Participation in religious activities}
#'   \item{relig}{Religious preference}
#'   \item{religinf}{Religion has too much influence in politics}
#'   \item{reliten}{Strength of religious affiliation}
#'   \item{relpersn}{Self-identified as religious person}
#'   \item{reltrad}{Religious tradition classification}
#'   \item{rghtsmin}{Government should protect minority rights}
#'   \item{richwork}{If rich, would continue working}
#'   \item{rifle}{Rifle in home}
#'   \item{rincome}{R's income}
#'   \item{rowngun}{Gun belongs to R}
#'   \item{rplace}{R's relationship to household head}
#'   \item{satdemoc}{Satisfaction with democracy in America}
#'   \item{satfin}{Satisfaction with financial situation}
#'   \item{satjob}{Satisfaction with job}
#'   \item{sei10}{Occupational socioeconomic index, 2010}
#'   \item{sei10educ}{Percent with some college in R's occupation}
#'   \item{sei10inc}{Percent with high income in R's occupation}
#'   \item{servpub}{Public service serves people}
#'   \item{sex}{R's sex}
#'   \item{sexbirth1}{Sex assigned at birth}
#'   \item{sexeduc}{Sex education in public schools}
#'   \item{sexnow2}{Current gender identity}
#'   \item{sexornt}{Self-identified sexual orientation}
#'   \item{shotgun}{Shotgun in home}
#'   \item{sibs}{Number of siblings}
#'   \item{smedia}{Impact of social media exposure}
#'   \item{socbar}{Evening at bar}
#'   \item{socfrend}{Evening with friends}
#'   \item{socommun}{Evening with neighbors}
#'   \item{socrel}{Evening with relatives}
#'   \item{solok}{Importance of adequate standard of living}
#'   \item{spaneng}{Interview conducted in Spanish or English}
#'   \item{spanking}{Favor spanking}
#'   \item{spkath}{Allow anti-religionist to speak}
#'   \item{spkcom}{Allow Communist to speak}
#'   \item{spklang}{How well R speaks other language}
#'   \item{spkmslm}{Allow Muslim cleric preaching hatred to speak}
#'   \item{spkrac}{Allow racist to speak}
#'   \item{suicide1}{Suicide if incurable disease}
#'   \item{suicide2}{Suicide if bankrupt}
#'   \item{suicide4}{Suicide if tired of living}
#'   \item{svyenjoy}{Enjoy taking surveys}
#'   \item{tax}{Federal income tax}
#'   \item{teens}{Number of children age 13 to 17 in household}
#'   \item{teensex}{Teen sex age 14 to 16}
#'   \item{thnkself}{Importance for children to think for themselves}
#'   \item{threaten}{R threatened or harassed}
#'   \item{trust}{People can be trusted}
#'   \item{tvhours}{Hours watching television per day}
#'   \item{unemp}{Unemployed in last ten years}
#'   \item{union}{R or spouse is a labor union member}
#'   \item{union1}{R's spouse or partner is a labor union member}
#'   \item{unrelat}{Number of unrelated people in household}
#'   \item{usmca1}{Familiarity with USMCA}
#'   \item{usmca2}{Belief that United States benefits from USMCA}
#'   \item{vetyears}{Years R served in the armed forces}
#'   \item{visitors}{Number of visitors in household}
#'   \item{vote16}{Voted in 2016 election}
#'   \item{vote20}{Voted in 2020 election}
#'   \item{voteprob}{Voting problems in US elections}
#'   \item{watchgov}{Importance of keeping watch on government}
#'   \item{weekswrk}{Weeks R worked last year}
#'   \item{whovote24}{Presidential vote intention, 2024 pre July 25}
#'   \item{whovote24a}{Presidential vote intention, 2024 post July 25}
#'   \item{whovotets}{Presidential vote intention, time series}
#'   \item{widowed}{Ever widowed}
#'   \item{wlthblks}{Stereotype of Blacks as rich or poor}
#'   \item{wlthhsps}{Stereotype of Hispanics as rich or poor}
#'   \item{wlthwhts}{Stereotype of whites as rich or poor}
#'   \item{wordsum}{Vocabulary test score}
#'   \item{workblks}{Stereotype of Blacks as hard working or lazy}
#'   \item{workhard}{Importance for children to work hard}
#'   \item{workhsps}{Stereotype of Hispanics as hard working or lazy}
#'   \item{workless}{Cut back on work}
#'   \item{workwhts}{Stereotype of whites as hard working or lazy}
#'   \item{wrkgovt1}{R works for government}
#'   \item{wrkgovt2}{R works for a private employer}
#'   \item{wrkslf}{Self-employed or works for someone else}
#'   \item{wrkstat}{Labor force status}
#'   \item{wrkwayup}{Belief that Blacks can overcome prejudice without favors}
#'   \item{wrldgovt}{International bodies should enforce environmental rules}
#'   \item{wtssnrps}{Post-stratification weight, adjusted}
#'   \item{wtssps}{Post-stratification weight}
#'   \item{xmarsex}{Sex with someone other than spouse}
#'   \item{xmovie}{Watched adult movie last year}
#'   \item{xnorcsiz}{Expanded NORC size code}
#'   \item{volunteer_assign}{Volunteer response assignment}
#'   \item{ab_index}{Number of abortion-access circumstances supported, 0 to 7}
#'   \item{suicide_index}{Number of circumstances where person has right to suicide, 0 to 3}
#'   \item{natspend_index}{Support for national spending index, -17 to +17}
#'   \item{social_index}{Social activity index, 0 to 24}
#'   \item{police_force_index}{Number of circumstances justifying police striking citizen, 0 to 5}
#'   \item{discrim_exp_index}{Discrimination experience index, 0 to 25}
#'   \item{con_inst_index}{Confidence in institutions index, 0 to 26}
#'   \item{speech_tol_index}{Free speech tolerance index, 0 to 12}
#'   \item{partyid7}{Political party affiliation, 7 categories}
#'   \item{partyid3}{Political party affiliation, 3 categories}
#' }
#' @source General Social Survey, 2024. See Appendix of printed textbook for further information.
#' @name gss
#' @docType data
#' @keywords datasets
NULL


#' Debate Experiment dataset for R Companion to Political Analysis, Fourth Edition
#'
#' A dataset with variables about students who participated in an experiment. This dataset is used to demonstrate application of R to political analysis. See book Appendix for variable names and descriptions.
#'
#' @format A data frame with 171 rows and 14 variables.
#' \describe{
#'   \item{obs}{Unique identification number for each subject}
#'   \item{assignment}{Name of condition subject was assigned to}
#'   \item{tv}{Did subject watch debate on TV?}
#'   \item{debinfo}{Number of correct answers on five-question quiz about the debate}
#'   \item{catholic}{Is subject Catholic? }
#'   \item{issues}{Which candidate do you agree with on policy issues?}
#'   \item{integrity}{Which candidate has more integrity?}
#'   \item{leadership}{Which candidate is the more effective leader?}
#'   \item{empathy}{Which candidate has more empathy?}
#'   \item{sophdum}{Is respondent a sophomore?}
#'   \item{won}{Which candidate won the debate? }
#'   \item{pid}{Self-reported partisan identification}
#'   \item{ideology}{Self-reported political ideology}
#'   \item{gender}{Subject's gender}
#' }
#' @source Jamie Druckman. See Appendix of printed textbook for further information.
#' @name debate
#' @docType data
#' @keywords datasets
NULL


#' Hypothetical Student Directory for An R Companion to Political Analysis, Fourth Edition
#'
#' Directory information for university students
#'
#' @format A data frame with 100 rows and 7 variables.
#' \describe{
#'   \item{id_number}{Unique identification number for each subject}     
#'   \item{last_name}{Last name of student}      
#'   \item{first_name}{First name of student}     
#'   \item{gender}{Gender of student}         
#'   \item{campus}{Campus attended}      
#'   \item{athlete}{Whether student has athletic scholarship}        
#'   \item{year_born}{Year of birth}      
#'   \item{year_in_school}{Year in school} 
#'   \item{modality}{Mode of attendance} 
#'   \item{major}{Academic major} 
#' }
#' @source Barry C. Edwards and Philip H. Pollock, III
#' @keywords internal
"students"





