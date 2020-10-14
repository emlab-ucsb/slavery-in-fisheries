# Metadata  

## raw_data  

This directory contains raw data files necessary for reproducing the analysis.

### s1_training_full.csv  

This is the training dataset for building the predictive risk model. Each row contains a "vessel-year" observation which describes vessel characteristics and behavior statistics for a single year. These data were obtained from Global Fishing Watch. The data have the following schema:  

* `mmsi`: Maritime Mobile Service Identity (MMSI) unique vessel identifier  
* `year`: Year of observation  
* `flag`: Flag the vessel flies.  
* `gear`: Predominant gear type used as classified by the GFW fishing algorithm. The study focuses on longliners, trawlers, and squid jiggers.  
* `engine_power_kw`: Vessel engine power (kw). Where available, this information comes from vessel registries. When registry information is not available, this information comes from the GFW vessel characteristics algorithm.  
* `tonnage_gt`: Vessel gross tonnage. Where available, this information comes from vessel registries. When registry information is not available, this information comes from the GFW vessel characteristics algorithm.  
* `length_m`: Vessel length (m). Where available, this information comes from vessel registries. When registry information is not available, this information comes from the GFW vessel characteristics algorithm. 
* `crew_size`: Number of crew on vessel. Where available, this information comes from vessel registries. When registry information is not available, this information comes from the GFW vessel characteristics algorithm.  
* `positions`: Number of AIS position messages.  
* `ais_type`: AIS device type, class A or B.
* `foc`: A boolean for whether or not a vessel flies a flag of convenience ([ITF 2019](https://www.itfglobal.org/en/sector/seafarers/flags-of-convenience))   
* `iuu`: A boolean for whether or not a vessel is on a Regional Fisheries Management Organisations (RFMO) IUU list or are subject to an INTERPOL Purple Notice ([Trygg Mat Tracking 2019](https://iuu-vessels.org)).
* `fldb_vessel_id`: Unique identifier created for the suspected forced labor vessel database   (`suspected_forced_labor_database.csv`) (this will be NA for vessel-years that do not have associated forced labor case)  
* `source_id`: Unique numeric ID for the media source from which a forced labor case report originated (this will be NA for vessel-years that do not have associated forced labor case)  
* `years_until_caught`: Number of years between the current year and the year the vessel was reported to be using forced labor (this will be 0 for years in which a vessel was caught for forced labor, a number >=1 for years prior to being caught, or NA for vessel-years that do not have associated forced labor case)  
* `hours`: Total active hours.  
* `fishing_hours`: Total fishing hours as classified by the GFW fishing algorithm.  
* `average_daily_fishing_hours`: Average daily fishing hours on days when fishing.  
* `fishing_hours_foreign_eez`:Total fishing hours in EEZs that are different than the vessel flag.  
* `fishing_hours_high_seas`: Total fishing hours in the high seas.  
* `distance_traveled_km`: Total distance traveled.  
* `max_distance_from_shore_km`: Maximum distance from shore (km).  
* `max_distance_from_port_km`: Maximum distance from any port (km).  
* `number_encounters`: Number of encounters with other vessels. Encounters are defined by events when two vessels are within 500 meters of each other, traveling less than 2 knots, and for a minimum duration of 2 hours. These encounters may represent transshipment, refueling, or transfer of supplies or crew ([Miller et al. 2018](https://doi.org/10.3389/fmars.2018.00240)).   
* `number_iuu_encounters`: Number of encounters with other vessels that are on a Regional Fisheries Management Organisations (RFMO) IUU list or are subject to an INTERPOL Purple Notice ([Trygg Mat Tracking 2019](https://iuu-vessels.org)).  
* `number_forced_labor_encounters`: Number of encounters with other vessels that are in the forced labor vessel database.  
* `average_encounter_duration_hours`: Average duration of encounters with other vessels. Encounters are defined as events when two vessels are within 500 meters of each other, traveling less than 2 knots, and for a minimum duration of 2 hours. These encounters may represent transshipment, refueling, or transfer of supplies or crew ([Miller et al. 2018](https://doi.org/10.3389/fmars.2018.00240)).  
* `gaps_24_hours`:  Number of AIS gaps greater than 24 hours. Gaps are defined as periods during which no AIS messages are received in areas of the ocean where AIS reception is expected. AIS coverage maps are calculated by determining the global vessel average number of daily position transmissions thinned to 5 minute intervals in 0.25x0.25 degree grid cells annually and for both A and B device classes. Gaps are defined as occurring when a vessel is at least 10km from shore and last transmitted in an area that has at least 10% daily AIS coverage for its given year and AIS device class.
* `number_voyages`: Total number of voyages between two ports.  
* `number_foreign_port_visits`: Number of visits to ports in countries that are different than the vessel flag.  
* `number_poc_port_visits`: Number of visits to ports in countries that had not ratified the Port State Measures Agreement (PSMA) at the time of port visit.  
* `average_voyage_duration_hours`: Average duration of all voyages. A voyage is defined as a trip between two ports.  
* `number_loitering_events`: Total number of loitering events. Loitering events are defined when a vessel is traveling less than 2 knots, at least 1 kilometer from shore, for at least one hour. These events may represent encounters if the second vessel is not broadcasting AIS.  
* `average_loitering_duration_hours`: Average duration of loitering events.  
* `offender`: Boolean for whether this vessel-year is classified as positive (1, e.g., a boat that has reported cases of forced labor) or unlabeld (0, e.g., a boat that does not have reported cases of forced labor)          

### s2_feature_lookup.csv  

This is a lookup table that has human-readable descriptions of each model feature code, as well as the data source for each feature.

* `indicator`: 	Short-hand model feature code, with codes for the model features found in `s1_training_full.csv`  
* `indicator_name`: Human-readable feature description  
* `indicator_type`: Category for where data come from: directly observed using AIS data (`directly_observed`); inferred using the GFW fishing algorithm (`gfw_fishing_model`); or vessel characteristics that come from either a vessel registry or the GFW vessel characteristic algorithm (`registry_or_gfw_vessel_model`)  

### s3_suspected_forced_labor_database.csv  

Each row contains information for a case of alleged forced labor that was reported for a specific fishing vessel. These data should be used with extreme care. We highlight in the manuscript:

>Recognizing that the presence of ILO indicators does not guarantee involuntary or forced labor, and also recognizing that cases often simultaneously exhibit a number of different forced labor indicators, we endeavor only to detect forced labor broadly as specified by any of the 11 ILO indicators of forced labor, and do not distinguish between whether vessels may be using bonded labor or slave labor, or which of the 11 indicators a particular vessel may be exhibiting.  

>Importantly, since these forced labor cases exhibit varying levels of evidence, and occurred within various areas of legal jurisdiction, we are not implicating these vessels with any specific crimes or actions. Rather, we are labeling these vessels as “high-risk” vessels that warrant further scrutiny according to the ILO forced labor indicators. Case information was often sparse and did not usually indicate whether labor was involuntary or not, so we endeavored to consistently capture which ILO forced labor indicators were present. Additionally, vessel identification information sometimes included MMSI number, IMO number, or call sign, but often just included vessel name. In these cases, we searched for names in online databases such as MarineTraffic.com and endeavored to find matching vessels from the same flag as was reported in the case. Since this may not always provide a perfect match to the vessel in question, we again are not implicating these vessels with any specific crimes or actions. 

>We label vessel-years as “positive” if the vessel is contained in our database of reported forced labor cases, and if the year is the single year prior to when the case was reported. Since most case reports do not specify the time period during which abuses took place, we assume the abuses took place in the year prior to the report. While we will refer to this as our “baseline model variation, this is an assumption we also test through a robustness check, where we vary our assumption to be that vessels should be labeled as “positive” in the two years prior to the report, in the three years prior, etc. 

> To apply positive labels, we matched our database of forced labor vessel cases to the Global Fishing Watch training dataset using MMSI number, IMO number, callsign, and/or vessel name, and for the year prior to which the case was reported. In cases where the only vessel identification information that could be matched was vessel name, we conservatively disregard matches that use common vessel names including Viking, Lucky Star, and Greenstar. SI Figure S2 summarizes the number of labeled and unlabeled vessel-years by fishing gear and year.  

* `fldb_vessel_id`: Unique identifier created for the vessels in this suspected forced labor vessel database  
* `forced_labor_indicator`: List of ILO forced labor indicators that were present in the publicly available reports, with multiple indicators separated by a ` | `  
* `brief_narrative`: Brief narrative of the alleged forced labor abuses    
* `eyewitness_account`: Category for whether or not the report described an "eyewitness account"   
* `official_investigation`: Category for whether or not the reported described an "official investigation (e.g., by a government enforcement body)" (TRUE, FALSE, or NA)  
* `non_official_investigations`:  Category for whether or not the reported described a "non-official investigation (e.g., by an NGO through investigative journalism)" (TRUE, FALSE, or NA)      
* `arrests_made`: Category for whether or not the reported described "arrests made" (TRUE, FALSE, or NA)    
* `charges_filed`:  Category for whether or not the reported described "charges filed" (TRUE, FALSE, or NA)    
* `conviction_made`:  Category for whether or not the reported described "convictions made" (TRUE, FALSE, or NA)    
* `penalties_sanctioned`:  Category for whether or not the reported described "penalties sanctioned" (TRUE, FALSE, or NA)   
* `source_1`: Link to original source report   
* `source_2`: Link to second source report (if available)  
* `source_3`: Link to third source report (if available)      
* `source_4`: Link to fourth source report (if available)      
* `source_5`: Link to final source report (if available)     
* `mmsi`: Maritime Mobile Service Identity (MMSI) unique vessel identifier   
* `year`: Year of observation  (we include all years prior to when the report for alleged forced labor was published) 
* `flag`:  Flag the vessel flies.  
* `gear`:  Predominant gear type used as classified by the GFW fishing algorithm. The study focuses on longliners, trawlers, and squid jiggers.  