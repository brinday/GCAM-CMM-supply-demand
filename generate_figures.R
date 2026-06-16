# Load packages ---------------------------------------------------------------
library(patchwork)
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(magrittr)
library(data.table)
library(purrr)
library(devtools)
library(rgcam)
library(gcamdata)
library(readxl)
library(RColorBrewer)
library(rmap)
library(sf)
library(giscoR)
library(paletteer)
library(ggnewscale)
library(tidyverse)
library(scatterpie)
library(ggforce)   # for geom_circle



# Clear env / set working directory ------------------------------------------

setwd("C:/Minerals/Supply/GCAM-CMM-supply-demand")  ## set wd based on user's workspace!

PLOT_FOLDER <- paste(getwd(), "/output/", sep = "")

# FUNCTIONS ---------------------------------------------------------------
# Historical years for level 1 data processing. All chunks that produce historical data
# for model calibration are required to produce annual data covering this entire span.
HISTORICAL_YEARS        <- 1971:2021

# Future years for level 1 data processing, for the few chunks that
# produce future data (e.g., population projections)
FUTURE_YEARS            <- (max(HISTORICAL_YEARS)+1):2100

# Calibrated periods in the model. Only level 2 chunks should reference these
MODEL_BASE_YEARS        <- unique(c(1975, 1990, 2005, 2010, 2015, max(HISTORICAL_YEARS)))
MODEL_FINAL_BASE_YEAR   <- max(MODEL_BASE_YEARS)

# Future (not calibrated) model periods. Only level 2 chunks should reference these
MODEL_FUTURE_YEARS      <- seq(2025, 2100, 5)

energy.RSRC_MINERAL            <- c("steel", "aluminium", "silicon", "copper", "graphite", "lithium", "cobalt", "nickel", "manganese", "neodymium", "tellurium", "vanadium", "platinum")
energy.TRADED_MINERAL          <- c("copper", "lithium", "nickel")

#Function "is not an element of" (opposite of %in%)
'%!in%' <- function( x, y ) !( '%in%'( x, y ) )

#Fill annual (fill to all GCAM years). If CUMULATIVE = T, get cumulative
Fill_annual <- function(.df, CUMULATIVE = FALSE,
                        CUM_YEAR_START = 2021){
  YEAR_START <- min(unique(.df$year))
  YEAR_END <- max(unique(.df$year))
  .df %>% mutate(year = as.integer(year)) -> .df


  .df %>% filter(year >= YEAR_START) %>%
    bind_rows(
      .df %>%
        #assuming YEAR_END has values for all
        filter(year == YEAR_END) %>% select(-year) %>%
        mutate(value = NA) %>%
        gcamdata::repeat_add_columns(tibble(year = setdiff(seq(YEAR_START,YEAR_END), unique(.df$year))))
    ) %>% arrange(year) %>%
    mutate(value = gcamdata::approx_fun(year, value, rule = 2)) -> .df1

  if (CUMULATIVE == TRUE ) {
    assertthat::assert_that(CUM_YEAR_START >= YEAR_START)
    .df1 %>% filter(year >= CUM_YEAR_START) %>%
      mutate(value = cumsum(value)) %>% filter(year >= CUM_YEAR_START) -> .df1
  }
  return(.df1)
}


aggregate_rows <- function(df, filter_var, var_name, filter_group, ...) {
  group_var <- quos(...)
  filter_var <- enquo(filter_var)
  filter_var_name <- quo_name(filter_var)
  df %>%
    filter(!!filter_var %in% filter_group) %>%
    group_by(!!!group_var) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::mutate(!!filter_var_name := !!var_name)
}

#parse the output for scenarios with just the one "scenarios" parameter
parse_output_scenario <- function (df) {
  #remove the duplicated headers and columnn names
  df <- df[!(duplicated(df) | duplicated(df, fromLast = TRUE)), ]

  #remove 1990
  try(df <- select(df, -c("1990")))
  try(df <- select(df, -c("1980", "1985", "1995", "2000")))

  #remove columns with NA
  df <- df[,!apply(is.na(df), 2, any)]


  #separate scenario name and date
  df <- separate(df, col = "scenario", into = c("scenario", "date"), sep = c(","))

  #tidy data
  YEARS <- as.character(seq(2005,2100,by=5))
  df <- gather(df, YEARS, key = "year", value = "value") %>%
    dplyr::mutate(year = as.integer(year)) %>%
    select(-date)

  return (df)
}


#returns difference from a scenario
diff_from_scen <- function(df, diff_scenarios, ref_scenario, join_var, ...){
  net_join_var <- quos(...)

  diff_df <- df %>%
    filter(scenario %in% diff_scenarios)

  ref_df <- df %>%
    filter(scenario %in% ref_scenario)

  output_df <- diff_df %>%
    full_join(ref_df, by = join_var,
              suffix = c(".diff", ".ref")) %>%

    mutate(value.diff = if_else(is.na(value.diff),0,value.diff),
           value.ref = if_else(is.na(value.ref),0,value.ref),
           value = value.diff - value.ref)

  net_df <- output_df %>%
    group_by(scenario.diff, !!!net_join_var) %>%
    dplyr::summarise(value.net = sum(value)) %>%
    ungroup()

  output_df_new <- output_df %>%
    left_join(net_df,
              suffix = c("", ".net"))

  return(output_df_new)
}


rel_diff_from_scen <- function(df, diff_scenarios, ref_scenario, join_var){
  
  diff_df <- df %>%
    filter(scenario %in% diff_scenarios)
  
  ref_df <- df %>%
    filter(scenario %in% ref_scenario)
  
  output_df <- diff_df %>%
    full_join(ref_df, by = join_var,
              suffix = c(".diff", ".ref")) %>%
    mutate(value = value.diff/value.ref)
  
  return(output_df)
}

#returns % difference from a scenario
pct_diff_from_scen <- function(df, diff_scenarios, ref_scenario, join_var){

  diff_df <- df %>%
    filter(scenario %in% diff_scenarios)

  ref_df <- df %>%
    filter(scenario %in% ref_scenario)

  output_df <- diff_df %>%
    full_join(ref_df, by = join_var,
              suffix = c(".diff", ".ref")) %>%
    mutate(value = ((value.diff - value.ref)/value.ref) * 100)

  return(output_df)
}


#returns difference from 2021
diff_from_2021 <- function(df, diff_scenarios, ref_scenario, join_var, ...){
  net_join_var <- quos(...)

  diff_2021 <- df %>%
    filter(scenario == ref_scenario,year == 2021)

  output_df <- df %>%
    full_join(diff_2015, by = join_var,
              suffix = c(".diff", ".2021")) %>%

    mutate(value.diff = if_else(is.na(value.diff),0,value.diff),
           value.2021 = if_else(is.na(value.2021),0,value.2021),
           value = value.diff - value.2021)

  net_df <- output_df %>%
    group_by(scenario.diff, !!!net_join_var) %>%
    dplyr::summarise(value.net = sum(value)) %>%
    ungroup()

  output_df_new <- output_df %>%
    left_join(net_df,
              suffix = c("", ".net"))

  return(output_df_new)
}

#returns pct difference from 2021
pct_diff_from_2021 <- function(df, diff_scenarios, ref_scenario, join_var, ...){
  net_join_var <- quos(...)

  diff_2021 <- df %>%
    filter(scenario == ref_scenario,year == 2021)

  output_df <- df %>%
    full_join(diff_2021, by = join_var,
              suffix = c(".diff", ".2021")) %>%

    mutate(value.diff = if_else(is.na(value.diff),0,value.diff),
           value.2021 = if_else(is.na(value.2021),0,value.2021),
           value = ((value.diff - value.2021)/value.2021)*100)

  net_df <- output_df %>%
    group_by(scenario.diff, !!!net_join_var) %>%
    dplyr::summarise(value.net = sum(value)) %>%
    ungroup()

  output_df_new <- output_df %>%
    left_join(net_df,
              suffix = c("", ".net"))

  return(output_df_new)
}

#' regionalize_mineral_inputs
#'
#' Helper function to pre-pend "regional" to mineral inputs that are traded.
#' Mineral demands (on the consumption side; imports and domestic)
#' that are traded must be labeled as "regional {mineral_name}" to be differentiated
#' from their corresponding mineral supplies (on the production side) which just have the mineral name.
#'
#' @param df Base tibble to start from that contains minicam.energy.input of minerals
#' @param to_group Character vector indicating the set of traded minerals
#' @importFrom dplyr filter anti_join rename mutate group_by_at select summarize ungroup bind_rows
#' @return tibble with the relevan regionalized mineral inputs.
regionalize_mineral_inputs <- function(df, names = energy.TRADED_MINERAL) {

  df %>%
    mutate(minicam.energy.input = if_else(minicam.energy.input %in% energy.TRADED_MINERAL,
                                          paste("regional", minicam.energy.input),
                                          minicam.energy.input)) -> reg_df

  return (reg_df)
}



# READ IN CSV -------------------------------------------------------------

# mapping files
elec_tech_map <- readr::read_csv("./input/mapping/elec_tech_map.csv")
trn_tech_map <- readr::read_csv("./input/mapping/trn_tech_map.csv")
mineral_demand_tech_mapping <- readr::read_csv("./input/mapping/mineral_demand_tech_mapping.csv")
mineral_reg_mapping <- readr::read_csv("./input/mapping/mineral_reg_mapping.csv")
stages_mapping <- readr::read_csv("./input/mapping/stages_mapping.csv")
GCAM_region_names <- readr::read_csv("./input/mapping/GCAM_region_names.csv", skip = 6)


# READ IN DATA ------------------------------------------------------------

prj_A <- loadProject("input/data/prj_01272026")
prj_B <- loadProject("input/data/prj_03022026")
prj_C <- loadProject("input/data/prj_03072026")
prj_D <- loadProject("input/data/prj_03192026")
prj_E <- loadProject("input/data/prj_04092026")


prj <- c(prj_A, prj_C, prj_D) # most queries
prj2 <- c(prj_B, prj_E) # electricity/transport queries


# COLORS, LABELS, LEVELS --------------------------------------------------


sector_colors <- c("Electricity generation" = "#F2C14E",
                 "Electricity T&D" = "#F08A24",
                 "H2 production" ="#C94A38",
                 "Transport" =  "#7C2D5A",
                 "Other" = "grey60")

sector_levels <- names(sector_colors)

sector_blank_colors <- c(sector_colors,
                         "Total" = "white")


mine_stages <- c("Economic reserves associated with operating mines",
                 "Economic reserves associated with projects under development",
                 "Physical resources not associated with any mining project")


vir_cols <- viridisLite::viridis(
  n = length(mine_stages),
  begin = 0.15,  # avoid darkest purple
  end   = 0.85   # avoid light yellow
)

mine_stage_colors <- setNames(vir_cols, mine_stages)

mine_stage_gap_colors <- c(mine_stage_colors,
                           "Production reduction from unconstrained supply" = "grey80")


sector_levels <- names(sector_colors)

commodity_colors <- c(
  "copper" = "#1B9E77", 
  "lithium" = "#D95F02",
  "nickel" = "#7570B3",
  "LDV 4W" = "#333333",
  "BEV" = "#1F78B4",
  "Liquids" = "#B2182B",
  "FCEV" = "#BF812D"
)

elec_tech_colors <- c(
  "Biomass"         = "#00931D",  
  "Gas"             = "#A569BD", 
  "Hydro"           = "lightblue",  
  "Refined Liquids" = "#C92525", 
  "Solar PV"           = "#F4D03F", 
  "Solar PV + Storage" = "#C9A72F",  
  "Wind"            = "#5DADE2",  
  "Wind + Storage"  = "#2E86C1",  
  "Wind Offshore"   = "#1B4F72",  
  "Nuclear"         = "#EF8E27",
  "Solar CSP"             = "#D4A017",  
  "Geothermal"      = "#8D6E63",  
  "Coal"            = "grey60",
  "Electricity generation" = "black"
)

trn_tech_colors <- c(
  "BEV" = "#1F78B4",
  "BEV (LFP)" = "#66C2A5",
  "BEV (NCA)" = "#FF7F0E",
  "BEV (NMC111)"= "#5E4FA2",
  "BEV (NMC622)" = "#3B528B",
  "BEV (NMC811)" = "#1F78B4",
  "BEV (Na-ion)" = "#8C510A",
  "BEV (SSB)" = "#BF812D",
  "Liquids" = "#B2182B",
  "Hybrid Liquids" = "#67001F",
  "Other" = "grey",
  "Non-road Electric" = "#FDAE61",
  "FCEV" = "#FEE08B"
)

region_colors_reg32 <- c(
  # North America
  "USA" = "#1F77B4",
  "Canada" = "#66B2FF",
  "Mexico" = "#4B9CD3",
  "Central America and Caribbean" = "#76C7C0",
  
  # Central & South America
  "Brazil" = "#2CA02C",
  "Argentina" = "#66C266",
  "Colombia" = "#7BAF5C",
  "South America_Northern" = "#A6D854",
  "South America_Southern" = "#3CB371",   # adjusted
  
  # Africa
  "Africa_Northern" = "#FFD700",
  "Africa_Western" = "#FFCC00",
  "Africa_Eastern" = "#FFE066",
  "Africa_Southern" = "#FFB347",
  "South Africa" = "#E6A600",
  
  # Europe
  "EU-15" = "#8B5FBF",
  "EU-12" = "#A874D1",
  "Europe_Eastern" = "#C77CFF",
  "Europe_Non_EU" = "#D291BC",
  "European Free Trade Association" = "#E0A3C2",
  
  # Russia & Central Asia
  "Russia" = "#8B4513",
  "Central Asia" = "#A0522D",
  
  
  "Middle East" = "#C2B280",
  
  # Asia
  "China" = "#B22222",
  "India" = "#E41A1C",
  "Pakistan" = "#F46D43",
  "South Asia" = "#FB6A4A",
  "Southeast Asia" = "#FF7F0E",
  "Indonesia" = "#E67E22",
  "Japan" = "#E377C2",
  "South Korea" = "#C71585",
  "Taiwan" = "#D45087",
  
  
  "Australia_NZ" = "#4D4D4D",
  # Rest of World
  "ROW"                           = "#4D4D4D"  # dark grey
)

region_levels_reg32 <- names(region_colors_reg32)

source_palette <- c(
  "Castillo and Eggert (2020)" = "#1B9E77",
  "Calvo et al. (2017)" = "#D95F02",
  "Sverdrup et al. (2017)" = "#56B4E9",
  "Fleming et al. (2024)" = "#D4A017",
  "Busch et al. (2025)" = "#7C2D5A",
  "USGS MCS (2015)" = "#E41A1C",
  "USGS MCS (2026)" = "#7570B3",
  "Elshkaki et al. (2017)" = "#8D6E63",
  "Olafsdottir & Sverdrup (2021)" = "#C92525",
  "Bradley et al. (2025)" = "#A6D854",
  "Zhang et al. (2025)" = "#984EA3",
  "Watari et al. (2018)" = "#4DAF4A",      
  "Valero et al. (2018)" = "#FF7F00",      
  "Hache et al. (2019)" = "#377EB8"        
)

source_palette_annual <- c(
  "IEA (2025)" = "#E78AC3",               
  "Northey et al. (2014)" = "#66C2A5",     
  "Elshkaki et al. (2016)" = "#A6761D",  
  "Elshkaki et al. (2017)" = "#8D6E63",    
  "Busch et al. (2025)" = "#7C2D5A",      
  "Wu et al. (2025)" = "#4C72B0",          
  "Bradley et al. (2025)" = "#A6D854",    
  "de Koning et al. (2018)" = "#E7298A",   
  "Schipper et al. (2018)" = "#1F78B4",    
  "Watari et al. (2018)" = "#33A02C",      
  "Valero et al. (2018)" = "#FF7F00",      
  "Hache et al. (2019)" = "#6A3D9A",       
  "Ziemann et al. (2018)" = "#B15928",     
  "Harvey (2018)" = "#FB9A99",             
  "Sverdrup (2016)" = "#17BECF",           
  "Vikström et al. (2013)" = "#BC80BD",    
  "Kushnir and Sandén (2012)" = "#FDB462"  
)

# SCENARIO LABELS, LEVELS, etc --------------------------------------------

SCENARIO_order <- c("01272026_UnlimitSupply_BR",
                     "01272026_UnlimitSupply_EnR",
                     "01272026_His_constrSupply_BR_noTC",
                     "01272026_His_constrSupply_BR_SS_noTC",
                     "01272026_His_constrSupply_BR_shortLT_noTC",
                     "01272026_His_constrSupply_BR_SS_shortLT_noTC",
                     "01272026_His_constrSupply_EnR_noTC",
                     "01272026_His_constrSupply_EnR_SS_noTC",
                     "01272026_His_constrSupply_EnR_shortLT_noTC",
                     "01272026_His_constrSupply_EnR_SS_shortLT_noTC",
                     "03192026_UnlimitSupply_highDemand",
                     "03192026_His_constrSupply_highDemand",
                     "03192026_His_constrSupply_SS_highDemand",
                     "03192026_His_constrSupply_shortLT_highDemand",
                     "03192026_His_constrSupply_SS_shortLT_highDemand"
)


SCENARIO_labels <- c("01272026_UnlimitSupply_BR" = "Unconstrained supply",
                     "01272026_UnlimitSupply_EnR" = "Unconstrained supply: Increased recycling",
                     "01272026_His_constrSupply_BR_noTC" = "Reference",
                     "01272026_His_constrSupply_BR_SS_noTC" = "Steady-state supply",
                     "01272026_His_constrSupply_BR_shortLT_noTC" = "Short lead times",
                     "01272026_His_constrSupply_BR_SS_shortLT_noTC" = "Steady-state supply: Short lead times",
                     "01272026_His_constrSupply_EnR_noTC" = "Increased recycling",
                     "01272026_His_constrSupply_EnR_SS_noTC" = "Steady-state supply: Increased recycling",
                     "01272026_His_constrSupply_EnR_shortLT_noTC" = "Short lead times + Increased recycling",
                     "01272026_His_constrSupply_EnR_SS_shortLT_noTC" = "Steady-state supply: Short lead times + Increased recycling",
                     "03192026_UnlimitSupply_highDemand" = "Unconstrained supply: High EV demand",
                     "03192026_His_constrSupply_highDemand" = "High EV demand",
                     "03192026_His_constrSupply_SS_highDemand" = "Steady-state supply: High EV demand",
                     "03192026_His_constrSupply_shortLT_highDemand" = "Short lead times + High EV demand",
                     "03192026_His_constrSupply_SS_shortLT_highDemand" = "Steady-state supply: Short lead times + High EV demand"
                     )

SCENARIO_constrained <- c("01272026_His_constrSupply_BR_noTC",
                          "01272026_His_constrSupply_BR_SS_noTC",
                          "01272026_His_constrSupply_BR_shortLT_noTC",
                          "01272026_His_constrSupply_BR_SS_shortLT_noTC",
                          "01272026_His_constrSupply_EnR_noTC",
                          "01272026_His_constrSupply_EnR_SS_noTC",
                          "01272026_His_constrSupply_EnR_shortLT_noTC",
                          "01272026_His_constrSupply_EnR_SS_shortLT_noTC",
                          "03192026_His_constrSupply_highDemand",
                          "03192026_His_constrSupply_SS_highDemand",
                          "03192026_His_constrSupply_shortLT_highDemand",
                          "03192026_His_constrSupply_SS_shortLT_highDemand")
                     
SCENARIOS_BR <- c( "01272026_His_constrSupply_BR_noTC",
                  "01272026_His_constrSupply_BR_SS_noTC",
                  "01272026_His_constrSupply_BR_shortLT_noTC",
                  "01272026_His_constrSupply_BR_SS_shortLT_noTC")

SCENARIOS_EnR <- c("01272026_His_constrSupply_EnR_noTC",
                   "01272026_His_constrSupply_EnR_SS_noTC",
                   "01272026_His_constrSupply_EnR_shortLT_noTC",
                   "01272026_His_constrSupply_EnR_SS_shortLT_noTC")

SCENARIOS_hiEVD <- c( "03192026_His_constrSupply_highDemand",
                      "03192026_His_constrSupply_SS_highDemand",
                      "03192026_His_constrSupply_shortLT_highDemand",
                      "03192026_His_constrSupply_SS_shortLT_highDemand")

SCENARIOS_averageLT <- c("01272026_His_constrSupply_BR_noTC",
                         "01272026_His_constrSupply_BR_SS_noTC",
                         "01272026_His_constrSupply_EnR_noTC",
                         "01272026_His_constrSupply_EnR_SS_noTC",
                         "03152026_His_constrSupply_highDemand",
                         "03152026_His_constrSupply_SS_highDemand")

SCENARIOS_shortLT <- c("01272026_His_constrSupply_BR_shortLT_noTC",
                       "01272026_His_constrSupply_BR_SS_shortLT_noTC",
                       "01272026_His_constrSupply_EnR_shortLT_noTC",
                       "01272026_His_constrSupply_EnR_SS_shortLT_noTC",
                       "03192026_His_constrSupply_shortLT_highDemand",
                       "03192026_His_constrSupply_SS_shortLT_highDemand")

SCENARIOS_SS <- c("01272026_His_constrSupply_BR_SS_noTC",
                  "01272026_His_constrSupply_BR_SS_shortLT_noTC",
                  "01272026_His_constrSupply_EnR_SS_noTC",
                  "01272026_His_constrSupply_EnR_SS_shortLT_noTC",
                  "03192026_His_constrSupply_SS_highDemand",
                  "03192026_His_constrSupply_SS_shortLT_highDemand")

#Single scenarios
SCENARIO_ref <- c("01272026_His_constrSupply_BR_noTC")
SCENARIO_SS_ref <- c("01272026_His_constrSupply_BR_SS_noTC")
SCENARIO_ref_shortLT <- c("01272026_His_constrSupply_BR_shortLT_noTC")
SCENARIO_SS_ref_shortLT <- c("01272026_His_constrSupply_BR_SS_shortLT_noTC")
SCENARIO_ref_EnR <- c("01272026_His_constrSupply_EnR_noTC")
SCENARIO_ref_hiEVD <- c("03192026_His_constrSupply_highDemand")
SCENARIO_SS_ref_EnR <- c("01272026_His_constrSupply_EnR_SS_noTC")
SCENARIO_SS_ref_hiEVD <- c("03192026_His_constrSupply_SS_highDemand")

SCENARIO_ref_unconstrained <- c("01272026_UnlimitSupply_BR")
SCENARIO_EnR_unconstrained <- c("01272026_UnlimitSupply_EnR")
SCENARIO_hiEVD_unconstrained <- c( "03192026_UnlimitSupply_highDemand")


SCENARIO_colors <- c("01272026_UnlimitSupply_BR" = "black",
                     "01272026_His_constrSupply_BR_noTC" = "#C51B7D",
                     "01272026_His_constrSupply_BR_SS_noTC" = "#1F77B4",
                     "01272026_His_constrSupply_BR_shortLT_noTC" = "#E6A600",
                     "01272026_His_constrSupply_BR_SS_shortLT_noTC" = "#2CA02C",
                     "01272026_UnlimitSupply_EnR" = "black",
                     "01272026_His_constrSupply_EnR_noTC" = "#C51B7D",
                     "01272026_His_constrSupply_EnR_SS_noTC" = "#1F77B4",
                     "01272026_His_constrSupply_EnR_shortLT_noTC" = "#E6A600",
                     "01272026_His_constrSupply_EnR_SS_shortLT_noTC" = "#2CA02C",
                     "03192026_UnlimitSupply_highDemand" = "black",
                     "03192026_His_constrSupply_highDemand" =  "#C51B7D",
                     "03192026_His_constrSupply_SS_highDemand" =  "#1F77B4",
                     "03192026_His_constrSupply_shortLT_highDemand" = "#E6A600",
                     "03192026_His_constrSupply_SS_shortLT_highDemand" = "#2CA02C")


supply_lead_combined_colors <- c("Average lead times Steady-state constrained supply" ="#1F77B4",
                                 "Average lead times Baseline constrained supply" = "#C51B7D",
                       "Short lead times Baseline constrained supply" = "#E6A600",
                       "Short lead times Steady-state constrained supply" = "#2CA02C")

lead_times_colors <- c("Average lead times" = "#C51B7D",
                       "Short lead times" = "#E6A600")


SCENARIO_lines <- c("01272026_UnlimitSupply_BR" = "solid",
                    "01272026_His_constrSupply_BR_noTC" = "solid",
                    "01272026_His_constrSupply_BR_SS_noTC" = "solid",
                    "01272026_His_constrSupply_BR_shortLT_noTC" = "solid",
                    "01272026_His_constrSupply_BR_SS_shortLT_noTC"  = "solid",
                    "01272026_UnlimitSupply_EnR" = "dotted",
                    "01272026_His_constrSupply_EnR_noTC" = "dotted",
                    "01272026_His_constrSupply_EnR_SS_noTC" = "dotted",
                    "01272026_His_constrSupply_EnR_shortLT_noTC" = "dotted",
                    "01272026_His_constrSupply_EnR_SS_shortLT_noTC" = "dotted",
                    "03192026_UnlimitSupply_highDemand" = "dashed",
                    "03192026_His_constrSupply_highDemand" = "dashed",
                    "03192026_His_constrSupply_SS_highDemand" = "dashed",
                    "03192026_His_constrSupply_shortLT_highDemand" = "dashed",
                    "03192026_His_constrSupply_SS_shortLT_highDemand" = "dashed")

SCENARIO_lines_2 <- c("01272026_UnlimitSupply_BR" = "solid",
                      "01272026_His_constrSupply_BR_noTC" = "solid",
                      "01272026_His_constrSupply_BR_SS_noTC" = "solid",
                      "01272026_His_constrSupply_BR_shortLT_noTC" = "dotdash",
                      "01272026_His_constrSupply_BR_SS_shortLT_noTC"  = "dotdash",
                      "01272026_UnlimitSupply_EnR" = "dotted",
                      "01272026_His_constrSupply_EnR_noTC" = "dotted",
                      "01272026_His_constrSupply_EnR_SS_noTC" = "dotted",
                      "01272026_His_constrSupply_EnR_shortLT_noTC" = "dotdash",
                      "01272026_His_constrSupply_EnR_SS_shortLT_noTC" = "dotdash",
                      "03192026_UnlimitSupply_highDemand" = "dashed",
                      "03192026_His_constrSupply_highDemand" = "dashed",
                      "03192026_His_constrSupply_SS_highDemand" = "dashed",
                      "03192026_His_constrSupply_shortLT_highDemand" = "dotdash",
                      "03192026_His_constrSupply_SS_shortLT_highDemand" = "dotdash")


# MAIN FIGURES ----------------------------------- ------------------------


# FIGURE 1: REGIONAL MINERAL SUPPLY ---------------------------------------------------------

SCENARIO_order <- c("01272026_His_constrSupply_BR_noTC",
                    "01272026_His_constrSupply_BR_SS_noTC",
                    "01272026_His_constrSupply_BR_shortLT_noTC",
                    "01272026_His_constrSupply_BR_SS_shortLT_noTC",
                    "01272026_His_constrSupply_EnR_noTC",
                    "01272026_His_constrSupply_EnR_SS_noTC",
                    "01272026_His_constrSupply_EnR_shortLT_noTC",
                    "01272026_His_constrSupply_EnR_SS_shortLT_noTC",
                    "03192026_His_constrSupply_highDemand",
                    "03192026_His_constrSupply_SS_highDemand",
                    "03192026_His_constrSupply_shortLT_highDemand",
                    "03192026_His_constrSupply_SS_shortLT_highDemand")


top_allMinerals_region_order <- rev(c("South America_Southern",
                                      "Australia_NZ",
                                      "Indonesia",
                                      "Russia",
                                      "USA",
                                      "China",
                                      "Africa_Western",
                                      "Brazil",
                                      "Southeast Asia",
                                      "ROW"))

# first, reconstruct the supply curves
# y-axis: costs
Fig1b_mineral_extraction_costs <- getQuery(prj, "resource extraction cost") %>%
  filter(resource %in% c("copper", "lithium", "nickel")) %>%
  mutate(value = value*1000,
         Units = "1975$/t")

#x-axis: cumulative quantity
Fig1b_res_quantity <- getQuery(prj, "resource supply curves") %>%
  filter(resource %in% c("copper", "lithium", "nickel")) %>%
  mutate(value = value*10^6,
         Units = "tonnes")

Fig1b_res_cum_quantity <- Fig1b_res_quantity %>%
  group_by(Units, scenario, region, resource, subresource, year) %>%
  arrange(grade) %>%
  mutate(value = cumsum(value)) 

Fig1b_res_cum_prod <- getQuery(prj, "resource cumulative production") %>%
  filter(resource %in% c("copper", "lithium", "nickel")) %>%
  mutate(value = value*10^6,
         Units = "tonnes")

# connect up the cumulative quantity and cost info
Fig1b_res_supply_curve <- Fig1b_res_cum_quantity %>%
  left_join(Fig1b_mineral_extraction_costs, by = c("scenario", "region", "resource", "subresource",
                                                  "grade", "year"), suffix = c(".Q", ".cost")) 

Fig1b_res_supply_curve_origin <- Fig1b_res_supply_curve %>%
  select(-grade, -value.Q, -value.cost) %>%
  distinct() %>%
  mutate(grade = "grade origin",
         value.Q = 0,
         value.cost = 0) %>%
  bind_rows(Fig1b_res_supply_curve) %>%
  arrange(scenario, region, resource, subresource, value.Q, year)

Fig1b_res_supply_curve_plot <- Fig1b_res_supply_curve_origin %>%
  left_join(Fig1b_res_cum_prod, by = c("scenario", "region", "resource", "subresource", "year", "Units.Q" = "Units")) %>%
  rename(value.prod = value) %>%
  mutate(vintage = subresource) %>%
  separate(vintage, into = c(NA, "vintage"), sep = "_") %>%
  mutate(vintage = as.numeric(vintage))  %>%
  ungroup()

#Incremental supply curve
Fig1b_res_supply_curve_incr <- Fig1b_res_supply_curve_origin %>%
  ungroup() %>%
  select(-year) %>%
  distinct() %>%
  separate(subresource, into = c(NA, "year"), sep = "_") %>%
  group_by(Units.Q, Units.cost, scenario, region, resource, grade) %>%
  mutate(cum_value.Q = cumsum(value.Q))


# --- The core function (unchanged) ---
area_under_pwl <- function(x, y, lower, upper) {
  if (is.na(lower) || is.na(upper) || lower >= upper) return(NA_real_)
  
  ord <- order(x)
  x <- x[ord]
  y <- y[ord]
  
  # rule = 2: extrapolate using nearest boundary value if bounds exceed data range
  y_lower <- approx(x, y, xout = lower, rule = 2)$y
  y_upper <- approx(x, y, xout = upper, rule = 2)$y
  
  inside <- x > lower & x < upper
  x_sub  <- c(lower, x[inside], upper)
  y_sub  <- c(y_lower, y[inside], y_upper)
  
  n <- length(x_sub)
  if (n < 2) return(0)
  
  dx         <- x_sub[2:n] - x_sub[1:(n - 1)]
  avg_height <- (y_sub[2:n] + y_sub[1:(n - 1)]) / 2
  
  sum(dx * avg_height)
}

# 1. Extract the supply curve per group (no year)
curves <- Fig1b_res_supply_curve_plot %>%
  distinct(scenario, region, resource, subresource, value.Q, value.cost) %>%
  group_by(scenario, region, resource, subresource) %>%
  nest(curve_data = c(value.Q, value.cost))


# 2. Extract the year-specific production bounds
bounds <- Fig1b_res_supply_curve_plot %>%
  distinct(scenario, region, resource, subresource, year, value.prod) %>%
  arrange(scenario, region, resource, subresource, year) %>%
  group_by(scenario, region, resource, subresource) %>%
  mutate(prev_prod = lag(value.prod, default = 0)) %>%
  ungroup()

#incremental cost by year
# by subresource
reg_cost_subres <- bounds %>%
  left_join(curves, by = c("scenario", "region", "resource", "subresource")) %>%
  rowwise() %>%
  mutate(
    total_cost = area_under_pwl(
      x     = curve_data$value.Q,
      y     = curve_data$value.cost,
      lower = prev_prod,
      upper = value.prod
    )
  ) %>%
  select(-curve_data) %>%
  ungroup() 

# incremental cost by year
# across all subresources
reg_cost_total <- reg_cost_subres %>%
  mutate(total_cost = if_else(is.na(total_cost), 0, total_cost)) %>%
  group_by(scenario, region, resource, year) %>%
  dplyr::summarise(value = sum(total_cost)*(125.428/27.8)/(10^12),
                   Units = "trillion 2025$") %>%
  ungroup() %>%
  mutate(region = factor(region, levels = region_levels_reg32))

#Reg cost from 2025-2050
reg_cost_total_2025_2050 <- reg_cost_total %>%
  filter(year >= 2025 & year <= 2050) %>%
  group_by(scenario, region, resource, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(year = "2025-2050") %>%
  ungroup()

reg_cost_total_2050_2075 <- reg_cost_total %>%
  filter(year > 2050 & year <= 2075) %>%
  group_by(scenario, region, resource, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(year = "2050-2075") %>%
  ungroup()

reg_cost_total_2025_2075 <- bind_rows(reg_cost_total_2025_2050,
                                      reg_cost_total_2050_2075) %>%
  left_join_error_no_match(mineral_reg_mapping, by = c("region")) %>%
  select(-region) %>%
  rename(region = top_reg_map) %>%
  group_by(scenario, resource, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(region = factor(region, levels = top_allMinerals_region_order)) 

# cumulative cost by year
# across all subresources
cum_reg_cost_total <- reg_cost_total %>%
  group_by(scenario, region, resource, Units) %>%
  filter(year >= 2021) %>%
  dplyr::mutate(value = cumsum(value)) %>%
  ungroup() %>%
  mutate(region = factor(region, levels = region_levels_reg32)) %>%
  mutate(scenario = factor(scenario, levels = SCENARIO_order)) %>%
  group_by(scenario, region, resource, Units) %>%
  Fill_annual()

cum_global_cost_total <- cum_reg_cost_total %>%
  group_by(scenario, resource, year, Units) %>%
  dplyr::summarise(value = sum(value))

cum_global_cost_total_allMinerals <- cum_global_cost_total %>%
  group_by(scenario, year, Units) %>%
  dplyr::summarise(value = sum(value))


# FIGURE 1A: REGIONAL SUPPLY MAP ------------------------------------------

source('input/robinson_proj.R')


output.workflow.dir <- 'output/'



# Load data and Prepare Data

# Load World Basemap
world <- ggplot() +
  # Ocean background rectangle layer
  geom_sf(data = NE_box_rob, fill = "#d9f5f4", color = "black", linewidth = 0.5) +  # light blue ocean + black frame
  geom_sf(data = NE_countries_rob, color = "black", fill = "white", size = 0.3) +
  geom_sf(data = NE_graticules_rob, color = "gray70", linetype = "dotted", linewidth = 0.3) +
  geom_text(
    data = lbl.Y.prj,
    aes(x = X.prj, y = Y.prj, label = lbl),
    size = 1.5, color = "gray30"
  ) +
  coord_sf(crs = PROJ) +
  theme_void()



res_prod_tech_vintage <- getQuery(prj, "resource production by tech and vintage") %>%
  filter(resource %in% c("copper", "lithium", "nickel")) %>%
  separate(technology, into = c("technology", "vintage"), sep = ",year=")

reg_total_res_prod <- res_prod_tech_vintage %>%
  filter(scenario %in% SCENARIO_constrained) %>%
  group_by(Units, scenario, region, resource, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup() %>%
  group_by(Units, scenario, region, resource) 

reg_total_res_prod_map <- reg_total_res_prod %>%
  mutate(region = gsub("EU-12", "EU_12", region),
         region = gsub("EU-15", "EU_15", region)) %>%
  filter(scenario == SCENARIO_ref)

# query gives available by grade, let's add up all
res_avail <- getQuery(prj, "resource supply curves") %>%
  filter(resource %in% c("copper", "lithium", "nickel")) %>%
  group_by(Units, scenario, region, resource, subresource, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup()

#combine across subresources
res_avail_total <- res_avail %>%
  separate(subresource, into = c(NA, "vintage"), sep = "_") %>%
  filter(vintage == 2021 | year >= vintage) %>%
  group_by(Units, scenario, region, resource, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup()
# 
res_avail_total_map <- res_avail_total %>%
  mutate(region = gsub("EU-12", "EU_12", region),
         region = gsub("EU-15", "EU_15", region)) %>%
  filter(scenario == SCENARIO_ref)


# Spatial Referencing

# set epsg code 4326: https://spatialreference.org/ref/epsg/wgs-84/
epsg_code <- 4326

# create region simple feature
region_sf <- rmap::mapGCAMReg32 %>% 
  dplyr::select(region_id = subRegionAlt,
                region = subRegion,
                geometry) %>% 
  st_transform(epsg_code)

# region dataframe
region_df <- region_sf %>% 
  sf::st_drop_geometry()

# get fill shapefile (for resources)
AnnRes_fill <- region_sf %>%
  right_join(res_avail_total_map, by = c("region")) %>%
  rename(Year = year) %>%
  filter(Year >= 2021, Year <= 2050)



# get bubble position - centroids
symbol_pos <- sf::st_centroid(region_sf, of_largest_polygon = T)


AnnProd_points <- symbol_pos %>%
  right_join(reg_total_res_prod_map, by = c("region")) %>%
  rename(Year = year) %>%
  filter(Year >= 2021, Year <= 2050)


reg_total_res_prod_pie <- reg_total_res_prod_map %>%
  tidyr::pivot_wider(
    names_from = resource,
    values_from = value,
    values_fill = 0                             # fill missing minerals with 0
  ) %>%
  mutate(total = copper + lithium + nickel) 

reg_total_res_prod_pie_pos <- symbol_pos %>%
  dplyr::left_join(reg_total_res_prod_pie, by = c("region")) %>%
  sf::st_transform(crs = PROJ) %>%
  mutate(x = sf::st_coordinates(.)[,1],
         y = sf::st_coordinates(.)[,2]) %>%
  sf::st_drop_geometry()

# --- Define legend parameters ---
legend_totals <- c(0.5, 2, 5, 10, 25)
legend_radii <- sqrt(legend_totals) * 300000

# Exact label lookup
labeller_fn <- function(r) {
  idx <- sapply(r, function(ri) which.min(abs(ri - legend_radii)))
  paste0(legend_totals[idx], " Mt/yr")
}


# --- Radius scaling: define ONCE and reuse ---
radius_scale <- 300000
to_radius <- function(total) sqrt(total) * radius_scale

# --- Legend bubbles: pick round data values ---
legend_breaks <- c(1, 5, 10)            
legend_radii  <- to_radius(legend_breaks)

# Anchor position (bottom-left of map, in projected CRS units)
legend_x0 <- -14000000
legend_y0 <- -13000000

# Lay circles side by side with a small gap, bottom-aligned
gap <- max(legend_radii) * 0.8
x_centers <- numeric(length(legend_radii))
x_centers[1] <- legend_x0 + legend_radii[1]
for (i in seq_along(legend_radii)[-1]) {
  x_centers[i] <- x_centers[i - 1] + legend_radii[i - 1] +
    legend_radii[i] + gap
}

legend_df <- data.frame(
  x0    = x_centers,
  y0    = legend_y0 + legend_radii,   # align bottoms of circles
  r     = legend_radii,
  label = paste0(legend_breaks, " Mt/yr")
)

# --- Plot ---
world +
  geom_sf(data = region_sf, fill = "white", color = "grey20", size = 0.2) +
  scatterpie::geom_scatterpie(
    aes(x = x, y = y, r = to_radius(total)),
    data = filter(reg_total_res_prod_pie_pos,
                  year == 2050, scenario == SCENARIO_ref),
    cols  = c("copper", "lithium", "nickel"),
    color = "grey20",
    alpha = 0.9,
    lwd   = 0.2
  ) +
  
geom_circle(
  data = legend_df,
  aes(x0 = x0, y0 = y0, r = r),
  fill = "grey90", color = "grey20", lwd = 0.3,
  inherit.aes = FALSE
) +
  geom_text(
    data = legend_df,
    aes(x = x0, y = y0 - r - 300000, label = label),
    size = 3, vjust = 1, inherit.aes = FALSE
  ) +
  annotate(
    "text",
    x = legend_x0,
    y = legend_y0 + 2 * max(legend_radii) + 600000,
    label = "Total production",
    size = 3.2, fontface = "bold", hjust = 0
  ) +
  
  coord_sf(crs = PROJ) +
  scale_fill_brewer(palette = "Dark2", name = "Mineral") +
  theme_void() +
  theme(
    plot.background       = element_rect(fill = "white", color = NA),
    panel.background      = element_rect(fill = NA,      color = NA),
    legend.background     = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA),
    panel.grid.major      = element_line(color = "grey70",
                                         linetype = "dotted", size = 0.3),
    legend.position       = "bottom",
    legend.title          = element_text(size = 10),
    legend.text           = element_text(size = 9)
  )

ggsave(paste0(PLOT_FOLDER, "Fig1a.png"),
       width = 9, height = 6, dpi = 300, units = "in")

# FIGURE 1B: CUMULATIVE COST STACKED BAR BY REGION  -----------------------------------


cum_regAgg_cost_total_allMinerals <- cum_reg_cost_total %>%
  ungroup() %>%
  left_join_error_no_match(mineral_reg_mapping, by = c("region")) %>%
  select(-region) %>%
  rename(region = top_reg_map) %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) 

cum_regAgg_cost_total_allMinerals_diff <- pct_diff_from_2021(cum_regAgg_cost_total_allMinerals,
                                            ref_scenario = SCENARIO_ref,
                                            diff_scenarios = SCENARIO_order,
                                            join_var = c("region"))


cum_regAgg_cost_total <- cum_reg_cost_total %>%
  ungroup() %>%
  left_join_error_no_match(mineral_reg_mapping, by = c("region")) %>%
  select(-region) %>%
  rename(region = top_reg_map) %>%
  group_by(scenario, region, resource, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(region = factor(region, levels = top_allMinerals_region_order)) 

cum_regAgg_cost_total_diff <- pct_diff_from_2021(cum_regAgg_cost_total,
                                            ref_scenario = SCENARIO_ref,
                                            diff_scenarios = SCENARIO_order,
                                            join_var = c("region", "resource"))


Fig1b <-
  ggplot() +
  geom_bar(data = filter(cum_regAgg_cost_total,
                         scenario %in% SCENARIO_ref,
                         year %in% c(2025, 2050, 2075)),
           aes(x = region, y = value, group = resource, fill = resource),
           stat = "identity", position = position_stack(reverse = TRUE)) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  facet_grid(~year, labeller = labeller(year = as_labeller(c("2025" = "2021-2025",
                                                           "2050" = "2021-2050",
                                                           "2075" = "2021-2075"))),
             scale = "free")+
  scale_fill_brewer(name = "Mineral", palette = "Dark2")+
  #scale_x_discrete(breaks = rev(top_allMinerals_region_order))+
  theme_bw() +
  theme(
    text = element_text(size = 16),
  #  axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "right",
    legend.key.width = unit(1, "cm")
  ) +
  coord_flip()+
  labs(x = "", y = "trillion 2025$", title = "Cumulative extraction costs") 


print(Fig1b)

ggsave(paste0(PLOT_FOLDER,"Fig1b.png", sep = ""),width=9, height=3, units="in")

# FIGURE 2: GLOBAL SUPPLY-SIDE DYNAMICS ----------------------------------------------------------------


# FIGURE 2A: GLOBAL MINERAL PRODUCTION (LINES) - UNCONSTRAINED AND CONSTRAINED --------
SCENARIO_order <- c("01272026_UnlimitSupply_BR",
                    "01272026_His_constrSupply_BR_noTC",
                    "01272026_His_constrSupply_BR_shortLT_noTC")

mineral_inputs_tech <- getQuery(prj, "minerals inputs by tech") %>%
  filter(input %!in% c("traded copper", "traded nickel", "traded lithium", "traded iron and steel", "iron and steel"),
         sector %!in% c("traded copper", "traded nickel", "traded lithium", "traded iron and steel", "iron and steel"))

global_sector_demand <- mineral_inputs_tech %>%
  left_join(mineral_demand_tech_mapping, by = c("sector", "subsector", "technology")) %>%
  group_by(Units, scenario, sector0, year, input) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year <= 2100) %>%
  rename(sector = sector0) %>%
  mutate(sector = factor(sector, levels = sector_levels))

global_total_demand <- global_sector_demand %>%
  group_by(scenario, input, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(scenario = factor(scenario, levels = SCENARIO_order)) %>%
  mutate(input = gsub("regional ", "", input)) %>%
  rename(resource = input) %>%
  filter(resource %in% c("copper", "lithium", "nickel")) %>%
  filter(scenario %in% SCENARIO_order)



fig2a <-
  ggplot() +
  geom_line(data = filter(global_total_demand, year >= 2021, year <= 2075,
                          scenario %in% SCENARIO_order),
            aes(x = year, y = value, group = scenario, color = scenario),  linewidth = 1) +
  theme_bw() +
  facet_grid(resource~., scales = "free_y")+
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "right",
    legend.key.width = unit(1.5, "cm")
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  scale_y_continuous(limits = c(0, NA))+
  scale_color_manual(labels = SCENARIO_labels, name = "Scenario", values = SCENARIO_colors)+
  labs(y = "Mt/yr", x = "")

print(fig2a)


# FIGURE 2B: GLOBAL MINERAL PRODUCTION BY STAGE (REFERENCE, SHORT LEAD TIMES) --------------------------------------------------------------------

# calculate supply-demand gap
global_total_demand_unconstrained <- global_total_demand %>%
  filter(scenario == SCENARIO_ref_unconstrained)

global_total_demand_diff <- global_total_demand %>%
  filter(scenario %in% SCENARIO_constrained,
         scenario %in% SCENARIO_order) %>%
  left_join(global_total_demand_unconstrained, by = c("resource", "year"),
            suffix = c(".constr", ".unconstr")) %>%
  mutate(value = value.unconstr - value.constr,
         Units = "Mt", 
         stage = "Production reduction from unconstrained supply") %>%
  rename(scenario = scenario.constr) %>%
  select(Units, scenario, resource, stage, year, value)


res_prod_tech_vintage <- getQuery(prj, "resource production by tech and vintage") %>%
  filter(resource %in% c("copper", "lithium", "nickel")) %>%
  separate(technology, into = c("technology", "vintage"), sep = ",year=")

# get mine stage data
all_years <- sort(unique(res_prod_tech_vintage$year))

global_res_prod_tech <- res_prod_tech_vintage %>%
  separate(technology, into = c(NA, "technology"), sep = "_") %>%
  group_by(Units, scenario, resource, technology, year) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  group_by(Units, scenario, resource, technology) %>%
  tidyr::complete(year = all_years, fill = list(value = 0)) %>%
  group_by(Units, scenario, resource, technology) %>%
  Fill_annual()

global_res_prod_tech_stages <- global_res_prod_tech %>%
  mutate(technology = as.numeric(technology)) %>%
  left_join(stages_mapping, by = c("resource", "technology")) %>%
  mutate(stage = if_else(scenario %in% SCENARIOS_shortLT, stage_shortLT, stage_average)) %>%
  group_by(Units, scenario, resource, stage, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup() %>%
  # add the supply demand gap 
  bind_rows(global_total_demand_diff) %>%
  mutate(stage = factor(stage, levels = c(mine_stages, "Production reduction from unconstrained supply")))



  fig2b <-
    ggplot() +
    geom_area(
      data = filter(global_res_prod_tech_stages, scenario %in% SCENARIO_order,
                    year >= 2021, year <= 2075),
      aes(x = year, y = value, group = stage, fill = stage), alpha = 0.7,
      stat = "identity", position = position_stack(reverse = TRUE),
    ) +
    geom_line(
      data = filter(global_total_demand, scenario %in% c(SCENARIO_ref, SCENARIO_ref_shortLT),
                    year >= 2021, year <= 2075),
      aes(x = year, y = value, group = scenario, color = scenario), linewidth = 1
    ) +
    facet_grid(resource~scenario, scales = "free_y",
               labeller = labeller(
                 scenario = as_labeller(SCENARIO_labels)
               )) +
    scale_fill_manual(
      values = mine_stage_gap_colors,
      name = "Reserve/resource stage in 2021"
    ) +
    geom_hline(yintercept = 0, color = "black", linetype = 2) +
    #scale_linetype_manual(values = SCENARIO_lines,
    #                      labels = SCENARIO_labels)+
    scale_color_manual(values = SCENARIO_colors,
                       labels = SCENARIO_labels)+
    theme_bw() +
    theme(
      text = element_text(size = 16),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
      strip.text = element_text(size = 16),
      legend.position = "right",
      legend.key.width = unit(1.5, "cm")
    ) +
    labs(y = "Mt/yr", x = "") + 
    guides(linetype = "none",
           color = "none")
  
  print(fig2b)
  
# FIGURE 2 combined -------------------------------------------------------

Fig2ab <- (fig2a | fig2b) +
    plot_layout(widths = c(1, 2),
                guides = "collect")+ 
  plot_annotation(tag_levels = "A")+
  theme(
    legend.position = "right"
  )


  print(Fig2ab)
  
  ggsave(paste0(PLOT_FOLDER,"Fig2.png", sep = ""),width=15, height=7, units="in")
  

# FIGURE 3: DEMAND-SIDE DYNAMICS ----------------------------------------------------------------
# FIGURE 3A (SAME AS FIGURE 2A) -------------------------------------------


# FIGURE 3B: SECTORAL DEMAND (UNCONSTRAINED) ---------------------------------

  global_sector_demand_area <- global_sector_demand %>%
    group_by(Units, scenario, sector, input) %>%
    Fill_annual() %>%
    mutate(input = gsub("regional ", "", input)) %>%
    filter(input %in% c("copper", "lithium", "nickel")) %>%
    rename(resource = input)
  
  
  
  
  fig3b <-
    ggplot() +
    geom_area(
      data = filter(global_sector_demand_area, scenario == SCENARIO_ref_unconstrained,
                    year >= 2021, year <= 2075),
      aes(x = year, y = value, group = sector, fill = sector), alpha = 0.7,
      stat = "identity", position = position_stack(reverse = TRUE),
    ) +
    geom_line(
      data = filter(global_total_demand, scenario == SCENARIO_ref_unconstrained,
                    year >= 2021, year <= 2075),
      aes(x = year, y = value, group = scenario, color = scenario), linewidth = 1
    ) +
    geom_hline(yintercept = 0, color = "black", linetype = 2) +
    facet_grid(resource~scenario, scales = "free_y",
               labeller = labeller(
                 scenario = as_labeller(SCENARIO_labels)
               )) +
    scale_fill_manual(
      values = sector_colors,
      name = "End-use sector"
    ) +
   # scale_linetype_manual(values = SCENARIO_lines,
  #                        labels = SCENARIO_labels)+
    scale_color_manual(values = SCENARIO_colors,
                       labels = SCENARIO_labels,
                       name = "Scenario")+
    theme_bw() +
    theme(
      text = element_text(size = 16),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
      strip.text = element_text(size = 16),
      legend.position = "right",
      legend.key.width = unit(1.5, "cm")
    ) +
    labs(y = "Mt/yr", x = "") +
    guides(color = "none")
  
  print(fig3b)
  

# FIGURE 3C: SECTORAL DEMAND (DIFF FROM UNCONSTRAINED SUPPLY) -------------


global_sector_demand_area_diff <- diff_from_scen(global_sector_demand_area,
                                                 diff_scenarios = SCENARIO_order,
                                                 ref_scenario = SCENARIO_ref_unconstrained,
                                                 c("sector", "resource", "year"))  %>%
    rename(Units = Units.diff,
           scenario = scenario.diff) %>%
  ungroup() %>%
    select(Units, scenario, sector, resource, year, value) 

fig3c <-
  ggplot() +
  geom_area(
    data = filter(global_sector_demand_area_diff, scenario %in% c(SCENARIO_ref, SCENARIO_ref_shortLT),
                  year >= 2021, year <= 2075),
    aes(x = year, y = value, group = sector, fill = sector), alpha = 0.7,
    stat = "identity", position = position_stack(reverse = FALSE),
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  facet_grid(resource~scenario, scales = "free_y",
             labeller = labeller(
               scenario = as_labeller(SCENARIO_labels)
             )) +
  scale_fill_manual(
    values = sector_colors,
    name = "End-use sector"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "right",
    legend.key.width = unit(1.5, "cm")
  ) +
  labs(y = "Mt/yr", x = "") 

print(fig3c)


# FIGURE 3 COMBINED -------------------------------------------------------
#3a is the same as 2a
Fig3 <- (fig2a | fig3b | plot_spacer() | fig3c) + 
  plot_layout(
    widths = c(1,1,0.2,2),
    guides = "collect"
  )+
  plot_annotation(tag_levels = "A")+
  theme(
    legend.position = "right"
  )

print(Fig3)

ggsave(paste0(PLOT_FOLDER,"Fig3.png", sep = ""),width=16, height=7, units="in")


# FIGURE 4A: GLOBAL ANNUAL MINERAL PRODUCTION (LINES/RIBBON) - BASELINE VS STEADY-STATE --------
SCENARIO_order <- c(
  "01272026_His_constrSupply_BR_noTC",
  "01272026_His_constrSupply_EnR_noTC",
  "03192026_His_constrSupply_highDemand",
  "01272026_His_constrSupply_BR_SS_noTC",
  "01272026_His_constrSupply_EnR_SS_noTC",
  "03192026_His_constrSupply_SS_highDemand")

mineral_inputs_tech <- getQuery(prj, "minerals inputs by tech") %>%
  filter(input %!in% c("traded copper", "traded nickel", "traded lithium", "traded iron and steel", "iron and steel"),
         sector %!in% c("traded copper", "traded nickel", "traded lithium", "traded iron and steel", "iron and steel"))

global_sector_demand <- mineral_inputs_tech %>%
  left_join(mineral_demand_tech_mapping, by = c("sector", "subsector", "technology")) %>%
  group_by(Units, scenario, sector0, year, input) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year <= 2100) %>%
  rename(sector = sector0) %>%
  mutate(sector = factor(sector, levels = sector_levels))

global_total_demand <- global_sector_demand %>%
  group_by(scenario, input, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(scenario = factor(scenario, levels = SCENARIO_order)) %>%
  mutate(input = gsub("regional ", "", input)) %>%
  rename(resource = input) %>%
  filter(resource %in% c("copper", "lithium", "nickel")) %>%
  mutate(lead_times = case_when(str_detect(scenario, "shortLT") ~ "Short lead times",
                                str_detect(scenario, "Unlimit") ~ "Unconstrained supply",
                                TRUE ~ "Average lead times")) %>%
  mutate(supply_factor = if_else(str_detect(scenario, "SS"),"Steady-state constrained supply","Baseline constrained supply")) %>%
  mutate(supply_lead_combined = paste(lead_times, supply_factor)) %>%
  filter(scenario %in% SCENARIO_order) %>%
  mutate(Units = "Mt/yr")
  



fig4a <-
  ggplot() +
  geom_ribbon(
    data = global_total_demand %>%
      filter(scenario %in% SCENARIO_order,
             year >= 2021, year <= 2075) %>%
      group_by(year, resource, supply_lead_combined) %>%
      summarise(
        ymin = min(value, na.rm = TRUE),
        ymax = max(value, na.rm = TRUE),
        .groups = "drop"
      ),
    aes(x = year, ymin = ymin, ymax = ymax,
        fill = supply_lead_combined, group = supply_lead_combined),
    alpha = 0.15
  ) +
  geom_line(data = filter(global_total_demand, year >= 2021, year <= 2075,
                          scenario %in% SCENARIO_order),
            aes(x = year, y = value, group = scenario, color = scenario, linetype = scenario)) +
  theme_bw() +
  facet_grid(resource~., scales = "free_y")+
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "right",
    legend.key.width = unit(1.5, "cm")
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  scale_y_continuous(limits = c(0, NA))+
  scale_color_manual(labels = SCENARIO_labels, name = "Scenario", values = SCENARIO_colors)+
  scale_linetype_manual(labels = SCENARIO_labels, name = "Scenario", values = SCENARIO_lines)+
  scale_fill_manual(values = supply_lead_combined_colors, name = "Scenario", labels = c("Average lead times Baseline constrained supply" = "Baseline constrained supply scenarios",
                                                                              "Average lead times Steady-state constrained supply" = "Steady-state constrained supply scenarios"))+
  labs(y = "Mt/yr", x = "")

print(fig4a)




# FIGURE 4B: GLOBAL CUMULATIVE MINERAL PRODUCTIONB (LINES/RIBBON) - BASELINE, STEADY-STATE (MAX PROD POTENTIAL VS MODEL RESOLVED) ----------------------------------------------------------------

# query gives available by grade, let's add up all
res_avail <- getQuery(prj, "resource supply curves") %>%
  filter(resource %in% c("copper", "lithium", "nickel")) %>%
  group_by(Units, scenario, region, resource, subresource, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup()

#combine across subresources
res_avail_total <- res_avail %>%
  separate(subresource, into = c(NA, "vintage"), sep = "_") %>%
  filter(vintage == 2021 | year >= vintage) %>%
  group_by(Units, scenario, region, resource, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup()

#combine across regions
global_res_avail_total <- res_avail_total %>%
  filter(scenario %in% SCENARIO_order) %>%
  group_by(Units, scenario, resource, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(supply_factor = if_else(str_detect(scenario, "SS"),"Steady-state constrained supply","Baseline constrained supply")) %>%
  mutate(demand_factor = gsub("_SS", "", scenario)) %>%
  mutate(lead_times = if_else(str_detect(scenario, "shortLT"), "Short lead times", "Average lead times")) %>%
  mutate(scenario = "Maximum production potential") %>%
  mutate(supply_lead_combined = paste(lead_times, supply_factor))


res_cum_prod <- getQuery(prj, "resource cumulative production") %>%
  filter(resource %in% c("copper", "lithium", "nickel"))


global_total_cum_res_prod <- res_cum_prod %>%
  filter(scenario %in% SCENARIO_order) %>%
  group_by(Units, scenario, resource, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup() %>%
  group_by(Units, scenario, resource) %>%
  Fill_annual() %>%
  mutate(supply_factor = if_else(str_detect(scenario, "SS"),"Steady-state constrained supply","Baseline constrained supply")) %>%
  mutate(demand_factor = gsub("_SS", "", scenario)) %>%
  mutate(lead_times = if_else(str_detect(scenario, "shortLT"), "Short lead times", "Average lead times")) %>%
  mutate(demand_factor = if_else(demand_factor == "03122026_His_constrSupply_highDemand", "03072026_His_constrSupply_highDemand", demand_factor)) %>%
  mutate(supply_lead_combined = paste(lead_times, supply_factor))


Fig4b <-
  ggplot() +
  geom_ribbon(
    data = global_total_cum_res_prod %>%
      filter(scenario %in% SCENARIO_order,
             lead_times == "Average lead times",
             year >= 2021, year <= 2075) %>%
      group_by(year, resource, supply_lead_combined) %>%
      summarise(
        ymin = min(value, na.rm = TRUE),
        ymax = max(value, na.rm = TRUE),
        .groups = "drop"
      ),
    aes(x = year, ymin = ymin, ymax = ymax,
        fill = supply_lead_combined, group = supply_lead_combined),
    alpha = 0.15
  ) +
  geom_line(
    data = filter(global_total_cum_res_prod, scenario %in% c(SCENARIO_ref, SCENARIO_SS_ref), lead_times == "Average lead times", year >= 2021, year <= 2075),
    aes(x = year, y = value, group = scenario, color = scenario)
  ) +
  geom_line(
    data = filter(global_res_avail_total , lead_times == "Average lead times", year >= 2021, year <= 2075),
    aes(x = year, y = value, group = scenario, color = scenario)
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  facet_grid(resource~supply_lead_combined, scales = "free_y",
             labeller = labeller(supply_lead_combined = function(x) stringr::str_wrap((labels = c("Average lead times Baseline constrained supply" = "Baseline constrained supply",
                                                                                                  "Average lead times Steady-state constrained supply" = "Steady-state constrained supply")), width = 12)) ) +
  scale_color_manual(name = "",
                     values = c(SCENARIO_colors, "Maximum production potential" = "black"),
                     labels = c(SCENARIO_labels, "Maximum production potential" = "Maximum production potential"))+
  scale_fill_manual(values = supply_lead_combined_colors, name = "Scenario", labels = c("Average lead times Baseline constrained supply" = "Baseline constrained supply scenarios",
                                                                                        "Average lead times Steady-state constrained supply" = "Steady-state constrained supply scenarios"))+
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "right",
    legend.key.width = unit(1.5, "cm")
  ) +
  labs(y = "Mt", x = "") +
  guides(linetype = "none")

print(Fig4b)


# FIGURE 4 COMBINED ---------------------------------------------------

Fig4 <- (fig4a | Fig4b) + 
  plot_layout(
    guides = "collect",
    widths = c(1,2)
  )+
  plot_annotation(tag_levels = "A")+
  theme(
    legend.position = "right"
  )

print(Fig4)


ggsave(paste0(PLOT_FOLDER,"Fig4.png", sep = ""),width=12, height=7, units="in")






# FIGURE 5: PRICES --------------------------------------------------------


# FIGURE 5 MINERAL PRICES -----------------------------------------------
SCENARIO_order <- c("01272026_UnlimitSupply_BR",
  "01272026_His_constrSupply_BR_noTC",
  "01272026_His_constrSupply_EnR_noTC",
  "03192026_His_constrSupply_highDemand",
  "01272026_His_constrSupply_BR_shortLT_noTC",
  "01272026_His_constrSupply_EnR_shortLT_noTC",
  "03192026_His_constrSupply_shortLT_highDemand")


GloConsPriceUnconstr <- getQuery(prj, "regional prices (Mt)") %>%
  filter(sector %in% c("regional copper", "regional lithium", "regional nickel")) %>%
  select(-scenario, -value) %>%
  mutate(value = case_when(sector == "regional copper" ~ 1.45,
                                      sector == "regional lithium" ~ 14.44,
                                      sector == "regional nickel" ~ 3.11)) %>%
  mutate(sector = gsub("regional ", "", sector)) %>%
  mutate(scenario = SCENARIO_ref_unconstrained) %>%
  mutate(value = value*125.428/27.8,
         Units = "2025$/kg") %>%
  distinct()

#Consumer price is the same across regions
# "Regional price"
# We can just query a single region to get the global price
GloConsPrice <- getQuery(prj, "regional prices (Mt)") %>%
  filter(sector %in% c("regional copper", "regional lithium", "regional nickel")) %>%
  mutate(value = value*125.428/27.8,
         Units = "2025$/kg") %>%
  filter(scenario %in% SCENARIO_order) %>%
  mutate(scenario = factor(scenario, levels = SCENARIO_order)) %>%
  mutate(sector = gsub("regional ", "", sector)) %>%
  bind_rows(GloConsPriceUnconstr)

GloConsPriceRelDiff <- GloConsPrice %>%
  filter(region == "USA") %>%
  select(-region) %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year", "sector")) %>%
  mutate(lead_times = if_else(str_detect(scenario.diff, "shortLT"), "Short lead times", "Average lead times"))
  


# FIGURE 5 ELECTRICITY PRICES -------------------------------------------

PricesMkt <- getQuery(prj, "prices of all markets")

PricesElecReg <- PricesMkt %>%
  filter(grepl("electricity", market),
         !grepl("Demand_int", market),
         !grepl("ownuse", market)) %>%
  mutate(market = gsub("electricity", "", market)) %>%
  mutate(value = value*125.428/27.8,
         Units = "$2025/GJ") %>%
  rename(region = market) %>%
  filter(year >= 2021)

PricesWindReg <- PricesMkt %>%
  filter(grepl("wind_mineral", market),
         !grepl("fixed-output", market)) %>%
  mutate(market = gsub("wind_mineral", "", market)) %>%
  mutate(value = value*125.428/27.8,
         Units = "$2025/GJ") %>%
  rename(region = market) %>%
  filter(year >= 2021) %>%
  mutate(market = "wind")

PricesWindOffshoreReg <- PricesMkt %>%
  filter(grepl("wind_offshore_mineral", market),
         !grepl("fixed-output", market)) %>%
  mutate(market = gsub("wind_offshore_mineral", "", market)) %>%
  mutate(value = value*125.428/27.8,
         Units = "$2025/GJ") %>%
  rename(region = market) %>%
  filter(year >= 2021) %>%
  mutate(market = "wind_offshore")

PricesWindStorageReg <- PricesMkt %>%
  filter(grepl("wind_storage_mineral", market),
         !grepl("fixed-output", market)) %>%
  mutate(market = gsub("wind_storage_mineral", "", market)) %>%
  mutate(value = value*125.428/27.8,
         Units = "$2025/GJ") %>%
  rename(region = market) %>%
  filter(year >= 2021) %>%
  mutate(market = "wind_storage")

PricesPVReg <- PricesMkt %>%
  filter(grepl("pv_mineral", market),
         !grepl("rooftop", market),
         !grepl("fixed-output", market)) %>%
  mutate(market = gsub("pv_mineral", "", market)) %>%
  mutate(value = value*125.428/27.8,
         Units = "$2025/GJ") %>%
  rename(region = market) %>%
  filter(year >= 2021) %>%
  mutate(market = "PV")

PricesPVStorageReg <- PricesMkt %>%
  filter(grepl("pv_storage_mineral", market),
         !grepl("rooftop", market),
         !grepl("fixed-output", market)) %>%
  mutate(market = gsub("pv_storage_mineral", "", market)) %>%
  mutate(value = value*125.428/27.8,
         Units = "$2025/GJ") %>%
  rename(region = market) %>%
  filter(year >= 2021) %>%
  mutate(market = "PV_storage")

PricesCSPReg <- PricesMkt %>%
  filter(grepl("elec_CSP", market),
         !grepl("fixed-output", market)) %>%
  separate(market, into = c("region", "technology"), sep = c("elec_")) %>%
  mutate(value = value*125.428/27.8,
         Units = "$2025/GJ") %>%
  filter(year >= 2021)

PricesGasReg <- PricesMkt %>%
  filter(grepl("elec_gas", market),
         !grepl("fixed-output", market)) %>%
  separate(market, into = c("region", "technology"), sep = c("elec_")) %>%
  mutate(value = value*125.428/27.8,
         Units = "$2025/GJ") %>%
  filter(year >= 2021)

PricesGeothermalReg <- PricesMkt %>%
  filter(grepl("elec_geothermal", market),
         !grepl("fixed-output", market)) %>%
  separate(market, into = c("region", "technology"), sep = c("elec_")) %>%
  mutate(value = value*125.428/27.8,
         Units = "$2025/GJ") %>%
  filter(year >= 2021)

PricesBiomassReg <- PricesMkt %>%
  filter(grepl("elec_biomass", market),
         !grepl("fixed-output", market)) %>%
  separate(market, into = c("region", "technology"), sep = c("elec_")) %>%
  mutate(value = value*125.428/27.8,
         Units = "$2025/GJ") %>%
  filter(year >= 2021)

PricesCoalReg <- PricesMkt %>%
  filter(grepl("elec_coal", market),
         !grepl("fixed-output", market)) %>%
  separate(market, into = c("region", "technology"), sep = c("elec_")) %>%
  mutate(value = value*125.428/27.8,
         Units = "$2025/GJ") %>%
  filter(year >= 2021)

PricesRefLiqReg <- PricesMkt %>%
  filter(grepl("elec_refined liquids", market),
         !grepl("fixed-output", market)) %>%
  separate(market, into = c("region", "technology"), sep = c("elec_")) %>%
  mutate(value = value*125.428/27.8,
         Units = "$2025/GJ") %>%
  filter(year >= 2021)

PricesNucReg <- PricesMkt %>%
  filter(grepl("elec_Gen_I", market),
         !grepl("fixed-output", market)) %>%
  separate(market, into = c("region", "technology"), sep = c("elec_")) %>%
  mutate(value = value*125.428/27.8,
         Units = "$2025/GJ") %>%
  filter(year >= 2021)

# Get total electricity generation by region so that we can weight the elec prices
# and calculate a global weighted average
TotalElecGenReg <- getQuery(prj2, "elec gen by gen tech") %>%
  group_by(Units, scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

#specific techs
WindElecGenReg <- getQuery(prj2, "elec gen by gen tech") %>%
  filter(technology == "wind") %>%
  group_by(Units, scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

WindOffshoreElecGenReg <- getQuery(prj2, "elec gen by gen tech") %>%
  filter(technology == "wind_offshore") %>%
  group_by(Units, scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

WindStorageElecGenReg <- getQuery(prj2, "elec gen by gen tech") %>%
  filter(technology == "wind_storage") %>%
  group_by(Units, scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

PVElecGenReg <- getQuery(prj2, "elec gen by gen tech") %>%
  filter(technology == "PV") %>%
  group_by(Units, scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

PVStorageElecGenReg <- getQuery(prj2, "elec gen by gen tech") %>%
  filter(technology == "PV_storage") %>%
  group_by(Units, scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

CSPElecGenReg <- getQuery(prj2, "elec gen by gen tech") %>%
  filter(technology %in% c("CSP", "CSP_storage")) %>%
  group_by(Units, scenario, region, technology, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

GasElecGenReg <- getQuery(prj2, "elec gen by gen tech") %>%
  filter(subsector == "gas") %>%
  group_by(Units, scenario, region, technology, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

GeothermalElecGenReg <- getQuery(prj2, "elec gen by gen tech") %>%
  filter(subsector == "geothermal") %>%
  group_by(Units, scenario, region, technology, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

BiomassElecGenReg <- getQuery(prj2, "elec gen by gen tech") %>%
  filter(subsector == "biomass") %>%
  group_by(Units, scenario, region, technology, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

CoalElecGenReg <- getQuery(prj2, "elec gen by gen tech") %>%
  filter(subsector == "coal") %>%
  group_by(Units, scenario, region, technology, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

RefLiqElecGenReg <- getQuery(prj2, "elec gen by gen tech") %>%
  filter(subsector == "refined liquids") %>%
  group_by(Units, scenario, region, technology, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

NucElecGenReg <- getQuery(prj2, "elec gen by gen tech") %>%
  filter(subsector == "nuclear") %>%
  group_by(Units, scenario, region, technology, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

#weighted average global electricity prices
PricesElecGlo <- PricesElecReg %>%
  left_join(TotalElecGenReg, by = c("scenario", "region", "year"),
                           suffix = c(".price", ".gen")) %>%
  na.omit() %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.gen, na.rm = TRUE), .groups = "drop")

PricesWindGlo <- PricesWindReg %>%
  left_join(WindElecGenReg, by = c("scenario", "region", "year"),
            suffix = c(".price", ".gen")) %>%
  na.omit() 

PricesWindOffshoreGlo <- PricesWindOffshoreReg %>%
  left_join(WindOffshoreElecGenReg, by = c("scenario", "region", "year"),
            suffix = c(".price", ".gen")) %>%
  na.omit() 

PricesWindStorageGlo <- PricesWindStorageReg %>%
  left_join(WindStorageElecGenReg, by = c("scenario", "region", "year"),
            suffix = c(".price", ".gen")) %>%
  na.omit()

PricesWindGlo_all <- bind_rows(PricesWindGlo,
                               PricesWindOffshoreGlo,
                               PricesWindStorageGlo) %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.gen, na.rm = TRUE), .groups = "drop")

PricesPVGlo <- PricesPVReg %>%
  left_join(PVElecGenReg, by = c("scenario", "region", "year"),
            suffix = c(".price", ".gen")) %>%
  na.omit() 

PricesPVStorageGlo <- PricesPVStorageReg %>%
  left_join(PVStorageElecGenReg, by = c("scenario", "region", "year"),
            suffix = c(".price", ".gen")) %>%
  na.omit()

PricesPVGlo_all <- bind_rows(PricesPVGlo,
                             PricesPVStorageGlo) %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.gen, na.rm = TRUE), .groups = "drop")

PricesCSPGlo <- PricesCSPReg %>%
  left_join(CSPElecGenReg, by = c("scenario", "region", "technology", "year"),
            suffix = c(".price", ".gen")) %>%
  na.omit() %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.gen, na.rm = TRUE), .groups = "drop")

PricesGasGlo <- PricesGasReg %>%
  left_join(GasElecGenReg, by = c("scenario", "region", "technology", "year"),
            suffix = c(".price", ".gen")) %>%
  na.omit() %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.gen, na.rm = TRUE), .groups = "drop")

PricesGeothermalGlo <- PricesGeothermalReg %>%
  left_join(GeothermalElecGenReg, by = c("scenario", "region", "technology", "year"),
            suffix = c(".price", ".gen")) %>%
  na.omit() %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.gen, na.rm = TRUE), .groups = "drop")

PricesBiomassGlo <- PricesBiomassReg %>%
  left_join(BiomassElecGenReg, by = c("scenario", "region", "technology", "year"),
            suffix = c(".price", ".gen")) %>%
  na.omit() %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.gen, na.rm = TRUE), .groups = "drop")

PricesCoalGlo <- PricesCoalReg %>%
  left_join(CoalElecGenReg, by = c("scenario", "region", "technology", "year"),
            suffix = c(".price", ".gen")) %>%
  na.omit() %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.gen, na.rm = TRUE), .groups = "drop")

PricesRefLiqGlo <- PricesRefLiqReg %>%
  left_join(RefLiqElecGenReg, by = c("scenario", "region", "technology", "year"),
            suffix = c(".price", ".gen")) %>%
  na.omit() %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.gen, na.rm = TRUE), .groups = "drop")

PricesNucGlo <- PricesNucReg %>%
  left_join(NucElecGenReg, by = c("scenario", "region", "technology", "year"),
            suffix = c(".price", ".gen")) %>%
  na.omit() %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.gen, na.rm = TRUE), .groups = "drop")


# diff from unconstrained
PricesElecGloRelDiff <- PricesElecGlo %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                 ref_scenario = SCENARIO_ref_unconstrained,
                 c("year")) %>%
  mutate(sector = "Electricity generation")

PricesWindGlo_allRelDiff <- PricesWindGlo_all %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(sector = "Wind")

PricesPVGlo_allRelDiff <- PricesPVGlo_all %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(sector = "Solar PV")

PricesCSPGloRelDiff <- PricesCSPGlo %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(sector = "Solar CSP")

PricesGasGloRelDiff <- PricesGasGlo %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(sector = "Gas")

PricesGeothermalGloRelDiff <- PricesGeothermalGlo %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(sector = "Geothermal")

PricesBiomassGloRelDiff <- PricesBiomassGlo %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(sector = "Biomass")

PricesCoalGloRelDiff <- PricesCoalGlo %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(sector = "Coal")

PricesRefLiqGloRelDiff <- PricesRefLiqGlo %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(sector = "Refined Liquids")

PricesNucGloRelDiff <- PricesNucGlo %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(sector = "Nuclear")


PricesAllElecGloRelDiff <- bind_rows(PricesElecGloRelDiff,
                                     PricesWindGlo_allRelDiff,
                                     PricesPVGlo_allRelDiff,
                                     PricesCSPGloRelDiff,
                                     PricesGasGloRelDiff,
                                     PricesGeothermalGloRelDiff,
                                     PricesBiomassGloRelDiff,
                                     PricesCoalGloRelDiff,
                                     PricesRefLiqGloRelDiff,
                                     PricesNucGloRelDiff)


# FIGURE 5 TRANSPORTATION SERVICE OUTPUT PRICE -------------------------------------

TrnServPrices <- getQuery(prj_E, "costs of transport techs")

PricesTrnServPassReg <- TrnServPrices %>%
  filter(sector == "trn_pasg_road_ldv_4w_pass") %>%
  mutate(value = value*105.377/60.055,
         Units = "$2020/pass-km") %>%
  filter(year >= 2021)

PricesBEVPassReg <- TrnServPrices %>%
  filter(sector == "trn_pasg_road_ldv_4w_pass") %>%
  filter(grepl("bev_pass", technology)) %>%
  mutate(value = value*105.377/60.055,
         Units = "$2020/pass-km") %>%
  filter(year >= 2021)

PricesLiqPassReg <- TrnServPrices %>%
  filter(sector == "trn_pasg_road_ldv_4w_pass") %>%
  filter(grepl("Liquids", technology),
         !grepl("Hybrid", technology)) %>%
  mutate(value = value*105.377/60.055,
         Units = "$2020/pass-km") %>%
  filter(year >= 2021)

PricesHybridLiqPassReg <- TrnServPrices %>%
  filter(sector == "trn_pasg_road_ldv_4w_pass") %>%
  filter(grepl("Hybrid Liquids", technology)) %>%
  mutate(value = value*105.377/60.055,
         Units = "$2020/pass-km") %>%
  filter(year >= 2021)

PricesFCEVPassReg <- TrnServPrices %>%
  filter(sector == "trn_pasg_road_ldv_4w_pass") %>%
  filter(grepl("FCEV", technology)) %>%
  mutate(value = value*105.377/60.055,
         Units = "$2020/pass-km") %>%
  filter(year >= 2021)




TotalTrnServPassReg <- getQuery(prj2, "transport service output by tech and vintage") %>%
  filter(sector == "trn_pasg_road_ldv_4w_pass") %>%
  group_by(Units, scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

TotalBEVTrnServPassReg <- getQuery(prj2, "transport service output by tech and vintage") %>%
  filter(sector == "trn_pasg_road_ldv_4w_pass") %>%
  filter(grepl("bev_pass", technology)) %>%
  group_by(Units, scenario, sector, subsector, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

TotalLiqTrnServPassReg <- getQuery(prj2, "transport service output by tech and vintage") %>%
  filter(sector == "trn_pasg_road_ldv_4w_pass") %>%
  filter(grepl("Liquids", technology),
         !grepl("Hybrid", technology)) %>%
  group_by(Units, scenario, sector, subsector, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

TotalHybridLiqTrnServPassReg <- getQuery(prj2, "transport service output by tech and vintage") %>%
  filter(sector == "trn_pasg_road_ldv_4w_pass") %>%
  filter(grepl("Hybrid Liquids", technology)) %>%
  group_by(Units, scenario, sector, subsector, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

TotalFCEVTrnServPassReg <- getQuery(prj2, "transport service output by tech and vintage") %>%
  filter(sector == "trn_pasg_road_ldv_4w_pass") %>%
  filter(grepl("FCEV", technology)) %>%
  group_by(Units, scenario, sector, subsector, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

PricesTrnServPassGlo <- PricesTrnServPassReg %>%
  left_join(TotalTrnServPassReg, by = c("scenario", "region", "year"),
                           suffix = c(".price", ".serv")) %>%
  na.omit() %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.serv, na.rm = TRUE), .groups = "drop")

PricesBEVTrnServPassGlo <- PricesBEVPassReg %>%
  left_join(TotalBEVTrnServPassReg, by = c("scenario", "sector", "subsector", "region", "year"),
            suffix = c(".price", ".serv")) %>%
  na.omit() %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.serv, na.rm = TRUE), .groups = "drop")

PricesLiqTrnServPassGlo <- PricesLiqPassReg %>%
  left_join(TotalLiqTrnServPassReg, by = c("scenario", "sector", "subsector", "region", "year"),
            suffix = c(".price", ".serv")) %>%
  na.omit()

PricesHybridLiqTrnServPassGlo <- PricesHybridLiqPassReg %>%
  left_join(TotalHybridLiqTrnServPassReg, by = c("scenario", "sector", "subsector", "region", "year"),
            suffix = c(".price", ".serv")) %>%
  na.omit()

PricesLiq_allTrnServPassGlo <- bind_rows(PricesLiqTrnServPassGlo,
                                         PricesHybridLiqTrnServPassGlo) %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.serv, na.rm = TRUE), .groups = "drop")


PricesFCEVTrnServPassGlo <- PricesFCEVPassReg %>%
  left_join(TotalFCEVTrnServPassReg, by = c("scenario", "sector", "subsector", "region", "year"),
            suffix = c(".price", ".serv")) %>%
  na.omit() %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.serv, na.rm = TRUE), .groups = "drop")


PricesTrnServPassGloRelDiff <- PricesTrnServPassGlo %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(lead_times = if_else(str_detect(scenario.diff, "shortLT"), "Short lead times", "Average lead times")) %>%
  mutate(sector = "LDV 4W")

PricesBEVTrnServPassGloRelDiff <- PricesBEVTrnServPassGlo %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(lead_times = if_else(str_detect(scenario.diff, "shortLT"), "Short lead times", "Average lead times")) %>%
  mutate(sector = "BEV")

PricesLiq_allTrnServPassGloRelDiff <- PricesLiq_allTrnServPassGlo %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(lead_times = if_else(str_detect(scenario.diff, "shortLT"), "Short lead times", "Average lead times")) %>%
  mutate(sector = "Liquids")

PricesFCEVTrnServPassGloRelDiff <- PricesFCEVTrnServPassGlo %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(lead_times = if_else(str_detect(scenario.diff, "shortLT"), "Short lead times", "Average lead times")) %>%
  mutate(sector = "FCEV")

PricesAllTrnServPassGloRelDiff <- bind_rows(PricesTrnServPassGloRelDiff,
                                            PricesBEVTrnServPassGloRelDiff,
                                            PricesLiq_allTrnServPassGloRelDiff,
                                            PricesFCEVTrnServPassGloRelDiff)

# FIGURE 5A/B: MINERAL PRICES PLOT  ------------------------------------------------------------

SCENARIO_order <- c("01272026_UnlimitSupply_BR",
                    # "01272026_UnlimitSupply_EnR",
                    "01272026_His_constrSupply_BR_noTC",
                    "01272026_His_constrSupply_EnR_noTC",
                    "03192026_His_constrSupply_highDemand",
                    # "01272026_His_constrSupply_BR_SS_noTC",
                    "01272026_His_constrSupply_BR_shortLT_noTC",
                    # "01272026_His_constrSupply_BR_SS_shortLT_noTC")
                    
                    #"01272026_His_constrSupply_EnR_SS_noTC",
                    "01272026_His_constrSupply_EnR_shortLT_noTC",
                    "03192026_His_constrSupply_shortLT_highDemand")
#"01272026_His_constrSupply_EnR_SS_shortLT_noTC")



Fig5a_alt <-
  ggplot() +
  geom_ribbon(
    data = GloConsPriceRelDiff %>%
      filter(scenario.diff %in% c(SCENARIO_ref,
                                  SCENARIO_ref_shortLT,
                                  SCENARIO_ref_hiEVD,
                                  SCENARIO_ref_EnR), scenario.diff != SCENARIO_ref_unconstrained,
             year >= 2021, year <= 2075) %>%
      group_by(year, sector) %>%
      summarise(
        ymin = min(value, na.rm = TRUE),
        ymax = max(value, na.rm = TRUE),
        .groups = "drop"
      ),
    aes(x = year, ymin = ymin, ymax = ymax,
        fill = sector, group = sector),
    alpha = 0.15
  ) +
  geom_line(data = filter(GloConsPriceRelDiff,
                          scenario.diff %in% c(SCENARIO_ref,
                                               SCENARIO_ref_shortLT,
                                               SCENARIO_ref_hiEVD,
                                               SCENARIO_ref_EnR),
                          year >= 2021 & year <= 2075),
            aes(x = year, y = value, group = interaction(sector, scenario.diff), color = sector, linetype = scenario.diff), linewidth = 1) +
  geom_hline(yintercept = 1, color = "black", linetype = 2) +
  scale_color_manual(name = "Commodity",
                     values = commodity_colors) +
  scale_linetype_manual(name = "Scenario",
                        values = SCENARIO_lines_2,
                        labels = SCENARIO_labels) +
  scale_fill_manual(name = "Commodity",
                    values = commodity_colors)+
  scale_y_continuous(limits = c(0,18))+
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "right",
    legend.key.width = unit(1, "cm")
  ) +
  labs(x = "", y = "Relative change \n (1=Unconstrained supply)", title = "Mineral commodity prices \nBaseline constrained supply scenarios") +
  guides(color = "none", 
         fill = "none",
         linetype = "none")

print(Fig5a_alt)

SCENARIO_order <- c("01272026_UnlimitSupply_BR",
                    #"01272026_His_constrSupply_BR_noTC",
                    "01272026_His_constrSupply_BR_SS_noTC",
                    #"01272026_His_constrSupply_BR_shortLT_noTC",
                    "01272026_His_constrSupply_BR_SS_shortLT_noTC",
                    #"01272026_His_constrSupply_EnR_noTC",
                    "01272026_His_constrSupply_EnR_SS_noTC",
                    #"01272026_His_constrSupply_EnR_shortLT_noTC",
                    "01272026_His_constrSupply_EnR_SS_shortLT_noTC",
                    #"03192026_His_constrSupply_highDemand",
                    "03192026_His_constrSupply_SS_highDemand",
                    #"03192026_His_constrSupply_shortLT_highDemand",
                    "03192026_His_constrSupply_SS_shortLT_highDemand")



GloConsPriceUnconstr <- getQuery(prj, "regional prices (Mt)") %>%
  filter(sector %in% c("regional copper", "regional lithium", "regional nickel")) %>%
  select(-scenario, -value) %>%
  mutate(value = case_when(sector == "regional copper" ~ 1.45,
                           sector == "regional lithium" ~ 14.44,
                           sector == "regional nickel" ~ 3.11)) %>%
  mutate(sector = gsub("regional ", "", sector)) %>%
  mutate(scenario = SCENARIO_ref_unconstrained) %>%
  mutate(value = value*125.428/27.8,
         Units = "2025$/kg") %>%
  distinct()

#Consumer price is the same across regions
# "Regional price"
# We can just query a single region to get the global price
GloConsPrice <- getQuery(prj, "regional prices (Mt)") %>%
  filter(sector %in% c("regional copper", "regional lithium", "regional nickel")) %>%
  mutate(value = value*125.428/27.8,
         Units = "2025$/kg") %>%
  filter(scenario %in% SCENARIO_order) %>%
  mutate(scenario = factor(scenario, levels = SCENARIO_order)) %>%
  mutate(sector = gsub("regional ", "", sector)) %>%
  bind_rows(GloConsPriceUnconstr)

GloConsPriceRelDiff <- GloConsPrice %>%
  filter(region == "USA") %>%
  select(-region) %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year", "sector")) %>%
  mutate(lead_times = if_else(str_detect(scenario.diff, "shortLT"), "Short lead times", "Average lead times"))



FigS9 <-
  ggplot() +
  geom_ribbon(
    data = GloConsPriceRelDiff %>%
      filter(scenario.diff %in% c(SCENARIO_SS_ref,
                                  SCENARIO_SS_ref_shortLT,
                                  SCENARIO_SS_ref_hiEVD,
                                  SCENARIO_SS_ref_EnR), scenario.diff != SCENARIO_ref_unconstrained,
             year >= 2021, year <= 2075) %>%
      group_by(year, sector) %>%
      summarise(
        ymin = min(value, na.rm = TRUE),
        ymax = max(value, na.rm = TRUE),
        .groups = "drop"
      ),
    aes(x = year, ymin = ymin, ymax = ymax,
        fill = sector, group = sector),
    alpha = 0.15
  ) +
  geom_line(data = filter(GloConsPriceRelDiff,
                          scenario.diff %in% c(SCENARIO_SS_ref,
                                               SCENARIO_SS_ref_shortLT,
                                               SCENARIO_SS_ref_hiEVD,
                                               SCENARIO_SS_ref_EnR),
                          year >= 2021 & year <= 2075),
            aes(x = year, y = value, group = interaction(sector, scenario.diff), color = sector, linetype = scenario.diff), linewidth = 1) +
  geom_hline(yintercept = 1, color = "black", linetype = 2) +
  scale_color_manual(name = "Commodity",
                     values = commodity_colors) +
  scale_linetype_manual(name = "Scenario",
                        values = SCENARIO_lines_2,
                        labels = SCENARIO_labels) +
  scale_fill_manual(name = "Commodity",
                    values = commodity_colors)+
  scale_y_continuous(limits = c(0,18))+
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "right",
    legend.key.width = unit(1, "cm")
  ) +
  labs(x = "", y = "Relative change \n (1=Unconstrained supply)", title = "Mineral commodity prices \nSteady-state constrained supply scenarios") +
  guides(fill = "none",
         color = "none",
         linetype = "none")

print(FigS9)



# FIGURE 5C ELECTRICITY PRICES PLOT ---------------------------------------


 Fig5b_alt <-
  ggplot() +
   geom_line(data = filter(PricesAllElecGloRelDiff,
                           scenario.diff %in% c(SCENARIO_ref),
                          year >= 2021 & year <= 2075,
                          sector %in% c("Geothermal",
                                        "Solar PV",
                                        "Wind",
                                        "Gas",
                                        "Coal",
                                        "Nuclear",
                                        "Electricity generation")),
            aes(x = year, y = value, group = sector, color = sector), linewidth = 1) +
  geom_hline(yintercept = 1, color = "black", linetype = 2) +
  scale_color_manual(name = "Technology",
                     values = elec_tech_colors) +
  scale_y_continuous(limits = c(NA,2))+
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "none",
    legend.key.width = unit(1, "cm")
  ) +
  labs(x = "", y = "Relative change \n (1=Unconstrained supply)", title = "Electricity generation prices") 

print(Fig5b_alt)

# FIGURE 5D: TRANSPORT PRICES PLOT ---------------------------------------


Fig5c_alt <-
  ggplot() +
  geom_line(data = filter(PricesAllTrnServPassGloRelDiff ,
                          scenario.diff %in% c(SCENARIO_ref),
                          #SCENARIO_ref_shortLT,
                          #SCENARIO_ref_hiEVD,
                          #SCENARIO_ref_EnR),
                          year >= 2021 & year <= 2075),
            aes(x = year, y = value, group = sector, color = sector), linewidth = 1) +
  # geom_line(data = filter(PricesTrnServPassGloRelDiff,
  #                         scenario.diff %in% c(SCENARIO_ref),
  #                         #SCENARIO_ref_shortLT,
  #                         #SCENARIO_ref_hiEVD,
  #                         #SCENARIO_ref_EnR),
  #                         year >= 2021 & year <= 2075),
  #           aes(x = year, y = value, group = interaction(sector, scenario.diff), color = sector, linetype = scenario.diff), linewidth = 1) +
  geom_hline(yintercept = 1, color = "black", linetype = 2) +
  scale_color_manual(name = "Commodity",
                     values = commodity_colors) +
  scale_y_continuous(limits = c(NA,2))+
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "none",
    legend.key.width = unit(1, "cm")
  ) +
  labs(x = "", y = "Relative change \n (1=Unconstrained supply)", title = "4W LDV prices") 

print(Fig5c_alt)


# FIGURE 5E: ELECTRICITY GENERATION DIFF ----------------------------------


ElecGenSubsector <- getQuery(prj2, "elec gen by gen tech") %>%
  left_join(elec_tech_map, by = c("technology")) %>%
  group_by(Units, scenario, technology0, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

TotalElecGen <- ElecGenSubsector %>%
  group_by(Units, scenario, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup() %>%
  mutate(technology0 = "Electricity generation")

RelElecGenSubsectorDiff <- rel_diff_from_scen(bind_rows(ElecGenSubsector, TotalElecGen),
                                              ref_scenario = SCENARIO_ref_unconstrained,
                                              diff_scenarios = c(SCENARIO_ref),
                                              join_var = c("Units", "technology0", "year"))

diff_ElecGenSubsector_1 <- diff_from_scen(ElecGenSubsector,
                                          ref_scenario = SCENARIO_ref_unconstrained,
                                          diff_scenarios = c(SCENARIO_ref, SCENARIO_ref_shortLT),
                                          join_var = c("Units", "technology0", "year"))

diff_ElecGenSubsector_2 <- diff_from_scen(ElecGenSubsector,
                                          ref_scenario = SCENARIO_EnR_unconstrained,
                                          diff_scenarios = SCENARIOS_EnR,
                                          join_var = c("Units", "technology0", "year"))

diff_ElecGenSubsector_3 <- diff_from_scen(ElecGenSubsector,
                                          ref_scenario = SCENARIO_hiEVD_unconstrained,
                                          diff_scenarios = SCENARIOS_hiEVD,
                                          join_var = c("Units", "technology0", "year"))

diff_ElecGenSubsector <- bind_rows(diff_ElecGenSubsector_1,
                                   diff_ElecGenSubsector_2,
                                   diff_ElecGenSubsector_3)


Fig5d <-
  ggplot() +
  geom_line(data = filter(RelElecGenSubsectorDiff,
                          scenario.diff %in% c(SCENARIO_ref),
                          year >= 2021 & year <= 2075,
                          technology0 %in% c("Geothermal",
                                        "Solar PV",
                                        "Wind",
                                        "Gas",
                                        "Coal",
                                        "Nuclear",
                                        "Electricity generation")),
            aes(x = year, y = value, group = technology0, color = technology0), linewidth = 1) +
 geom_hline(yintercept = 1, color = "black", linetype = 2) +
  scale_color_manual(name = "Technology",
                     values = elec_tech_colors) +
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "none",
    legend.key.width = unit(1, "cm")
  ) +
  labs(x = "", y = "Relative change \n (1=Unconstrained supply)", title = "Electricity generation") 

print(Fig5d)





# FIGURE 5F: TRANSPORT SERVICE DIFF ----------------------------------

TrnServTech4W <- getQuery(prj2, "transport service output by tech and vintage") %>%
  separate(technology, into = c("technology", "vintage"), sep = ",year=") %>%
  left_join(trn_tech_map, by = c("sector", "subsector", "technology")) %>%
  group_by(Units, scenario, subsector_agg, technology_agg, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021, subsector_agg == "4-Wheel LDV") %>%
  ungroup()

TotalServ4W <- TrnServTech4W %>%
  group_by(Units, scenario, subsector_agg, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(technology_agg = "LDV 4W")

RelTrnServTechDiff <- rel_diff_from_scen(bind_rows(TrnServTech4W, TotalServ4W),
                                              ref_scenario = SCENARIO_ref_unconstrained,
                                              diff_scenarios = c(SCENARIO_ref),
                                              join_var = c("Units",  "subsector_agg", "technology_agg", "year"))



Fig5e <-
  ggplot() +
  geom_line(data = filter(RelTrnServTechDiff,
                          scenario.diff %in% c(SCENARIO_ref),
                          year >= 2021 & year <= 2075),
            aes(x = year, y = value, group = technology_agg, color = technology_agg), linewidth = 1) +
  geom_hline(yintercept = 1, color = "black", linetype = 2) +
  scale_color_manual(name = "Fuel",
                     values = commodity_colors) +
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "none",
    legend.key.width = unit(1, "cm")
  ) +
  labs(x = "", y = "Relative change \n (1=Unconstrained supply)", title = "4W LDV service output") 


print(Fig5e)





# FIGURE 5 COMBINED -------------------------------------------------------

row1 <- (Fig5a_alt + plot_spacer() + FigS9) +
  plot_layout(widths = c(1, 0.5, 1))
row2 <- (Fig5b_alt + plot_spacer() + Fig5c_alt) +
  plot_layout(widths = c(1, 0.5, 1))
row3 <- Fig5d + plot_spacer() + Fig5e +
  plot_layout(widths = c(1, 0.5, 1))

Fig5alt <- (row1 / plot_spacer() / row2 / row3) + 
  plot_layout(heights = c(1,0.2,1,1))+
  plot_annotation(tag_levels = "A",
                  theme = theme(
                    plot.title = element_text(size = 20)
                  ))+
  theme(
    legend.position = "right"
  )

print(Fig5alt)

ggsave(paste0(PLOT_FOLDER,"Fig5.png", sep = ""),width=15, height=17, units="in")

# SUPPLEMENTARY RESULTS FIGURES ----------------------------------- ---------------------------------------------------



# FIGURES S1-S2: LITERATURE COMPARISON FIGURES ----------------------------

source("literature_comparison_figures.R")
# FIGURE S3: REGIONAL MINERAL PRODUCTION ----------------------------------


res_prod_tech_vintage <- getQuery(prj, "resource production by tech and vintage") %>%
  filter(resource %in% c("copper", "lithium", "nickel")) %>%
  separate(technology, into = c("technology", "vintage"), sep = ",year=")


reg_res_prod_tech <- res_prod_tech_vintage %>%
  separate(technology, into = c(NA, "technology"), sep = "_") %>%
  group_by(Units, scenario, region, resource, technology, year) %>%
  dplyr::summarise(value = sum(value), .groups = "drop") 

reg_prod_tech <- reg_res_prod_tech %>%
  group_by(Units, scenario, region, resource, year) %>%
  dplyr::summarise(value = sum(value))

reg_prod_raw <- reg_prod_tech %>%
  group_by(Units, scenario, resource, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(region = factor(region, levels = region_levels_reg32))


# Raw 32 regions
p0 <-
  ggplot() +
  #geom_line(data = filter(global_mineral_prod, year <= 2050),
  #          aes(x = year, y = value, group = scenario)) +
  geom_bar(data = filter(reg_prod_raw, year >= 2020 & year <= 2075,
                         scenario %in% SCENARIO_ref),
           aes(x = year, y = value, group = region, fill = region),
           stat = "identity", position = position_stack(reverse = FALSE)) +
  facet_wrap(~resource, scales = "free", nrow = 1) +
  scale_fill_manual(values = region_colors_reg32) +
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "right",
    legend.key.width = unit(1, "cm")
  ) +
  labs(y = "Mt/yr", x = "", title = "Mineral production by region")+
  ggsave(paste0(PLOT_FOLDER,"FigS3.png", sep = ""),width=15, height=4, units="in")


# FIGURE S4 (FIGURE 2, with enhanced recycling)----------------------------------------------------------------

SCENARIO_order <- c( "01272026_UnlimitSupply_EnR",
                     "01272026_His_constrSupply_EnR_noTC",
                     "01272026_His_constrSupply_EnR_shortLT_noTC")

SCENARIO_ref <- c("01272026_His_constrSupply_EnR_noTC")
SCENARIO_ref_shortLT <- c("01272026_His_constrSupply_EnR_shortLT_noTC")
SCENARIO_ref_unconstrained <- c("01272026_UnlimitSupply_EnR")

# FIGURE S4A: GLOBAL MINERAL PRODUCTION (LINES) - UNCONSTRAINED AND CONSTRAINED --------

mineral_inputs_tech <- getQuery(prj, "minerals inputs by tech") %>%
  filter(input %!in% c("traded copper", "traded nickel", "traded lithium", "traded iron and steel", "iron and steel"),
         sector %!in% c("traded copper", "traded nickel", "traded lithium", "traded iron and steel", "iron and steel"))

global_sector_demand <- mineral_inputs_tech %>%
  left_join(mineral_demand_tech_mapping, by = c("sector", "subsector", "technology")) %>%
  group_by(Units, scenario, sector0, year, input) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year <= 2100) %>%
  rename(sector = sector0) %>%
  mutate(sector = factor(sector, levels = sector_levels))

global_total_demand <- global_sector_demand %>%
  group_by(scenario, input, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(scenario = factor(scenario, levels = SCENARIO_order)) %>%
  mutate(input = gsub("regional ", "", input)) %>%
  rename(resource = input) %>%
  filter(resource %in% c("copper", "lithium", "nickel"))

figS4a <-
  ggplot() +
  geom_line(data = filter(global_total_demand, year >= 2021, year <= 2075,
                          scenario %in% SCENARIO_order),
            aes(x = year, y = value, group = scenario, color = scenario, linetype = scenario), linewidth = 1) +
  theme_bw() +
  facet_grid(resource~., scales = "free_y")+
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "right",
    legend.key.width = unit(1.5, "cm")
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  scale_y_continuous(limits = c(0, NA))+
  scale_color_manual(labels = SCENARIO_labels, name = "Scenario", values = SCENARIO_colors)+
   scale_linetype_manual(labels = SCENARIO_labels, name = "Scenario", values = SCENARIO_lines)+
  labs(y = "Mt/yr", x = "")

print(figS4a)


# FIGURE S4B: GLOBAL MINERAL PRODUCTION BY STAGE (REFERENCE, SHORT LEAD TIMES) --------------------------------------------------------------------

# calculate supply-demand gap
global_total_demand_unconstrained <- global_total_demand %>%
  filter(scenario == SCENARIO_ref_unconstrained)

global_total_demand_diff <- global_total_demand %>%
  filter(scenario %in% SCENARIO_constrained,
         scenario %in% SCENARIO_order) %>%
  left_join(global_total_demand_unconstrained, by = c("resource", "year"),
            suffix = c(".constr", ".unconstr")) %>%
  mutate(value = value.unconstr - value.constr,
         Units = "Mt", 
         stage = "Production reduction from unconstrained supply") %>%
  rename(scenario = scenario.constr) %>%
  select(Units, scenario, resource, stage, year, value)


res_prod_tech_vintage <- getQuery(prj, "resource production by tech and vintage") %>%
  filter(resource %in% c("copper", "lithium", "nickel")) %>%
  separate(technology, into = c("technology", "vintage"), sep = ",year=")

# get mine stage data
all_years <- sort(unique(res_prod_tech_vintage$year))

global_res_prod_tech <- res_prod_tech_vintage %>%
  separate(technology, into = c(NA, "technology"), sep = "_") %>%
  group_by(Units, scenario, resource, technology, year) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  group_by(Units, scenario, resource, technology) %>%
  tidyr::complete(year = all_years, fill = list(value = 0)) %>%
  group_by(Units, scenario, resource, technology) %>%
  Fill_annual()

global_res_prod_tech_stages <- global_res_prod_tech %>%
  mutate(technology = as.numeric(technology)) %>%
  left_join(stages_mapping, by = c("resource", "technology")) %>%
  mutate(stage = if_else(scenario %in% SCENARIOS_shortLT, stage_shortLT, stage_average)) %>%
  group_by(Units, scenario, resource, stage, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup() %>%
  # add the supply demand gap 
  bind_rows(global_total_demand_diff) %>%
  mutate(stage = factor(stage, levels = c(mine_stages, "Production reduction from unconstrained supply")))



figS4b <-
  ggplot() +
  geom_area(
    data = filter(global_res_prod_tech_stages, scenario %in% SCENARIO_order,
                  year >= 2021, year <= 2075),
    aes(x = year, y = value, group = stage, fill = stage), alpha = 0.7,
    stat = "identity", position = position_stack(reverse = TRUE),
  ) +
  geom_line(
    data = filter(global_total_demand, scenario %in% c(SCENARIO_ref, SCENARIO_ref_shortLT),
                  year >= 2021, year <= 2075),
    aes(x = year, y = value, group = scenario, color = scenario, linetype = scenario), linewidth = 1
  ) +
  facet_grid(
    resource ~ scenario,
    scales = "free_y",
    labeller = labeller(
      scenario = function(x) stringr::str_wrap(
        as_labeller(SCENARIO_labels)(x),
        width = 12
      )
    )
  )+
  scale_fill_manual(
    values = mine_stage_gap_colors,
    name = "Reserve/resource stage in 2021"
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  scale_linetype_manual(values = SCENARIO_lines,
                        labels = SCENARIO_labels)+
  scale_color_manual(values = SCENARIO_colors,
                     labels = SCENARIO_labels)+
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "right",
    legend.key.width = unit(1.5, "cm")
  ) +
  labs(y = "Mt/yr", x = "") + 
  guides(linetype = "none",
         color = "none")

print(figS4b)

# FIGURE S4 combined -------------------------------------------------------


FigS4 <- (figS4a | figS4b) +
  plot_layout(widths = c(1, 2),
              guides = "collect")+ 
  plot_annotation(tag_levels = "A")+
  theme(
    legend.position = "right"
  )


print(FigS4)

ggsave(paste0(PLOT_FOLDER,"FigS4.png", sep = ""),width=15, height=7, units="in")




# FIGURE S5 (FIGURE 2, with high EV demand)----------------------------------------------------------------

SCENARIO_order <- c("03192026_UnlimitSupply_highDemand",
                    "03192026_His_constrSupply_highDemand",
                    "03192026_His_constrSupply_shortLT_highDemand")

SCENARIO_ref <- c("03192026_His_constrSupply_highDemand")
SCENARIO_ref_shortLT <- c("03192026_His_constrSupply_shortLT_highDemand")
SCENARIO_ref_unconstrained <- c("03192026_UnlimitSupply_highDemand")

# FIGURE S5A: GLOBAL MINERAL PRODUCTION (LINES) - UNCONSTRAINED AND CONSTRAINED --------

mineral_inputs_tech <- getQuery(prj, "minerals inputs by tech") %>%
  filter(input %!in% c("traded copper", "traded nickel", "traded lithium", "traded iron and steel", "iron and steel"),
         sector %!in% c("traded copper", "traded nickel", "traded lithium", "traded iron and steel", "iron and steel"))

global_sector_demand <- mineral_inputs_tech %>%
  left_join(mineral_demand_tech_mapping, by = c("sector", "subsector", "technology")) %>%
  group_by(Units, scenario, sector0, year, input) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year <= 2100) %>%
  rename(sector = sector0) %>%
  mutate(sector = factor(sector, levels = sector_levels))

global_total_demand <- global_sector_demand %>%
  group_by(scenario, input, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(scenario = factor(scenario, levels = SCENARIO_order)) %>%
  mutate(input = gsub("regional ", "", input)) %>%
  rename(resource = input) %>%
  filter(resource %in% c("copper", "lithium", "nickel"))

figS5a <-
  ggplot() +
  geom_line(data = filter(global_total_demand, year >= 2021, year <= 2075,
                          scenario %in% SCENARIO_order),
            aes(x = year, y = value, group = scenario, color = scenario, linetype = scenario), linewidth = 1) +
  theme_bw() +
  facet_grid(resource~., scales = "free_y")+
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "right",
    legend.key.width = unit(1.5, "cm")
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  scale_y_continuous(limits = c(0, NA))+
  scale_color_manual(labels = SCENARIO_labels, name = "Scenario", values = SCENARIO_colors)+
  scale_linetype_manual(labels = SCENARIO_labels, name = "Scenario", values = SCENARIO_lines)+
  labs(y = "Mt/yr", x = "")

print(figS5a)


# FIGURE S5B: GLOBAL MINERAL PRODUCTION BY STAGE (REFERENCE, SHORT LEAD TIMES) --------------------------------------------------------------------

# calculate supply-demand gap
global_total_demand_unconstrained <- global_total_demand %>%
  filter(scenario == SCENARIO_ref_unconstrained)

global_total_demand_diff <- global_total_demand %>%
  filter(scenario %in% SCENARIO_constrained,
         scenario %in% SCENARIO_order) %>%
  left_join(global_total_demand_unconstrained, by = c("resource", "year"),
            suffix = c(".constr", ".unconstr")) %>%
  mutate(value = value.unconstr - value.constr,
         Units = "Mt", 
         stage = "Production reduction from unconstrained supply") %>%
  rename(scenario = scenario.constr) %>%
  select(Units, scenario, resource, stage, year, value)


res_prod_tech_vintage <- getQuery(prj, "resource production by tech and vintage") %>%
  filter(resource %in% c("copper", "lithium", "nickel")) %>%
  separate(technology, into = c("technology", "vintage"), sep = ",year=")

# get mine stage data
all_years <- sort(unique(res_prod_tech_vintage$year))

global_res_prod_tech <- res_prod_tech_vintage %>%
  separate(technology, into = c(NA, "technology"), sep = "_") %>%
  group_by(Units, scenario, resource, technology, year) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  group_by(Units, scenario, resource, technology) %>%
  tidyr::complete(year = all_years, fill = list(value = 0)) %>%
  group_by(Units, scenario, resource, technology) %>%
  Fill_annual()

global_res_prod_tech_stages <- global_res_prod_tech %>%
  mutate(technology = as.numeric(technology)) %>%
  left_join(stages_mapping, by = c("resource", "technology")) %>%
  mutate(stage = if_else(scenario %in% SCENARIOS_shortLT, stage_shortLT, stage_average)) %>%
  group_by(Units, scenario, resource, stage, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup() %>%
  # add the supply demand gap 
  bind_rows(global_total_demand_diff) %>%
  mutate(stage = factor(stage, levels = c(mine_stages, "Production reduction from unconstrained supply")))



figS5b <-
  ggplot() +
  geom_area(
    data = filter(global_res_prod_tech_stages, scenario %in% SCENARIO_order,
                  year >= 2021, year <= 2075),
    aes(x = year, y = value, group = stage, fill = stage), alpha = 0.7,
    stat = "identity", position = position_stack(reverse = TRUE),
  ) +
  geom_line(
    data = filter(global_total_demand, scenario %in% c(SCENARIO_ref, SCENARIO_ref_shortLT),
                  year >= 2021, year <= 2075),
    aes(x = year, y = value, group = scenario, color = scenario, linetype = scenario), linewidth = 1
  ) +
  facet_grid(
    resource ~ scenario,
    scales = "free_y",
    labeller = labeller(
      scenario = function(x) stringr::str_wrap(
        as_labeller(SCENARIO_labels)(x),
        width = 12
      )
    )
  )+
  scale_fill_manual(
    values = mine_stage_gap_colors,
    name = "Reserve/resource stage in 2021"
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  scale_linetype_manual(values = SCENARIO_lines,
                        labels = SCENARIO_labels)+
  scale_color_manual(values = SCENARIO_colors,
                     labels = SCENARIO_labels)+
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "right",
    legend.key.width = unit(1.5, "cm")
  ) +
  labs(y = "Mt/yr", x = "") + 
  guides(linetype = "none",
         color = "none")

print(figS5b)

# FIGURE S5 combined -------------------------------------------------------


FigS5 <- (figS5a | figS5b) +
  plot_layout(widths = c(1, 2),
              guides = "collect")+ 
  plot_annotation(tag_levels = "A")+
  theme(
    legend.position = "right"
  )


print(FigS5)

ggsave(paste0(PLOT_FOLDER,"FigS5.png", sep = ""),width=15, height=7, units="in")




# FIGURE S6 (FIGURE 3, with enhanced recycling) ----------------------------------------------------------------
SCENARIO_order <- c(#"01272026_UnlimitSupply_BR",
  "01272026_UnlimitSupply_EnR",
  #"01272026_His_constrSupply_BR_noTC",
  # "01272026_His_constrSupply_BR_SS_noTC",
  #"01272026_His_constrSupply_BR_shortLT_noTC")
  # "01272026_His_constrSupply_BR_SS_shortLT_noTC")
  "01272026_His_constrSupply_EnR_noTC",
  #"01272026_His_constrSupply_EnR_SS_noTC",
  "01272026_His_constrSupply_EnR_shortLT_noTC")
#"01272026_His_constrSupply_EnR_SS_shortLT_noTC")

SCENARIO_ref <- c("01272026_His_constrSupply_EnR_noTC")
SCENARIO_ref_shortLT <- c("01272026_His_constrSupply_EnR_shortLT_noTC")
SCENARIO_ref_unconstrained <- c("01272026_UnlimitSupply_EnR")
# FIGURE S6B: SECTORAL DEMAND (UNCONSTRAINED) ---------------------------------

global_sector_demand_area <- global_sector_demand %>%
  group_by(Units, scenario, sector, input) %>%
  Fill_annual() %>%
  mutate(input = gsub("regional ", "", input)) %>%
  filter(input %in% c("copper", "lithium", "nickel")) %>%
  rename(resource = input)

global_total_demand <- global_sector_demand %>%
  group_by(scenario, input, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(scenario = factor(scenario, levels = SCENARIO_order)) %>%
  mutate(input = gsub("regional ", "", input)) %>%
  rename(resource = input) %>%
  filter(resource %in% c("copper", "lithium", "nickel"))


figS6b <-
  ggplot() +
  geom_area(
    data = filter(global_sector_demand_area, scenario == SCENARIO_ref_unconstrained,
                  year >= 2021, year <= 2075),
    aes(x = year, y = value, group = sector, fill = sector), alpha = 0.7,
    stat = "identity", position = position_stack(reverse = TRUE),
  ) +
  geom_line(
    data = filter(global_total_demand, scenario == SCENARIO_ref_unconstrained,
                  year >= 2021, year <= 2075),
    aes(x = year, y = value, group = scenario, color = scenario, linetype = scenario), linewidth = 1
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  facet_grid(
    resource ~ scenario,
    scales = "free_y",
    labeller = labeller(
      scenario = function(x) stringr::str_wrap(
        as_labeller(SCENARIO_labels)(x),
        width = 12
      )
    )
  )+
  scale_fill_manual(
    values = sector_colors,
    name = "End-use sector"
  ) +
  scale_linetype_manual(values = SCENARIO_lines,
                        labels = SCENARIO_labels)+
  scale_color_manual(values = SCENARIO_colors,
                     labels = SCENARIO_labels,
                     name = "Scenario")+
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "right",
    legend.key.width = unit(1.5, "cm")
  ) +
  labs(y = "Mt/yr", x = "") +
  guides(color = "none",
         linetype = "none")

print(figS6b)


# FIGURE S6C: SECTORAL DEMAND (DIFF FROM UNCONSTRAINED SUPPLY) -------------

#   
# global_total_demand_area <- global_total_demand %>%
#     mutate(sector = "Total") %>%
#     group_by(scenario, sector, resource) %>%
#     Fill_annual()

global_sector_demand_area_diff <- diff_from_scen(global_sector_demand_area,
                                                 diff_scenarios = SCENARIO_order,
                                                 ref_scenario = SCENARIO_ref_unconstrained,
                                                 c("sector", "resource", "year"))  %>%
  rename(Units = Units.diff,
         scenario = scenario.diff) %>%
  # mutate(value = value*-1) %>%
  ungroup() %>%
  select(Units, scenario, sector, resource, year, value) 
# bind with total
# bind_rows(global_total_demand_area) %>%
#  mutate(sector = factor(sector, levels = c(sector_levels, "Total")))

figS6c <-
  ggplot() +
  geom_area(
    data = filter(global_sector_demand_area_diff, scenario %in% c(SCENARIO_ref, SCENARIO_ref_shortLT),
                  year >= 2021, year <= 2075),
    aes(x = year, y = value, group = sector, fill = sector), alpha = 0.7,
    stat = "identity", position = position_stack(reverse = FALSE),
  ) +
  # geom_line(
  #   data = filter(global_total_demand, scenario %in% c(SCENARIO_ref, SCENARIO_ref_shortLT),
  #                 year >= 2021, year <= 2075),
  #   aes(x = year, y = value, group = scenario, color = scenario), linewidth = 1.5
  # ) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  facet_grid(
    resource ~ scenario,
    scales = "free_y",
    labeller = labeller(
      scenario = function(x) stringr::str_wrap(
        as_labeller(SCENARIO_labels)(x),
        width = 12
      )
    )
  )+
  scale_fill_manual(
    values = sector_colors,
    name = "End-use sector"
  ) +
  # scale_linetype_manual(values = SCENARIO_lines,
  #                        labels = SCENARIO_labels)+
  # scale_color_manual(values = SCENARIO_colors,
  #                    labels = SCENARIO_labels,
  #                    name = "Scenario")+
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "right",
    legend.key.width = unit(1.5, "cm")
  ) +
  labs(y = "Mt/yr", x = "") 

print(figS6c)


# FIGURE S6 COMBINED -------------------------------------------------------
#3a is the same as 2a
FigS6 <- (figS4a | figS6b | plot_spacer() | figS6c) + 
  plot_layout(
    widths = c(1,1,0.2,2),
    guides = "collect"
  )+
  plot_annotation(tag_levels = "A")+
  theme(
    legend.position = "right"
  )

print(FigS6)

ggsave(paste0(PLOT_FOLDER,"FigS6.png", sep = ""),width=16, height=7, units="in")









# FIGURE S7 (FIGURE 3, with high EV demand) ----------------------------------------------------------------

SCENARIO_order <- c("03192026_UnlimitSupply_highDemand",
                    "03192026_His_constrSupply_highDemand",
                    "03192026_His_constrSupply_shortLT_highDemand")

SCENARIO_ref <- c("03192026_His_constrSupply_highDemand")
SCENARIO_ref_shortLT <- c("03192026_His_constrSupply_shortLT_highDemand")
SCENARIO_ref_unconstrained <- c("03192026_UnlimitSupply_highDemand")


# FIGURE S7B: SECTORAL DEMAND (UNCONSTRAINED) ---------------------------------

global_sector_demand_area <- global_sector_demand %>%
  group_by(Units, scenario, sector, input) %>%
  Fill_annual() %>%
  mutate(input = gsub("regional ", "", input)) %>%
  filter(input %in% c("copper", "lithium", "nickel")) %>%
  rename(resource = input)

global_total_demand <- global_sector_demand %>%
  group_by(scenario, input, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(scenario = factor(scenario, levels = SCENARIO_order)) %>%
  mutate(input = gsub("regional ", "", input)) %>%
  rename(resource = input) %>%
  filter(resource %in% c("copper", "lithium", "nickel"))


figS7b <-
  ggplot() +
  geom_area(
    data = filter(global_sector_demand_area, scenario == SCENARIO_ref_unconstrained,
                  year >= 2021, year <= 2075),
    aes(x = year, y = value, group = sector, fill = sector), alpha = 0.7,
    stat = "identity", position = position_stack(reverse = TRUE),
  ) +
  geom_line(
    data = filter(global_total_demand, scenario == SCENARIO_ref_unconstrained,
                  year >= 2021, year <= 2075),
    aes(x = year, y = value, group = scenario, color = scenario, linetype = scenario), linewidth = 1
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  facet_grid(
    resource ~ scenario,
    scales = "free_y",
    labeller = labeller(
      scenario = function(x) stringr::str_wrap(
        as_labeller(SCENARIO_labels)(x),
        width = 12
      )
    )
  )+
  scale_fill_manual(
    values = sector_colors,
    name = "End-use sector"
  ) +
  scale_linetype_manual(values = SCENARIO_lines,
                        labels = SCENARIO_labels)+
  scale_color_manual(values = SCENARIO_colors,
                     labels = SCENARIO_labels,
                     name = "Scenario")+
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "right",
    legend.key.width = unit(1.5, "cm")
  ) +
  labs(y = "Mt/yr", x = "") +
  guides(color = "none",
         linetype = "none")

print(figS7b)


# FIGURE S7C: SECTORAL DEMAND (DIFF FROM UNCONSTRAINED SUPPLY) -------------

#   
# global_total_demand_area <- global_total_demand %>%
#     mutate(sector = "Total") %>%
#     group_by(scenario, sector, resource) %>%
#     Fill_annual()

global_sector_demand_area_diff <- diff_from_scen(global_sector_demand_area,
                                                 diff_scenarios = SCENARIO_order,
                                                 ref_scenario = SCENARIO_ref_unconstrained,
                                                 c("sector", "resource", "year"))  %>%
  rename(Units = Units.diff,
         scenario = scenario.diff) %>%
  # mutate(value = value*-1) %>%
  ungroup() %>%
  select(Units, scenario, sector, resource, year, value) 
# bind with total
# bind_rows(global_total_demand_area) %>%
#  mutate(sector = factor(sector, levels = c(sector_levels, "Total")))

figS7c <-
  ggplot() +
  geom_area(
    data = filter(global_sector_demand_area_diff, scenario %in% c(SCENARIO_ref, SCENARIO_ref_shortLT),
                  year >= 2021, year <= 2075),
    aes(x = year, y = value, group = sector, fill = sector), alpha = 0.7,
    stat = "identity", position = position_stack(reverse = FALSE),
  ) +
  # geom_line(
  #   data = filter(global_total_demand, scenario %in% c(SCENARIO_ref, SCENARIO_ref_shortLT),
  #                 year >= 2021, year <= 2075),
  #   aes(x = year, y = value, group = scenario, color = scenario), linewidth = 1.5
  # ) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  facet_grid(
    resource ~ scenario,
    scales = "free_y",
    labeller = labeller(
      scenario = function(x) stringr::str_wrap(
        as_labeller(SCENARIO_labels)(x),
        width = 12
      )
    )
  )+
  scale_fill_manual(
    values = sector_colors,
    name = "End-use sector"
  ) +
  # scale_linetype_manual(values = SCENARIO_lines,
  #                        labels = SCENARIO_labels)+
  # scale_color_manual(values = SCENARIO_colors,
  #                    labels = SCENARIO_labels,
  #                    name = "Scenario")+
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "right",
    legend.key.width = unit(1.5, "cm")
  ) +
  labs(y = "Mt/yr", x = "") 

print(figS7c)


# FIGURE S7 COMBINED -------------------------------------------------------
#3a is the same as 2a
FigS7 <- (figS5a | figS7b | plot_spacer() | figS7c) + 
  plot_layout(
    widths = c(1,1,0.2,2),
    guides = "collect"
  )+
  plot_annotation(tag_levels = "A")+
  theme(
    legend.position = "right"
  )

print(FigS7)

ggsave(paste0(PLOT_FOLDER,"FigS7.png", sep = ""),width=16, height=7, units="in")









# FIGURE S8A: SHORT LT GLOBAL MINERAL PRODUCTION (LINES/RIBBON) - UNCONSTRAINED AND CONSTRAINED --------
SCENARIO_order <- c(
  "01272026_His_constrSupply_BR_shortLT_noTC",
  "01272026_His_constrSupply_EnR_shortLT_noTC",
  "03122026_His_constrSupply_shortLT_highDemand",
  "01272026_His_constrSupply_BR_SS_shortLT_noTC",
  "01272026_His_constrSupply_EnR_SS_shortLT_noTC",
  "03122026_His_constrSupply_SS_shortLT_highDemand")

mineral_inputs_tech <- getQuery(prj, "minerals inputs by tech") %>%
  filter(input %!in% c("traded copper", "traded nickel", "traded lithium", "traded iron and steel", "iron and steel"),
         sector %!in% c("traded copper", "traded nickel", "traded lithium", "traded iron and steel", "iron and steel"))

global_sector_demand <- mineral_inputs_tech %>%
  left_join(mineral_demand_tech_mapping, by = c("sector", "subsector", "technology")) %>%
  group_by(Units, scenario, sector0, year, input) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year <= 2100) %>%
  rename(sector = sector0) %>%
  mutate(sector = factor(sector, levels = sector_levels))

global_total_demand <- global_sector_demand %>%
  group_by(scenario, input, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(scenario = factor(scenario, levels = SCENARIO_order)) %>%
  mutate(input = gsub("regional ", "", input)) %>%
  rename(resource = input) %>%
  filter(resource %in% c("copper", "lithium", "nickel")) %>%
  mutate(lead_times = case_when(str_detect(scenario, "shortLT") ~ "Short lead times",
                                str_detect(scenario, "Unlimit") ~ "Unconstrained supply",
                                TRUE ~ "Average lead times")) %>%
  mutate(supply_factor = if_else(str_detect(scenario, "SS"),"Steady-state constrained supply","Baseline constrained supply")) %>%
  mutate(supply_lead_combined = paste(lead_times, supply_factor))




figS8a <-
  ggplot() +
  geom_ribbon(
    data = global_total_demand %>%
      filter(scenario %in% SCENARIO_order,
             year >= 2021, year <= 2075) %>%
      group_by(year, resource, supply_lead_combined) %>%
      summarise(
        ymin = min(value, na.rm = TRUE),
        ymax = max(value, na.rm = TRUE),
        .groups = "drop"
      ),
    aes(x = year, ymin = ymin, ymax = ymax,
        fill = supply_lead_combined, group = supply_lead_combined),
    alpha = 0.15
  ) +
  geom_line(data = filter(global_total_demand, year >= 2021, year <= 2075,
                          scenario %in% SCENARIO_order),
            aes(x = year, y = value, group = scenario, color = scenario, linetype = scenario)) +
  theme_bw() +
  facet_grid(resource~., scales = "free_y")+
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "right",
    legend.key.width = unit(1.5, "cm")
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  scale_y_continuous(limits = c(0, NA))+
  scale_color_manual(labels = SCENARIO_labels, name = "Scenario", values = SCENARIO_colors)+
  scale_linetype_manual(labels = SCENARIO_labels, name = "Scenario", values = SCENARIO_lines)+
  scale_fill_manual(values = supply_lead_combined_colors, name = "Scenario", labels = c("Short lead times Baseline constrained supply" = "Baseline constrained supply scenarios",
                                                                                        "Short lead times Steady-state constrained supply" = "Steady-state constrained supply scenarios"))+
  labs(y = "Mt/yr", x = "")

print(figS8a)

# FIGURE S8B SHORT LT  ----------------------------------------------------------------

# query gives available by grade, let's add up all
res_avail <- getQuery(prj, "resource supply curves") %>%
  filter(resource %in% c("copper", "lithium", "nickel")) %>%
  group_by(Units, scenario, region, resource, subresource, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup()

#combine across subresources
res_avail_total <- res_avail %>%
  separate(subresource, into = c(NA, "vintage"), sep = "_") %>%
  filter(vintage == 2021 | year >= vintage) %>%
  group_by(Units, scenario, region, resource, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup()

#combine across regions
global_res_avail_total <- res_avail_total %>%
  group_by(Units, scenario, resource, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(supply_factor = if_else(str_detect(scenario, "SS"),"Steady-state constrained supply","Baseline constrained supply")) %>%
  mutate(demand_factor = gsub("_SS", "", scenario)) %>%
  mutate(lead_times = if_else(str_detect(scenario, "shortLT"), "Short lead times", "Average lead times")) %>%
  mutate(scenario = "Maximum production potential") %>%
  mutate(supply_lead_combined = paste(lead_times, supply_factor))


res_cum_prod <- getQuery(prj, "resource cumulative production") %>%
  filter(resource %in% c("copper", "lithium", "nickel"))


global_total_cum_res_prod <- res_cum_prod %>%
  group_by(Units, scenario, resource, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup() %>%
  group_by(Units, scenario, resource) %>%
  Fill_annual() %>%
  mutate(supply_factor = if_else(str_detect(scenario, "SS"),"Steady-state constrained supply","Baseline constrained supply")) %>%
  mutate(demand_factor = gsub("_SS", "", scenario)) %>%
  mutate(lead_times = if_else(str_detect(scenario, "shortLT"), "Short lead times", "Average lead times")) %>%
  mutate(demand_factor = if_else(demand_factor == "03122026_His_constrSupply_highDemand", "03072026_His_constrSupply_highDemand", demand_factor)) %>%
  mutate(supply_lead_combined = paste(lead_times, supply_factor))



# FIGURE S8B: SHORT LT CUMULATIVE PRODUCTION (AVERAGE LEAD TIMES CASES) ------------------------------------------------------------------

FigS8b <-
  ggplot() +
  geom_ribbon(
    data = global_total_cum_res_prod %>%
      filter(scenario %in% SCENARIO_order,
             lead_times == "Short lead times",
             year >= 2021, year <= 2075) %>%
      group_by(year, resource, supply_lead_combined) %>%
      summarise(
        ymin = min(value, na.rm = TRUE),
        ymax = max(value, na.rm = TRUE),
        .groups = "drop"
      ),
    aes(x = year, ymin = ymin, ymax = ymax,
        fill = supply_lead_combined, group = supply_lead_combined),
    alpha = 0.15
  ) +
  geom_line(
    data = filter(global_total_cum_res_prod, scenario %in% c(SCENARIO_ref_shortLT, SCENARIO_SS_ref_shortLT), lead_times == "Short lead times", year >= 2021, year <= 2075),
    aes(x = year, y = value, group = scenario, color = scenario)
  ) +
  geom_line(
    data = filter(global_res_avail_total , lead_times == "Short lead times", year >= 2021, year <= 2075),
    aes(x = year, y = value, group = scenario, color = scenario)
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  facet_grid(
    resource ~ supply_lead_combined,
    scales = "free_y",
    labeller = labeller(
      supply_lead_combined = function(x) {
        labels <- c(
          "Short lead times Baseline constrained supply" = "Baseline constrained supply",
          "Short lead times Steady-state constrained supply" = "Steady-state constrained supply"
        )
        stringr::str_wrap(labels[x], width = 12)
      }
    )
  )+
  scale_color_manual(name = "",
                     values = c(SCENARIO_colors, "Maximum production potential" = "black"),
                     labels = c(SCENARIO_labels, "Maximum production potential" = "Maximum production potential"))+
  scale_fill_manual(values = supply_lead_combined_colors, name = "Scenario", labels = c("Short lead times Baseline constrained supply" = "Baseline constrained supply scenarios",
                                                                                        "Short lead times Steady-state constrained supply" = "Steady-state constrained supply scenarios"))+
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "right",
    legend.key.width = unit(1.5, "cm")
  ) +
  labs(y = "Mt", x = "") +
  guides(linetype = "none")

print(FigS8b)


# FIGURE S8 COMBINED ---------------------------------------------------

FigS8 <- (figS8a | FigS8b) + 
  plot_layout(
    guides = "collect",
    widths = c(1,2)
  )+
  plot_annotation(tag_levels = "A")+
  theme(
    legend.position = "right"
  )

print(FigS8)


ggsave(paste0(PLOT_FOLDER,"FigS8.png", sep = ""),width=15, height=7, units="in")


# FIGURE S9: ENERGY PRICES (STEADY STATE CASES) ----------------------------------
# FIGURE S9A ELECTRICITY PRICES -------------------------------------------

SCENARIO_order <- c("01272026_UnlimitSupply_BR",
                    #"01272026_His_constrSupply_BR_noTC",
                    "01272026_His_constrSupply_BR_SS_noTC",
                    #"01272026_His_constrSupply_BR_shortLT_noTC",
                    "01272026_His_constrSupply_BR_SS_shortLT_noTC",
                    #"01272026_His_constrSupply_EnR_noTC",
                    "01272026_His_constrSupply_EnR_SS_noTC",
                    #"01272026_His_constrSupply_EnR_shortLT_noTC",
                    "01272026_His_constrSupply_EnR_SS_shortLT_noTC",
                    #"03192026_His_constrSupply_highDemand",
                    "03192026_His_constrSupply_SS_highDemand",
                    #"03192026_His_constrSupply_shortLT_highDemand",
                    "03192026_His_constrSupply_SS_shortLT_highDemand")

SCENARIO_ref_unconstrained <- "01272026_UnlimitSupply_BR"

PricesMkt <- getQuery(prj, "prices of all markets")


PricesElecReg <- PricesMkt %>%
  filter(grepl("electricity", market),
         !grepl("Demand_int", market),
         !grepl("ownuse", market)) %>%
  mutate(market = gsub("electricity", "", market)) %>%
  mutate(value = value*125.428/27.8,
         Units = "$2025/GJ") %>%
  rename(region = market) %>%
  filter(year >= 2021)

PricesWindReg <- PricesMkt %>%
  filter(grepl("wind_mineral", market),
         !grepl("fixed-output", market)) %>%
  mutate(market = gsub("wind_mineral", "", market)) %>%
  mutate(value = value*125.428/27.8,
         Units = "$2025/GJ") %>%
  rename(region = market) %>%
  filter(year >= 2021) %>%
  mutate(market = "wind")

PricesWindOffshoreReg <- PricesMkt %>%
  filter(grepl("wind_offshore_mineral", market),
         !grepl("fixed-output", market)) %>%
  mutate(market = gsub("wind_offshore_mineral", "", market)) %>%
  mutate(value = value*125.428/27.8,
         Units = "$2025/GJ") %>%
  rename(region = market) %>%
  filter(year >= 2021) %>%
  mutate(market = "wind_offshore")

PricesWindStorageReg <- PricesMkt %>%
  filter(grepl("wind_storage_mineral", market),
         !grepl("fixed-output", market)) %>%
  mutate(market = gsub("wind_storage_mineral", "", market)) %>%
  mutate(value = value*125.428/27.8,
         Units = "$2025/GJ") %>%
  rename(region = market) %>%
  filter(year >= 2021) %>%
  mutate(market = "wind_storage")

PricesPVReg <- PricesMkt %>%
  filter(grepl("pv_mineral", market),
         !grepl("rooftop", market),
         !grepl("fixed-output", market)) %>%
  mutate(market = gsub("pv_mineral", "", market)) %>%
  mutate(value = value*125.428/27.8,
         Units = "$2025/GJ") %>%
  rename(region = market) %>%
  filter(year >= 2021) %>%
  mutate(market = "PV")

PricesPVStorageReg <- PricesMkt %>%
  filter(grepl("pv_storage_mineral", market),
         !grepl("rooftop", market),
         !grepl("fixed-output", market)) %>%
  mutate(market = gsub("pv_storage_mineral", "", market)) %>%
  mutate(value = value*125.428/27.8,
         Units = "$2025/GJ") %>%
  rename(region = market) %>%
  filter(year >= 2021) %>%
  mutate(market = "PV_storage")

PricesCSPReg <- PricesMkt %>%
  filter(grepl("elec_CSP", market),
         !grepl("fixed-output", market)) %>%
  separate(market, into = c("region", "technology"), sep = c("elec_")) %>%
  mutate(value = value*125.428/27.8,
         Units = "$2025/GJ") %>%
  filter(year >= 2021)

PricesGasReg <- PricesMkt %>%
  filter(grepl("elec_gas", market),
         !grepl("fixed-output", market)) %>%
  separate(market, into = c("region", "technology"), sep = c("elec_")) %>%
  mutate(value = value*125.428/27.8,
         Units = "$2025/GJ") %>%
  filter(year >= 2021)

PricesGeothermalReg <- PricesMkt %>%
  filter(grepl("elec_geothermal", market),
         !grepl("fixed-output", market)) %>%
  separate(market, into = c("region", "technology"), sep = c("elec_")) %>%
  mutate(value = value*125.428/27.8,
         Units = "$2025/GJ") %>%
  filter(year >= 2021)

PricesBiomassReg <- PricesMkt %>%
  filter(grepl("elec_biomass", market),
         !grepl("fixed-output", market)) %>%
  separate(market, into = c("region", "technology"), sep = c("elec_")) %>%
  mutate(value = value*125.428/27.8,
         Units = "$2025/GJ") %>%
  filter(year >= 2021)

PricesCoalReg <- PricesMkt %>%
  filter(grepl("elec_coal", market),
         !grepl("fixed-output", market)) %>%
  separate(market, into = c("region", "technology"), sep = c("elec_")) %>%
  mutate(value = value*125.428/27.8,
         Units = "$2025/GJ") %>%
  filter(year >= 2021)

PricesRefLiqReg <- PricesMkt %>%
  filter(grepl("elec_refined liquids", market),
         !grepl("fixed-output", market)) %>%
  separate(market, into = c("region", "technology"), sep = c("elec_")) %>%
  mutate(value = value*125.428/27.8,
         Units = "$2025/GJ") %>%
  filter(year >= 2021)

PricesNucReg <- PricesMkt %>%
  filter(grepl("elec_Gen_I", market),
         !grepl("fixed-output", market)) %>%
  separate(market, into = c("region", "technology"), sep = c("elec_")) %>%
  mutate(value = value*125.428/27.8,
         Units = "$2025/GJ") %>%
  filter(year >= 2021)

# Get total electricity generation by region so that we can weight the elec prices
# and calculate a global weighted average
TotalElecGenReg <- getQuery(prj2, "elec gen by gen tech") %>%
  group_by(Units, scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

#specific techs
WindElecGenReg <- getQuery(prj2, "elec gen by gen tech") %>%
  filter(technology == "wind") %>%
  group_by(Units, scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

WindOffshoreElecGenReg <- getQuery(prj2, "elec gen by gen tech") %>%
  filter(technology == "wind_offshore") %>%
  group_by(Units, scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

WindStorageElecGenReg <- getQuery(prj2, "elec gen by gen tech") %>%
  filter(technology == "wind_storage") %>%
  group_by(Units, scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

PVElecGenReg <- getQuery(prj2, "elec gen by gen tech") %>%
  filter(technology == "PV") %>%
  group_by(Units, scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

PVStorageElecGenReg <- getQuery(prj2, "elec gen by gen tech") %>%
  filter(technology == "PV_storage") %>%
  group_by(Units, scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

CSPElecGenReg <- getQuery(prj2, "elec gen by gen tech") %>%
  filter(technology %in% c("CSP", "CSP_storage")) %>%
  group_by(Units, scenario, region, technology, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

GasElecGenReg <- getQuery(prj2, "elec gen by gen tech") %>%
  filter(subsector == "gas") %>%
  group_by(Units, scenario, region, technology, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

GeothermalElecGenReg <- getQuery(prj2, "elec gen by gen tech") %>%
  filter(subsector == "geothermal") %>%
  group_by(Units, scenario, region, technology, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

BiomassElecGenReg <- getQuery(prj2, "elec gen by gen tech") %>%
  filter(subsector == "biomass") %>%
  group_by(Units, scenario, region, technology, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

CoalElecGenReg <- getQuery(prj2, "elec gen by gen tech") %>%
  filter(subsector == "coal") %>%
  group_by(Units, scenario, region, technology, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

RefLiqElecGenReg <- getQuery(prj2, "elec gen by gen tech") %>%
  filter(subsector == "refined liquids") %>%
  group_by(Units, scenario, region, technology, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

NucElecGenReg <- getQuery(prj2, "elec gen by gen tech") %>%
  filter(subsector == "nuclear") %>%
  group_by(Units, scenario, region, technology, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

#weighted average global electricity prices
PricesElecGlo <- PricesElecReg %>%
  left_join(TotalElecGenReg, by = c("scenario", "region", "year"),
            suffix = c(".price", ".gen")) %>%
  na.omit() %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.gen, na.rm = TRUE), .groups = "drop")

PricesWindGlo <- PricesWindReg %>%
  left_join(WindElecGenReg, by = c("scenario", "region", "year"),
            suffix = c(".price", ".gen")) %>%
  na.omit() 

PricesWindOffshoreGlo <- PricesWindOffshoreReg %>%
  left_join(WindOffshoreElecGenReg, by = c("scenario", "region", "year"),
            suffix = c(".price", ".gen")) %>%
  na.omit() 

PricesWindStorageGlo <- PricesWindStorageReg %>%
  left_join(WindStorageElecGenReg, by = c("scenario", "region", "year"),
            suffix = c(".price", ".gen")) %>%
  na.omit()

PricesWindGlo_all <- bind_rows(PricesWindGlo,
                               PricesWindOffshoreGlo,
                               PricesWindStorageGlo) %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.gen, na.rm = TRUE), .groups = "drop")

PricesPVGlo <- PricesPVReg %>%
  left_join(PVElecGenReg, by = c("scenario", "region", "year"),
            suffix = c(".price", ".gen")) %>%
  na.omit() 

PricesPVStorageGlo <- PricesPVStorageReg %>%
  left_join(PVStorageElecGenReg, by = c("scenario", "region", "year"),
            suffix = c(".price", ".gen")) %>%
  na.omit()

PricesPVGlo_all <- bind_rows(PricesPVGlo,
                             PricesPVStorageGlo) %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.gen, na.rm = TRUE), .groups = "drop")

PricesCSPGlo <- PricesCSPReg %>%
  left_join(CSPElecGenReg, by = c("scenario", "region", "technology", "year"),
            suffix = c(".price", ".gen")) %>%
  na.omit() %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.gen, na.rm = TRUE), .groups = "drop")

PricesGasGlo <- PricesGasReg %>%
  left_join(GasElecGenReg, by = c("scenario", "region", "technology", "year"),
            suffix = c(".price", ".gen")) %>%
  na.omit() %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.gen, na.rm = TRUE), .groups = "drop")

PricesGeothermalGlo <- PricesGeothermalReg %>%
  left_join(GeothermalElecGenReg, by = c("scenario", "region", "technology", "year"),
            suffix = c(".price", ".gen")) %>%
  na.omit() %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.gen, na.rm = TRUE), .groups = "drop")

PricesBiomassGlo <- PricesBiomassReg %>%
  left_join(BiomassElecGenReg, by = c("scenario", "region", "technology", "year"),
            suffix = c(".price", ".gen")) %>%
  na.omit() %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.gen, na.rm = TRUE), .groups = "drop")

PricesCoalGlo <- PricesCoalReg %>%
  left_join(CoalElecGenReg, by = c("scenario", "region", "technology", "year"),
            suffix = c(".price", ".gen")) %>%
  na.omit() %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.gen, na.rm = TRUE), .groups = "drop")

PricesRefLiqGlo <- PricesRefLiqReg %>%
  left_join(RefLiqElecGenReg, by = c("scenario", "region", "technology", "year"),
            suffix = c(".price", ".gen")) %>%
  na.omit() %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.gen, na.rm = TRUE), .groups = "drop")

PricesNucGlo <- PricesNucReg %>%
  left_join(NucElecGenReg, by = c("scenario", "region", "technology", "year"),
            suffix = c(".price", ".gen")) %>%
  na.omit() %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.gen, na.rm = TRUE), .groups = "drop")


# diff from unconstrained
PricesElecGloRelDiff <- PricesElecGlo %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(sector = "Electricity generation")

PricesWindGlo_allRelDiff <- PricesWindGlo_all %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(sector = "Wind")

PricesPVGlo_allRelDiff <- PricesPVGlo_all %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(sector = "Solar PV")

PricesCSPGloRelDiff <- PricesCSPGlo %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(sector = "Solar CSP")

PricesGasGloRelDiff <- PricesGasGlo %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(sector = "Gas")

PricesGeothermalGloRelDiff <- PricesGeothermalGlo %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(sector = "Geothermal")

PricesBiomassGloRelDiff <- PricesBiomassGlo %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(sector = "Biomass")

PricesCoalGloRelDiff <- PricesCoalGlo %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(sector = "Coal")

PricesRefLiqGloRelDiff <- PricesRefLiqGlo %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(sector = "Refined Liquids")

PricesNucGloRelDiff <- PricesNucGlo %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(sector = "Nuclear")


PricesAllElecGloRelDiff <- bind_rows(PricesElecGloRelDiff,
                                     PricesWindGlo_allRelDiff,
                                     PricesPVGlo_allRelDiff,
                                     PricesCSPGloRelDiff,
                                     PricesGasGloRelDiff,
                                     PricesGeothermalGloRelDiff,
                                     PricesBiomassGloRelDiff,
                                     PricesCoalGloRelDiff,
                                     PricesRefLiqGloRelDiff,
                                     PricesNucGloRelDiff)


# FIGURE S9B TRANSPORTATION SERVICE OUTPUT PRICE -------------------------------------

TrnServPrices <- getQuery(prj_E, "costs of transport techs")

PricesTrnServPassReg <- TrnServPrices %>%
  filter(sector == "trn_pasg_road_ldv_4w_pass") %>%
  mutate(value = value*105.377/60.055,
         Units = "$2020/pass-km") %>%
  filter(year >= 2021)

PricesBEVPassReg <- TrnServPrices %>%
  filter(sector == "trn_pasg_road_ldv_4w_pass") %>%
  filter(grepl("bev_pass", technology)) %>%
  mutate(value = value*105.377/60.055,
         Units = "$2020/pass-km") %>%
  filter(year >= 2021)

PricesLiqPassReg <- TrnServPrices %>%
  filter(sector == "trn_pasg_road_ldv_4w_pass") %>%
  filter(grepl("Liquids", technology),
         !grepl("Hybrid", technology)) %>%
  mutate(value = value*105.377/60.055,
         Units = "$2020/pass-km") %>%
  filter(year >= 2021)

PricesHybridLiqPassReg <- TrnServPrices %>%
  filter(sector == "trn_pasg_road_ldv_4w_pass") %>%
  filter(grepl("Hybrid Liquids", technology)) %>%
  mutate(value = value*105.377/60.055,
         Units = "$2020/pass-km") %>%
  filter(year >= 2021)

PricesFCEVPassReg <- TrnServPrices %>%
  filter(sector == "trn_pasg_road_ldv_4w_pass") %>%
  filter(grepl("FCEV", technology)) %>%
  mutate(value = value*105.377/60.055,
         Units = "$2020/pass-km") %>%
  filter(year >= 2021)




TotalTrnServPassReg <- getQuery(prj2, "transport service output by tech and vintage") %>%
  filter(sector == "trn_pasg_road_ldv_4w_pass") %>%
  group_by(Units, scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

TotalBEVTrnServPassReg <- getQuery(prj2, "transport service output by tech and vintage") %>%
  filter(sector == "trn_pasg_road_ldv_4w_pass") %>%
  filter(grepl("bev_pass", technology)) %>%
  group_by(Units, scenario, sector, subsector, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

TotalLiqTrnServPassReg <- getQuery(prj2, "transport service output by tech and vintage") %>%
  filter(sector == "trn_pasg_road_ldv_4w_pass") %>%
  filter(grepl("Liquids", technology),
         !grepl("Hybrid", technology)) %>%
  group_by(Units, scenario, sector, subsector, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

TotalHybridLiqTrnServPassReg <- getQuery(prj2, "transport service output by tech and vintage") %>%
  filter(sector == "trn_pasg_road_ldv_4w_pass") %>%
  filter(grepl("Hybrid Liquids", technology)) %>%
  group_by(Units, scenario, sector, subsector, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

TotalFCEVTrnServPassReg <- getQuery(prj2, "transport service output by tech and vintage") %>%
  filter(sector == "trn_pasg_road_ldv_4w_pass") %>%
  filter(grepl("FCEV", technology)) %>%
  group_by(Units, scenario, sector, subsector, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

PricesTrnServPassGlo <- PricesTrnServPassReg %>%
  left_join(TotalTrnServPassReg, by = c("scenario", "region", "year"),
            suffix = c(".price", ".serv")) %>%
  na.omit() %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.serv, na.rm = TRUE), .groups = "drop")

PricesBEVTrnServPassGlo <- PricesBEVPassReg %>%
  left_join(TotalBEVTrnServPassReg, by = c("scenario", "sector", "subsector", "region", "year"),
            suffix = c(".price", ".serv")) %>%
  na.omit() %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.serv, na.rm = TRUE), .groups = "drop")

PricesLiqTrnServPassGlo <- PricesLiqPassReg %>%
  left_join(TotalLiqTrnServPassReg, by = c("scenario", "sector", "subsector", "region", "year"),
            suffix = c(".price", ".serv")) %>%
  na.omit()

PricesHybridLiqTrnServPassGlo <- PricesHybridLiqPassReg %>%
  left_join(TotalHybridLiqTrnServPassReg, by = c("scenario", "sector", "subsector", "region", "year"),
            suffix = c(".price", ".serv")) %>%
  na.omit()

PricesLiq_allTrnServPassGlo <- bind_rows(PricesLiqTrnServPassGlo,
                                         PricesHybridLiqTrnServPassGlo) %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.serv, na.rm = TRUE), .groups = "drop")


PricesFCEVTrnServPassGlo <- PricesFCEVPassReg %>%
  left_join(TotalFCEVTrnServPassReg, by = c("scenario", "sector", "subsector", "region", "year"),
            suffix = c(".price", ".serv")) %>%
  na.omit() %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = weighted.mean(value.price, w = value.serv, na.rm = TRUE), .groups = "drop")


PricesTrnServPassGloRelDiff <- PricesTrnServPassGlo %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(lead_times = if_else(str_detect(scenario.diff, "shortLT"), "Short lead times", "Average lead times")) %>%
  mutate(sector = "LDV 4W")

PricesBEVTrnServPassGloRelDiff <- PricesBEVTrnServPassGlo %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(lead_times = if_else(str_detect(scenario.diff, "shortLT"), "Short lead times", "Average lead times")) %>%
  mutate(sector = "BEV")

PricesLiq_allTrnServPassGloRelDiff <- PricesLiq_allTrnServPassGlo %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(lead_times = if_else(str_detect(scenario.diff, "shortLT"), "Short lead times", "Average lead times")) %>%
  mutate(sector = "Liquids")

PricesFCEVTrnServPassGloRelDiff <- PricesFCEVTrnServPassGlo %>%
  rel_diff_from_scen(diff_scenarios = SCENARIO_order,
                     ref_scenario = SCENARIO_ref_unconstrained,
                     c("year")) %>%
  mutate(lead_times = if_else(str_detect(scenario.diff, "shortLT"), "Short lead times", "Average lead times")) %>%
  mutate(sector = "FCEV")

PricesAllTrnServPassGloRelDiff <- bind_rows(PricesTrnServPassGloRelDiff,
                                            PricesBEVTrnServPassGloRelDiff,
                                            PricesLiq_allTrnServPassGloRelDiff,
                                            PricesFCEVTrnServPassGloRelDiff)
# FIGURE S9A ELECTRICITY PRICES PLOT ---------------------------------------


FigS9a_alt <-
  ggplot() +
  # geom_ribbon(
  #   data = PricesAllElecGloRelDiff %>%
  #     filter(scenario.diff %in% SCENARIO_order, scenario.diff != SCENARIO_ref_unconstrained,
  #            year >= 2021, year <= 2075) %>%
  #     group_by(year, sector) %>%
  #     summarise(
  #       ymin = min(value, na.rm = TRUE),
  #       ymax = max(value, na.rm = TRUE),
  #       .groups = "drop"
  #     ),
  #   aes(x = year, ymin = ymin, ymax = ymax,
  #       fill = sector, group = sector),
  #   alpha = 0.15
  # ) +
  # geom_ribbon(
  #   data = PricesTrnServPassGloRelDiff %>%
  #     filter(scenario.diff %in% SCENARIO_order, scenario.diff != SCENARIO_ref_unconstrained,
  #            year >= 2021, year <= 2075) %>%
  #     group_by(year, sector) %>%
  #     summarise(
  #       ymin = min(value, na.rm = TRUE),
  #       ymax = max(value, na.rm = TRUE),
  #       .groups = "drop"
  #     ),
  #   aes(x = year, ymin = ymin, ymax = ymax,
  #       fill = sector, group = sector),
  #   alpha = 0.15
  # ) +
  geom_line(data = filter(PricesAllElecGloRelDiff,
                          scenario.diff %in% c(SCENARIO_SS_ref),
                          #SCENARIO_ref_shortLT,
                          #SCENARIO_ref_hiEVD,
                          #SCENARIO_ref_EnR),
                          year >= 2021 & year <= 2075,
                          sector %in% c("Geothermal",
                                        "Solar PV",
                                        "Wind",
                                        "Gas",
                                        "Coal",
                                        "Nuclear",
                                        "Electricity generation")),
            aes(x = year, y = value, group = sector, color = sector), linewidth = 1) +
  # geom_line(data = filter(PricesTrnServPassGloRelDiff,
  #                         scenario.diff %in% c(SCENARIO_ref),
  #                         #SCENARIO_ref_shortLT,
  #                         #SCENARIO_ref_hiEVD,
  #                         #SCENARIO_ref_EnR),
  #                         year >= 2021 & year <= 2075),
  #           aes(x = year, y = value, group = interaction(sector, scenario.diff), color = sector, linetype = scenario.diff), linewidth = 1) +
  geom_hline(yintercept = 1, color = "black", linetype = 2) +
  scale_color_manual(name = "Technology",
                     values = elec_tech_colors) +
  scale_y_continuous(limits = c(NA,2))+
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "none",
    legend.key.width = unit(1, "cm")
  ) +
  labs(x = "", y = "Relative change \n (1=Unconstrained supply)", title = "Electricity generation prices") 

print(FigS9a_alt)

# FIGURE S9B TRANSPORT PRICES PLOT ---------------------------------------


FigS9b_alt <-
  ggplot() +
  geom_line(data = filter(PricesAllTrnServPassGloRelDiff ,
                          scenario.diff %in% c(SCENARIO_SS_ref),
                          #SCENARIO_ref_shortLT,
                          #SCENARIO_ref_hiEVD,
                          #SCENARIO_ref_EnR),
                          year >= 2021 & year <= 2075),
            aes(x = year, y = value, group = sector, color = sector), linewidth = 1) +
  # geom_line(data = filter(PricesTrnServPassGloRelDiff,
  #                         scenario.diff %in% c(SCENARIO_ref),
  #                         #SCENARIO_ref_shortLT,
  #                         #SCENARIO_ref_hiEVD,
  #                         #SCENARIO_ref_EnR),
  #                         year >= 2021 & year <= 2075),
  #           aes(x = year, y = value, group = interaction(sector, scenario.diff), color = sector, linetype = scenario.diff), linewidth = 1) +
  geom_hline(yintercept = 1, color = "black", linetype = 2) +
  scale_color_manual(name = "Commodity",
                     values = commodity_colors) +
  scale_y_continuous(limits = c(NA,2))+
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "none",
    legend.key.width = unit(1, "cm")
  ) +
  labs(x = "", y = "Relative change \n (1=Unconstrained supply)", title = "4W LDV prices") 

print(FigS9b_alt)


# FIGURE S9c: ELECTRICITY GENERATION DIFF ----------------------------------


ElecGenSubsector <- getQuery(prj2, "elec gen by gen tech") %>%
  left_join(elec_tech_map, by = c("technology")) %>%
  group_by(Units, scenario, technology0, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

TotalElecGen <- ElecGenSubsector %>%
  group_by(Units, scenario, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup() %>%
  mutate(technology0 = "Electricity generation")

RelElecGenSubsectorDiff <- rel_diff_from_scen(bind_rows(ElecGenSubsector, TotalElecGen),
                                              ref_scenario = SCENARIO_ref_unconstrained,
                                              diff_scenarios = c(SCENARIO_SS_ref),
                                              join_var = c("Units", "technology0", "year"))

diff_ElecGenSubsector_1 <- diff_from_scen(ElecGenSubsector,
                                          ref_scenario = SCENARIO_ref_unconstrained,
                                          diff_scenarios = c(SCENARIO_SS_ref, SCENARIO_SS_ref_shortLT),
                                          join_var = c("Units", "technology0", "year"))

diff_ElecGenSubsector_2 <- diff_from_scen(ElecGenSubsector,
                                          ref_scenario = SCENARIO_EnR_unconstrained,
                                          diff_scenarios = SCENARIOS_EnR,
                                          join_var = c("Units", "technology0", "year"))

diff_ElecGenSubsector_3 <- diff_from_scen(ElecGenSubsector,
                                          ref_scenario = SCENARIO_hiEVD_unconstrained,
                                          diff_scenarios = SCENARIOS_hiEVD,
                                          join_var = c("Units", "technology0", "year"))

diff_ElecGenSubsector <- bind_rows(diff_ElecGenSubsector_1,
                                   diff_ElecGenSubsector_2,
                                   diff_ElecGenSubsector_3)


FigS9c <-
  ggplot() +
  geom_line(data = filter(RelElecGenSubsectorDiff,
                          scenario.diff %in% c(SCENARIO_SS_ref),
                          year >= 2021 & year <= 2075,
                          technology0 %in% c("Geothermal",
                                             "Solar PV",
                                             "Wind",
                                             "Gas",
                                             "Coal",
                                             "Nuclear",
                                             "Electricity generation")),
            aes(x = year, y = value, group = technology0, color = technology0), linewidth = 1) +
  geom_hline(yintercept = 1, color = "black", linetype = 2) +
  scale_color_manual(name = "Technology",
                     values = elec_tech_colors) +
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "none",
    legend.key.width = unit(1, "cm")
  ) +
  labs(x = "", y = "Relative change \n (1=Unconstrained supply)", title = "Electricity generation") 

print(FigS9c)





# FIGURE S9D: TRANSPORT SERVICE DIFF ----------------------------------

TrnServTech4W <- getQuery(prj2, "transport service output by tech and vintage") %>%
  separate(technology, into = c("technology", "vintage"), sep = ",year=") %>%
  left_join(trn_tech_map, by = c("sector", "subsector", "technology")) %>%
  group_by(Units, scenario, subsector_agg, technology_agg, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021, subsector_agg == "4-Wheel LDV") %>%
  ungroup()

TotalServ4W <- TrnServTech4W %>%
  group_by(Units, scenario, subsector_agg, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(technology_agg = "LDV 4W")

RelTrnServTechDiff <- rel_diff_from_scen(bind_rows(TrnServTech4W, TotalServ4W),
                                         ref_scenario = SCENARIO_ref_unconstrained,
                                         diff_scenarios = c(SCENARIO_SS_ref),
                                         join_var = c("Units",  "subsector_agg", "technology_agg", "year"))



FigS9d <-
  ggplot() +
  geom_line(data = filter(RelTrnServTechDiff,
                          scenario.diff %in% c(SCENARIO_SS_ref),
                          year >= 2021 & year <= 2075),
            aes(x = year, y = value, group = technology_agg, color = technology_agg), linewidth = 1) +
  geom_hline(yintercept = 1, color = "black", linetype = 2) +
  scale_color_manual(name = "Fuel",
                     values = commodity_colors) +
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "none",
    legend.key.width = unit(1, "cm")
  ) +
  labs(x = "", y = "Relative change \n (1=Unconstrained supply)", title = "4W LDV service output") 


print(FigS9d)





# FIGURE S9 COMBINED -------------------------------------------------------

row2 <- (FigS9a_alt + plot_spacer() + FigS9b_alt) +
  plot_layout(widths = c(1, 0.5, 1))
row3 <- FigS9c + plot_spacer() + FigS9d +
  plot_layout(widths = c(1, 0.5, 1))

FigS9alt <- ( row2 / row3) + 
  plot_layout(heights = c(1,1))+
  plot_annotation(tag_levels = "A",
                  theme = theme(
                    plot.title = element_text(size = 20)
                  ))+
  theme(
    legend.position = "right"
  )

print(FigS9alt)

ggsave(paste0(PLOT_FOLDER,"FigS9.png", sep = ""),width=15, height=10, units="in")



print(FigS9alt)

ggsave(paste0(PLOT_FOLDER,"FigS9.png", sep = ""),width=15, height=10, units="in")


# FIGURE S10: ABSOLUTE DIFF ELEC GEN AND TRN SERV -------------------------
#Single scenarios
SCENARIO_ref <- c("01272026_His_constrSupply_BR_noTC")
SCENARIO_ref_shortLT <- c("01272026_His_constrSupply_BR_shortLT_noTC")
SCENARIO_ref_EnR <- c("01272026_His_constrSupply_EnR_noTC")
SCENARIO_ref_hiEVD <- c("03192026_His_constrSupply_highDemand")

SCENARIO_ref_unconstrained <- c("01272026_UnlimitSupply_BR")
SCENARIO_EnR_unconstrained <- c("01272026_UnlimitSupply_EnR")
SCENARIO_hiEVD_unconstrained <- c( "03192026_UnlimitSupply_highDemand")

# FIGURE S10A: ELECTRICITY GENERATION DIFF ----------------------------------


ElecGenSubsector <- getQuery(c(prj2), "elec gen by gen tech") %>%
  left_join(elec_tech_map, by = c("technology")) %>%
  group_by(Units, scenario, technology0, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()

diff_ElecGenSubsector_1 <- diff_from_scen(ElecGenSubsector,
                                          ref_scenario = SCENARIO_ref_unconstrained,
                                          diff_scenarios = c(SCENARIO_ref, SCENARIO_ref_shortLT),
                                          join_var = c("Units", "technology0", "year"))

diff_ElecGenSubsector_2 <- diff_from_scen(ElecGenSubsector,
                                          ref_scenario = SCENARIO_EnR_unconstrained,
                                          diff_scenarios = SCENARIOS_EnR,
                                          join_var = c("Units", "technology0", "year"))

diff_ElecGenSubsector_3 <- diff_from_scen(ElecGenSubsector,
                                          ref_scenario = SCENARIO_hiEVD_unconstrained,
                                          diff_scenarios = SCENARIOS_hiEVD,
                                          join_var = c("Units", "technology0", "year"))

diff_ElecGenSubsector <- bind_rows(diff_ElecGenSubsector_1,
                                   diff_ElecGenSubsector_2,
                                   diff_ElecGenSubsector_3)

FigS10a <-
  ggplot() +
  geom_bar(
    data = filter(diff_ElecGenSubsector, scenario.diff %in% c(SCENARIO_ref),
                  year >= 2021, year <= 2075),
    aes(x = year, y = value, group = technology0, fill = technology0), alpha = 0.7,
    stat = "identity", position = position_stack(reverse = FALSE),
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  scale_fill_manual(
    values = elec_tech_colors,
    name = "Technology"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "right",
    legend.key.width = unit(1.5, "cm")
  ) +
  labs(y = "EJ \n (Reference-Unconstrained supply)", x = "", title = "Electricity generation") 

print(FigS10a)


# FIGURE S10B: TRANSPORT SERVICE DIFF ----------------------------------

TrnServTech <- getQuery(prj2, "transport service output by tech and vintage") %>%
  separate(technology, into = c("technology", "vintage"), sep = ",year=") %>%
  left_join(trn_tech_map, by = c("sector", "subsector", "technology")) %>%
  group_by(Units, scenario, subsector_agg, technology_agg, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  filter(year >= 2021) %>%
  ungroup()



diff_TrnServTech_1 <- diff_from_scen(TrnServTech,
                                     ref_scenario = SCENARIO_ref_unconstrained,
                                     diff_scenarios = c(SCENARIO_ref, SCENARIO_ref_shortLT),
                                     join_var = c("Units", "subsector_agg", "technology_agg", "year"))

diff_TrnServTech_2 <- diff_from_scen(TrnServTech,
                                     ref_scenario = SCENARIO_EnR_unconstrained,
                                     diff_scenarios = SCENARIOS_EnR,
                                     join_var = c("Units", "subsector_agg", "technology_agg", "year"))

diff_TrnServTech_3 <- diff_from_scen(TrnServTech,
                                     ref_scenario = SCENARIO_hiEVD_unconstrained,
                                     diff_scenarios = SCENARIOS_hiEVD,
                                     join_var = c("Units", "subsector_agg", "technology_agg", "year"))

diff_TrnServTech <- bind_rows(diff_TrnServTech_1,
                              diff_TrnServTech_2,
                              diff_TrnServTech_3)

FigS10b <-
  ggplot() +
  geom_bar(
    data = filter(diff_TrnServTech, subsector_agg == "4-Wheel LDV", scenario.diff %in% c(SCENARIO_ref),
                  year >= 2021, year <= 2075),
    aes(x = year, y = value/(10^6), group = technology_agg, fill = technology_agg), alpha = 0.7,
    stat = "identity", position = position_stack(reverse = FALSE),
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  scale_fill_manual(
    values = trn_tech_colors,
    name = "Technology"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "right",
    legend.key.width = unit(1.5, "cm")
  ) +
  labs(y = "trillion pass-km \n (Reference-Unconstrained supply)", x = "", title = "4W LDV service output") 

print(FigS10b)


# FIGURE S10 COMBINED -----------------------------------------------------

FigS10 <- (FigS10a + FigS10b) + 
  plot_layout(
    widths = c(1,1)
  )+
  plot_annotation(tag_levels = "A")

print(FigS10)


ggsave(paste0(PLOT_FOLDER,"FigS10.png", sep = ""),width=15, height=7, units="in")


# METHODOLOGY FIGURES ----------------------------------- --------------------------------------------------

# FIGURES S14-16: GCAM32 REG SUPPLY CURVES ------------------------------------------------


#Incremental supply curve
Fig1b_res_supply_curve_incr <- Fig1b_res_supply_curve_origin %>%
  filter(grade != "grade 0") %>%
  ungroup() %>%
  select(-year) %>%
  distinct() %>%
  separate(subresource, into = c(NA, "year"), sep = "_") %>%
  group_by(Units.Q, Units.cost, scenario, region, resource, grade) %>%
  mutate(cum_value.Q = cumsum(value.Q)/10^6) %>%
  mutate(value.cost = value.cost *(125.428/27.8),
         Units.cost = "2025$/tonne",
         Units.cum_Q = "Mt") %>%
  mutate(year = as.integer(year))

ggplot() +
  geom_line(data = filter(Fig1b_res_supply_curve_incr, scenario == SCENARIO_ref, resource == "copper",
                          year >= 2021 & year <= 2075),
            aes(x = cum_value.Q, y = value.cost, color = year, group = year)) +
  facet_wrap(~region, scales = "free") +
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "right",
    legend.key.width = unit(1, "cm")
  ) +
  scale_color_viridis_c(option="turbo")+
  scale_y_continuous(limits = c(0,NA))+
  scale_x_continuous(limits = c(0,NA))+
  labs(y = "2025$/t Cu", x = "Mt", title = "GCAM32 copper supply curves")+
ggsave(paste0(PLOT_FOLDER,"FigS14.png", sep = ""),width=17, height=10, units="in")

ggplot() +
  geom_line(data = filter(Fig1b_res_supply_curve_incr, scenario == SCENARIO_ref, resource == "lithium",
                          year >= 2021 & year <= 2075),
            aes(x = cum_value.Q, y = value.cost*0.189, color = year, group = year)) +
  facet_wrap(~region, scales = "free") +
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "right",
    legend.key.width = unit(1, "cm")
  ) +
  scale_color_viridis_c(option="turbo")+
  scale_y_continuous(limits = c(0,NA))+
  scale_x_continuous(limits = c(0,NA))+
  labs(y = "2025$/t LCE", x = "Mt", title = "GCAM32 lithium supply curves")+
ggsave(paste0(PLOT_FOLDER,"FigS15.png", sep = ""),width=17, height=10, units="in")


ggplot() +
  geom_line(data = filter(Fig1b_res_supply_curve_incr, scenario == SCENARIO_ref, resource == "nickel",
                          year >= 2021 & year <= 2075),
            aes(x = cum_value.Q, y = value.cost, color = year, group = year)) +
  facet_wrap(~region, scales = "free") +
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "right",
    legend.key.width = unit(1, "cm")
  ) +
  scale_color_viridis_c(option="turbo")+
  scale_y_continuous(limits = c(0,NA))+
  scale_x_continuous(limits = c(0,NA))+
  labs(y = "2025$/t Ni", x = "Mt", title = "GCAM32 nickel supply curves")+
ggsave(paste0(PLOT_FOLDER,"FigS16.png", sep = ""),width=17, height=10, units="in")


# FIGURE S17: RELATIVE SUBSTITUTABILITY -----------------------------------------------

SCENARIO_order <- c("01272026_UnlimitSupply_BR",
                    "01272026_UnlimitSupply_EnR",
                    "01272026_His_constrSupply_BR_noTC",
                    "01272026_His_constrSupply_BR_SS_noTC",
                    "01272026_His_constrSupply_BR_shortLT_noTC",
                    "01272026_His_constrSupply_BR_SS_shortLT_noTC",
                    "01272026_His_constrSupply_EnR_noTC",
                    "01272026_His_constrSupply_EnR_SS_noTC",
                    "01272026_His_constrSupply_EnR_shortLT_noTC",
                    "01272026_His_constrSupply_EnR_SS_shortLT_noTC",
                    "03192026_UnlimitSupply_highDemand",
                    "03192026_His_constrSupply_highDemand",
                    "03192026_His_constrSupply_SS_highDemand",
                    "03192026_His_constrSupply_shortLT_highDemand",
                    "03192026_His_constrSupply_SS_shortLT_highDemand"
)

mineral_inputs_tech <- getQuery(prj, "minerals inputs by tech") %>%
  filter(input %!in% c("traded copper", "traded nickel", "traded lithium", "traded iron and steel", "iron and steel"),
         sector %!in% c("traded copper", "traded nickel", "traded lithium", "traded iron and steel", "iron and steel"))

global_sector_demand_stack <- mineral_inputs_tech %>%
  left_join(mineral_demand_tech_mapping, by = c("sector", "subsector", "technology")) %>%
  group_by(Units, scenario, sector0, year, input) %>%
  dplyr::summarise(value = sum(value)) %>%
  rename(sector = sector0,
         resource = input) %>%
  mutate(sector = factor(sector, levels = sector_levels),
         scenario = factor(scenario, levels = SCENARIO_order)) %>%
  mutate(resource = gsub("regional ", "", resource))


global_sector_demand_pct_diff <- global_sector_demand_stack %>%
  filter(scenario %in% SCENARIO_order) %>%
  pct_diff_from_scen(diff_scenarios = c(SCENARIO_order),
                     ref_scenario = SCENARIO_ref_unconstrained,
                     join_var = c("Units", "sector", "year", "resource")) %>%
  filter(year <= 2050, resource %in% c("copper", "lithium", "nickel"))

p0 <-
  ggplot() +
  #geom_line(data = filter(global_mineral_prod, year <= 2050),
  #          aes(x = year, y = value, group = scenario)) +
  geom_line(data = filter(global_sector_demand_pct_diff, year >= 2020 & year <= 2050,
                          scenario.diff == SCENARIO_ref),
            aes(x = year, y = value, group = sector, color = sector), linewidth = 1.5) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  facet_grid(~resource, scales = "free_y", labeller = as_labeller(c(SCENARIO_labels,
                                                                      "copper" = "copper",
                                                                      "lithium" = "lithium",
                                                                      "nickel" = "nickel"))) +
  scale_color_manual(values = sector_colors,
                     breaks = sector_levels)+
  theme_bw() +
  theme(
    text = element_text(size = 16),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    strip.text = element_text(size = 16),
    legend.position = "right",
    legend.key.width = unit(1, "cm")
  ) +
  labs(y = "% (Reference - Unconstrained)", x = "", title = "Reductions in mineral consumption by sector")+
  ggsave(paste0(PLOT_FOLDER,"FigS17.png", sep = ""),width=12, height=5, units="in")


