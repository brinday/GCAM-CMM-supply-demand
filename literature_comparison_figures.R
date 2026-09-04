# Program Name: literature_comparison_figures.R
# Author: Nastya Zagoruichyk
# Date Last Updated: 9/3/2026

# =====================================================================
# Constants and Setup
# =====================================================================

# Load packages ---------------------------------------------------------------
library(patchwork)
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(data.table)
library(purrr)
library(readxl)
library(tidyverse)
library(ggnewscale)
library(grid)

# Required packages
library(ggnewscale)
if (!requireNamespace("ggh4x", quietly = TRUE)) {
  install.packages("ggh4x")
}
library(ggh4x)


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
                     "03192026_His_constrSupply_SS_shortLT_highDemand" = "Steady-state supply: Short lead times + High EV demand")


source_palette <- c(
  "IEA (2025)" = "#E78AC3",   
  "Castillo and Eggert (2020)" = "blue",
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
  "Parpan et al. (2026)" = "#008080",
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
  "Kushnir and Sandén (2012)" = "#FDB462",
  "Simon et al. (2026)" = "#D73027",
  "Parpan et al. (2026)" = "#008080",
  "Klose and Pauliuk (2023)" = "#984EA3",
  "BHP (2024)" = "#E6AB02",
  "S&P Global (2022)" = "#333333"
)

# =====================================================================
# Load Inputs 
# =====================================================================

CMM_data <- read_csv(
  "input/data/Table_S1_Summary_CMM.csv")

GCAM_data_reserves <- read_csv("input/data/global_res_avail_total.csv") %>% 
  filter(year %in% 2021:2100) %>%
  mutate(scenario = SCENARIO_labels[scenario]) 

GCAM_data_cumulative_demand <- read_csv(
  "input/data/global_total_cum_res_prod.csv"
) %>% 
  filter(year %in% 2021:2100) %>%
  mutate(scenario = SCENARIO_labels[scenario]) 

GCAM_data_annual_demand <- read_csv(
  "input/data/global_total_demand.csv"
) %>% 
  filter(year %in% 2021:2075) %>%
  mutate(scenario = SCENARIO_labels[scenario]) 

# =====================================================
# 1. PREPARE CMM CUMULATIVE DATA 
# =====================================================

CMM_long <- CMM_data %>% 
  select(-`annual supply projections`, -`annual demand projections`) %>%
  pivot_longer(
    cols = c(`Global Reserves`, `Global Resources`, `Cumulative Demand Projections`),
    names_to = "data_type",
    values_to = "value"
  ) %>% 
  filter(!is.na(value)) %>%
  mutate(
    type = case_when(
      data_type == "Cumulative Demand Projections" ~ "Cumulative Demand",
      data_type == "Global Reserves" ~ "Reserves",
      data_type == "Global Resources" ~ "Resources"
    )
  )

# Create Reserves + Resources
res_plus_res <- CMM_long %>%
  filter(type %in% c("Reserves", "Resources")) %>%
  group_by(year, mineral, source) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  mutate(type = "Reserves+Resources")

# Final CMM dataset
CMM_plot <- bind_rows(CMM_long, res_plus_res) %>%
  filter(type != "Resources") %>%
  select(year, mineral, source, type, value)

# =====================================================
# 2. PREPARE GCAM CUMULATIVE DATA (ALL MINERALS)
# =====================================================

# GCAM DEMAND
GCAM_demand <- GCAM_data_cumulative_demand %>%
  mutate(
    mineral = resource,
    scenario_group = case_when(
      scenario %in% c(
        "Increased recycling","High EV demand","Short lead times",
        "Short lead times + High EV demand","Short lead times + Increased recycling"
      ) ~ "Baseline constrained supply scenarios",
      
      scenario %in% c(
        "Steady-state supply: Increased recycling","Steady-state supply: High EV demand",
        "Steady-state supply: Short lead times",
        "Steady-state supply: Short lead times + Increased recycling",
        "Steady-state supply: Short lead times + High EV demand"
      ) ~ "Steady-state constrained supply scenarios",
      
      TRUE ~ scenario
    ),
    type = "GCAM: Cumulative Demand"
  )

# GCAM RESERVES
GCAM_reserves <- GCAM_data_reserves %>%
  mutate(
    mineral = resource,
    type = "GCAM: Maximum production potential",
    scenario_group = scenario
  )


GCAM_lines <- bind_rows(GCAM_demand, GCAM_reserves) %>%
  filter(scenario %in% c("Reference", "Steady-state supply")) %>% 
  mutate(
    scenario = case_when(
      scenario == "Reference" ~ "GCAM: Reference",
      scenario == "Steady-state supply" ~ "GCAM: Steady-state supply",
      TRUE ~ scenario
    )
  )

GCAM_ribbons <- GCAM_demand %>%
  filter(scenario_group %in% c(
    "Baseline constrained supply scenarios",
    "Steady-state constrained supply scenarios"
  )) %>%
  mutate(scenario_group = recode(
    scenario_group,
    "Baseline constrained supply scenarios" = "GCAM: Baseline constrained supply scenarios",
    "Steady-state constrained supply scenarios" = "GCAM: Steady-state constrained supply scenarios"
  )) %>%
  group_by(mineral, year, scenario_group) %>%
  summarise(
    ymin = min(value, na.rm = TRUE),
    ymax = max(value, na.rm = TRUE),
    .groups = "drop"
  )

# Stacking in the order: copper, lithium, nickel
CMM_plot <- CMM_plot %>%
  mutate(mineral = factor(mineral, levels = c("copper", "lithium", "nickel")))

GCAM_lines <- GCAM_lines %>%
  mutate(mineral = factor(mineral, levels = c("copper", "lithium", "nickel")))

GCAM_ribbons <- GCAM_ribbons %>%
  mutate(mineral = factor(mineral, levels = c("copper", "lithium", "nickel")))

# =====================================================
# 3. Arranging y-axis for the S1 figure
# =====================================================
CMM_plot_history <- CMM_plot %>%
  filter(type != "Cumulative Demand")

CMM_plot_future <- CMM_plot %>%
  filter(type == "Cumulative Demand")

# Ensure both plots use the same mineral order
mineral_levels <- sort(unique(c(
  as.character(CMM_plot_history$mineral),
  as.character(CMM_plot_future$mineral),
  as.character(GCAM_ribbons$mineral),
  as.character(GCAM_lines$mineral)
)))

CMM_plot_history$mineral <- factor(CMM_plot_history$mineral, levels = mineral_levels)
CMM_plot_future$mineral <- factor(CMM_plot_future$mineral, levels = mineral_levels)
GCAM_ribbons$mineral <- factor(GCAM_ribbons$mineral, levels = mineral_levels)
GCAM_lines$mineral <- factor(GCAM_lines$mineral, levels = mineral_levels)

# =====================================================
# Mineral-specific y-axis limits shared by panels A and B
# =====================================================
y_limits_by_mineral <- lapply(mineral_levels, function(m) {
  values <- c(
    CMM_plot_history$value[CMM_plot_history$mineral == m],
    CMM_plot_future$value[CMM_plot_future$mineral == m],
    GCAM_ribbons$ymin[GCAM_ribbons$mineral == m],
    GCAM_ribbons$ymax[GCAM_ribbons$mineral == m],
    GCAM_lines$value[GCAM_lines$mineral == m]
  )
  
  values <- values[is.finite(values)]
  y_range <- range(values, na.rm = TRUE)
  y_pad <- 0.05 * diff(y_range)
  
  if (!is.finite(y_pad) || y_pad == 0) {
    y_pad <- max(abs(y_range[1]) * 0.05, 1)
  }
  
  c(y_range[1] - y_pad, y_range[2] + y_pad)
})

names(y_limits_by_mineral) <- mineral_levels

mineral_y_scales <- lapply(y_limits_by_mineral, function(lim) {
  scale_y_continuous(
    name = "Mt",
    limits = lim,
    expand = expansion(mult = c(0, 0))
  )
})

# =====================================================
# PLOT (S1 A): historical data only
# =====================================================
combined_plot_a <- ggplot() +
  geom_point(
    data = CMM_plot_history,
    aes(x = year, y = value, shape = type, color = source),
    size = 3
  ) +
  scale_color_manual(
    name = "Historical Estimates sources",
    values = source_palette,
    drop = FALSE
  ) +
  scale_shape_manual(
    name = "Point type",
    values = c(
      "Reserves" = 17,
      "Reserves+Resources" = 2
    )
  ) +
  facet_wrap(~ mineral, scales = "free_y", ncol = 1) +
  ggh4x::facetted_pos_scales(y = mineral_y_scales) +
  scale_x_continuous(breaks = seq(1995, 2025, by = 5)) +
  labs(x = NULL, y = "Mt") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.key.width = unit(1, "cm"),
    legend.spacing.y = unit(0.05, "cm"),
    legend.key.height = unit(0.4, "cm"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(
    color = guide_legend(order = 1),
    shape = guide_legend(order = 2)
  )

# =====================================================
# PLOT (S1 B): GCAM data and cumulative demand
# =====================================================
combined_plot_b <- ggplot() +
  annotate(
    "rect",
    xmin = -Inf, xmax = 2026,
    ymin = -Inf, ymax = Inf,
    fill = "grey90", alpha = 0.5
  ) +
  geom_vline(
    xintercept = 2026,
    linetype = "dashed",
    color = "grey40"
  ) +
  geom_ribbon(
    data = GCAM_ribbons,
    aes(x = year, ymin = ymin, ymax = ymax, fill = scenario_group),
    alpha = 0.2
  ) +
  geom_line(
    data = GCAM_lines,
    aes(x = year, y = value, color = scenario, linetype = type),
    linewidth = 1
  ) +
  scale_color_manual(
    name = NULL,
    breaks = c(
      "GCAM: Reference",
      "GCAM: Steady-state supply"
    ),
    values = c(
      "GCAM: Reference" = "#C51B7D",
      "GCAM: Steady-state supply" = "#1F77B4"
    )
  ) +
  scale_fill_manual(
    name = "Scenario",
    breaks = c(
      "GCAM: Baseline constrained supply scenarios",
      "GCAM: Steady-state constrained supply scenarios"
    ),
    values = c(
      "GCAM: Baseline constrained supply scenarios" = "#C51B7D",
      "GCAM: Steady-state constrained supply scenarios" = "#1F77B4"
    )
  ) +
  ggnewscale::new_scale_color() +
  geom_point(
    data = CMM_plot_future,
    aes(x = year, y = value, shape = type, color = source),
    size = 3
  ) +
  scale_color_manual(
    name = "Future Projections sources",
    values = source_palette,
    drop = FALSE
  ) +
  scale_linetype_manual(
    name = "Line type",
    values = c(
      "GCAM: Cumulative Demand" = "solid",
      "GCAM: Maximum production potential" = "dashed"
    )
  ) +
  scale_shape_manual(
    name = "Point type",
    values = c(
      "Cumulative Demand" = 16,
      "Reserves" = 17,
      "Reserves+Resources" = 2
    )
  ) +
  facet_wrap(~ mineral, scales = "free_y", ncol = 1) +
  ggh4x::facetted_pos_scales(y = mineral_y_scales) +
  scale_x_continuous(breaks = seq(2020, 2100, by = 10)) +
  labs(x = NULL, y = "Mt") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.key.width = unit(1, "cm"),
    legend.spacing.y = unit(0.05, "cm"),
    legend.key.height = unit(0.4, "cm"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(
    fill = guide_legend(order = 4),
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 3),
    shape = guide_legend(order = 2)
  )

# =====================================================
# Combine: first plot = A, second plot = B
# =====================================================

combined_plot_a <- combined_plot_a +
  labs(title = "A. Historical Estimates")

combined_plot_b <- combined_plot_b +
  labs(title = "B. Future Projections")

FigS1 <- combined_plot_a + combined_plot_b +
  patchwork::plot_layout(
    ncol = 2,
    guides = "collect"
  ) &
  theme(
    plot.title = element_text(face = "bold", hjust = 0, size = 12),
    legend.position = "right"
  )


# FigS1 <- combined_plot_a + combined_plot_b +
#   plot_layout(
#     ncol = 2,
#     widths = c(1, 1),
#     guides = "collect"
#   ) +
#   plot_annotation(tag_levels = "A") &
#   theme(
#     plot.tag = element_text(face = "bold", size = 14),
#     legend.position = "right"
#   )

FigS1

ggsave(
  "output/FigS1.png",
  FigS1,
  width = 12,
  height = 8,
  dpi = 300
)

#===============================================================================
# 4. PREPARE CMM ANNUAL DATA 
#===============================================================================

CMM_long_annual <- CMM_data %>% 
  select(-`Global Reserves`,-`Global Resources`, -`Cumulative Demand Projections`) %>%
  filter(!year == "2100") %>% 
  pivot_longer(
    cols = c(`annual supply projections`, `annual demand projections`),
    names_to = "data_type",
    values_to = "value"
  ) %>% 
  filter(!is.na(value)) %>%
  mutate(
    type = case_when(
      data_type == "annual supply projections" ~ "Annual Supply",
      data_type == "annual demand projections" ~ "Annual Demand"
    )
  )

CMM_annual_data <- CMM_long_annual  %>%
  mutate(mineral = factor(mineral, levels = c("copper", "lithium", "nickel")))

#===============================================================================
# 5. PREPARE GCAM ANNUAL DATA 
#===============================================================================

GCAM_demand_annual <- GCAM_data_annual_demand %>% 
  mutate(
    mineral = resource,
    scenario_group = case_when(
      scenario %in% c(
        "Increased recycling","High EV demand","Short lead times",
        "Short lead times + High EV demand","Short lead times + Increased recycling"
      ) ~ "Baseline constrained supply scenarios",
      
      scenario %in% c(
        "Steady-state supply: Increased recycling","Steady-state supply: High EV demand",
        "Steady-state supply: Short lead times",
        "Steady-state supply: Short lead times + Increased recycling",
        "Steady-state supply: Short lead times + High EV demand"
      ) ~ "Steady-state constrained supply scenarios",
      
      scenario %in% c(
        "Unconstrained supply: High EV demand", "Unconstrained supply: Increased recycling"
      ) ~ "Unconstrained supply scenarios",
      
      TRUE ~ scenario
    ),
    type = "GCAM: Annual Demand"
  )

GCAM_lines_annual <- GCAM_demand_annual %>%
  filter(scenario %in% c("Reference", "Steady-state supply", "Unconstrained supply")) %>% 
  mutate(
    scenario = case_when(
      scenario == "Reference" ~ "GCAM: Reference",
      scenario == "Steady-state supply" ~ "GCAM: Steady-state supply",
      scenario == "Unconstrained supply" ~ "GCAM: Unconstrained supply",
      TRUE ~ scenario
    )
  )

GCAM_ribbons_annual <- GCAM_demand_annual %>%
  filter(scenario_group %in% c(
    "Baseline constrained supply scenarios",
    "Steady-state constrained supply scenarios",
    "Unconstrained supply scenarios"
  )) %>%
  mutate(scenario_group = recode(
    scenario_group,
    "Baseline constrained supply scenarios" = "GCAM: Baseline constrained supply scenarios",
    "Steady-state constrained supply scenarios" = "GCAM: Steady-state constrained supply scenarios",
    "Unconstrained supply scenarios" = "GCAM: Unconstrained supply scenarios"
  )) %>%
  group_by(mineral, year, scenario_group) %>%
  summarise(
    ymin = min(value, na.rm = TRUE),
    ymax = max(value, na.rm = TRUE),
    .groups = "drop"
  )

#===============================================================================
# 6. PLOT
#===============================================================================

# Combined annual plot

CMM_annual_data$type <- factor(
  CMM_annual_data$type,
  levels = c("Annual Supply", "Annual Demand")
)

GCAM_ribbons_demand_annual <- transform(
  GCAM_ribbons_annual,
  type = factor(
    "Annual Demand",
    levels = levels(CMM_annual_data$type)
  )
)

combined_plot_annual <- ggplot() +
  geom_ribbon(
    data = GCAM_ribbons_demand_annual,
    aes(x = year, ymin = ymin, ymax = ymax,
        fill = scenario_group),
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  geom_line(
    data = GCAM_lines_annual,
    aes(x = year, y = value,
        color = scenario,
        linetype = type),
    linewidth = 1
  ) +
  scale_color_manual(
    name = NULL,
    breaks = c(
      "GCAM: Reference",
      "GCAM: Steady-state supply",
      "GCAM: Unconstrained supply"
    ),
    values = c(
      "GCAM: Reference" = "#C51B7D",
      "GCAM: Steady-state supply" = "#1F77B4",
      "GCAM: Unconstrained supply" = "forestgreen"
    )
  ) +
  scale_fill_manual(
    name = "Scenario",
    breaks = c(
      "GCAM: Baseline constrained supply scenarios",
      "GCAM: Steady-state constrained supply scenarios",
      "GCAM: Unconstrained supply scenarios"
    ),
    values = c(
      "GCAM: Baseline constrained supply scenarios" = "#C51B7D",
      "GCAM: Steady-state constrained supply scenarios" = "#1F77B4",
      "GCAM: Unconstrained supply scenarios" = "forestgreen"
    )
  ) +
  new_scale_color() +
  # Supply points 
  geom_point(
    data = subset(CMM_annual_data, type == "Annual Supply"),
    aes(x = year, y = value,
        shape = type,
        color = source),
    size = 3
  ) +
  # Demand points 
  geom_point(
    data = subset(CMM_annual_data, type == "Annual Demand"),
    aes(x = year, y = value,
        shape = type,
        color = source),
    size = 3
  ) +
  scale_color_manual(
    name = "Source",
    values = source_palette_annual
  ) +
  scale_linetype_manual(
    name = "GCAM results",
    values = c(
      "Annual Supply" = "solid",
      "Annual Demand" = "solid"
    )
  ) +
  scale_shape_manual(
    name = "Point type",
    values = c(
      "Annual Supply" = 17,
      "Annual Demand" = 16
      )
  ) +
  facet_grid(
    rows = vars(mineral),
    cols = vars(type),
    scales = "free_y",
    drop = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(2025, 2100, by = 5)
  ) +
  
  labs(
    x = NULL,
    y = "Mt/yr"
  ) +
  
  theme_bw(base_size = 12) +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.key.width = unit(1, "cm"), 
    legend.spacing.y = unit(0.05, "cm"),   
    legend.key.height = unit(0.4, "cm"),   
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "grey90", colour = "grey40")
    ) +
  guides(
    fill = guide_legend(order = 4),
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 3),
    shape = guide_legend(order = 2)
  ) 


ggsave(
  "output/FigS2.png",
  combined_plot_annual,
  width = 10,  
  height = 8,   
  dpi = 300
)

#END