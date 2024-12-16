---
# Title: USF FDAC Bioretention 
# Description: Bioretention Visualization
# author: Natchaya Luangphairin
# date last revised: 11/30/24
# output: R Script
---


# Load packages and libraries ---------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(Hmisc, broom, ggpmisc, psych, tidyverse, maps, viridis, readxl, writexl, tictoc, zoo, ggplot2, ggrepel, ggthemes,patchwork, tidycensus, sf, tigris, findSVI)
library(ggplot2)
library(MASS)      # For robust regression
library(dplyr)     # For data manipulation
library(ggpmisc)   # For equation annotations

setwd("C:/Users/nluan/Box Sync/FDAC_Bioretention_project")
data <- read_excel("bioretention_master_data.xlsx")



# Define the mapping of Configuration Key to Description
description_mapping <- c(
  "1" = "Low HLR, Lower Outlet Height, No Plants",
  "2" = "Medium HLR, Lower Outlet Height, No Plants",
  "3" = "High HLR, Lower Outlet Height, No Plants",
  "4" = "High HLR, Upper Outlet Height, No Plants",
  "5" = "High HLR, Lower Outlet Height, Yes Plants",
  "6" = "Medium HLR, Upper Outlet Height, Yes Plants"
)

# Clean -------------------------------------------------------------------
data <- data %>%
  mutate(NOx = NO2+NO3)


data_mean <- data %>%
  filter(EVENT != 2) %>%
  group_by(EVENT,TANK_ID) %>%
  mutate(TIN_EVENT_TANK_AVG = mean(TIN, na.rm = TRUE),
         TIN_EVENT_TANK_SD = sd(TIN, na.rm = TRUE),
         NOx_EVENT_TANK_AVG = mean(NOx, na.rm = TRUE),
         NOx_EVENT_TANK_SD = sd(NOx, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(CONFIGURATION_ID, EVENT) %>%
  mutate(
    TIN_PERC_REMOVED = ifelse(
      TANK_ID %in% c("tank 1", "tank 2"), 
      (1 - (TIN_EVENT_TANK_AVG / TIN_EVENT_TANK_AVG[TANK_ID == "in"])) * 100,
      NA
    ),
    NOx_PERC_REMOVED = ifelse(
      TANK_ID %in% c("tank 1", "tank 2"), 
      (1 - (NOx_EVENT_TANK_AVG / NOx_EVENT_TANK_AVG[TANK_ID == "in"])) * 100,
      NA
    )
  )

data_avg <- data_mean %>%
  group_by(CONFIGURATION_ID, TANK_ID) %>%
  mutate(
    TIN_CONFIG_TANK_AVG = mean(TIN_EVENT_TANK_AVG, na.rm = TRUE),
    TIN_CONFIG_TANK_SD = sd(unique(TIN_EVENT_TANK_AVG), na.rm = TRUE), # Use unique values
    TIN_CONFIG_TANK_PERC_REMOVED_AVG = mean(TIN_PERC_REMOVED, na.rm = TRUE),
    TIN_CONFIG_TANK_PERC_REMOVED_SD = sd(unique(TIN_PERC_REMOVED), na.rm = TRUE), # Use unique values
    NOx_CONFIG_TANK_AVG = mean(NOx_EVENT_TANK_AVG, na.rm = TRUE),
    NOx_CONFIG_TANK_SD = sd(unique(NOx_EVENT_TANK_AVG), na.rm = TRUE), # Use unique values
    NOx_CONFIG_TANK_PERC_REMOVED_AVG = mean(NOx_PERC_REMOVED, na.rm = TRUE),
    NOx_CONFIG_TANK_PERC_REMOVED_SD = sd(unique(NOx_PERC_REMOVED), na.rm = TRUE) # Use unique values
  ) %>%
  ungroup() %>%
  group_by(CONFIGURATION_ID) %>% # Group by CONFIGURATION_ID
  mutate(
    TIN_PERC_TOTAL_LOAD_REMOVED = ifelse(
      TANK_ID == "tank 1" | TANK_ID == "tank 2", 
      (1 - (mean(TIN[TANK_ID %in% c("tank 1", "tank 2")], na.rm = TRUE) / 
              mean(TIN[TANK_ID == "in"], na.rm = TRUE))) * 100,
      NA
    ),
    NOx_PERC_TOTAL_LOAD_REMOVED = ifelse(
      TANK_ID == "tank 1" | TANK_ID == "tank 2", 
      (1 - (mean(NOx[TANK_ID %in% c("tank 1", "tank 2")], na.rm = TRUE) / 
              mean(NOx[TANK_ID == "in"], na.rm = TRUE))) * 100,
      NA
    )
  ) %>%
  ungroup() %>%
  group_by(CONFIGURATION_ID) %>%
  mutate(TIN_PERC_TOTAL_LOAD_REMOVED_AVG = mean(TIN_PERC_TOTAL_LOAD_REMOVED, na.rm = TRUE),
         NOx_PERC_TOTAL_LOAD_REMOVED_AVG = mean(NOx_PERC_TOTAL_LOAD_REMOVED, na.rm = TRUE),
         DESCRIPTION = description_mapping[as.character(CONFIGURATION_ID)])


data_clean <- data_avg

# By HLR ------------------------------------------------------------------
# Define configurations and corresponding groups
by_hlr_config <- c("1", "2", "3") # Configuration IDs
by_hlr_event <- c("1", "2", "3", "4", "5", "6", "7", "8", "9") # Event IDs
hlr_groups <- c(
  "Low HLR, Lower Outlet Height, No Plants",
  "Low HLR, Lower Outlet Height, No Plants",
  "Low HLR, Lower Outlet Height, No Plants",
  "Medium HLR, Lower Outlet Height, No Plants",
  "Medium HLR, Lower Outlet Height, No Plants",
  "Medium HLR, Lower Outlet Height, No Plants",
  "High HLR, Upper Outlet Height, No Plants",
  "High HLR, Upper Outlet Height, No Plants",
  "Medium HLR, Upper Outlet Height, No Plants"
)


# Define configurations and corresponding descriptions
description_mapping <- c(
  "1" = "Low HLR, Lower Outlet Height, No Plants",
  "2" = "Medium HLR, Lower Outlet Height, No Plants",
  "3" = "High HLR, Lower Outlet Height, No Plants",
  "4" = "High HLR, Upper Outlet Height, No Plants",
  "5" = "High HLR, Lower Outlet Height, Yes Plants",
  "6" = "Medium HLR, Upper Outlet Height, Yes Plants"
)

# Filter and preprocess data: keep distinct rows
data_clean_distinct <- data_clean %>%
  filter(CONFIGURATION_ID %in% by_hlr_config & EVENT %in% by_hlr_event) %>% # Filter data
  distinct(CONFIGURATION_ID, EVENT, TANK_ID, TIN_EVENT_TANK_AVG, .keep_all = TRUE) %>% # Keep distinct rows
  mutate(
    CONFIGURATION_ID = factor(CONFIGURATION_ID), # Convert CONFIGURATION_ID to a factor
    DESCRIPTION = factor(
      description_mapping[as.character(CONFIGURATION_ID)], # Map CONFIGURATION_ID to DESCRIPTION
      levels = c(
        "Low HLR, Lower Outlet Height, No Plants",
        "Medium HLR, Lower Outlet Height, No Plants",
        "High HLR, Lower Outlet Height, No Plants",
        "High HLR, Upper Outlet Height, No Plants",
        "High HLR, Lower Outlet Height, Yes Plants",
        "Medium HLR, Upper Outlet Height, Yes Plants"
      )
    ),
    TANK_LABEL = factor(
      case_when(
        TANK_ID == "in" ~ "Influent",
        TANK_ID == "tank 1" ~ "CBA Unit",
        TANK_ID == "tank 2" ~ "PBA Unit"
      ),
      levels = c("Influent", "CBA Unit", "PBA Unit")
    )
  )

# Calculate % load reduction using influent as reference
data_clean_distinct <- data_clean_distinct %>%
  group_by(EVENT, DESCRIPTION) %>%
  mutate(
    Influent_TIN = TIN_EVENT_TANK_AVG[TANK_ID == "in"],
    Load_Reduction = ifelse(
      TANK_ID != "in",
      (1 - TIN_EVENT_TANK_AVG / Influent_TIN) * 100,
      NA
    )
  ) %>%
  ungroup()

# Calculate summary statistics for Mean, Median, SD, and Max
summary_stats_hlr <- data_clean_distinct %>%
  group_by(DESCRIPTION, TANK_LABEL) %>%
  summarise(
    Mean = mean(TIN_EVENT_TANK_AVG, na.rm = TRUE),
    Median = median(TIN_EVENT_TANK_AVG, na.rm = TRUE),
    SD = sd(TIN_EVENT_TANK_AVG, na.rm = TRUE),
    Load_Reduction_Mean = ifelse(
      is.nan(mean(Load_Reduction, na.rm = TRUE)),
      NA, # Replace NaN with NA
      mean(Load_Reduction, na.rm = TRUE)
      ), # % Load reduction
    Max = max(TIN_EVENT_TANK_AVG, na.rm = TRUE), # Maximum value
    Annotation_Position = Max + 0.2, # Position annotations 1 axis value above the maximum
    .groups = "drop"
  )

# Create the boxplot with Greek letters for annotations
hlr_plot <- ggplot(data_clean_distinct, aes(x = TANK_LABEL, y = TIN_EVENT_TANK_AVG, color = CONFIGURATION_ID)) +
  geom_boxplot(alpha = 0, outlier.shape = 16, size = 0.5) + # Thinner boxplot outlines
  stat_boxplot(
    geom = "errorbar", 
    width = 0.15, # Adjust width of T-shaped caps
    size = 0.5 # Thickness of whisker lines and caps
  ) +
  geom_point(
    data = summary_stats_hlr,
    aes(
      x = TANK_LABEL,
      y = Mean
    ),
    inherit.aes = FALSE,
    shape = 4, # "x" shape
    size = 3, # Adjust the size of the marker
    color = "black" # Black color for the mean marker
  ) +
  geom_text(
    data = summary_stats_hlr,
    aes(
      x = TANK_LABEL,
      y = Annotation_Position, # Position annotations above the maximum value
      label = paste0(
        "\u03BC: ", round(Mean, 2), "\n",  # Mean as μ
        "\u03C3: ", round(SD, 2), "\n",   # SD as σ
        "\u03BD: ", round(Median, 2), "\n",      # Median as ν
        "Removal: ", round(Load_Reduction_Mean, 1), "%" # % Load reduction
      )
    ),
    inherit.aes = FALSE,
    color = "black",
    size = 4,
    vjust = 0 # Center the text at the specified position
  ) +
  facet_wrap(~ DESCRIPTION, ncol = 3) + # Facet by DESCRIPTION in the specified order
  scale_color_manual(
    values = c("1" = "darkblue", "2" = "darkred", "3" = "darkgreen", 
               "4" = "purple", "5" = "brown", "6" = "orange")
  ) + # Apply custom colors to outlines
  scale_y_continuous(
    limits = c(0, 5), # Increase y-axis limit to accommodate annotations
    breaks = seq(0, 5, by = 0.5) # Set y-axis ticks at intervals of 0.5
  ) +
  labs(
    x = NULL, # Remove x-axis label
    y = "TIN Concentrations (mg N/L)" # Set y-axis label
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14), # Adjust x-axis text
    axis.title.y = element_text(size = 15, face = "bold", margin = margin(r = 10)), # Add space to the right of y-axis label
    axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 10)), # Add space to the top of x-axis label
    strip.text = element_text(size = 15, face = "bold"), # Bold facet titles
    legend.position = "none", # Remove legend
    plot.title = element_blank(), # Remove plot title
    axis.text.y = element_text(size = 14), # Adjust y-axis text size
    strip.background = element_blank(), # Remove background from facet strips
    panel.grid.major = element_blank(), # Remove major gridlines
    panel.grid.minor = element_blank()  # Remove minor gridlines
  )

# Save the plot
ggsave("hlr_boxplot.png", hlr_plot, width = 15, height = 6, dpi = 300)


# By Outlet Height --------------------------------------------------------
by_outlet_config <- c("3", "4", "5") # config 3, 4, 5
by_outlet_event <- c("7", "8", "9", "10", "11", "12", "13" ,"14", "15") 

# Filter and preprocess data: keep distinct rows
data_clean_distinct <- data_clean %>%
  filter(CONFIGURATION_ID %in% by_outlet_config & EVENT %in% by_outlet_event) %>% # Filter data
  distinct(CONFIGURATION_ID, EVENT, TANK_ID, TIN_EVENT_TANK_AVG, .keep_all = TRUE) %>% # Keep distinct rows
  mutate(
    CONFIGURATION_ID = factor(CONFIGURATION_ID), # Convert CONFIGURATION_ID to a factor
    DESCRIPTION = factor(
      description_mapping[as.character(CONFIGURATION_ID)], # Map CONFIGURATION_ID to DESCRIPTION
      levels = c(
        "Low HLR, Lower Outlet Height, No Plants",
        "Medium HLR, Lower Outlet Height, No Plants",
        "High HLR, Lower Outlet Height, No Plants",
        "High HLR, Upper Outlet Height, No Plants",
        "High HLR, Lower Outlet Height, Yes Plants",
        "Medium HLR, Upper Outlet Height, Yes Plants"
      )
    ),
    TANK_LABEL = factor(
      case_when(
        TANK_ID == "in" ~ "Influent",
        TANK_ID == "tank 1" ~ "CBA Unit",
        TANK_ID == "tank 2" ~ "PBA Unit"
      ),
      levels = c("Influent", "CBA Unit", "PBA Unit")
    )
  )

# Calculate % load reduction using influent as reference
data_clean_distinct <- data_clean_distinct %>%
  group_by(EVENT, DESCRIPTION) %>%
  mutate(
    Influent_TIN = TIN_EVENT_TANK_AVG[TANK_ID == "in"],
    Load_Reduction = ifelse(
      TANK_ID != "in",
      (1 - TIN_EVENT_TANK_AVG / Influent_TIN) * 100,
      NA
    )
  ) %>%
  ungroup()

# Calculate summary statistics for Mean, Median, SD, and Max
summary_stats_outlet <- data_clean_distinct %>%
  group_by(DESCRIPTION, TANK_LABEL) %>%
  summarise(
    Mean = mean(TIN_EVENT_TANK_AVG, na.rm = TRUE),
    Median = median(TIN_EVENT_TANK_AVG, na.rm = TRUE),
    SD = sd(TIN_EVENT_TANK_AVG, na.rm = TRUE),
    Load_Reduction_Mean = ifelse(
      is.nan(mean(Load_Reduction, na.rm = TRUE)),
      NA, # Replace NaN with NA
      mean(Load_Reduction, na.rm = TRUE)
    ), # % Load reduction
    Max = max(TIN_EVENT_TANK_AVG, na.rm = TRUE), # Maximum value
    Annotation_Position = Max + 0.2, # Position annotations 1 axis value above the maximum
    .groups = "drop"
  )

# Create the boxplot with Greek letters for annotations
outlet_plot <- ggplot(data_clean_distinct, aes(x = TANK_LABEL, y = TIN_EVENT_TANK_AVG, color = CONFIGURATION_ID)) +
  geom_boxplot(alpha = 0, outlier.shape = 16, size = 0.5) + # Thinner boxplot outlines
  stat_boxplot(
    geom = "errorbar", 
    width = 0.15, # Adjust width of T-shaped caps
    size = 0.5 # Thickness of whisker lines and caps
  ) +
  geom_point(
    data = summary_stats_outlet,
    aes(
      x = TANK_LABEL,
      y = Mean
    ),
    inherit.aes = FALSE,
    shape = 4, # "x" shape
    size = 3, # Adjust the size of the marker
    color = "black" # Black color for the mean marker
  ) +
  geom_text(
    data = summary_stats_outlet,
    aes(
      x = TANK_LABEL,
      y = Annotation_Position, # Position annotations above the maximum value
      label = paste0(
        "\u03BC: ", round(Mean, 2), "\n",  # Mean as μ
        "\u03C3: ", round(SD, 2), "\n",   # SD as σ
        "\u03BD: ", round(Median, 2), "\n",      # Median as ν
        "Removal: ", round(Load_Reduction_Mean, 1), "%" # % Load reduction
      )
    ),
    inherit.aes = FALSE,
    color = "black",
    size = 4,
    vjust = 0 # Center the text at the specified position
  ) +
  facet_wrap(~ DESCRIPTION, ncol = 3) + # Facet by DESCRIPTION in the specified order
  scale_color_manual(
    values = c("3" = "darkblue", "4" = "darkred", "5" = "darkgreen")
  ) + # Apply custom colors to outlines
  scale_y_continuous(
    limits = c(0, 5), # Increase y-axis limit to accommodate annotations
    breaks = seq(0, 5, by = 0.5) # Set y-axis ticks at intervals of 0.5
  ) +
  labs(
    x = NULL, # Remove x-axis label
    y = "TIN Concentrations (mg N/L)" # Set y-axis label
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14), # Adjust x-axis text
    axis.title.y = element_text(size = 15, face = "bold", margin = margin(r = 10)), # Add space to the right of y-axis label
    axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 10)), # Add space to the top of x-axis label
    strip.text = element_text(size = 15, face = "bold"), # Bold facet titles
    legend.position = "none", # Remove legend
    plot.title = element_blank(), # Remove plot title
    axis.text.y = element_text(size = 14), # Adjust y-axis text size
    strip.background = element_blank(), # Remove background from facet strips
    panel.grid.major = element_blank(), # Remove major gridlines
    panel.grid.minor = element_blank()  # Remove minor gridlines
  )

# Save the plot
ggsave("outlet_boxplot.png", outlet_plot, width = 15, height = 6, dpi = 300)



# By Plant ----------------------------------------------------------------
by_plant_config <- c("6") # config 6 (optimal)
by_plant_event <- c("16", "17", "18") 

# Filter and preprocess data: keep distinct rows
data_clean_distinct <- data_clean %>%
  filter(CONFIGURATION_ID %in% by_plant_config & EVENT %in% by_plant_event) %>% # Filter data
  distinct(CONFIGURATION_ID, EVENT, TANK_ID, TIN_EVENT_TANK_AVG, .keep_all = TRUE) %>% # Keep distinct rows
  mutate(
    CONFIGURATION_ID = factor(CONFIGURATION_ID), # Convert CONFIGURATION_ID to a factor
    DESCRIPTION = factor(
      description_mapping[as.character(CONFIGURATION_ID)], # Map CONFIGURATION_ID to DESCRIPTION
      levels = c(
        "Low HLR, Lower Outlet Height, No Plants",
        "Medium HLR, Lower Outlet Height, No Plants",
        "High HLR, Lower Outlet Height, No Plants",
        "High HLR, Upper Outlet Height, No Plants",
        "High HLR, Lower Outlet Height, Yes Plants",
        "Medium HLR, Upper Outlet Height, Yes Plants"
      )
    ),
    TANK_LABEL = factor(
      case_when(
        TANK_ID == "in" ~ "Influent",
        TANK_ID == "tank 1" ~ "CBA Unit",
        TANK_ID == "tank 2" ~ "PBA Unit"
      ),
      levels = c("Influent", "CBA Unit", "PBA Unit")
    )
  )

# Calculate % load reduction using influent as reference
data_clean_distinct <- data_clean_distinct %>%
  group_by(EVENT, DESCRIPTION) %>%
  mutate(
    Influent_TIN = TIN_EVENT_TANK_AVG[TANK_ID == "in"],
    Load_Reduction = ifelse(
      TANK_ID != "in",
      (1 - TIN_EVENT_TANK_AVG / Influent_TIN) * 100,
      NA
    )
  ) %>%
  ungroup()

# Calculate summary statistics for Mean, Median, SD, and Max
summary_stats_plant <- data_clean_distinct %>%
  group_by(DESCRIPTION, TANK_LABEL) %>%
  summarise(
    Mean = mean(TIN_EVENT_TANK_AVG, na.rm = TRUE),
    Median = median(TIN_EVENT_TANK_AVG, na.rm = TRUE),
    SD = sd(TIN_EVENT_TANK_AVG, na.rm = TRUE),
    Load_Reduction_Mean = ifelse(
      is.nan(mean(Load_Reduction, na.rm = TRUE)),
      NA, # Replace NaN with NA
      mean(Load_Reduction, na.rm = TRUE)
    ), # % Load reduction
    Max = max(TIN_EVENT_TANK_AVG, na.rm = TRUE), # Maximum value
    Annotation_Position = Max + 0.7, # Position annotations 1 axis value above the maximum
    .groups = "drop"
  )

# Create the boxplot with Greek letters for annotations
plant_plot <- ggplot(data_clean_distinct, aes(x = TANK_LABEL, y = TIN_EVENT_TANK_AVG, color = CONFIGURATION_ID)) +
  geom_boxplot(alpha = 0, outlier.shape = 16, size = 0.5) + # Thinner boxplot outlines
  stat_boxplot(
    geom = "errorbar", 
    width = 0.15, # Adjust width of T-shaped caps
    size = 0.5 # Thickness of whisker lines and caps
  ) +
  geom_point(
    data = summary_stats_plant,
    aes(
      x = TANK_LABEL,
      y = Mean
    ),
    inherit.aes = FALSE,
    shape = 4, # "x" shape
    size = 3, # Adjust the size of the marker
    color = "black" # Black color for the mean marker
  ) +
  geom_text(
    data = summary_stats_plant,
    aes(
      x = TANK_LABEL,
      y = Annotation_Position, # Position annotations above the maximum value
      label = paste0(
        "\u03BC: ", round(Mean, 2), "\n",  # Mean as μ
        "\u03C3: ", round(SD, 2), "\n",   # SD as σ
        "\u03BD: ", round(Median, 2), "\n",      # Median as ν
        "Removal: ", round(Load_Reduction_Mean, 1), "%" # % Load reduction
      )
    ),
    inherit.aes = FALSE,
    color = "black",
    size = 4,
    vjust = 0 # Center the text at the specified position
  ) +
  facet_wrap(~ DESCRIPTION, ncol = 3) + # Facet by DESCRIPTION in the specified order
  scale_color_manual(
    values = c("6" = "darkorange")
  ) + # Apply custom colors to outlines
  scale_y_continuous(
    limits = c(0, 12), # Increase y-axis limit to accommodate annotations
    breaks = seq(0, 12, by = 1) # Set y-axis ticks at intervals of 0.5
  ) +
  labs(
    x = NULL, # Remove x-axis label
    y = "TIN Concentrations (mg N/L)" # Set y-axis label
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14), # Adjust x-axis text
    axis.title.y = element_text(size = 15, face = "bold", margin = margin(r = 10)), # Add space to the right of y-axis label
    axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 10)), # Add space to the top of x-axis label
    strip.text = element_text(size = 15, face = "bold"), # Bold facet titles
    legend.position = "none", # Remove legend
    plot.title = element_blank(), # Remove plot title
    axis.text.y = element_text(size = 14), # Adjust y-axis text size
    strip.background = element_blank(), # Remove background from facet strips
    panel.grid.major = element_blank(), # Remove major gridlines
    panel.grid.minor = element_blank()  # Remove minor gridlines
  )

# Save the plot
ggsave("plant_boxplot.png", plant_plot, width = 8, height = 6, dpi = 300)



# Modeling ----------------------------------------------------------------
# Define the EBCT mapping
ebct_mapping <- c(
  "1" = 496.6772727,
  "2" = 248.3386364,
  "3" = 99.33545455,
  "4" = 145.3240909,
  "5" = 99.33545455,
  "6" = 363.3102273
)

# Add the EBCT column to data_clean_NLR
data_clean_NLR <- data_clean %>%
  group_by(CONFIGURATION_ID, EVENT) %>%
  mutate(
    EBCT = as.numeric(ebct_mapping[as.character(CONFIGURATION_ID)]), # Map EBCT values based on CONFIGURATION_ID
    NLR = (TIN_EVENT_TANK_AVG[TANK_ID == "in"]/EBCT/1000)/1000*(10^6)*1440, # in g/m^3/day
    NRR = ((TIN_EVENT_TANK_AVG[TANK_ID == "in"] - TIN_EVENT_TANK_AVG)/EBCT/1000)/1000*(10^6)*1440, # in g/m^3/day,
    NOxLR = (NOx_EVENT_TANK_AVG[TANK_ID == "in"]/EBCT/1000)/1000*(10^6)*1440, # in g/m^3/day
    NOxRR = ((NOx_EVENT_TANK_AVG[TANK_ID == "in"] - NOx_EVENT_TANK_AVG)/EBCT/1000)/1000*(10^6)*1440 # in g/m^3/day
  )

# Filter and preprocess for tank-specific plots
data_clean_NLR_distinct <- data_clean_NLR %>%
  filter(!is.na(NLR) & !is.na(NRR) & !is.na(NOxLR) & !is.na(NOxRR) & TANK_ID %in% c("tank 1", "tank 2")) %>% # Keep relevant tanks
  distinct(CONFIGURATION_ID, EVENT, TANK_ID, NLR, NRR, NOxLR, NOxRR, .keep_all = TRUE) %>%
  mutate(
    TANK_LABEL = case_when(
      TANK_ID == "tank 1" ~ "CBA Unit",
      TANK_ID == "tank 2" ~ "PBA Unit"
    ),
    CONFIGURATION_ID = factor(CONFIGURATION_ID) # Ensure CONFIGURATION_ID is a factor
  )

# Step 1: Fit the initial model to calculate residuals
data_clean_NLR_distinct <- data_clean_NLR_distinct %>%
  group_by(TANK_LABEL) %>%
  mutate(
    Initial_Log_Model = list(lm(NRR ~ log(NLR), data = cur_data())),
    Residuals = NRR - predict(Initial_Log_Model[[1]])
  ) %>%
  ungroup()

# Step 2: Identify and remove outliers
# IQR measures the spread of the middle 50% of the data.
# Count points before outlier removal, grouped by TANK_LABEL
before_removal <- data_clean_NLR_distinct %>%
  group_by(TANK_LABEL) %>%
  summarise(Total_Points = n(), .groups = "drop")

# Perform outlier removal
data_clean_NLR_distinct <- data_clean_NLR_distinct %>%
  filter(TANK_ID == "tank 1") %>%
  group_by(TANK_LABEL) %>%
  mutate(
    Q1 = quantile(Residuals, 0.25, na.rm = TRUE),
    Q3 = quantile(Residuals, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR
  ) %>%
  filter(Residuals >= Lower_Bound & Residuals <= Upper_Bound) %>% # Keep non-outliers
  ungroup()

# Count points after outlier removal, grouped by TANK_LABEL
after_removal <- data_clean_NLR_distinct %>%
  group_by(TANK_LABEL) %>%
  summarise(Final_Points = n(), .groups = "drop")

# Merge the counts and calculate the dropped points
summary_table <- before_removal %>%
  left_join(after_removal, by = "TANK_LABEL") %>%
  mutate(
    Points_Dropped = Total_Points - Final_Points
  )

# Display the result
print(summary_table)



# --- 1. Standard Linear Regression ---
linear_model <- lm(NRR ~ NLR, data = data_clean_NLR_distinct)
linear_r2 <- summary(linear_model)$r.squared
linear_coeff <- coef(linear_model)

# --- 2. Cook's Distance ---
# Calculate Cook's distance
cooks_dist <- cooks.distance(linear_model)
threshold <- 4 / nrow(data_clean_NLR_distinct) # Common threshold
data_no_influential <- data_clean_NLR_distinct[cooks_dist < threshold, ]
refit_model <- lm(NRR ~ NLR, data = data_no_influential)
refit_r2 <- summary(refit_model)$r.squared
refit_coeff <- coef(refit_model)

# --- 3. Robust Regression ---
robust_model <- rlm(NRR ~ NLR, data = data_clean_NLR_distinct)
robust_coeff <- coef(robust_model)
# Calculate R^2 for robust regression
robust_pred <- predict(robust_model, data_clean_NLR_distinct)
robust_r2 <- 1 - sum((data_clean_NLR_distinct$NRR - robust_pred)^2) / 
  sum((data_clean_NLR_distinct$NRR - mean(data_clean_NLR_distinct$NRR))^2)

# --- 4. Winsorization ---
data_winsorized <- data_clean_NLR_distinct %>%
  mutate(
    NLR = ifelse(NLR < quantile(NLR, 0.25), quantile(NLR, 0.25),
                 ifelse(NLR > quantile(NLR, 0.75), quantile(NLR, 0.75), NLR)),
    NRR = ifelse(NRR < quantile(NRR, 0.25), quantile(NRR, 0.25),
                 ifelse(NRR > quantile(NRR, 0.75), quantile(NRR, 0.75), NRR))
  )
winsor_model <- lm(NRR ~ NLR, data = data_winsorized)
winsor_r2 <- summary(winsor_model)$r.squared
winsor_coeff <- coef(winsor_model)

# --- 5. Log Transformation ---
data_transformed <- data_clean_NLR_distinct %>%
  mutate(NLR_log = log(NLR), NRR_log = log(NRR))
log_model <- lm(NRR_log ~ NLR_log, data = data_transformed)
log_r2 <- summary(log_model)$r.squared
log_coeff <- coef(log_model)

# --- Plot Comparison ---
linear_compare <- ggplot(data_clean_NLR_distinct, aes(x = NLR, y = NRR)) +
  geom_point(size = 2, alpha = 0.8, color = "black") +
  
  # Standard linear regression line
  geom_abline(
    intercept = linear_coeff[1],
    slope = linear_coeff[2],
    linetype = "solid",
    color = "black",
    size = 1
  ) +
  annotate("text", x = max(data_clean_NLR_distinct$NLR) * 0.7, 
           y = max(data_clean_NLR_distinct$NRR) * 0.9, 
           label = paste0("Linear: NRR = ", round(linear_coeff[1], 2), 
                          " + ", round(linear_coeff[2], 2), " * NLR\n",
                          "R² = ", round(linear_r2, 2)),
           color = "black", size = 4, hjust = 0) +
  
  # Refit model line (Cook's distance)
  geom_abline(
    intercept = refit_coeff[1],
    slope = refit_coeff[2],
    linetype = "dotted",
    color = "darkred",
    size = 1
  ) +
  annotate("text", x = max(data_clean_NLR_distinct$NLR) * 0.75, 
           y = max(data_clean_NLR_distinct$NRR) * 0.75, 
           label = paste0("Refit: NRR = ", round(refit_coeff[1], 2), 
                          " + ", round(refit_coeff[2], 2), " * NLR\n",
                          "R² = ", round(refit_r2, 2)),
           color = "darkred", size = 4, hjust = 0) +
  
  # Robust regression line
  geom_abline(
    intercept = robust_coeff[1],
    slope = robust_coeff[2],
    linetype = "dashed",
    color = "darkblue",
    size = 1
  ) +
  annotate("text", x = max(data_clean_NLR_distinct$NLR) * 0.7, 
           y = max(data_clean_NLR_distinct$NRR) * 0.6, 
           label = paste0("Robust: NRR = ", round(robust_coeff[1], 2), 
                          " + ", round(robust_coeff[2], 2), " * NLR\n",
                          "R² = ", round(robust_r2, 2)),
           color = "darkblue", size = 4, hjust = 0) +
  
  # Winsorization line
  geom_abline(
    intercept = winsor_coeff[1],
    slope = winsor_coeff[2],
    linetype = "twodash",
    color = "darkgreen",
    size = 1
  ) +
  annotate("text", x = max(data_clean_NLR_distinct$NLR) * 0.7, 
           y = max(data_clean_NLR_distinct$NRR) * 0.45, 
           label = paste0("Winsorized: NRR = ", round(winsor_coeff[1], 2), 
                          " + ", round(winsor_coeff[2], 2), " * NLR\n",
                          "R² = ", round(winsor_r2, 2)),
           color = "darkgreen", size = 4, hjust = 0) +
  
  # Log-transformed regression line
  geom_line(
    data = data_transformed,
    aes(x = exp(NLR_log), y = exp(predict(log_model, data_transformed))),
    linetype = "dotdash",
    color = "orange",
    size = 1,
    inherit.aes = FALSE
  ) +
  scale_x_continuous(
    limits = c(0, 50), 
    breaks = seq(0, 50, by = 5) 
  ) +
  scale_y_continuous(
    limits = c(0, 30), # Increase y-axis limit to accommodate annotations
    breaks = seq(0, 30, by = 5) # Set y-axis ticks at intervals of 0.5
  ) +
  annotate("text", x = max(data_clean_NLR_distinct$NLR) * 0.7, 
           y = max(data_clean_NLR_distinct$NRR) * 0.3, 
           label = paste0("Log-Transformed:\nlog(NRR) = ", round(log_coeff[1], 2), 
                          " + ", round(log_coeff[2], 2), " * log(NLR)\n",
                          "R² = ", round(log_r2, 2)),
           color = "orange", size = 4, hjust = 0) +
  
  labs(
    x = "Total Nitrogen Loading Rate (NLR, mg/L-day)",
    y = "Total Nitrogen Removal Rate (NRR, mg/L-day)",
    title = "Comparison of Outlier Removal Methods Using Linear Regression"
  ) +
  theme_bw(base_family = "Arial") +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )

# Save the plot
ggsave("linear_compare_CBA_outlierremoved.png", linear_compare, width = 8, height = 6, dpi = 300)



# TIN Logarithmic plot NLR, NRR -----------------------------------------------
# Filter and preprocess for tank-specific plots
data_clean_NLR_distinct <- data_clean_NLR %>%
  filter(!is.na(NLR) & !is.na(NRR) & !is.na(NOxLR) & !is.na(NOxRR) & TANK_ID %in% c("tank 1", "tank 2")) %>% # Keep relevant tanks
  distinct(CONFIGURATION_ID, EVENT, TANK_ID, NLR, NRR, NOxLR, NOxRR, .keep_all = TRUE) %>%
  mutate(
    TANK_LABEL = case_when(
      TANK_ID == "tank 1" ~ "CBA Unit",
      TANK_ID == "tank 2" ~ "PBA Unit"
    ),
    CONFIGURATION_ID = factor(CONFIGURATION_ID) # Ensure CONFIGURATION_ID is a factor
  )


data_clean_NLR_distinct <- data_clean_NLR_distinct %>%
  filter(!EVENT %in% c("16", "17", "18"))


# Step 1: Fit the initial model to calculate residuals
data_clean_NLR_distinct <- data_clean_NLR_distinct %>%
  group_by(TANK_LABEL) %>%
  mutate(
    Initial_Log_Model = list(lm(NRR ~ log(NLR), data = cur_data())),
    Residuals = NRR - predict(Initial_Log_Model[[1]])
  ) %>%
  ungroup()

# Step 2: Identify and remove outliers
# IQR measures the spread of the middle 50% of the data.
before_removal <- data_clean_NLR_distinct %>%
  group_by(TANK_LABEL) %>%
  summarise(Total_Points = n(), .groups = "drop")

# Perform outlier removal
data_clean_NLR_distinct <- data_clean_NLR_distinct %>%
  group_by(TANK_LABEL) %>%
  mutate(
    Q1 = quantile(Residuals, 0.25, na.rm = TRUE),
    Q3 = quantile(Residuals, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR
  ) %>%
  filter(Residuals >= Lower_Bound & Residuals <= Upper_Bound) %>% # Keep non-outliers
  ungroup()

# Count points after outlier removal, grouped by TANK_LABEL
after_removal <- data_clean_NLR_distinct %>%
  group_by(TANK_LABEL) %>%
  summarise(Final_Points = n(), .groups = "drop")

# Merge the counts and calculate the dropped points
summary_table <- before_removal %>%
  left_join(after_removal, by = "TANK_LABEL") %>%
  mutate(
    Points_Dropped = Total_Points - Final_Points
  )

# Display the result
print(summary_table)

# Fit logarithmic regression models by TANK_LABEL
log_models <- data_clean_NLR_distinct %>%
  group_by(TANK_LABEL) %>%
  summarise(
    log_model = list(lm(NRR ~ log(NLR), data = cur_data())),
    .groups = "drop"
  )

# Extract coefficients and R-squared for each tank
log_summary <- log_models %>%
  group_by(TANK_LABEL) %>%
  mutate(
    Intercept = map_dbl(log_model, ~ coef(.x)[1]),
    Slope = map_dbl(log_model, ~ coef(.x)[2]),
    R2 = map_dbl(log_model, ~ summary(.x)$r.squared)
  )

log_summary <- log_summary %>%
  mutate(
    Equation = paste0(
      "NRR = ", round(Slope, 2), "log(NLR)",
      " + ", round(Intercept, 2), 
      "\nR² = ", format(round(R2, 2), nsmall = 2) # Ensure two decimal places
    )
  )


# Add predictions back to the dataset
data_clean_NLR_distinct <- data_clean_NLR_distinct %>%
  group_by(TANK_LABEL) %>%
  left_join(
    log_summary, by = "TANK_LABEL"
  ) %>%
  mutate(
    Log_Predicted_NRR = Intercept + Slope * log(NLR) # Generate predicted values
  )

# Plot with untransformed x-axis and logarithmic regression line
TIN_log_plot_model <- ggplot(data_clean_NLR_distinct, aes(x = NLR, y = NRR)) +
  geom_point(size = 2, alpha = 0.8, color = "black") + # Scatter points
  geom_line(aes(y = Log_Predicted_NRR), color = "blue", size = 1, linetype = "dashed") + # Logarithmic regression line
  facet_wrap(~ TANK_LABEL, scales = "free") + # Separate facets for CBA and PBA
  geom_text(
    data = log_summary,
    aes(
      x = 5, # Set x position for annotation, adjust as needed
      y = 18, # Set y position for annotation, adjust as needed
      label = Equation
    ),
    inherit.aes = FALSE,
    color = "blue",
    size = 4,
    hjust = 0
  ) +
  scale_x_continuous(
    limits = c(0, 45), # Set x-axis limits
    breaks = seq(0, 45, by = 5)
  ) +
  scale_y_continuous(
    limits = c(0, 20), # Set y-axis limits
    breaks = seq(0, 20, by = 2)
  ) +
  labs(
    x = "Total Nitrogen Loading Rate (mg/L-day)",
    y = "Total Nitrogen Removal Rate (mg/L-day)",
    #caption = "Regression line follows a logarithmic model"
  ) +
  theme_bw(base_family = "Arial") +
  theme(
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 15, face = "bold", margin = margin(r = 10)),
    axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 10)),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "none",
    plot.caption = element_text(size = 10, hjust = 0.5),
    axis.text.y = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Save the plot
ggsave("TIN_log_plot_model.png", TIN_log_plot_model, width = 10, height = 6, dpi = 300)



# NOx Logarithmic plot NOxLR, NOxRR -------------------------------------------
# Filter and preprocess for tank-specific plots
data_clean_NOxLR_distinct <- data_clean_NLR %>%
  filter(!is.na(NOxLR) & !is.na(NOxRR) & TANK_ID %in% c("tank 1", "tank 2")) %>% # Keep relevant tanks
  distinct(CONFIGURATION_ID, EVENT, TANK_ID, NOxLR, NOxRR, .keep_all = TRUE) %>%
  mutate(
    TANK_LABEL = case_when(
      TANK_ID == "tank 1" ~ "CBA Unit",
      TANK_ID == "tank 2" ~ "PBA Unit"
    ),
    CONFIGURATION_ID = factor(CONFIGURATION_ID) # Ensure CONFIGURATION_ID is a factor
  )


data_clean_NOxLR_distinct <- data_clean_NOxLR_distinct %>%
  filter(!EVENT %in% c("16", "17", "18"))


# Step 1: Fit the initial model to calculate residuals
data_clean_NOxLR_distinct <- data_clean_NOxLR_distinct %>%
  group_by(TANK_LABEL) %>%
  mutate(
    Initial_Log_Model = list(lm(NOxRR ~ log(NOxLR), data = cur_data())),
    Residuals = NOxRR - predict(Initial_Log_Model[[1]])
  ) %>%
  ungroup()

# Step 2: Identify and remove outliers
# IQR measures the spread of the middle 50% of the data.
before_removal <- data_clean_NOxLR_distinct %>%
  group_by(TANK_LABEL) %>%
  summarise(Total_Points = n(), .groups = "drop")

# Perform outlier removal
data_clean_NOxLR_distinct <- data_clean_NOxLR_distinct %>%
  group_by(TANK_LABEL) %>%
  mutate(
    Q1 = quantile(Residuals, 0.25, na.rm = TRUE),
    Q3 = quantile(Residuals, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR
  ) %>%
  filter(Residuals >= Lower_Bound & Residuals <= Upper_Bound) %>% # Keep non-outliers
  ungroup()

# Count points after outlier removal, grouped by TANK_LABEL
after_removal <- data_clean_NOxLR_distinct %>%
  group_by(TANK_LABEL) %>%
  summarise(Final_Points = n(), .groups = "drop")

# Merge the counts and calculate the dropped points
summary_table <- before_removal %>%
  left_join(after_removal, by = "TANK_LABEL") %>%
  mutate(
    Points_Dropped = Total_Points - Final_Points
  )

# Display the result
print(summary_table)


# Fit logarithmic regression models by TANK_LABEL
log_models <- data_clean_NOxLR_distinct %>%
  group_by(TANK_LABEL) %>%
  summarise(
    log_model = list(lm(NOxRR ~ log(NOxLR), data = cur_data())),
    .groups = "drop"
  )

# Extract coefficients and R-squared for each tank
log_summary <- log_models %>%
  group_by(TANK_LABEL) %>%
  mutate(
    Intercept = map_dbl(log_model, ~ coef(.x)[1]),
    Slope = map_dbl(log_model, ~ coef(.x)[2]),
    R2 = map_dbl(log_model, ~ summary(.x)$r.squared)
  )

log_summary <- log_summary %>%
  mutate(
    Equation = paste0(
      "NOxRR = ", round(Slope, 2), "log(NOxLR)",
      " + ", round(Intercept, 2), 
      "\nR² = ", format(round(R2, 2), nsmall = 2) # Ensure two decimal places
    )
  )

# Add predictions back to the dataset
data_clean_NOxLR_distinct <- data_clean_NOxLR_distinct %>%
  group_by(TANK_LABEL) %>%
  left_join(
    log_summary, by = "TANK_LABEL"
  ) %>%
  mutate(
    Log_Predicted_NOxRR = Intercept + Slope * log(NOxLR) # Generate predicted values
  )

# Plot with untransformed x-axis and logarithmic regression line
NOx_log_plot_model <- ggplot(data_clean_NOxLR_distinct, aes(x = NOxLR, y = NOxRR)) +
  geom_point(size = 2, alpha = 0.8, color = "black") + # Scatter points
  geom_line(aes(y = Log_Predicted_NOxRR), color = "blue", size = 1, linetype = "dashed") + # Logarithmic regression line
  facet_wrap(~ TANK_LABEL, scales = "free") + # Separate facets for CBA and PBA
  geom_text(
    data = log_summary,
    aes(
      x = 5, # Set x position for annotation
      y = 18, # Set y position for annotation
      label = Equation
    ),
    inherit.aes = FALSE,
    color = "blue",
    size = 4,
    hjust = 0
  ) +
  scale_x_continuous(
    limits = c(0, 45), # Set x-axis limits
    breaks = seq(0, 45, by = 5)
  ) +
  scale_y_continuous(
    limits = c(0, 20), # Set y-axis limits
    breaks = seq(0, 20, by = 2)
  ) +
  labs(
    x = "NOx-N Loading Rate (mg/L-day)",
    y = "NOx-N Removal Rate (mg/L-day)",
    #caption = "Regression line follows a logarithmic model"
  ) +
  theme_bw(base_family = "Arial") +
  theme(
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 15, face = "bold", margin = margin(r = 10)),
    axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 10)),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "none",
    plot.caption = element_text(size = 10, hjust = 0.5),
    axis.text.y = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Save the plot
ggsave("NOx_log_plot_model.png", NOx_log_plot_model, width = 10, height = 6, dpi = 300)



# TIN Modeling Linear ----------------------------------------------------------------
# Define the EBCT mapping
ebct_mapping <- c(
  "1" = 496.6772727,
  "2" = 248.3386364,
  "3" = 99.33545455,
  "4" = 145.3240909,
  "5" = 99.33545455,
  "6" = 363.3102273
)

# Add the EBCT column to data_clean_NLR
data_clean_NLR <- data_clean %>%
  group_by(CONFIGURATION_ID, EVENT) %>%
  mutate(
    EBCT = as.numeric(ebct_mapping[as.character(CONFIGURATION_ID)]), # Map EBCT values based on CONFIGURATION_ID
    NLR = (TIN_EVENT_TANK_AVG[TANK_ID == "in"]/EBCT/1000)/1000*(10^6)*1440, # in g/m^3/day
    NRR = ((TIN_EVENT_TANK_AVG[TANK_ID == "in"] - TIN_EVENT_TANK_AVG)/EBCT/1000)/1000*(10^6)*1440, # in g/m^3/day,
    NOxLR = (NOx_EVENT_TANK_AVG[TANK_ID == "in"]/EBCT/1000)/1000*(10^6)*1440, # in g/m^3/day
    NOxRR = ((NOx_EVENT_TANK_AVG[TANK_ID == "in"] - NOx_EVENT_TANK_AVG)/EBCT/1000)/1000*(10^6)*1440 # in g/m^3/day
  )


# Filter and preprocess data
data_clean_NLR_distinct <- data_clean_NLR %>%
  filter(!is.na(NLR) & !is.na(NRR) & TANK_ID %in% c("tank 1", "tank 2")) %>%
  distinct(CONFIGURATION_ID, EVENT, TANK_ID, NLR, NRR, .keep_all = TRUE) %>%
  mutate(
    TANK_LABEL = case_when(
      TANK_ID == "tank 1" ~ "CBA Unit",
      TANK_ID == "tank 2" ~ "PBA Unit"
    ),
    CONFIGURATION_ID = factor(CONFIGURATION_ID) # Ensure CONFIGURATION_ID is a factor
  )

# Step 1: Fit the initial model to calculate residuals
data_clean_NLR_distinct <- data_clean_NLR_distinct %>%
  group_by(TANK_LABEL) %>%
  mutate(
    Initial_Linear_Model = list(lm(NRR ~ NLR, data = cur_data())),
    Residuals = NRR - predict(Initial_Linear_Model[[1]])
  ) %>%
  ungroup()

# Step 2: Remove outliers using IQR
before_removal <- data_clean_NLR_distinct %>%
  group_by(TANK_LABEL) %>%
  summarise(Total_Points = n(), .groups = "drop")

# Perform outlier removal
data_clean_NLR_distinct <- data_clean_NLR_distinct %>%
  group_by(TANK_LABEL) %>%
  mutate(
    Q1 = quantile(Residuals, 0.25, na.rm = TRUE),
    Q3 = quantile(Residuals, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR
  ) %>%
  filter(Residuals >= Lower_Bound & Residuals <= Upper_Bound) %>% # Keep non-outliers
  ungroup()

# Count points after outlier removal, grouped by TANK_LABEL
after_removal <- data_clean_NLR_distinct %>%
  group_by(TANK_LABEL) %>%
  summarise(Final_Points = n(), .groups = "drop")

# Merge the counts and calculate the dropped points
summary_table <- before_removal %>%
  left_join(after_removal, by = "TANK_LABEL") %>%
  mutate(
    Points_Dropped = Total_Points - Final_Points
  )

# Display the result
print(summary_table)


# Step 3: Fit linear regression models by TANK_LABEL
linear_models <- data_clean_NLR_distinct %>%
  group_by(TANK_LABEL) %>%
  summarise(
    linear_model = list(lm(NRR ~ NLR, data = cur_data())),
    .groups = "drop"
  )

# Step 4: Extract coefficients and R^2 values
linear_summary <- linear_models %>%
  mutate(
    Intercept = map_dbl(linear_model, ~ coef(.x)[1]),
    Slope = map_dbl(linear_model, ~ coef(.x)[2]),
    R2 = map_dbl(linear_model, ~ summary(.x)$r.squared),
    Equation = paste0(
      "NRR = ", round(Slope, 2), "NLR + ", round(Intercept, 2), "\n",
      "R² = ", format(round(R2, 2), nsmall = 2)
    )
  )

# Step 5: Add predictions back to the dataset
data_clean_NLR_distinct <- data_clean_NLR_distinct %>%
  left_join(linear_summary, by = "TANK_LABEL") %>%
  mutate(
    Predicted_NRR = Intercept + Slope * NLR # Predicted NRR
  )

# Step 6: Plot with regression equation and R^2
TIN_log_plot_model <- ggplot(data_clean_NLR_distinct, aes(x = NLR, y = NRR)) +
  geom_point(size = 2, alpha = 0.8, color = "black") +
  geom_line(aes(y = Predicted_NRR), color = "blue", size = 1, linetype = "dashed") +
  scale_x_continuous(
    limits = c(0, 45), # Set x-axis limits
    breaks = seq(0, 45, by = 5)
  ) +
  scale_y_continuous(
    limits = c(0, 24), # Set y-axis limits
    breaks = seq(0, 24, by = 2)
  ) +
  geom_text(
    data = linear_summary,
    aes(x = 5, y = max(data_clean_NLR_distinct$NRR, na.rm = TRUE) * 0.9, label = Equation),
    inherit.aes = FALSE,
    color = "blue",
    size = 4,
    hjust = 0
  ) +
  facet_wrap(~ TANK_LABEL, scales = "free") +
  labs(
    x = "Total Nitrogen Loading Rate (mg/L-day)",
    y = "TOtal Nitrogen Removal Rate (mg/L-day)",
    #title = "Linear Regression with Outlier Removal"
  ) +
  theme_bw(base_family = "Arial") +
  theme(
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 15, face = "bold", margin = margin(r = 10)),
    axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 10)),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "none",
    plot.caption = element_text(size = 10, hjust = 0.5),
    axis.text.y = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Save the plot
ggsave("TIN_linear_plot_model.png", TIN_log_plot_model, width = 10, height = 6, dpi = 300)



# NOx Modeling Linear ----------------------------------------------------------------
# Define the EBCT mapping
ebct_mapping <- c(
  "1" = 496.6772727,
  "2" = 248.3386364,
  "3" = 99.33545455,
  "4" = 145.3240909,
  "5" = 99.33545455,
  "6" = 363.3102273
)

# Add the EBCT column to data_clean_NOxLR
data_clean_NOxLR <- data_clean %>%
  group_by(CONFIGURATION_ID, EVENT) %>%
  mutate(
    EBCT = as.numeric(ebct_mapping[as.character(CONFIGURATION_ID)]), # Map EBCT values based on CONFIGURATION_ID
    NLR = (TIN_EVENT_TANK_AVG[TANK_ID == "in"]/EBCT/1000)/1000*(10^6)*1440, # in g/m^3/day
    NRR = ((TIN_EVENT_TANK_AVG[TANK_ID == "in"] - TIN_EVENT_TANK_AVG)/EBCT/1000)/1000*(10^6)*1440, # in g/m^3/day,
    NOxLR = (NOx_EVENT_TANK_AVG[TANK_ID == "in"]/EBCT/1000)/1000*(10^6)*1440, # in g/m^3/day
    NOxRR = ((NOx_EVENT_TANK_AVG[TANK_ID == "in"] - NOx_EVENT_TANK_AVG)/EBCT/1000)/1000*(10^6)*1440 # in g/m^3/day
  )


# Filter and preprocess data
data_clean_NOxLR_distinct <- data_clean_NOxLR %>%
  filter(!is.na(NOxLR) & !is.na(NOxRR) & TANK_ID %in% c("tank 1", "tank 2")) %>%
  distinct(CONFIGURATION_ID, EVENT, TANK_ID, NOxLR, NOxRR, .keep_all = TRUE) %>%
  mutate(
    TANK_LABEL = case_when(
      TANK_ID == "tank 1" ~ "CBA Unit",
      TANK_ID == "tank 2" ~ "PBA Unit"
    ),
    CONFIGURATION_ID = factor(CONFIGURATION_ID) # Ensure CONFIGURATION_ID is a factor
  )

# Step 1: Fit the initial model to calculate residuals
data_clean_NOxLR_distinct <- data_clean_NOxLR_distinct %>%
  group_by(TANK_LABEL) %>%
  mutate(
    Initial_Linear_Model = list(lm(NOxRR ~ NOxLR, data = cur_data())),
    Residuals = NOxRR - predict(Initial_Linear_Model[[1]])
  ) %>%
  ungroup()

# Step 2: Remove outliers using IQR
before_removal <- data_clean_NOxLR_distinct %>%
  group_by(TANK_LABEL) %>%
  summarise(Total_Points = n(), .groups = "drop")

# Perform outlier removal
data_clean_NOxLR_distinct <- data_clean_NOxLR_distinct %>%
  group_by(TANK_LABEL) %>%
  mutate(
    Q1 = quantile(Residuals, 0.25, na.rm = TRUE),
    Q3 = quantile(Residuals, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR
  ) %>%
  filter(Residuals >= Lower_Bound & Residuals <= Upper_Bound) %>% # Keep non-outliers
  ungroup()

# Count points after outlier removal, grouped by TANK_LABEL
after_removal <- data_clean_NOxLR_distinct %>%
  group_by(TANK_LABEL) %>%
  summarise(Final_Points = n(), .groups = "drop")

# Merge the counts and calculate the dropped points
summary_table <- before_removal %>%
  left_join(after_removal, by = "TANK_LABEL") %>%
  mutate(
    Points_Dropped = Total_Points - Final_Points
  )

# Display the result
print(summary_table)


# Step 3: Fit linear regression models by TANK_LABEL
linear_models <- data_clean_NOxLR_distinct %>%
  group_by(TANK_LABEL) %>%
  summarise(
    linear_model = list(lm(NOxRR ~ NOxLR, data = cur_data())),
    .groups = "drop"
  )

# Step 4: Extract coefficients and R^2 values
linear_summary <- linear_models %>%
  mutate(
    Intercept = map_dbl(linear_model, ~ coef(.x)[1]),
    Slope = map_dbl(linear_model, ~ coef(.x)[2]),
    R2 = map_dbl(linear_model, ~ summary(.x)$r.squared),
    Equation = paste0(
      "NOxRR = ", round(Slope, 2), "NOxLR + ", round(Intercept, 2), "\n",
      "R² = ", format(round(R2, 2), nsmall = 2)
    )
  )

# Step 5: Add predictions back to the dataset
data_clean_NOxLR_distinct <- data_clean_NOxLR_distinct %>%
  left_join(linear_summary, by = "TANK_LABEL") %>%
  mutate(
    Predicted_NOxRR = Intercept + Slope * NOxLR # Predicted NOxRR
  )

# Step 6: Plot with regression equation and R^2
NOx_log_plot_model <- ggplot(data_clean_NOxLR_distinct, aes(x = NOxLR, y = NOxRR)) +
  geom_point(size = 2, alpha = 0.8, color = "black") +
  geom_line(aes(y = Predicted_NOxRR), color = "blue", size = 1, linetype = "dashed") +
  scale_x_continuous(
    limits = c(0, 45), # Set x-axis limits
    breaks = seq(0, 45, by = 5)
  ) +
  scale_y_continuous(
    limits = c(0, 24), # Set y-axis limits
    breaks = seq(0, 24, by = 2)
  ) +
  geom_text(
    data = linear_summary,
    aes(x = 5, y = max(data_clean_NOxLR_distinct$NOxRR, na.rm = TRUE) * 0.9, label = Equation),
    inherit.aes = FALSE,
    color = "blue",
    size = 4,
    hjust = 0
  ) +
  facet_wrap(~ TANK_LABEL, scales = "free") +
  labs(
    x = "Total Nitrogen Loading Rate (mg/L-day)",
    y = "TOtal Nitrogen Removal Rate (mg/L-day)",
    #title = "Linear Regression with Outlier Removal"
  ) +
  theme_bw(base_family = "Arial") +
  theme(
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 15, face = "bold", margin = margin(r = 10)),
    axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 10)),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "none",
    plot.caption = element_text(size = 10, hjust = 0.5),
    axis.text.y = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Save the plot
ggsave("NOx_linear_plot_model.png", NOx_log_plot_model, width = 10, height = 6, dpi = 300)



# Summary Table with mean and sd ------------------------------------------
# Define the EBCT mapping
ebct_mapping <- c(
  "1" = 496.6772727,
  "2" = 248.3386364,
  "3" = 99.33545455,
  "4" = 145.3240909,
  "5" = 99.33545455,
  "6" = 363.3102273
)

# Add the EBCT column to data_clean_NOxLR
data_clean_NOxLR <- data_clean %>%
  group_by(CONFIGURATION_ID, EVENT) %>%
  mutate(
    EBCT = as.numeric(ebct_mapping[as.character(CONFIGURATION_ID)]), # Map EBCT values based on CONFIGURATION_ID
    NLR = (TIN_EVENT_TANK_AVG[TANK_ID == "in"]/EBCT/1000)/1000*(10^6)*1440, # in g/m^3/day
    NRR = ((TIN_EVENT_TANK_AVG[TANK_ID == "in"] - TIN_EVENT_TANK_AVG)/EBCT/1000)/1000*(10^6)*1440, # in g/m^3/day,
    NOxLR = (NOx_EVENT_TANK_AVG[TANK_ID == "in"]/EBCT/1000)/1000*(10^6)*1440, # in g/m^3/day
    NOxRR = ((NOx_EVENT_TANK_AVG[TANK_ID == "in"] - NOx_EVENT_TANK_AVG)/EBCT/1000)/1000*(10^6)*1440 # in g/m^3/day
  )

NOx_data_config_avg_sd <- data_clean_NOxLR %>%
  group_by(TANK_ID) %>%
  summarise(
    Average_NOx = mean(NOx_CONFIG_TANK_AVG, na.rm = TRUE), # Calculate average (mean) of NOx
    SD_NOx = sd(NOx_CONFIG_TANK_AVG, na.rm = TRUE),       # Calculate standard deviation (SD) of NOx
    Average_Perc_NOxRR = mean(NOx_CONFIG_TANK_PERC_REMOVED_AVG, na.rm = TRUE), # Calculate average (mean) of NOxRR
    SD_Perc_NOxRR = sd(NOx_CONFIG_TANK_PERC_REMOVED_AVG, na.rm = TRUE),       # Calculate standard deviation (SD) of NOxRR
    .groups = "drop"                          # Ensure ungrouped output
  )

# Print the resulting data frame
View(NOx_data_config_avg_sd)

  

