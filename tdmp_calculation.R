library(dplyr)
library(ggplot2)

crop <- 'Maize'

# Load RUE data by crop
RUE <- read.csv('https://raw.githubusercontent.com/DS4Ag/Introduction_PBCMs/refs/heads/main/RUE_crop.csv')

# Filter the crop defined RUE
rue_maize_value <- RUE %>%
      filter(Crop_species == crop) %>%
      pull(`RUE..g..MJ..1.`)

# Load ACRE radiation data
SRAD_acre <- read.csv('https://raw.githubusercontent.com/DS4Ag/Introduction_PBCMs/refs/heads/main/SRAD_acre.csv')

# Convert the Date column to datetime format
SRAD_acre$Date <- as.Date(SRAD_acre$Date, format = "%m/%d/%Y")

# Step 1: Convert W/m² to MJ/m²/day
SRAD_acre <- SRAD_acre %>%
      mutate(
            SRAD_MJ = Solar.Radiation..W.m.2. * 3600 * 24 / 1e6
      )

# Step 2: Calculate PAR
SRAD_acre <- SRAD_acre %>%
      mutate(
            PAR = SRAD_MJ * 0.5
      )

# Step 3: Calculate DDMP
SRAD_acre <- SRAD_acre %>%
      mutate(
            DDMP = PAR * rue_maize_value
      )

# Step 4: Calculate TDMP (cumulative sum)
SRAD_acre <- SRAD_acre %>%
      mutate(
            TDMP = cumsum(DDMP)
      )

# Plot Daily Solar Radiation (MJ/m²/day)
ggplot(SRAD_acre, aes(x = Date, y = SRAD_MJ)) +
      geom_line(color = "darkorange", size = 1) +
      labs(
            title = "Daily Solar Radiation",
            x = "Date",
            y = "Solar Radiation (MJ/m²/day)"
      ) +
      theme_minimal()

# Plot Cumulative growth (TDMP) by days after simulation started
ggplot(SRAD_acre, aes(x = Date, y = TDMP)) +
      geom_line(color = "darkgreen", size = 1) +
      labs(
            title = "Cumulative Dry Matter Production (TDMP) for Maize",
            x = "Date",
            y = "TDMP (g/m²)"
      ) +
      theme_minimal()