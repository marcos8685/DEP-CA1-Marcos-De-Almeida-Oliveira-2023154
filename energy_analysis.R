# =============================================================================
# From Coal to Clean: What Shapes a Country's Electricity Mix? (2000–2023)
# Data Exploration & Preparation – Project (Area: Energy)
# Author: Marcos de Almeida Oliveira Filho – Student number 2023154
#
# Dataset: Our World in Data – Energy dataset (CC BY 4.0)
#          https://github.com/owid/energy-data
#
# How to run it: open the file DEP_Energy_Project.Rproj (double-click it).
# RStudio then opens the project and automatically sets the working directory
# to the project folder, so all the paths in this script are relative
# ("data/...") and it runs on any computer without changing anything. Then
# open energy_analysis.R and run everything with "Source with Echo"
# (Cmd+Shift+Enter / Ctrl+Shift+Enter on Windows) so all tables and plots show
# up, or go line by line with Cmd+Enter.
#
# Note on the data version: Our World in Data updates this file regularly, so
# a fresh download could change the numbers a little. That's why the script
# reads the copy saved in the "data" folder (downloaded on 7 October 2026),
# and every number quoted in the comments comes from running it on that copy.
# Packages we need: tidyverse, countrycode, gridExtra, scales
#   install.packages(c("tidyverse", "countrycode", "gridExtra", "scales"))
# =============================================================================


# =============================================================================
# PROBLEM DOMAIN AND MOTIVATION
# =============================================================================
#
# Making electricity and heat is the single biggest source of energy-related
# CO2 emissions in the world (IEA, 2024), so cleaning it up is at the centre
# of every climate plan (EU Green Deal, Paris Agreement). But countries start from very different places: some already run
# almost entirely on hydro or nuclear, while others still depend on coal, oil
# or gas.
#
# Research question: how has the electricity mix of the world's countries
# changed between 2000 and 2023, what separates "clean" from "dirty" grids,
# and can all these share/size variables be summarised in just a few
# underlying dimensions?
#
# The questions I want to answer in the EDA:
#   1. How is the carbon intensity of electricity distributed? Is there one
#      "typical" country, or are there separate groups?
#   2. Does a richer, more energy-hungry country have a cleaner grid?
#   3. Which sources are replacing coal and oil, and how fast are solar and
#      wind growing?
#   4. How do the continents compare, and which countries are unusual (outliers)?
#
# Why I picked this: people often talk about energy as "rich vs. poor" or
# "renewables vs. fossil". With data on 200+ countries over 24 years we can
# check whether those simple stories actually hold.
#

# =============================================================================
# THE DATASET
# =============================================================================
#
# Name:    Our World in Data – Energy dataset (Ritchie, Rosado and Roser, 2023)
#          file: owid-energy-data.csv
# Source:  https://github.com/owid/energy-data
#          raw file: https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv
#          (downloaded on 7 October 2026)
# Built from: Energy Institute Statistical Review of World Energy, Ember
#          (electricity), U.S. EIA and UN population data
# Licence: CC BY 4.0
# Raw size: 23,377 rows x 130 columns (each row is one country or region in
#          one year, from 1900 to 2025)
#

# ---- Loading the packages ---------------------------------------------------
# First we load the packages we are going to use (all run in R, R Core Team,
# 2026):
#   - tidyverse (Wickham et al., 2019): this brings readr (read_csv to read the file), dplyr
#     (filter/select/mutate/summarise to work with the data), tidyr
#     (pivot_longer to reshape it) and ggplot2 (for all the plots).
#   - countrycode (Arel-Bundock, Enevoldsen and Yetman, 2018): turns a 3-letter country code like "PRT" into its continent
#     ("Europe"). The dataset doesn't have a region column, so we need this to
#     create a categorical variable. If it's not installed, just run
#     install.packages("countrycode").
#   - gridExtra (Auguie, 2026): we use grid.arrange() to put two plots side by side.
#   - scales (Wickham, Pedersen and Seidel, 2025): gives us nicer axis labels, like percent() and comma().
library(tidyverse)
library(countrycode)
library(gridExtra)
library(scales)

# Here we set one clean, simple look for all the charts so they match.
theme_set(theme_minimal(base_size = 12))

# ---- Loading the raw data ---------------------------------------------------
# I downloaded the file once from the Our World in Data GitHub page with the
# line below. I left it commented out so it doesn't download it again every
# time we run the script:
# download.file("https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv",
#               "data/owid-energy-data.csv")

# Now we read the CSV file into R. show_col_types = FALSE just stops R from
# printing the type of all 130 columns on the screen.
raw <- read_csv("data/owid-energy-data.csv", show_col_types = FALSE)

# Let's check how big it is: dim() gives us (number of rows, number of columns).
dim(raw)

# =============================================================================
# DATA PREPARATION
# =============================================================================
#
# ---- The challenges I found in this dataset ----
# - Countries and groups are mixed together. Rows like "World", "Europe",
#   "High-income countries" or "Africa (EI)" sit in the same table as the
#   countries. They have no ISO code (or a fake "OWID_" code), and if we kept
#   them we would be counting the same electricity twice.
# - The table is very wide (130 columns) and lots of the columns are just
#   calculations from other columns (_change_pct, _change_twh, _per_capita...).
# - The years are uneven. Electricity data only really becomes complete
#   around 2000, and 2024–2025 are still missing many countries.
# - Missing values: GDP is missing for about 28 % of the rows from 2000 to
#   2023 (it stops in 2022), and some share columns are NA when the real
#   value is 0.
# - Very skewed data and outliers: population goes from about 3,000 people
#   (small territories) to 1.4 billion (China), and electricity generation
#   from 0.01 TWh (Falkland Islands) to about 9,456 TWh (China), so around
#   6 orders of magnitude.
# - There is no region or categorical column, so I had to create the
#   continent from the country code.
#

# ---- Step 1 – keeping only real countries from 2000 to 2023 -----------------
# Now we filter the rows. A row only stays if ALL of these conditions are TRUE.
countries <- raw %>%
  filter(
    # 1) Groups like "World" or "Europe" have no ISO code (NA), so we drop them.
    !is.na(iso_code),
    # 2) Real country codes always have exactly 3 letters (like "PRT", "BRA").
    nchar(iso_code) == 3,
    # 3) Our World in Data makes up codes starting with "OWID_" for groups,
    #    e.g. "OWID_WRL" for the World. str_detect() finds them and "!" removes them.
    !str_detect(iso_code, "^OWID"),
    # 4) We only keep 2000 to 2023: before 2000 the electricity data has big
    #    gaps, and 2024–2025 aren't complete yet, so this way we compare like
    #    with like.
    year >= 2000, year <= 2023
  )

# Let's see how many rows we still have and how many different countries.
nrow(countries); n_distinct(countries$country)

# ---- Step 2 – choosing the columns we need ----------------------------------
# Out of the 130 columns we only need 18: the identifiers, a few size
# variables, and the SHARE of electricity that comes from each source.
# I used shares (%) instead of absolute TWh because a percentage can be
# compared between a tiny island and China, while TWh can't.
energy <- countries %>%
  select(country, iso_code, year,              # who and when
         population, gdp,                       # size and wealth
         electricity_generation,                # total electricity made, in TWh
         per_capita_electricity,                # electricity per person, in kWh
         energy_per_capita,                     # ALL energy used per person, in kWh
         carbon_intensity_elec,                 # grams of CO2 for each kWh produced
         coal_share_elec, gas_share_elec, oil_share_elec,     # fossil sources, %
         nuclear_share_elec,                                  # nuclear, %
         hydro_share_elec, wind_share_elec, solar_share_elec, # renewables, %
         renewables_share_elec, fossil_share_elec)            # group totals, %

# Now let's count how many missing values (NA) each column has.
colSums(is.na(energy))

# And the same as a percentage, so we can quote the exact number for GDP.
round(mean(is.na(energy$gdp)) * 100, 1)

# ---- Step 3 – dealing with missing values -----------------------------------
# First, let's make a chart with the % of missing values in each column, so
# it's easy to see where the problems are.
energy %>%
  # mean(is.na(x)) gives the proportion of NAs in a column, and *100 turns it
  # into a percentage. across(everything(), ...) does this for every column.
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  # Here we turn the one wide row into a long table (one row per column),
  # because ggplot needs one column for the x-axis and one for the y-axis.
  pivot_longer(everything(), names_to = "variable", values_to = "pct_missing") %>%
  # reorder() sorts the bars from the least missing to the most missing.
  ggplot(aes(reorder(variable, pct_missing), pct_missing)) +
  geom_col(fill = "#2a7ab9") +
  coord_flip() +                          # horizontal bars so the long names fit
  labs(title = "Missing values per attribute (before cleaning)",
       x = NULL, y = "% missing")

# Looking at the chart, these are the decisions I made:
#   1. I dropped GDP. About 28 % is missing (27.9 %) and it stops in 2022.
#      Filling in over a quarter of an economic variable would basically be
#      inventing data, so I
#      use energy_per_capita as the "how developed is the country" variable.
#   2. I deleted the rows with no electricity data at all (fossil share or
#      total generation missing). That's 184 rows, mostly very small
#      territories, and there is nothing there we could fill in from.
#      These are also exactly the 184 rows where carbon_intensity_elec is
#      missing, so this step removes all of its missing values too (we check
#      this below).
#   3. When a single source share is missing but the totals are there, I check
#      whether it is really zero. If fossil + nuclear + renewables already add
#      up to ~100 % without that source, then the missing value must be 0
#      (for example, a country that simply has no nuclear plant), so I fill it
#      with 0.

# Now we apply decisions 1 and 2. First I save the number of rows before,
# so we can see exactly how many rows get dropped.
rows_before <- nrow(energy)
energy <- energy %>%
  # Decision 1: remove the whole GDP column.
  select(-gdp) %>%
  # Decision 2: drop the rows that have no electricity information at all.
  filter(!is.na(fossil_share_elec), !is.na(electricity_generation))

# How many rows did we drop? (It should be 184.)
rows_before - nrow(energy)

# What happened to the missing carbon_intensity_elec values? It had 184 NAs
# and we never touch that column directly. But they were in exactly the same
# rows as the missing fossil shares, so decision 2 removed them as a side
# effect. Let's check that none are left (this has to be 0, otherwise cor()
# and prcomp() later would fail):
sum(is.na(energy$carbon_intensity_elec))

# Before doing decision 3, let's check that it makes sense. We add fossil +
# renewables + nuclear (coalesce() treats a missing nuclear value as 0). If
# the total is ~100 % in almost every row, then a missing share really means 0.
energy %>%
  mutate(total = fossil_share_elec + renewables_share_elec +
                 coalesce(nuclear_share_elec, 0)) %>%
  summarise(mean_total     = mean(total),               # we want this close to 100
            sd_total       = sd(total),                 # and this close to 0
            pct_within_1pp = mean(abs(total - 100) < 1) * 100)  # % of rows within 1 point of 100

# The totals come to 100 % in every single row (all within 1 point of 100),
# so it's safe to set a missing individual source to 0.

# Here I save the names of the 7 source-share columns in one vector, because
# we're going to use this list many times later (in the EDA and the PCA).
share_cols <- c("coal_share_elec", "gas_share_elec", "oil_share_elec",
                "nuclear_share_elec", "hydro_share_elec",
                "wind_share_elec", "solar_share_elec")

energy <- energy %>%
  mutate(
    # Decision 3: now we replace every NA in the 7 share columns with 0
    # (the check above showed this is correct).
    across(all_of(share_cols), ~ replace_na(., 0)),

    # energy_per_capita still has a few gaps. Here we fill each gap with that
    # country's OWN median, so a missing year in Portugal gets Portugal's usual
    # value, not the world average. I used the median instead of the mean
    # because outliers don't pull it around.
    energy_per_capita = ifelse(is.na(energy_per_capita),
                               ave(energy_per_capita, country,
                                   FUN = function(x) median(x, na.rm = TRUE)),
                               energy_per_capita)) %>%
  # A few countries have no energy_per_capita in ANY year (so their median is
  # also NA), or have no population. We can't fill those in, so we drop them.
  # This removes another 24 rows (5,076 -> 5,052).
  filter(!is.na(energy_per_capita), !is.na(population))

# Now I create a new column, "other renewables". Renewables = hydro + wind +
# solar + other (biomass, geothermal, waste...), so if we subtract the three
# we know, what's left is "other". pmax(..., 0) just avoids tiny negative
# numbers that come from rounding in the original data.
energy <- energy %>%
  mutate(other_ren_share_elec = pmax(renewables_share_elec - hydro_share_elec -
                                       wind_share_elec - solar_share_elec, 0))

# Let's check again: every column should now have 0 missing values.
colSums(is.na(energy))

# ---- Step 4 – creating categorical variables --------------------------------
energy <- energy %>%
  mutate(
    # Continent (nominal, no order): countrycode() turns the country code into
    # its continent, for example "PRT" becomes "Europe".
    continent = countrycode(iso_code, "iso3c", "continent"),

    # Period (ordinal, it has an order): cut() puts each year into one of three
    # 8-year groups. The breaks 1999, 2007, 2015, 2023 give us
    # 2000-2007, 2008-2015 and 2016-2023.
    period = cut(year, breaks = c(1999, 2007, 2015, 2023),
                 labels = c("2000-2007", "2008-2015", "2016-2023")),

    # Dominant source (nominal): which source makes the most electricity?
    # max.col() tells us, for each row, the POSITION of the biggest of the 7
    # shares (1 = coal, 2 = gas, ... 7 = solar), and we use that position to
    # pick the right name from the list.
    dominant_source = c("Coal", "Gas", "Oil", "Nuclear", "Hydro", "Wind", "Solar")[
      max.col(across(all_of(share_cols)), ties.method = "first")]
  )

# If "other renewables" (like geothermal in Kenya) is bigger than all of the 7
# sources, then that one becomes the dominant source instead.
# apply(..., 1, max) finds the biggest of the 7 shares in each row (1 = by row).
energy$dominant_source[energy$other_ren_share_elec >
                         apply(energy[share_cols], 1, max)] <- "Other renewables"

# Now let's look at the new categories to make sure they look right
# (useNA = "ifany" would show us any country code that didn't match a continent).
table(energy$continent, useNA = "ifany")
table(energy$dominant_source)

# ---- Step 5 – looking for outliers -------------------------------------------
# The size variables are very right-skewed, so now we look for outliers using
# the IQR rule: a value is an outlier if it's below Q1 - 1.5*IQR or above
# Q3 + 1.5*IQR.

# Here I write a small function that counts the outliers in a column.
iqr_outliers <- function(x) {
  q <- quantile(x, c(.25, .75), na.rm = TRUE)   # q[1] is Q1 and q[2] is Q3
  i <- q[2] - q[1]                               # the IQR is Q3 - Q1
  # Now we check every value against the two limits. sum() of TRUE/FALSE
  # gives us how many values are outside.
  sum(x < q[1] - 1.5 * i | x > q[2] + 1.5 * i, na.rm = TRUE)
}

# These are the numeric columns we want to check.
num_vars <- c("population", "electricity_generation", "per_capita_electricity",
              "energy_per_capita", "carbon_intensity_elec")

# sapply() runs our function on each of those columns and gives the counts.
sapply(energy[num_vars], iqr_outliers)

# Let's also measure the skewness with Pearson's formula: 3(mean - median) / sd.
# Above 0 means right-skewed (long tail of big values), below 0 means
# left-skewed, and close to 0 means more or less symmetric.
sapply(energy[num_vars], function(x) round(3 * (mean(x) - median(x)) / sd(x), 2))

# Now let's see it in a picture: a boxplot before and after a log transform.
# p1 is the raw electricity per person, where a few huge values squash the box.
p1 <- ggplot(energy, aes(y = per_capita_electricity)) + geom_boxplot(fill = "#f4a259") +
  labs(title = "Raw: electricity per capita", y = "kWh / person") +
  theme(axis.text.x = element_blank())          # the x-axis means nothing here
# p2 is the same thing on a log10 scale, and it looks much more balanced.
p2 <- ggplot(energy, aes(y = log10(per_capita_electricity))) + geom_boxplot(fill = "#5b8e7d") +
  labs(title = "After log10 transform", y = "log10(kWh / person)") +
  theme(axis.text.x = element_blank())
# Here we put the two plots side by side so we can compare them.
grid.arrange(p1, p2, ncol = 2)

# So who are these outliers? Let's take 2023, sort from the highest to the
# lowest electricity per person, and look at the top 8.
energy %>% filter(year == 2023) %>%
  arrange(desc(per_capita_electricity)) %>%
  select(country, per_capita_electricity, carbon_intensity_elec, dominant_source) %>%
  head(8)

# My decision: these outliers are real, not mistakes. Iceland's aluminium
# smelters run on geothermal and hydro, and the Gulf states use a lot of gas
# for desalination and air-conditioning. So I KEEP them. But so they don't
# take over methods based on distances (like PCA), I log-transform the size
# variables.

# log10 squeezes the big values (10 -> 1, 1000 -> 3, 1,000,000 -> 6), so China
# and Tuvalu end up on a comparable scale. I add a small number (+0.01 or +1)
# because log10(0) is -Infinity and a few tiny countries have a value of 0.
energy <- energy %>%
  mutate(log_population     = log10(population),
         log_generation     = log10(electricity_generation + 0.01),
         log_elec_per_cap   = log10(per_capita_electricity + 1),
         log_energy_per_cap = log10(energy_per_capita + 1))

# Let's count the outliers again. There should be far fewer now.
sapply(energy[c("log_population","log_generation","log_elec_per_cap","log_energy_per_cap")],
       iqr_outliers)

# ---- The final clean dataset ------------------------------------------------
# Now let's check the final size (the brief asks for at least 3000 rows and
# 7 columns) and have a quick look at every column.
dim(energy)
glimpse(energy)
# And here we save the clean data, so we don't have to clean it again next time.
write_csv(energy, "data/energy_clean.csv")

# How many years of data does each country have? (This matters later,
# because countries with more years count more in the pooled PCA.)
summary(as.vector(table(energy$country)))

# The clean data has 5,052 rows (212 countries x up to 24 years) and 26
# columns, with no missing values in the variables we use from here on.

# =============================================================================
# EXPLORATORY DATA ANALYSIS
# =============================================================================

# ---- Q1 – Variation: how is carbon intensity distributed? -------------------
# Here we make a histogram of carbon intensity for 2023 only, so each country
# is counted just once.
ggplot(energy %>% filter(year == 2023), aes(carbon_intensity_elec)) +
  # Each bar covers 50 grams of CO2 per kWh.
  geom_histogram(binwidth = 50, fill = "#2a7ab9", colour = "white") +
  # The dashed line shows the median, so we can see where the "middle" country is.
  geom_vline(xintercept = median(energy$carbon_intensity_elec[energy$year == 2023]),
             linetype = "dashed") +
  labs(title = "Carbon intensity of electricity, 2023",
       subtitle = "Dashed line = median",
       x = "gCO2 per kWh", y = "Number of countries")

# Here I count the countries in each part of the histogram, so the numbers I
# quote below come straight from R.
e23 <- energy %>% filter(year == 2023)
c(countries_2023  = nrow(e23),
  under_200       = sum(e23$carbon_intensity_elec < 200),
  from_200_to_450 = sum(e23$carbon_intensity_elec >= 200 & e23$carbon_intensity_elec <= 450),
  over_450_to_700 = sum(e23$carbon_intensity_elec > 450 & e23$carbon_intensity_elec <= 700),
  median          = round(median(e23$carbon_intensity_elec)))

# What I found unusual: there isn't one "typical" country, there are two
# groups. 50 countries have very clean grids under 200 g/kWh (hydro and
# nuclear countries like Norway, Paraguay and France), while the biggest group
# (96 of the 209 countries) sits between 450 and 700 g/kWh (oil, gas and coal
# countries), with fewer countries in between (48 between 200 and 450, and 15 above 700).

# Now let's look at each energy source separately, one small histogram per
# source, again for 2023.
energy %>% filter(year == 2023) %>%
  select(all_of(share_cols)) %>%                     # just the 7 share columns
  # We make it long: "name" is the source and "value" is the share in %.
  pivot_longer(everything()) %>%
  mutate(name = str_remove(name, "_share_elec")) %>% # "coal_share_elec" becomes "coal"
  ggplot(aes(value)) +
  geom_histogram(bins = 20, fill = "#5b8e7d", colour = "white") +
  # facet_wrap() draws one small panel per source, and scales = "free_y" lets
  # each panel have its own height.
  facet_wrap(~ name, scales = "free_y") +
  labs(title = "Share of electricity by source, 2023 (each panel = one source)",
       x = "% of electricity", y = "Countries")

# Let's see which countries get 90 % or more of their electricity from oil.
e23 %>% filter(oil_share_elec >= 90) %>% pull(country)

# Every source has a U-shape with a big spike at zero: most countries either
# don't use a source at all or depend heavily on it. Oil stands out: 26
# countries get 90 % or more of their electricity from oil, and 21 of them are
# small islands (Bahamas, Maldives, Grenada...). The other five are poorer
# countries with small grids, like Chad, South Sudan and Gambia.

# ---- Q2 – Covariation: are richer, energy-hungry countries cleaner? --------
# Here we plot energy use per person against carbon intensity for 2023. I also
# use colour for the continent and point size for the population, so we can
# see two extra things in the same chart.
ggplot(energy %>% filter(year == 2023),
       aes(energy_per_capita, carbon_intensity_elec, colour = continent,
           size = population)) +
  geom_point(alpha = .7) +                  # a bit transparent so overlapping points show
  scale_x_log10(labels = comma) +           # log x-axis because energy use is very skewed
  scale_size(range = c(1, 12), guide = "none") +
  labs(title = "Energy use per person vs. carbon intensity of electricity (2023)",
       x = "Primary energy per capita (kWh, log scale)", y = "gCO2 per kWh",
       colour = "Continent")

# Now let's put a number on it with the Pearson correlation (all years).
# Close to 0 means no linear relationship, close to +1 or -1 means a strong one.
cor(energy$log_energy_per_cap, energy$carbon_intensity_elec)

# What I found: the correlation is only weak (r = 0.21), so being rich or
# using a lot of energy does NOT really tell us if a grid is clean or dirty.
# High-energy countries show up at both ends: Iceland, Norway and Sweden at the
# bottom (under 40 g/kWh), and Bahrain, Brunei and Turkmenistan at the top
# (around 900 g/kWh or more). Geography (rivers for
# hydro) and policy (nuclear) seem to matter more than wealth.

# Next, let's look at how ALL the numeric variables relate to each other with a
# correlation matrix. cor() works out the correlation between every pair.
cm <- cor(energy %>% select(log_population, log_generation, log_elec_per_cap,
                            log_energy_per_cap, carbon_intensity_elec,
                            all_of(share_cols), other_ren_share_elec))

# To draw it with ggplot we first turn the matrix into a long table
# (Var1, Var2, Freq = the correlation), then draw one coloured tile per pair.
as.data.frame(as.table(cm)) %>%
  ggplot(aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = round(Freq, 2)), size = 2.6) +   # write the number in each tile
  # Blue means negative, white means 0, red means positive.
  scale_fill_gradient2(low = "#2a7ab9", mid = "white", high = "#c0392b",
                       limits = c(-1, 1), name = "r") +
  labs(title = "Correlation matrix of numeric features", x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# What stands out in the matrix:
# - Carbon intensity goes up with oil (0.42) and coal (0.31), and goes down
#   very strongly with hydro (-0.82). So around the world, hydro is the main
#   "clean" alternative, much more than nuclear (-0.23).
# - log_population and log_generation are strongly correlated (0.77, bigger
#   countries make more electricity). That's repeated information, and PCA
#   will be able to combine it.
# - Oil share goes down as population goes up (r = -0.61): oil-powered
#   grids are mostly a small-island thing.

# ---- Q3 – Change over time: what is replacing fossil fuels? ----------------
# (The numbers I quote in this section come from printing world_mix for 2000
# and 2023 and from the small table further down.)
# Now we work out the world electricity mix for each year. A simple average
# would give Tuvalu the same weight as China, so instead I use weighted.mean()
# with electricity_generation as the weight. That way each country counts as
# much as the electricity it actually produces.
world_mix <- energy %>%
  group_by(year) %>%                              # one result for each year
  summarise(across(c(all_of(share_cols), other_ren_share_elec),
                   ~ weighted.mean(., electricity_generation))) %>%
  pivot_longer(-year, names_to = "source", values_to = "share") %>%
  mutate(source = str_remove(source, "_share_elec"))

# Let's print the mix for the first and last year so we can compare them.
world_mix %>% filter(year %in% c(2000, 2023)) %>%
  pivot_wider(names_from = year, values_from = share)

# Here we draw a stacked area chart: each coloured band is one source, and
# together they add up to 100 %.
ggplot(world_mix, aes(year, share, fill = source)) +
  geom_area(colour = "white", linewidth = .2) +
  # I fixed the colours so each source is easy to recognise (coal is dark grey, etc.).
  scale_fill_manual(values = c(coal = "#4d4d4d", gas = "#e69f00", oil = "#8c510a",
                               nuclear = "#cc79a7", hydro = "#0072b2",
                               wind = "#56b4e9", solar = "#f0e442",
                               other_ren = "#009e73")) +
  labs(title = "Global electricity mix 2000–2023 (generation-weighted)",
       x = NULL, y = "% of electricity", fill = "Source")

# Now let's count how many countries get at least 10 % from wind + solar, in
# three snapshot years.
energy %>%
  filter(year %in% c(2000, 2010, 2023)) %>%
  group_by(year) %>%
  summarise(
    # sum() of a TRUE/FALSE condition counts how many countries meet it.
    countries_with_10pct_wind_solar = sum(wind_share_elec + solar_share_elec >= 10),
    # And this is the fossil share of the "typical" (median) country, not
    # weighted by size.
    median_fossil_share = median(fossil_share_elec))

# What I found: coal's share of world electricity barely moves (about 38 % to
# 35 %), oil collapses (about 9 % to 3 %), nuclear's share almost halves (about
# 17 % to 9 %), gas goes up (about 18 % to 23 %), and wind + solar grow from
# almost nothing to about 13 %. The number of countries getting at least 10 %
# from wind + solar went from just 1 in 2000 to 71 in 2023. But the median
# country's fossil share only drops from about 75 % to 66 %, so the change is
# happening mainly in a group of countries, not everywhere.

# ---- Q4 – How do the continents compare? -----------------------------------
# Here we make boxplots of carbon intensity by continent, with one panel for
# each period. reorder() sorts the continents from the cleanest to the
# dirtiest median, which makes it easier to read.
ggplot(energy, aes(reorder(continent, carbon_intensity_elec, median),
                   carbon_intensity_elec, fill = continent)) +
  geom_boxplot(outlier.alpha = .3, show.legend = FALSE) +  # outlier dots made lighter
  facet_wrap(~ period) +                                   # 3 panels: 2000-07, 2008-15, 2016-23
  labs(title = "Carbon intensity of electricity by continent and period",
       x = NULL, y = "gCO2 per kWh") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# The medians behind the boxplots, so we can quote them exactly.
energy %>% group_by(continent, period) %>%
  summarise(median_ci = round(median(carbon_intensity_elec)), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = median_ci)

# And now let's compare how many countries are dominated by each source in
# 2000 and in 2023.
energy %>% filter(year %in% c(2000, 2023)) %>%
  count(year, dominant_source) %>%                 # number of countries per source per year
  ggplot(aes(reorder(dominant_source, n), n, fill = factor(year))) +
  geom_col(position = "dodge") +                   # "dodge" puts the bars side by side
  coord_flip() +
  scale_fill_manual(values = c("#9ecae1", "#08519c")) +  # light blue = 2000, dark blue = 2023
  labs(title = "Number of countries by dominant electricity source",
       x = NULL, y = "Countries", fill = "Year")

# The same counts as a table.
energy %>% filter(year %in% c(2000, 2023)) %>%
  count(year, dominant_source) %>%
  pivot_wider(names_from = year, values_from = n, values_fill = 0)

# What I found: Europe has the lowest carbon intensity and it's falling the
# fastest (median from about 460 to 300 g/kWh). Asia (about 600–640) and
# Oceania (about 570–600) stay the highest. The Americas and Africa hardly
# change, and Africa has a really wide spread, from oil-powered islands to
# hydro countries like Ethiopia and DR Congo. Between 2000 and 2023 the number
# of countries dominated by oil goes from 81 to 62 and by coal from 29 to 22,
# while gas goes up from 30 to 53 countries, and wind becomes the main source
# in six countries for the first time (Denmark, Germany, Lithuania,
# Luxembourg, Portugal and Uruguay).
