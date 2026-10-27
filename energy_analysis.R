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

# =============================================================================
# ENCODING
# =============================================================================
#
# PCA only works with numbers, so first we need to turn the categorical
# variables into numbers:
# - Label encoding for "period", because it has a natural order
#   (2000-07 < 2008-15 < 2016-23), so it becomes 1, 2, 3.
# - One-hot encoding for "continent", because continents have no order.
#   We have k = 5 continents (Africa, Americas, Asia, Europe, Oceania), so we
#   make k - 1 = 4 dummy columns. Africa is the reference (all zeros). We
#   leave one out to avoid the "dummy variable trap" (the columns would
#   otherwise always add up to 1 and repeat information).
# - I did NOT put "dominant_source" into the PCA, because it comes from the
#   share columns and would just repeat them. Instead I use it later to colour
#   the PCA plot and check if the results make sense.

# Label encoding: period is a factor with the levels in order, and
# as.integer() swaps each level for its position (1, 2, 3). This is fine here
# because the numbers keep the real order.
energy$period_code <- as.integer(energy$period)
table(energy$period, energy$period_code)          # quick check of the mapping

# One-hot encoding: if we gave continents numbers 1 to 5, R would think
# "Europe > Asia", which makes no sense. So instead each continent gets its
# own 0/1 column.
energy$continent <- factor(energy$continent)
# model.matrix() creates those 0/1 columns for us, and it automatically leaves
# out the first level (Africa) as the reference, so we get k - 1 = 4 columns.
# A row with 0 in all four columns is Africa. The [, -1] removes the
# "(Intercept)" column of 1s that model.matrix adds.
continent_dummies <- model.matrix(~ continent, data = energy)[, -1]
# Here I rename the columns from "continentEurope" to "is_Europe" so they're
# easier to read.
colnames(continent_dummies) <- str_replace(colnames(continent_dummies), "continent", "is_")
head(continent_dummies)

# Now we add the 4 new columns to the data.
energy_enc <- bind_cols(energy, as.data.frame(continent_dummies))

# Let's check it worked: every row should have either one 1 (Americas, Asia,
# Europe or Oceania) or no 1s at all (Africa). If any row had two or more 1s,
# something would be wrong.
table(rowSums(continent_dummies))

# =============================================================================
# PRINCIPAL COMPONENT ANALYSIS
# =============================================================================

# ---- Choosing the variables and scaling them --------------------------------
# These are the 18 variables that go into the PCA: the 4 log size variables,
# carbon intensity, the 8 source shares, the label-encoded period, and the 4
# one-hot continent columns.
pca_vars <- c("log_population", "log_generation", "log_elec_per_cap",
              "log_energy_per_cap", "carbon_intensity_elec",
              share_cols, "other_ren_share_elec",
              "period_code", colnames(continent_dummies))

X <- energy_enc[, pca_vars]

# Now we run the PCA (method as described in Jolliffe and Cadima, 2016).
# - center = TRUE subtracts the mean of each column (PCA needs this).
# - scale. = TRUE divides each column by its standard deviation (z-score).
#   This is important because our variables are on very different scales:
#   carbon intensity goes up to about 1000, the shares go from 0 to 100, and
#   the dummies are just 0 or 1. Without scaling, carbon intensity would take
#   over the PCA just because its numbers are bigger.
pca <- prcomp(X, center = TRUE, scale. = TRUE)

# Let's look at how much variance each of the first 8 components explains
# (standard deviation, proportion of variance, cumulative proportion).
summary(pca)$importance[, 1:8]

# ---- How many components should we keep? ------------------------------------
# Here I put the variance of each component into a small table.
# pca$sdev is the standard deviation of each component, and squaring it gives
# the eigenvalue (its variance).
var_exp <- tibble(PC    = seq_along(pca$sdev),
                  eigen = pca$sdev^2,                  # eigenvalue
                  prop  = eigen / sum(eigen),          # share of the total variance
                  cum   = cumsum(prop))                # running total

# Now we draw the scree plot. The bars are the variance of each component, the
# red line is the running total, and the dashed line marks 80 %.
p_scree <- ggplot(var_exp, aes(PC, prop)) +
  geom_col(fill = "#2a7ab9") +
  geom_line(aes(y = cum), colour = "#c0392b") + geom_point(aes(y = cum), colour = "#c0392b") +
  geom_hline(yintercept = .8, linetype = "dashed") +
  scale_y_continuous(labels = percent) +
  labs(title = "Scree plot (bars) & cumulative variance (red)",
       x = "Principal component", y = "Variance explained")
p_scree

# I used two rules to decide how many components to keep:
# - Kaiser rule (Kaiser, 1960): keep the components with an eigenvalue above
#   1, meaning they explain more than ONE of the original (standardised)
#   variables would.
k_kaiser <- sum(var_exp$eigen > 1)
# - 80 % rule: how many components we need to reach 80 % of the variance.
k_80     <- which(var_exp$cum >= .8)[1]
c(kaiser_rule = k_kaiser, components_for_80pct = k_80, n_original = length(pca_vars))

# With the Kaiser rule (eigenvalue > 1) we keep 6 components, and we would
# need 8 to reach 80 % of the variance, out of the 18 original variables.

# ---- What does each component mean? (loadings) -------------------------------
# The loadings (pca$rotation) tell us how much each original variable counts
# in each component, from -1 to 1. A big number (positive or negative) means
# that variable is important for that component.
# Here we take the first 4 components and reshape them for ggplot.
load_df <- as.data.frame(pca$rotation[, 1:4]) %>%
  rownames_to_column("variable") %>%                # move the variable names into a column
  pivot_longer(-variable, names_to = "PC", values_to = "loading")

# One panel for each component: red bars are positive, blue bars are negative.
ggplot(load_df, aes(loading, reorder(variable, loading), fill = loading > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ PC, nrow = 1) +
  scale_fill_manual(values = c("#2a7ab9", "#c0392b")) +
  labs(title = "Loadings of the first four principal components", x = "Loading", y = NULL)

# And here are the same loadings as a table, rounded to 2 decimals.
round(pca$rotation[, 1:4], 2)

# To help me give each component a name, this little function shows the 5
# strongest loadings (sorted by size, ignoring the sign).
top_loadings <- function(pc, n = 5) {
  l <- pca$rotation[, pc]
  l[order(-abs(l))][1:n] |> round(2)
}
lapply(c(PC1 = 1, PC2 = 2, PC3 = 3, PC4 = 4), top_loadings)

# ---- Scores plot (biplot) ----------------------------------------------------
# The scores (pca$x) are where each country-year lands on the components.
# Here we take PC1 and PC2 and add back the country, year, continent and
# dominant source, so we can colour and label the points.
scores <- as.data.frame(pca$x[, 1:2]) %>%
  bind_cols(energy_enc %>% select(country, year, continent, dominant_source))

# Now the arrows for the variables (that's the "bi" in biplot). I multiply the
# loadings by 6 only so the arrows are big enough to see next to the points,
# and I only draw the variables with a loading above 0.2 so the plot isn't
# too crowded.
arrows <- as.data.frame(pca$rotation[, 1:2] * 6) %>% rownames_to_column("var") %>%
  filter(abs(PC1) > 1.2 | abs(PC2) > 1.2)

ggplot(scores, aes(PC1, PC2)) +
  # I colour the points by dominant source. This was NOT used to build the
  # PCA, so if the colours form groups it's a good sign the components make sense.
  geom_point(aes(colour = dominant_source), alpha = .35, size = 1) +
  geom_segment(data = arrows, aes(0, 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(.2, "cm")), colour = "black") +
  geom_text(data = arrows, aes(PC1 * 1.1, PC2 * 1.1, label = var), size = 3) +
  # Here I label a few well-known countries (where they are in 2023) to make
  # it easier to read.
  geom_text(data = scores %>% filter(year == 2023,
                                     country %in% c("China", "United States", "France",
                                                    "Norway", "Saudi Arabia", "Germany",
                                                    "Paraguay", "Barbados", "India", "Brazil")),
            aes(label = country), size = 3, fontface = "bold") +
  labs(title = "PCA biplot: country-years on PC1 vs PC2",
       subtitle = "Points coloured by dominant electricity source (not used in PCA)",
       x = paste0("PC1 (", percent(var_exp$prop[1], .1), ")"),   # % of variance in the axis title
       y = paste0("PC2 (", percent(var_exp$prop[2], .1), ")"),
       colour = "Dominant source") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3)))

# Finally, let's follow a few countries over time to see how they moved in the
# PCA space from 2000 to 2023.
sel <- c("Germany", "United Kingdom", "China", "Denmark", "Chile", "France")
ggplot(scores %>% filter(country %in% sel), aes(PC1, PC2, colour = country)) +
  # geom_path() joins the years in order, and the arrow shows the direction.
  geom_path(arrow = arrow(length = unit(.2, "cm"))) +
  # The dots mark where each country starts (2000) and ends (2023).
  geom_point(data = scores %>% filter(country %in% sel, year %in% c(2000, 2023)), size = 2) +
  labs(title = "Country trajectories in PCA space, 2000 → 2023",
       x = "PC1", y = "PC2")

# ---- Robustness checks: does the PCA still hold if we change the inputs? ----
# Before interpreting the PCA, I want to be critical about what went into it.
# Some of my 18 inputs are built-in duplicates of each other:
#   - electricity generation = population x electricity per person, so
#     log_generation is almost exactly log_population + log_elec_per_cap;
#   - the 8 source shares always add up to 100 %. Data that sums to a fixed
#     total like this is called compositional data, and it means each share
#     can be predicted from the other seven;
#   - carbon intensity is mostly decided by the shares (coal and oil are dirty,
#     hydro and nuclear are clean).
# Let's prove these three things with numbers first.

# 1) How close is log_generation to log_population + log_elec_per_cap?
cor(energy$log_generation, energy$log_population + energy$log_elec_per_cap)
# 2) What do the 8 shares add up to in each row?
summary(rowSums(energy[c(share_cols, "other_ren_share_elec")]))
# 3) How much of carbon intensity can the shares explain? (R-squared of a
#    simple linear regression of carbon intensity on the shares)
summary(lm(carbon_intensity_elec ~ ., data = energy[c("carbon_intensity_elec", share_cols)]))$r.squared

# The answers: r = 0.9996, the shares add up to 100 % in every row, and the shares explain 89 % of carbon intensity. So
# there is repeated information in the PCA inputs, which is part of the reason
# the variance is spread out and we need 8 of 18 components to reach 80 %.

# Now I run the PCA again in three different ways and compare. To avoid
# writing the same code three times, here is a small function that runs a
# PCA and returns the summary numbers we care about.
run_pca <- function(data, vars, label) {
  p  <- prcomp(data[, vars], center = TRUE, scale. = TRUE)
  ev <- p$sdev^2                                   # eigenvalues
  list(pca = p,
       summary = tibble(model     = label,
                        rows      = nrow(data),
                        variables = length(vars),
                        PC1_pct   = round(100 * ev[1] / sum(ev), 1),
                        PC2_pct   = round(100 * ev[2] / sum(ev), 1),
                        kaiser    = sum(ev > 1),                         # eigenvalue > 1
                        for_80pct = which(cumsum(ev) / sum(ev) >= .8)[1]))
}

# Check A – drop the exact duplicates: log_generation (= population x per
# person) and other_ren_share_elec (it's just 100 minus the other shares).
vars_no_dup <- setdiff(pca_vars, c("log_generation", "other_ren_share_elec"))
check_A <- run_pca(energy_enc, vars_no_dup, "A: no duplicates")

# Check B – numeric variables only, WITHOUT the encoded columns (no continent
# dummies and no period), to see if the "clean vs dirty" pattern exists on its
# own or if the encoded variables are creating it.
vars_numeric <- setdiff(pca_vars, c("period_code", colnames(continent_dummies)))
check_B <- run_pca(energy_enc, vars_numeric, "B: no encoded columns")

# Check C – one year only (2023), so every country appears exactly once. In
# the main PCA, Germany 2015 and Germany 2016 are almost the same row, so a
# country with 24 years counts much more than one with only a few years.
# period_code has to be removed here because it's the same for every row in
# 2023 (a constant column can't be scaled).
energy_2023 <- energy_enc %>% filter(year == 2023)
vars_2023   <- setdiff(pca_vars, "period_code")
check_C <- run_pca(energy_2023, vars_2023, "C: 2023 only")

# Now we put the main PCA and the three checks in one table to compare them.
bind_rows(run_pca(energy_enc, pca_vars, "Main PCA")$summary,
          check_A$summary, check_B$summary, check_C$summary)

# Is PC2 ("clean vs dirty") the same in check B as in the main PCA? Since both
# use the same rows, we can correlate the scores directly (1 = identical).
# The sign of a component is arbitrary (another computer or package version
# could flip it and give -0.98 instead of 0.98), so I use abs(): only the
# size of the correlation matters.
abs(cor(pca$x[, 2], check_B$pca$x[, 2]))
abs(cor(pca$x[, 1], check_B$pca$x[, 1]))
# For 2023 the rows are different, so instead we compare the loadings of the
# 17 shared variables (1 = same pattern), again ignoring the sign.
abs(cor(pca$rotation[vars_2023, 1], check_C$pca$rotation[, 1]))
abs(cor(pca$rotation[vars_2023, 2], check_C$pca$rotation[, 2]))

# Let's also show it in a picture: the PC2 loadings in the main PCA, without
# the encoded columns, and with 2023 only. (The sign of a component is
# arbitrary, so I flip each one so that hydro is positive, to compare them.)
pc2_loadings <- function(p, label) {
  l <- p$rotation[, 2]
  if (l["hydro_share_elec"] < 0) l <- -l
  tibble(variable = names(l), loading = l, model = label)
}
bind_rows(pc2_loadings(pca, "Main PCA"),
          pc2_loadings(check_B$pca, "B: no encoded columns"),
          pc2_loadings(check_C$pca, "C: 2023 only")) %>%
  ggplot(aes(loading, reorder(variable, loading), fill = model)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#2a7ab9", "#f4a259", "#5b8e7d")) +
  labs(title = "PC2 'clean vs dirty' loadings in three versions of the PCA",
       x = "Loading on PC2", y = NULL, fill = NULL)

# What I found:
# - A (no duplicates, 16 variables): PC1 18.4 %, PC2 16.4 %, still 6
#   components by Kaiser and still 8 for 80 %. PC1 and PC2 keep the same
#   meaning. So the duplicates add some repetition, but they are not the main
#   reason the variance is spread out: the shares with lots of zeros and the
#   0/1 dummies are.
# - B (no encoded columns, 13 variables): PC1 25.1 % and PC2 20.0 %, and only
#   5 components by Kaiser (6 for 80 %). The PC2 scores correlate r = 0.997
#   with the main PC2, and PC1 r = 0.98. So the "clean vs dirty" structure is
#   real on its own, and the continent and period columns mostly just add
#   their own extra components (PC3 = region, PC4 = time in the main PCA).
# - C (2023 only, 209 countries, one row each): PC1 22.0 % and PC2 15.3 %,
#   with 7 components by Kaiser and 8 for 80 %.
#   PC1 has practically the same loadings as before (r = 0.99). PC2 is still
#   "clean vs dirty" (carbon intensity -0.53, hydro +0.36), but in a single
#   year region mixes into it more (Asia -0.36, Europe +0.23), so the loading
#   pattern matches less closely (r = 0.74). So pooling the years did not
#   create the structure, although it does make PC2 a bit "cleaner".

# ---- What the PCA tells us ----------------------------------------------------
# - Reduction: the 18 standardised variables are summarised by 6 components
#   with an eigenvalue above 1. PC1 and PC2 alone explain 20.7 % and 14.8 % of
#   the variance. The variance is quite spread out (no single component
#   dominates), which makes sense because the source shares have lots of zeros
#   and the continent columns are almost independent of each other.
# - PC1 (20.7 %) – "size of the power system vs. small oil islands":
#   log generation (-0.47) and log population (-0.34), together with the coal,
#   gas and nuclear shares, are on the negative side, while oil share is on
#   the positive side (+0.42). So big industrial grids (China, France,
#   Germany) are at one end and small oil-powered islands (Barbados) at the
#   other. (The sign of a component doesn't matter, only the contrast does.)
# - PC2 (14.8 %) – "clean vs. dirty electricity": hydro share (+0.52) against
#   carbon intensity (-0.50) and energy/electricity per person. Hydro
#   countries (Paraguay, Norway, Brazil) score high, and coal, gas and oil
#   countries score low.
# - PC3 (12.0 %) – "Europe & wind vs. Asia & gas": the Europe column (+0.35)
#   and wind (+0.33) against the Asia column (-0.44), gas and carbon intensity.
# - PC4 (8.7 %) – "time / new renewables": solar (-0.56), period (-0.54) and
#   wind (-0.34) move together, which is the recent solar and wind boom.
# - Built-in duplicates: some inputs repeat each other (log_generation is
#   almost exactly log_population + log_elec_per_cap, the shares always add up
#   to 100 %, and the shares explain 89 % of carbon intensity). The checks
#   above show the main results stay the same when we remove them.
# - Encoded columns: the continent dummies and period mainly drive PC3 and
#   PC4. Without them (check B) the clean vs dirty component is unchanged
#   (r = 0.997), so it doesn't depend on the encoding.
# - Checking with a label we didn't use: when we colour by dominant source,
#   the coal, hydro, oil and gas countries end up in different parts of the
#   PC1–PC2 plot. So the components are picking up real structure.
# - The country paths show the energy transition: the UK (PC2 from -0.9 to
#   +0.3) and Germany (-0.9 to 0.0) move towards the clean side between 2000
#   and 2023. China moves a lot along PC1 (its grid grows), but stays on the
#   dirty side. France and Norway barely move (steady nuclear and hydro).

# =============================================================================
# CONCLUSIONS
# =============================================================================
#
# 1. The world's electricity systems split into two groups: very clean hydro
#    and nuclear grids, and fossil grids. Not many countries are in between.
# 2. Being rich doesn't mean having a clean grid (only r = 0.2 between energy
#    use and carbon intensity). Natural resources (rivers) and policy choices
#    (nuclear, support for renewables) matter more.
# 3. Between 2000 and 2023 oil almost disappears from electricity, gas takes
#    over in many countries, and wind + solar go from almost zero to a real
#    share. But coal's global share hardly falls, because of growth in Asia.
# 4. After label and one-hot encoding, PCA reduces 18 variables to 6
#    components (eigenvalue above 1). The first four can be read as: system
#    size, clean vs. dirty mix, region, and the recent wind/solar transition.
#    The "clean vs dirty" component is robust: it stays the same without the
#    encoded columns and without the duplicate variables, and still shows up
#    when we use only one year. These components could be used later for
#    clustering or modelling, without the overlap (multicollinearity) between
#    the original variables.
#
# Limitations:
# - Pooling the years: I treat every country-year as a separate, independent
#   observation, but they aren't. Germany 2015 and Germany 2016 are almost the
#   same row, so a country with 24 years of data counts more than one with
#   fewer years (the median country has all 24 years, but the minimum is 10,
#   as the summary in the "final clean dataset" step shows), and the PCA partly sees the same country many times. Check C
#   (2023 only) shows the main structure survives with one row per country,
#   but a stricter analysis could use country averages or one year.
# - Every country also counts the same no matter its size, so a tiny island
#   weighs as much as China.
# - Built-in duplicates: generation = population x per person, the shares add
#   up to 100 % (compositional data), and carbon intensity mostly follows the
#   shares. A method made for compositional data (e.g. a log-ratio transform)
#   would be more correct for the shares.
# - GDP had to be dropped because about 28 % of it was missing.
# - "Dominant source" is a simplification for countries with a mixed grid.
# - PCA is a linear method, while the shares (with lots of zeros) are not
#   normally distributed.
#

# =============================================================================
# REFERENCES (Harvard style)
# =============================================================================
#
# Arel-Bundock, V., Enevoldsen, N. and Yetman, C.J. (2018) 'countrycode: an R
#   package to convert country names and country codes', Journal of Open
#   Source Software, 3(28), p. 848. doi: 10.21105/joss.00848.
#
# Auguie, B. (2026) gridExtra: miscellaneous functions for 'grid' graphics.
#   R package version 2.3.1. doi: 10.32614/CRAN.package.gridExtra. Available
#   at: https://CRAN.R-project.org/package=gridExtra (Accessed: 20 November
#   2026).
#
# IEA (2024) CO2 emissions in 2023. Paris: International Energy Agency.
#   Available at: https://www.iea.org/reports/co2-emissions-in-2023
#   (Accessed: 20 November 2026).
#
# Jolliffe, I.T. and Cadima, J. (2016) 'Principal component analysis: a review
#   and recent developments', Philosophical Transactions of the Royal Society
#   A, 374(2065), 20150202. doi: 10.1098/rsta.2015.0202.
#
# Kaiser, H.F. (1960) 'The application of electronic computers to factor
#   analysis', Educational and Psychological Measurement, 20(1), pp. 141-151.
#   doi: 10.1177/001316446002000116.
#
# R Core Team (2026) R: a language and environment for statistical computing.
#   Version 4.6.1. Vienna: R Foundation for Statistical Computing.
#   doi: 10.32614/R.manuals. Available at: https://www.R-project.org/
#   (Accessed: 20 November 2026).
#
# Ritchie, H., Rosado, P. and Roser, M. (2023) 'Energy', Our World in Data.
#   Available at: https://github.com/owid/energy-data
#   (Accessed: 20 November 2026).
#
# Wickham, H. et al. (2019) 'Welcome to the tidyverse', Journal of Open
#   Source Software, 4(43), p. 1686. doi: 10.21105/joss.01686.
#
# Wickham, H., Pedersen, T.L. and Seidel, D. (2025) scales: scale functions
#   for visualization. R package version 1.4.0. doi:
#   10.32614/CRAN.package.scales. Available at:
#   https://CRAN.R-project.org/package=scales (Accessed: 20 November 2026).
