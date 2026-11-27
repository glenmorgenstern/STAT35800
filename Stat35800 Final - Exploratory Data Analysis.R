# STAT 35800 - Final Project
# Chicago Rodent-Related 311 Service Requests


# 1 -  Packages and Data
library(tidyverse)
library(lubridate)
data <- readRDS("chicago_rodents_2022_2026.rds")


# 2 - Preliminary Data Inspection
dim(data);names(data);str(data)
head(data)
View(data)
range(data$CREATED_DATE_PARSED, na.rm = TRUE)
sum(duplicated(data$SR_NUMBER))



# 3 - Missing Data Assessment
colSums(is.na(data))
round(colMeans(is.na(data)) * 100, 3)

# Number of incomplete/complete observations
sum(!complete.cases(data));sum(complete.cases(data))

# Missing vals summary table
missing_summary <- tibble(
  Variable = names(data),
  Missing = colSums(is.na(data)),
  Percent_Missing = round(colMeans(is.na(data)) * 100, 3)
) %>%
  arrange(desc(Missing))
print(missing_summary, n = Inf)



# 4 - Temporal and Geographic Coverage
# Number of requests by year
table(year(data$CREATED_DATE_PARSED))

# Number of requests by calendar month
table(month(data$CREATED_DATE_PARSED, label = TRUE))

# Number of unique community areas and wards
n_distinct(data$COMMUNITY_AREA, na.rm = TRUE)
n_distinct(data$WARD, na.rm = TRUE)

# Check community area and ward identifiers
sort(unique(data$COMMUNITY_AREA))
sort(unique(data$WARD))



# 5 - Request Characteristics
# Request status distribution
table(data$STATUS, useNA = "ifany")

# Percentage distribution of request status
round(prop.table(table(data$STATUS)) * 100, 2)

# Request origin distribution
table(data$ORIGIN, useNA = "ifany")

# Percentage distribution of known request origins
round(prop.table(table(data$ORIGIN)) * 100, 2)



# 6 - Exploratory Data Analysis

# Exclude incomplete November 2026 from monthly analyses
data_full_months <- data %>%
  filter(CREATED_DATE_PARSED < as.Date("2026-11-01"))


# ------------------------------------------------------------
# 6.1 Monthly Citywide Trends
monthly_counts <- data_full_months %>%
  mutate(
    Month = floor_date(CREATED_DATE_PARSED, unit = "month")
  ) %>%
  count(Month, name = "Requests")

ggplot(monthly_counts, aes(x = Month, y = Requests)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 1.5) +
  scale_x_datetime(
    date_breaks = "6 months",
    date_labels = "%b %Y"
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Monthly Rodent-Related 311 Requests in Chicago",
    subtitle = "January 2022 – October 2026",
    x = "Date",
    y = "Number of Service Requests"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# ------------------------------------------------------------
# 6.2 Seasonal Patterns by Year
seasonal_counts <- data_full_months %>%
  mutate(
    Year = factor(year(CREATED_DATE_PARSED)),
    Month = month(CREATED_DATE_PARSED, label = TRUE)
  ) %>%
  count(Year, Month, name = "Requests")

ggplot(
  seasonal_counts,
  aes(x = Month, y = Requests, color = Year, group = Year)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Seasonal Patterns in Rodent-Related Requests",
    subtitle = "January 2022 – October 2026",
    x = "Month",
    y = "Number of Service Requests",
    color = "Year"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 6.3 Geographic Distribution of Service Requests

# Total requests by community area across the full study period
community_counts <- data %>%
  filter(!is.na(COMMUNITY_AREA)) %>%
  count(COMMUNITY_AREA, name = "Requests") %>%
  arrange(desc(Requests))

# Display the top 15 community areas
head(community_counts, 15)

# Community area name lookup table
community_names <- tibble(
  COMMUNITY_AREA = c(
    24, 6, 15, 22, 64, 56, 7, 25,
    16, 19, 17, 2, 60, 23, 61
  ),
  Community_Name = c(
    "West Town",
    "Lake View",
    "Portage Park",
    "Logan Square",
    "Clearing",
    "Garfield Ridge",
    "Lincoln Park",
    "Austin",
    "Irving Park",
    "Belmont Cragin",
    "Dunning",
    "West Ridge",
    "Bridgeport",
    "Humboldt Park",
    "New City"
  )
)

# Join community area names to request counts
top15_communities <- community_counts %>%
  slice_max(Requests, n = 15, with_ties = FALSE) %>%
  left_join(community_names, by = "COMMUNITY_AREA")

print(top15_communities, n = 15)

# Plot top 15 community areas, highlighting Lincoln Park
ggplot(
  top15_communities,
  aes(
    x = reorder(Community_Name, Requests),
    y = Requests,
    fill = Community_Name == "Lincoln Park"
  )
) +
  geom_col() +
  geom_text(
    aes(label = scales::comma(Requests)),
    hjust = -0.15,
    size = 3.5
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "FALSE" = "steelblue",
      "TRUE" = "darkgreen"
    ),
    guide = "none"
  ) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Top 15 Community Areas by Rodent-Related Requests",
    subtitle = "January 2022 – November 23, 2026",
    x = "Community Area",
    y = "Total Service Requests"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 6.4 Monthly Trends in Lincoln Park
lincoln_park_counts <- data_full_months %>%
  filter(COMMUNITY_AREA == 7) %>%
  mutate(
    Month = floor_date(CREATED_DATE_PARSED, unit = "month")
  ) %>%
  count(Month, name = "Requests") %>%
  complete(
    Month = seq(
      as.POSIXct("2022-01-01", tz = "UTC"),
      as.POSIXct("2026-10-01", tz = "UTC"),
      by = "month"
    ),
    fill = list(Requests = 0)
  )

ggplot(lincoln_park_counts, aes(x = Month, y = Requests)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen", size = 1.5) +
  scale_x_datetime(
    date_breaks = "6 months",
    date_labels = "%b %Y"
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Monthly Rodent-Related Requests in Lincoln Park",
    subtitle = "Community Area 7 | January 2022 – October 2026",
    x = "Date",
    y = "Number of Service Requests"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# ------------------------------------------------------------
# 6.5 Lincoln Park vs. Rest of Chicago

# Standardize monthly requests relative to each area's
# average monthly request count in 2022
comparison_counts <- data_full_months %>%
  filter(!is.na(COMMUNITY_AREA)) %>%
  mutate(
    Month = floor_date(CREATED_DATE_PARSED, unit = "month"),
    Area = if_else(
      COMMUNITY_AREA == 7,
      "Lincoln Park",
      "Rest of Chicago"
    )
  ) %>%
  count(Area, Month, name = "Requests") %>%
  group_by(Area) %>%
  mutate(
    Baseline = mean(Requests[year(Month) == 2022]),
    Index = 100 * Requests / Baseline
  ) %>%
  ungroup()

ggplot(comparison_counts,
       aes(x = Month, y = Index, color = Area)) +
  geom_line(linewidth = 1) +
  geom_hline(
    yintercept = 100,
    linetype = "dashed",
    color = "gray50"
  ) +
  scale_color_manual(
    values = c(
      "Lincoln Park" = "darkgreen",
      "Rest of Chicago" = "steelblue"
    )
  ) +
  scale_x_datetime(
    date_breaks = "6 months",
    date_labels = "%b %Y"
  ) +
  labs(
    title = "Lincoln Park vs. Rest of Chicago",
    subtitle = "Monthly requests relative to 2022 monthly average",
    x = "Date",
    y = "Request Index (2022 Average = 100)",
    color = "Area"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



# ------------------------------------------------------------
# 6.6 Annual Trends Across Top 10 Community Areas

# Identify the top 10 community areas by total request volume
top10 <- community_counts %>%
  slice_max(Requests, n = 10, with_ties = FALSE) %>%
  pull(COMMUNITY_AREA)

# Restrict each year to January–October for comparability
annual_community <- data_full_months %>%
  filter(
    COMMUNITY_AREA %in% top10,
    month(CREATED_DATE_PARSED) <= 10
  ) %>%
  mutate(
    Year = year(CREATED_DATE_PARSED)
  ) %>%
  count(COMMUNITY_AREA, Year, name = "Requests") %>%
  left_join(community_names, by = "COMMUNITY_AREA")

ggplot(
  annual_community,
  aes(
    x = Year,
    y = Requests,
    color = Community_Name,
    group = Community_Name
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2022:2026) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Annual Trends Across Top 10 Community Areas",
    subtitle = "January–October of each year",
    x = "Year",
    y = "Number of Service Requests",
    color = "Community Area"
  ) +
  theme_minimal()



# ------------------------------------------------------------
# 6.7 Spatial Distribution of Service Requests

# Retain observations with valid coordinates within
# approximate Chicago geographic boundaries
spatial_data <- data %>%
  filter(
    !is.na(LATITUDE),
    !is.na(LONGITUDE),
    between(LATITUDE, 41.6, 42.1),
    between(LONGITUDE, -88.0, -87.5)
  )

# Number of observations retained for spatial analysis
nrow(spatial_data)

# Number of observations excluded from spatial analysis
nrow(data) - nrow(spatial_data)

ggplot(spatial_data,
       aes(x = LONGITUDE, y = LATITUDE)) +
  geom_bin2d(bins = 100) +
  scale_fill_viridis_c(
    trans = "log10",
    labels = scales::comma
  ) +
  coord_quickmap() +
  labs(
    title = "Spatial Distribution of Rodent-Related Requests",
    subtitle = "January 2022 – November 23, 2026",
    x = "Longitude",
    y = "Latitude",
    fill = "Requests"
  ) +
  theme_minimal()



# ------------------------------------------------------------
# 6.8 Heatmap of Monthly Service Requests by Year

# Reuse seasonal_counts from Section 6.2
ggplot(
  seasonal_counts,
  aes(x = Month, y = Year, fill = Requests)
) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = scales::comma(Requests)),
    size = 3
  ) +
  scale_fill_gradient(
    low = "lightyellow",
    high = "darkorange",
    labels = scales::comma
  ) +
  labs(
    title = "Monthly Rodent-Related Requests by Year",
    subtitle = "January 2022 – October 2026",
    x = "Month",
    y = "Year",
    fill = "Requests"
  ) +
  theme_minimal()






# ------------------------------------------------------------
# 6.9 Annual Relative Trends in Lincoln Park vs. Chicago

annual_comparison <- comparison_counts %>%
  filter(month(Month) <= 10) %>%
  mutate(Year = year(Month)) %>%
  group_by(Area, Year) %>%
  summarise(
    Requests = sum(Requests),
    .groups = "drop"
  ) %>%
  group_by(Area) %>%
  mutate(
    Baseline = Requests[Year == 2022],
    Index = 100 * Requests / Baseline,
    Percent_Change = Index - 100
  ) %>%
  ungroup()

print(annual_comparison, n = Inf)

ggplot(annual_comparison,
       aes(x = Year, y = Index, color = Area)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(
    yintercept = 100,
    linetype = "dashed",
    color = "gray50"
  ) +
  scale_color_manual(
    values = c(
      "Lincoln Park" = "darkgreen",
      "Rest of Chicago" = "steelblue"
    )
  ) +
  scale_x_continuous(breaks = 2022:2026) +
  labs(
    title = "Annual Relative Trends in Rodent-Related Requests",
    subtitle = "January–October of each year | 2022 = 100",
    x = "Year",
    y = "Request Index",
    color = "Area"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 6.10 Lincoln Park's Share of Citywide Requests

lincoln_park_share <- annual_comparison %>%
  group_by(Year) %>%
  summarise(
    Lincoln_Park_Requests = Requests[Area == "Lincoln Park"],
    Total_Requests = sum(Requests),
    Percent_Share = 100 * Lincoln_Park_Requests / Total_Requests,
    .groups = "drop"
  )

print(lincoln_park_share)

ggplot(lincoln_park_share,
       aes(x = Year, y = Percent_Share)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen", size = 3) +
  geom_text(
    aes(label = paste0(round(Percent_Share, 2), "%")),
    vjust = -1,
    size = 3.5
  ) +
  scale_x_continuous(breaks = 2022:2026) +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0.1, 0.2))
  ) +
  labs(
    title = "Lincoln Park's Share of Citywide Rodent-Related Requests",
    subtitle = "January–October of each year",
    x = "Year",
    y = "Percentage of Total Requests"
  ) +
  theme_minimal()



