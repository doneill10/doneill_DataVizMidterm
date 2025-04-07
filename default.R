### Necessities

library(ggplot2)
library(plotly)
library(dplyr)
library(tidyverse)
library(forcats)
library(fmsb)

# Data set Link: https://www.kaggle.com/datasets/faa/wildlife-strikes

### ========================================================================###

### Data Set stuff
### -------------------------------

df <- read.csv("airstrike.csv")
df[df == ""] <- NA



### SINGLE CATE. VAR.
### -------------------------------

# Data Variable Manipulation for Plotting
df_cateCount <- df %>% count(Flight.Phase)
df_cateCount$Flight.Phase <- factor(df_cateCount$Flight.Phase, 
                                    levels = df_cateCount$Flight.Phase[order(-df_cateCount$n)])
# Single Cate. Bar Graph
plot_ly(data = df_cateCount,
  type = "bar",
  x = ~Flight.Phase, y = ~n,
  color = ~Flight.Phase,
  marker = list(
    line = list(
      color = "gray10",
      width = 1
    )
  )
) %>%
  layout(
    title = list(text = "Flight Phase when the Strike Occurred"),
    xaxis = list(title = "Flight Phase"),
    yaxis = list(title = "Count"),
    showlegend = FALSE,
    
    margin = list(t = 80)
  )

# Mostly happening when a plane is Approaching the tarmac, where the plane is 
# low and going over a large stretch of land where wildlife may be roosting.

# Other prevalent cases are other low-altitude phases where, 
# specifically, the engine is still on.


### SINGLE QUAN. VAR.
### -------------------------------

# Single Quan. Histogram
plot_ly(data = df,
        x = ~Height,
        type = "histogram",
        xbins = list(size = 500),
        marker = list(
          line = list(
            color = "midnightblue",
            width = 1
          )
        )
) %>%
  layout(
    title = list(text = "Plane's Altitude Where Strike Occurred"),
    xaxis = list(
      range = c(-20, 10000),
      tickvals = seq(1000, 10000, by = 1000),
      ticktext = paste0(seq(1, 10), "k")
    ),
    yaxis = list(title = "Count"),
    
    margin = list(t = 80, b = 80, l = 70, r = 60)
  )
# Mostly nearer to the ground, which supports the previous graph where 
# the phases usually affected are the low altitude ones.

# Note! There is an extreme outlier where there was a Wildlife Strike 
# at an altitude of 2500 ft.

### TWO CATE. VAR.
### -------------------------------

# Changing values from binary to Yes, No or Unknown
df$Landing.Gear.Strike <- ifelse(is.na(df$Landing.Gear.Strike), "Unknown",
                                 ifelse(df$Landing.Gear.Strike == 1, "Yes", "No"))

# Ordering the Bars
df$Flight.Phase <- df$Flight.Phase %>%
  fct_infreq()

# New DF specifically for graphing Flight Phase vs Landing Gear Strike
summary_df <- df %>%
  filter(!is.na(Landing.Gear.Strike), !is.na(Flight.Phase)) %>%
  count(Flight.Phase, Landing.Gear.Strike)

plot_ly(summary_df,
        x = ~Flight.Phase,
        y = ~n,
        color = ~Landing.Gear.Strike,
        type = "bar",
        marker = list(
          line = list(
            color = "gray10",
            width = 1
          )
        )) %>%
  layout(barmode = "group",
         title = list(text = "If Landing Gears were Struck"),
         xaxis = list(title = "Flight Phase", tickangle = -45),
         yaxis = list(title = "Strike Numbers"),
         legend = list(title = list(text = "Landing Gear Strike")),
         margin = list(t = 80, b = 80, l = 70)
        )

# The dataset is very particular in reporting which specific part(s) of the 
# airplane was affected by the Wildlife Strike, so I took a look to see how 
# often they were hit and when, as they’re quite important for a 
#successful flight.

# The proportion of struck Landing Gears vs Not is quite low - which is good - 
# but it is interesting to note that there is at least one incident for every 
# phase of flight, even the phases where the gears should have been put away.

# It’s also interesting to note that there’s an obvious proportional 
# difference between Approach vs Landing Roll, which notes that planes do run 
# over a number of wildlife.

### ONE CATE. & ONE QUAN
### -------------------------------

top_operators <- df %>%
  count(Operator, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(Operator)

filtered_df <- df %>%
  filter(Operator %in% top_operators, !is.na(Speed))

# Moving Unknown to the end
filtered_df$Operator <- factor(
  filtered_df$Operator,
  levels = c(
    sort(setdiff(unique(filtered_df$Operator), "UNKNOWN")),
    "UNKNOWN"
  )
)

plot_ly(filtered_df,
        x = ~Operator,
        y = ~Speed,
        type = "box",
        boxpoints = "outliers",  # show only outlier dots
        line = list(color = "royalblue4")  # border of the boxes
      ) %>%
  layout(
    title = "Speed Distribution by Operator",
    xaxis = list(title = "Operator", tickangle = -45),
    yaxis = list(title = "Speed (in knots)", range = c(0, 520)),
    margin = list(t = 80, b = 165, l = 70)
  )

# Everyone is generally within the same range of speed, of around 150 knots, 
# however the airlines that deal with packages are consistently faster than 
# regular commercial airlines.

#The Military is an interesting case with the most extreme outliers, with 
# even one at 2500 knots - not seen on the graph.

### TWO QUAN
### -------------------------------

df <- df %>%
  mutate(
    Total_Damage = Radome.Damage + Windshield.Damage + Nose.Damage + 
      Engine1.Damage + Engine2.Damage + Engine3.Damage + Engine4.Damage +
      Propeller.Damage + Wing.or.Rotor.Damage + Fuselage.Damage +
      Landing.Gear.Damage + Tail.Damage + Lights.Damage + Other.Damage
  )

filtered_df <- df %>%
  filter(!is.na(Speed), !is.na(Total_Damage), !is.na(Flight.Phase))

plot_ly(
  data = filtered_df,
  x = ~Speed,
  y = ~Total_Damage,
  color = ~Flight.Phase,
  type = 'scatter',
  mode = 'markers',
  marker = list(size = 6, opacity = 0.7)
) %>%
  layout(
    title = "Total Damage vs Speed Colored by Flight Phase",
    xaxis = list(title = "Speed", range = c(0,520)),
    yaxis = list(title = "Total Damage"),
    legend = list(title = list(text = "<b>Flight Phase</b>")),
    margin = list(t = 80, b = 80, l = 80, r = 80)
  )



### SUNBURSTING IT UP!
### Just kidding I gave up here's a Spiderweb
### -------------------------------
# Replace NA and blank values with "Unknown"
df_clean <- df %>%
  mutate(Aircraft.Type = ifelse(is.na(Aircraft.Type) | Aircraft.Type == "", "Unknown", Aircraft.Type))

# Summarize part strike data by Aircraft.Type
df_summary <- df_clean %>%
  group_by(Aircraft.Type) %>%
  summarise(
    Radome = sum(Radome.Strike, na.rm = TRUE),
    Windshield = sum(Windshield.Strike, na.rm = TRUE),
    Nose = sum(Nose.Strike, na.rm = TRUE),
    Engine1 = sum(Engine1.Strike, na.rm = TRUE),
    Propeller = sum(Propeller.Strike, na.rm = TRUE),
    Wing_or_Rotor = sum(Wing.or.Rotor.Strike, na.rm = TRUE),
    Fuselage = sum(Fuselage.Strike, na.rm = TRUE),
    Landing_Gear = sum(Landing.Gear.Strike, na.rm = TRUE),
    Tail = sum(Tail.Strike, na.rm = TRUE),
    Lights = sum(Lights.Strike, na.rm = TRUE),
    Other = sum(Other.Strike, na.rm = TRUE)
  ) %>%
  arrange(desc(rowSums(across(where(is.numeric)))))

# Select top aircraft types to visualize
top_types <- head(df_summary, 3)
actual_types <- as.character(top_types$Aircraft.Type)

# Normalize each aircraft type's values to proportions
radar_data <- top_types[, -1]
radar_data <- as.data.frame(t(apply(radar_data, 1, function(x) x / sum(x))))

# Add max and min rows for radar chart (needed by fmsb)
radar_data <- rbind(
  rep(1, ncol(radar_data)),  # Max values
  rep(0, ncol(radar_data)),  # Min values
  radar_data
)

# Assign row names
rownames(radar_data) <- c("Max", "Min", actual_types)

# Line colors
colors <- c("black", "red", "green")

# Radar chart
radarchart(radar_data,
           axistype = 1,
           title = "Birdstrike Locations by Aircraft Type",
           pcol = colors, plty = 1, plwd = 2,
           cglcol = "grey", cglty = 1, axislabcol = "grey")

# Add legend labels
legend("topright", legend = actual_types, col = colors, lty = 1, lwd = 2, bty = "n")











