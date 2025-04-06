### Necessities

library(ggplot2)
library(plotly)
library(dplyr)
library(tidyverse)
library(forcats)

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



### SINGLE QUAN. VAR.
### -------------------------------

# Single Quan. Histogram
plot_ly(data = df,
        x = ~Height,
        type = "histogram",
        xbins = list(size = 1000),
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
# Note: Military has a single outlier at 2500 knots



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








# Step 1: Create a mapping of each column to a grouped category
damage_map <- tibble::tibble(
  Damage_Type = c(
    "Engine1.Damage", "Engine2.Damage", "Engine3.Damage", "Engine4.Damage",
    "Radome.Damage", "Windshield.Damage", "Nose.Damage",
    "Propeller.Damage", "Wing.or.Rotor.Damage", "Fuselage.Damage",
    "Landing.Gear.Damage", "Tail.Damage", "Lights.Damage", "Other.Damage"
  ),
  Category = c(
    rep("Engine.Damage", 4),
    rep("Front.Damage", 3),
    "Propeller.Damage", "Wing.Rotor.Damage", "Fuselage.Damage",
    "Landing.Gear.Damage", "Tail.Damage", "Lights.Damage", "Other.Damage"
  )
)

# Step 2: Pivot longer and join with the category mapping
damage_long <- df %>%
  select(all_of(damage_map$Damage_Type)) %>%
  pivot_longer(cols = everything(), names_to = "Damage_Type", values_to = "Value") %>%
  filter(Value > 0) %>%  # Only actual damage cases
  left_join(damage_map, by = "Damage_Type")

# Step 3: Count totals for each Damage_Type and Category
sunburst_data <- damage_long %>%
  group_by(Category, Damage_Type) %>%
  summarise(Count = n(), .groups = "drop")

# Step 4: Prepare data for sunburst (include both parent and child rows)
parent_data <- sunburst_data %>%
  group_by(Category) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  mutate(Damage_Type = Category, Parent = "Total Damage")

child_data <- sunburst_data %>%
  mutate(Parent = Category)

sunburst_ready <- bind_rows(parent_data, child_data) %>%
  select(labels = Damage_Type, parents = Parent, values = Count)

# Step 5: Create the sunburst chart
plot_ly(
  labels = sunburst_ready$labels,
  parents = sunburst_ready$parents,
  values = sunburst_ready$values,
  type = 'sunburst',
  branchvalues = 'total'
) %>%
  layout(
    title = "Aircraft Damage Sunburst Chart (Grouped by Category)"
  )

