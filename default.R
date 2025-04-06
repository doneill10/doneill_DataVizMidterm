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





