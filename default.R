### Necessities

library(ggplot2)
library(plotly)
library(dplyr)

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












