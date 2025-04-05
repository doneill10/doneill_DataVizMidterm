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
  color = ~Flight.Phase
) %>%
  layout(
    title = list(text = "Flight Phase when the Strike Occurred"),
    xaxis = list(title = "Flight Phase"),
    yaxis = list(title = "Count"),
    showlegend = FALSE,
    
  # Title was too close to the top I didn't like it
    margin = list(t = 80)
  )

### SINGLE QUAN. VAR.
### -------------------------------

plot_ly(data = df,
        x = ~Height,
        type = "histogram",
        xbins = list(
          size = 1000
        )
) %>%
  layout(
    xaxis = list(range = c(0,10000))
  )

summary(df$Speed)


















