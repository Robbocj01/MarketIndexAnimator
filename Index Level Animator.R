library(tidyverse)
library(gganimate)
library(transformr)
library(gifski)
library(magick)
library(lubridate)
library(grid)
library(scales)

# Remove any residual lists and free up memory
rm(list = ls())
gc()

#Sets working directory for data load
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Use set.seed function to ensure the results are repeatable
set.seed(5)

df_stock <- read.csv("SPY.csv", stringsAsFactors = FALSE)
df_stock$Date <- as.Date(df_stock$Date, "%Y-%m-%d")

df_risk <- read.csv("VIX-I.csv", stringsAsFactors = FALSE)
df_risk$Date <- as.Date(df_risk$Date, "%Y-%m-%d")

# Subset Stock Data and Update labels
xStock <- df_stock %>% 
  select(Date,Close) %>%
  filter(Date > "2018-12-31")

xRisk <- df_risk %>% 
  select(Date,Close) %>%
  filter(Date > "2018-12-31")

# Create plots (p1 = cumulative sum, p2 = natural log of cumulative sum)
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p1 <- ggplot(xStock, aes(x = Date, y = Close, group = 1)) + 
  geom_line(size=1, color = "#00AFBB") + 
  scale_colour_manual(values=cbPalette) +
  theme_minimal() +
  transition_reveal(Date) +  # Changes plot by date
  coord_cartesian(clip = 'off') + 
  labs(title = 'Date: {frame_along}',
       y = "SP500 Index Level", 
       caption = "Market Correction | Chris Robertson  |  Twitter: @Robbocj01  |") + 
  theme(axis.title.x = element_text(face="bold", size=14),
        axis.title.y = element_text(face="bold", size=14),
        plot.caption = element_text(hjust=1, face="italic", color="#333333"),
        legend.position = c(.05, .95),
        legend.justification = c("left", "top")) 

a_gif <- animate(p1, 200, fps = 25, width = 310, height = 400)

p2 <- ggplot(xRisk, aes(x = Date, y = Close, group = 1)) + 
  geom_line(size=1, color = "#E69F00") + 
  scale_colour_manual(values=cbPalette) +
  theme_minimal() +
  transition_reveal(Date) +  # Changes plot by date
  coord_cartesian(clip = 'off') + 
  labs(y = "VIX Index Level", caption = " ") + 
  theme(axis.title.x = element_text(face="bold", size=14),
        axis.title.y = element_text(face="bold", size=14),
        plot.caption = element_text(hjust=1, face="italic", color="#333333"),
        legend.position = c(.05, .95),
        legend.justification = c("left", "top")) 

b_gif <- animate(p2, 200, fps = 25, width = 270, height = 400)

# Combine GIF (i must be indexed from 2 to the number of frames)
a_mgif <- image_read(a_gif)
b_mgif <- image_read(b_gif)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
for(i in 2:200){ 
  combined <- image_append(c(a_mgif[i], b_mgif[i]))
  new_gif <- c(new_gif, combined)
}

new_gif

# Save GIF
savename <- file.path(paste0("Index Movements_", format(Sys.Date(), "%d%m%Y"), ".gif")) #current date, data for day before
new_gif %>%
  image_write(path=savename)

