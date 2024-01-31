# load packages
library(tidyverse)
library(ggplot2)
library(readxl)

# get the data here: https://population.un.org/wpp/Download/Standard/MostUsed/

# download the data
world_data <- read_excel("child_mortality/data/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx")
glimpse(world_data)
head(world_data)

# select country name, year, population, birth rate and child mortality rate 
world_mortality <- world_data %>% 
  select(`Region, subregion, country or area *`, 
         Year,
         `Total Population, as of 1 January (thousands)`, 
         `Total Fertility Rate (live births per woman)`,
         `Under-Five Mortality (deaths under age 5 per 1,000 live births)`
  ) %>%
  rename(Country = `Region, subregion, country or area *`, 
         Population = `Total Population, as of 1 January (thousands)`, 
         BirthRate = `Total Fertility Rate (live births per woman)`,
         ChildMortality = `Under-Five Mortality (deaths under age 5 per 1,000 live births)`
  )


glimpse(world_mortality)

# plot Babies per Woman against children surviving to age 5 with population as bubble size for every year

for (year in unique(world_mortality$Year)) {
  plot <- world_mortality %>% 
    filter(Year == year) %>% 
    arrange(-Population) %>% 
    ggplot() +
    # add rectangles
    geom_rect(aes(xmin = 5, xmax = 8, ymin = 55, ymax = 95),
              fill = NA, 
              color = "#c0c5ce",
              size = 1.5) +
    geom_rect(aes(xmin = 1, xmax = 3.5, ymin = 90, ymax = 105),
              fill = NA, 
              color = "#c0c5ce",
              size = 1.5) + 
    # add scattered points with outlines
    geom_point(aes(BirthRate, (1000 - ChildMortality)/10,  size =  Population),
               shape = 21, stroke = 0.2,
               color = 'white',
               fill = '#343d46') + 
    # add axis
    
    # add text to the plot
    geom_text(x = -6.5, y = 98, label = "DEVELOPING",  
              color = "#c0c5ce", size = 11, family = "Roboto") + 
    geom_text(x = -2.25, y = 108, label = "DEVELOPED",  
              color = "#c0c5ce", size = 11, family = "Roboto") + 
    geom_text(x = -2.25, y = 51,label = year, vjust = -0.1, color = "#dddddd", size = 22, family = "Roboto") +
    geom_text(x = -1, y = 67,label = "Bubble size shows country population", angle = 90, color = "#dddddd", size = 4, family = "Roboto Thin") +
    geom_text(x = -6.5, y = 53,label = "Big families and many children die",  
              color = "#343d46", size = 4, family = "Roboto Thin") +
    geom_text(x = -2.25, y = 87, label = "Small families and\n few children die", 
              color = "#343d46", size = 4, family = "Roboto Thin") +
    labs(x = "Babies per woman", y ="Children surviving to age 5") + 
    # set the scale for the axis
    scale_size(range = c(1, 15)) + 
    scale_y_continuous(limits = c(50,115),
                       breaks = c(50,60,70,80,90,100), 
                       labels = c("50%","60%","70%","80%","90%","100%"),
                       expand = c(0, 0)) +
    scale_x_reverse(breaks = c(8:1), limits = c(8,1)) +
    # set the theme
    theme(text = element_text(family = "Roboto Light"), 
          axis.text.x = element_text(color = "#343d46"),
          axis.text.y = element_text(color = "#343d46"),
          axis.title = element_text(color = "#343d46"),
          panel.background = element_rect(fill = "white"), 
          legend.position = "none")
  # save the plot
  filename <- paste("child_mortality/plots/child_mortality_", year, ".svg", sep = "")
  ggsave(filename, plot = plot, width = 8, height = 6)
}

