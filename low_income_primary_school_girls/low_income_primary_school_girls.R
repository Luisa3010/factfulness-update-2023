# load packages 
library(tidyverse)
library(ggplot2)
library(grid) 

# get the data here: https://github.com/open-numbers/ddf--gapminder--systema_globalis/blob/master/global-regions-datapoints/ddf--datapoints--primary_school_completion_percent_of_girls--by--income_groups--time.csv

# load the data
school_time_df <- read_csv("low_income_primary_school_girls/data/ddf--datapoints--primary_school_completion_percent_of_girls--by--income_groups--time.csv")

school_time_df <- rename(school_time_df, girls_perc = primary_school_completion_percent_of_girls)
glimpse(school_time_df)

# plot the number of girls finishing primary school against time
main_plot <- school_time_df %>% 
  filter(income_groups == "low_income") %>% 
  # plot the data
  ggplot() +
  geom_line(aes(time, girls_perc), color = "darkblue", size = 0.8) +
  # add line for 2017
  geom_vline(xintercept = 2017, linetype = 2, color =	"darkgreen" ) +
  # add rectangle for the subplot area 
  geom_rect(aes(xmin = 2016.5, xmax = 2022.5, ymin = 60.5, ymax = 65.5), color = "lightgrey", fill = "NA",size = 1.3) +
  geom_rect(aes(xmin = 2016.3, xmax = 2022.7, ymin = 60.3, ymax = 65.7), color = "darkgreen", fill = "NA", size = .3) +
  # add labels
  labs(x = "Year", y ="Percent of girls finishing primary school", color = "#777777") +
  # set the theme
  theme(text = element_text(family = "Roboto Light"), 
        axis.text.x = element_text(color = "#777777"),
        axis.text.y = element_text(color = "#777777"),
        axis.title = element_text(color = "#777777"),
        panel.background = element_rect(fill = "white"), 
        legend.position = "none",
        axis.line = element_line(color = "#777777"))

# create a zoomed plot for the years 2017 to 2022
sub_plot <- school_time_df %>% 
  filter(income_groups == "low_income" & time > 2016) %>% 
  ggplot() +
  # plot the data
  geom_line(aes(time, girls_perc),color = "darkblue", size = 1.2) + 
  # add line for 2017
  geom_vline(xintercept = 2017, linetype = 2, color =	"darkgreen", size = 0.8 ) +
  # set the theme
  theme(text = element_text(family = "Roboto Light"), 
      axis.text.x = element_text(color = "#777777"),
      axis.text.y = element_text(color = "#777777"),
      axis.title = element_blank(),
      panel.background = element_rect(fill = "white"), 
      legend.position = "none",
      axis.line = element_line(color = "#777777")
      )
# Convert the zoomed plot to a grob
zoomed_grob <- ggplotGrob(sub_plot)

# Add the zoomed plot to the main plot
combined_plot <- main_plot +
  geom_rect(aes(xmin = 1969, xmax = 2002, ymin = 43, ymax = 70), color = "darkgreen", fill = "lightgrey") +
  annotation_custom(grob = zoomed_grob, xmin = 1970, xmax = 2001, ymin = 44, ymax = 69) 
   
# save the plot
filename = "low_income_primary_school_girls/percent_of_girls_finishing_primary_school_combined.svg" 
ggsave(filename, plot = combined_plot, width = 8, height = 6)
