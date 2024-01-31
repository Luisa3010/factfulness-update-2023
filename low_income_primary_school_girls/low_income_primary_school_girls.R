# load packages 
library(tidyverse)
library(ggplot2)
library(grid) 
library(jpeg)


# get the data here: https://github.com/open-numbers/ddf--gapminder--systema_globalis/blob/master/global-regions-datapoints/ddf--datapoints--primary_school_completion_percent_of_girls--by--income_groups--time.csv

# load the data
school_time_df <- read_csv("low_income_primary_school_girls/data/ddf--datapoints--primary_school_completion_percent_of_girls--by--income_groups--time.csv")

school_time_df <- rename(school_time_df, girls_perc = primary_school_completion_percent_of_girls)
glimpse(school_time_df)


# load girl icons 
girl_half_icon <- readJPEG("low_income_primary_school_girls/icons/child-dress-half.jpg")
girl_half_grob <- rasterGrob(girl_half_icon, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = FALSE)
girl_solid_icon <- readJPEG("low_income_primary_school_girls/icons/child-dress-solid.jpg")
girl_solid_grob <- rasterGrob(girl_solid_icon, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = FALSE)
girl_grey_icon <- readJPEG("low_income_primary_school_girls/icons/child-dress-grey.jpg")
girl_grey_grob <- rasterGrob(girl_grey_icon, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = FALSE)


# plot the number of girls finishing primary school against time
main_plot <- school_time_df %>% 
  filter(income_groups == "low_income") %>% 
  # plot the data
  ggplot() +
  geom_line(aes(time, girls_perc), color = "#343d46", size = 0.8) +
  # add line for 2017
  geom_vline(xintercept = 2017, linetype = 2, color =	"#343d46" ) +
  # add rectangle for the subplot area 
  geom_rect(aes(xmin = 2016.5, xmax = 2022.5, ymin = 60.5, ymax = 65.5), color = "#c0c5ce", fill = "NA",size = 1.3) +
  geom_rect(aes(xmin = 2016.3, xmax = 2022.7, ymin = 60.3, ymax = 65.7), color = "#343d46", fill = "NA", size = .3) +
  # add labels
  labs(x = "Year", y ="Percent of girls finishing primary school", color = "#343d46") +
  geom_text(x = 2013.5, y = 40, label = 2017,  
            color = "#343d46", size = 6, family = "Roboto") + 
  # add icons
  annotation_custom(girl_solid_grob, xmin=1970, xmax=1972.2, ymin=33, ymax=37) +
  annotation_custom(girl_grey_grob, xmin=1972.3, xmax=1974.5, ymin=33, ymax=37) +
  annotation_custom(girl_grey_grob, xmin=1974.6, xmax=1976.8, ymin=33, ymax=37) +
  annotation_custom(girl_solid_grob, xmin=2007, xmax=2009.2, ymin=62, ymax=66) +
  annotation_custom(girl_solid_grob, xmin=2009.3, xmax=2011.5, ymin=62, ymax=66) +
  annotation_custom(girl_grey_grob, xmin=2011.6, xmax=2013.8, ymin=62, ymax=66) +
  annotation_custom(girl_solid_grob, xmin=2008, xmax=2010.2, ymin=44, ymax=48) +
  annotation_custom(girl_half_grob, xmin=2010.3, xmax=2012.5, ymin=44, ymax=48) +
  annotation_custom(girl_grey_grob, xmin=2012.6, xmax=2014.8, ymin=44, ymax=48) +
  # set the theme
  theme(text = element_text(family = "Roboto Light"), 
        axis.text.x = element_text(color = "#343d46"),
        axis.text.y = element_text(color = "#343d46"),
        axis.title = element_text(color = "#343d46"),
        panel.background = element_rect(fill = "white"), 
        legend.position = "none",
        axis.line = element_line(color = "#343d46"))

# create a zoomed plot for the years 2017 to 2022
sub_plot <- school_time_df %>% 
  filter(income_groups == "low_income" & time > 2016) %>% 
  ggplot() +
  # plot the data
  geom_line(aes(time, girls_perc),color = "#343d46", size = 1.2) + 
  # add line for 2017
  geom_vline(xintercept = 2017, linetype = 2, color =	"#343d46", size = 0.8 ) +
  # set the theme
  theme(text = element_text(family = "Roboto Light"), 
      axis.text.x = element_text(color = "#343d46"),
      axis.text.y = element_text(color = "#343d46"),
      axis.title = element_blank(),
      panel.background = element_rect(fill = "white"), 
      legend.position = "none",
      axis.line = element_line(color = "#343d46")
      )
# Convert the zoomed plot to a grob
zoomed_grob <- ggplotGrob(sub_plot)

# Add the zoomed plot to the main plot
combined_plot <- main_plot +
  geom_rect(aes(xmin = 1969, xmax = 2002, ymin = 43, ymax = 70), color = "#343d46", fill = "#c0c5ce") +
  annotation_custom(grob = zoomed_grob, xmin = 1970, xmax = 2001, ymin = 44, ymax = 69) 

# preview the finished plot
combined_plot

# save the plot
filename = "low_income_primary_school_girls/percent_of_girls_finishing_primary_school_combined.svg" 
ggsave(filename, plot = combined_plot, width = 8, height = 6)





# load data seperated by countries

ddf--datapoints--primary_school_completion_percent_of_girls--by--geo--time.csv
