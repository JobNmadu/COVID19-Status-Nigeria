
library(dplyr)
library(gganimate)
library(tidyr)

#setwd("C:/Users/583413/Documents/barchart race")
#data = read.csv("budgets.csv")

library(readxl)
cumDaty <- read_excel("cumDat.xlsx")
cumDaty$Date <- as.Date(cumDaty$Date)
options(scipen = 999, digits = 2)
cumDat <- as.data.frame(cumDaty)
cumDat$Month <- factor(cumDat$Month, levels = month.name)
data <- cumDat #%>%
  #gather(Year, Month, Date, Cases, Region) #goes from wide to long

#data$Budget<- as.numeric(gsub(",", "",data$Budget)) #converts to numeric
#data$Year= gsub("X", "", data$Year) #removes the X

ranked_data = data %>%
  group_by(Date) %>%
  mutate(rank = min_rank(-Cases)*1, #for each year, rank the department by the budget (the - makes it desc)
         Value_lbl = paste0(" ", Region, ": ", format(Cases, big.mark = ",", scientific = F)))%>%
  filter(rank <= 10) %>%
  ungroup() 

#creating the anim object (which is a combo of ggplot and gganimate)
anim <- ggplot(ranked_data, aes( #remeber that the x and y axis will be flipped
  x = rank, #the rank is what creates the order for the bars. there should be 10 per year/frame
  y = Region,#the bars should be organized around department
  label = Region,
  group = Region,
  fill = Region
) #groups determine OBJECT PERMENANCE for gganimate. 
#this is very important to ensure that the same bar does not transition in and out 
) +
  geom_tile(
    aes(
      y = Cases/2, #unclear as to why but the y value MUST be the height variable/2
      height = Cases,
      width = 0.9,
      fill = Region # determine the color of the bars
    ), 
    alpha = 0.8, show.legend = TRUE)+
  geom_text(aes( 
    y = Cases, #the labels need to be mapped to the budget bar 
    label = Value_lbl, #the label variable 
    #color = ifelse(Budget > 600000, "#ffffff", "#000000"),
    hjust = ifelse(Cases > 400, 1, 0) # if the budget is large, we want the text to be in the bar instead of outside the bar
  ) #end of aes
  ) + #end of geom_text
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(name = 'Region', values = c("royalblue3", "salmon", "salmon3", "seagreen", "seagreen3",
                                                "seashell1", "seashell4", "sienna2", "skyblue", "skyblue3",
                                                "slateblue1", "slateblue4", "slategray2", "slategrey", "snow2",
                                                "springgreen", "springgreen3", "steelblue1", "steelblue4", "tan2",
                                                "thistle", "thistle3", "tomato1", "tomato4", "turquoise2",
                                                "blue", "red", "gold", "forestgreen", "darkblue", "darkcyan",
                                                "darkgoldenrod", "azure3", "bisque", "bisque3", "white",
                                                "aliceblue", "chocolate", "aquamarine"),
                    guide = guide_legend(
                      direction = "horizontal",
                      title.position = "top"
                    ))+
  scale_x_reverse() +
  theme_minimal()+ # removes a lot of plot elements
  theme(
    plot.title = element_text(color = "#01807e", face = "bold", hjust = 0, size = 30),
    axis.ticks.y = element_blank(), #removes axis ticks
    axis.text.y = element_blank(), #removes axis ticks
    panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(), #removes grid lines
    legend.position = "bottom",
    legend.justification = "left"
  )+
  labs(title = "{closest_state}",#shows the closest state (year) for that frame
       subtitle = "Confirmed Cases",
       y = "",
       x = "",
       caption = "Design: @JobNmadu | COVID-19 cases")+
  
  #this part provides the actual animation
  transition_states(Date, #what variable is going to be the frames
                    transition_length = 6, #relative length of transition frames
                    state_length = 3, #relative length of state frames
                    wrap = FALSE) + #should the gif go back to the start at the end
  ease_aes("cubic-in-out") #how do new elements transition in

#now that we have the anim object, we need to render it
animate(anim, 
        nframes = 500, #more frames for make it smoother but longer to render
        fps = 10, #how many frames are shown per second
        end_pause = 20)
 anim_save("COVID19race.gif", animation = last_animation())

 animate(anim,
         nframes = 500, #more frames for make it smoother but longer to render
         fps = 10, #how many frames are shown per second
         end_pause = 20,
         renderer = ffmpeg_renderer(), 
         width = 800,
         height = 450)
 anim_save("COVID19race.mp4", animation = last_animation())
 
 Colour <- c("red", "blue", "yellow", "green", "purple", "black")
 ggplot(ranked_data) +
   aes(x = Date, y = Cases, fill = rank, colour = Month) +
   geom_line(size = 1L) +
   scale_fill_gradient() +
   scale_color_hue() +
   theme_minimal() +
   facet_wrap(vars(Region))
 
library(tidyverse)
library(gganimate)
theme_set(theme_classic())


library(readxl)
cumDaty <- read_excel("C:/Users/Job Nmadu/Dropbox/COVID_data/cumDat.xlsx")
cumDaty$Date <- as.Date(cumDaty$Date)
options(scipen = 999, digits = 2)
cumDat <- as.data.frame(cumDaty)
cumDat$Month <- factor(cumDat$Month, levels = month.name)
data <- cumDat

gap <- cumDat %>%
  group_by(Date) %>%
  mutate(rank = min_rank(-Cases) * 1,
         Value_rel = Cases/Cases[rank==1],
         Value_lbl = paste0(" ", Cases)) %>%
  filter(rank <=10) %>%
  ungroup()

p <- ggplot(gap, aes(rank, group = Region, stat = "identity",
                     fill = as.factor(Region), color = as.factor(Region))) +
  geom_tile(aes(y = Cases/2,
                height = Cases,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Region, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=Cases,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  labs(title = "Date {closest_state}", x="", y = "Cases scored") +
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),  # These relate to the axes post-flip
        plot.margin = margin(1,1,1,4, "cm")) +
  transition_states(Date, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')

p

gap %>%
  
  # for each Region, note his the rank from his previous day
  group_by(Region) %>%
  arrange(Date) %>%
  mutate(prev.rank = lag(rank)) %>%
  ungroup() %>%
  
  # for every game day,
  # sort Regions by rank & break ties by previous day's rank
  group_by(Date) %>%
  arrange(rank, prev.rank) %>%
  mutate(x = seq(1, n())) %>%
  ungroup() %>%
  
  ggplot(aes(x = x, y = Cases, fill = Region, color = Region)) +
  # geom_tile(aes(y = Cases/2, height = Cases, width = width)) +
  geom_col() +
  geom_text(aes(y = 0, label = Region), hjust = 1) +
  geom_text(aes(label = Value_lbl), hjust = 0) +
  
  # rest of the code below is unchanged from the question
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  labs(title = "Date {closest_state}", x="", y = "Cases") +
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(), 
        axis.text.y  = element_blank(),
        plot.margin = margin(1,1,1,4, "cm")) +
  transition_states(Date, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')



gap %>%
  
  # for each Region, note his the rank from his previous day
  group_by(Region) %>%
  arrange(Date) %>%
  mutate(prev.rank = lag(rank)) %>%
  ungroup() %>%
  
  # for every game day & every rank,
  # reduce tile width if there are multiple Regions sharing that rank, 
  # sort Regions in order of who reached that rank first, 
  # & calculate the appropriate tile midpoint depending on how many Regions are there
  group_by(Date, rank) %>%
  mutate(n = n_distinct(Region)) %>%
  mutate(width = 0.9 / n_distinct(Region)) %>%
  arrange(prev.rank) %>%
  mutate(x = rank + 0.9 * (seq(1, 2 * n() - 1, by = 2) / 2 / n() - 0.5)) %>%
  ungroup() %>%
  
  ggplot(aes(x = x, fill = Region, color = Region)) +
  geom_tile(aes(y = Cases/2, height = Cases, width = width)) +
  geom_text(aes(y = 0, label = Region), hjust = 1) +
  geom_text(aes(y = Cases, label = Value_lbl), hjust = 0) +
  
  # rest of the code below is unchanged from the question
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  labs(title = "Date {closest_state}", x="", y = "Cases") +
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(), 
        axis.text.y  = element_blank(),
        plot.margin = margin(1,1,1,4, "cm")) +
  transition_states(Date, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')

