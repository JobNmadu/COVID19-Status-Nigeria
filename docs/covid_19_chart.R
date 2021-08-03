#devtools::install_github("RamiKrispin/coronavirus", force = TRUE) #load updates to data in github

#library(coronavirus)
library(tidyverse)
library(gganimate)
library(transformr)
library(gifski)
library(magick)
library(lubridate)
library(grid)
library(scales)

#DD <- data("coronavirus")
#head(DD)
#tail(coronavirus)
#names(DD)


# Subset Nigsie Data and Update labels
#xNig <- coronavirus %>% 
#  filter(Country.Region == "Nigeria")
#colnames(xNig) <- c("Region", "Country", "Lat", "Long", "Date", "Cases", "Type")
#xNig$Region <- factor(xNig$Region, labels = c("Lagos",	"FCT",	"Osun",	"Edo",	"Oyo",	"Ogun",	"Bauchi",	"Kaduna",	"Akwa Ibom",	"Katsina",	"Kwara",	"Kano",	"Delta",	"Ondo",	"Enugu",	"Ekiti",	"Rivers",	"Benue",	"Niger",	"Anambra"
#))

# Nigtralia by Region (Region/Territory)
#cumDat <- xNig %>%
#  filter(Region != "Diamond Princess") %>% #removed Diamond Princess in original iteration
#  filter(Type == "confirmed") %>%
#  filter(Cases != -1) %>% 
#  group_by(Region) %>%
#  mutate(cum_Cases = cumsum(Cases)) %>%
#  mutate(log_cum_Cases = log10(cumsum(Cases)+1)) %>% # Add 0 for transformation after cumSum 
#  ungroup()

library(readxl)
cumDat <- read_excel("cumDat.xlsx")
cumDat$Date <- as.Date(cumDat$Date)

# Create formatted labels
cumDatformatted <- cumDat %>%
  group_by(Date) %>%
  mutate(Value_lbl = paste0("  ",cum_Cases)) %>%
  group_by(Region) %>% 
  ungroup()


# Create plots (p1 = cumulative sum, p2 = natural log of cumulative sum)
cbPalette <- c("royalblue3", "salmon", "salmon3", "seagreen", "seagreen3",
               "seashell1", "seashell4", "sienna2", "skyblue", "skyblue3",
               "slateblue1", "slateblue4", "slategray2", "slategrey", "snow2",
               "springgreen", "springgreen3", "steelblue1", "steelblue4", "tan2",
               "thistle", "thistle3", "tomato1", "tomato4", "turquoise2",
               "blue", "red", "gold", "forestgreen", "darkblue", "darkcyan",
               "darkgoldenrod", "azure3", "bisque", "bisque3", "white",
               "aliceblue", "chocolate", "aquamarine")

p1 <- ggplot(cumDatformatted, aes(Date, cum_Cases, group = Region, colour = Region)) + 
  geom_line(size=1.2) + 
  scale_colour_manual(values=cbPalette) +
  theme_minimal() +
  geom_text(aes(y=cum_Cases, label = Value_lbl, hjust=0)) + # Adds changing text
  transition_reveal(Date) +  # Changes plot by date
  coord_cartesian(clip = 'off') + 
  labs(title = 'Date: {frame_along}',
       y = "Cumulative Confirmed Cases", 
       caption = "COVID-19 | Prof. Job Nmadu  |  Twitter: @JobNmadu  |") + 
  scale_x_date(limits = as.Date(c('2020-02-29','2020-09-01'))) +  #I add 2 weeks to x-axis so you can see the numbers
  theme(axis.title.x = element_text(face="bold", size=14),
        axis.title.y = element_text(face="bold", size=14),
        plot.caption = element_text(hjust=1, face="italic", color="#333333"),
        legend.position = c(.05, .95),
        legend.justification = c("left", "top")) 

a_gif <- animate(p1, 200, width = 600, height = 500, fps = 10, end_pause = 10)
#aa_gif <- animate(p1, 200, fps = 10, width = 420, height = 400, end_pause = 10)
a_mgif <- image_read(a_gif)

# Save GIF
savename <- file.path(paste0("a_mgif_", format(Sys.Date(), "%d%m%Y"), ".gif")) #current date, data for day before
a_mgif %>%
  image_write(path=savename)

p2 <- ggplot(cumDatformatted, aes(Date, log_cum_Cases, group = Region, colour = Region)) + 
  geom_line(size=1.2) + 
  scale_colour_manual(values=cbPalette) +
  theme_minimal() +
  geom_text(aes(y=log_cum_Cases, label = Value_lbl, hjust=0)) + # Adds changing text
  transition_reveal(Date) +  # Changes plot by date
  coord_cartesian(clip = 'off') + 
  labs(title = " ",
       y = "natural log of Cumulative Confirmed Cases", 
       caption = "Source: coronavirus package, last updated 10/05/2020") + 
  scale_x_date(limits = as.Date(c('2020-02-29','2020-09-01'))) + #I add 2 weeks to x-axis so you can see the numbers
  scale_y_continuous(breaks = c(0,1,2,3),
                     labels = c(1,10,100,1000)) +
  theme(axis.title.x = element_text(face="bold", size=14),
        axis.title.y = element_text(face="bold", size=14),
        plot.caption = element_text(hjust=4.8, face="italic", color="#333333"),
        legend.position = c(.05, .95),
        legend.justification = c("left", "top"))

b_gif <- animate(p2, 200, width = 600, height = 500, fps = 10, end_pause = 10)
b_mgif <- image_read(b_gif)

# Save GIF
savename <- file.path(paste0("b_mgif_", format(Sys.Date(), "%d%m%Y"), ".gif")) #current date, data for day before
b_mgif %>%
  image_write(path=savename)

p4 <- ggplot(cumDatformatted, aes(Date, Cases, group = Region, colour = Region)) + 
  geom_line(size=1.2) + 
  scale_colour_manual(values=cbPalette) +
  theme_minimal() +
  geom_text(aes(y= Cases, label = Value_lbl, hjust=0)) + # Adds changing text
  transition_reveal(Date) +  # Changes plot by date
  coord_cartesian(clip = 'off') + 
  labs(title = " ",
       y = "Confirmed Cases", 
       caption = "Source: coronavirus package, last updated 10/05/2020") + 
  scale_x_date(limits = as.Date(c('2020-02-29','2020-09-01'))) + #I add 2 weeks to x-axis so you can see the numbers
  scale_y_continuous(breaks = c(0,1,2,3),
                     labels = c(1,10,100,1000)) +
  theme(axis.title.x = element_text(face="bold", size=14),
        axis.title.y = element_text(face="bold", size=14),
        plot.caption = element_text(hjust=4.8, face="italic", color="#333333"),
        legend.position = c(.05, .95),
        legend.justification = c("left", "top"))

c_gif <- animate(p4, 200, width = 600, height = 500, fps = 10, end_pause = 10)
c_mgif <- image_read(c_gif)

# Save GIF
savename <- file.path(paste0("c_mgif_", format(Sys.Date(), "%d%m%Y"), ".gif")) #current date, data for day before
c_mgif %>%
  image_write(path=savename)

p3 <- ggplot(cumDatformatted, aes(Date, log_cum_Cases, group = Region, colour = Region)) + 
  geom_line(size=1.2) + 
  scale_colour_manual(values=cbPalette) +
  theme_minimal() +
  geom_text(aes(y=log_cum_Cases, label = Value_lbl, hjust=0)) + # Adds changing text
  transition_reveal(Date) +  # Changes plot by date
  coord_cartesian(clip = 'off') + 
  labs(title = " ",
       y = "natural log of Cumulative Confirmed Cases", 
       caption = "Source: coronavirus package, last updated 10/05/2020") + 
  scale_x_date(limits = as.Date(c('2020-01-22','2020-05-31'))) + #I add 2 weeks to x-axis so you can see the numbers
  scale_y_continuous(breaks = c(0,1,2,3),
                     labels = c(1,10,100,1000)) +
  theme(axis.title.x = element_text(face="bold", size=14),
        axis.title.y = element_text(face="bold", size=14),
        plot.caption = element_text(hjust=4.8, face="italic", color="#333333"),
        legend.position = "none")

#bb_gif <- animate(p3, 200, fps = 10, width = 270, height = 400, end_pause = 10)

# Combine GIF (i must be indexed from 2 to the number of frames)
aa_mgif <- image_read(a_gif)
bb_mgif <- image_read(b_gif)

new_gif <- image_append(c(aa_mgif[1], bb_mgif[1]))
for(i in 2:200){ 
  combined <- image_append(c(aa_mgif[i], bb_mgif[i]))
  new_gif <- c(new_gif, combined)
}

#new_gif

# Save GIF
savename <- file.path(paste0("C:/Users/Job Nmadu/Desktop/COVID_data/COVID_Nig_", format(Sys.Date(), "%d%m%Y"), ".gif")) #current date, data for day before
new_gif %>%
  image_write(path=savename)



#xlsx::write.xlsx(cumDatformatted, file = "C:/Users/Job Nmadu/Desktop/COVID_data/cumDataformatted.xlsx")


library(readxl)
covid <- read_excel("C:/Users/Job Nmadu/Desktop/COVID_data/covid.xlsx", sheet = "Sheet4")
library(tidyr)
COVID <- covid %>%
  pivot_longer(-c("Date", "case"), names_to = "region", values_to = "Cases")

library(xlsx)

file <- "C:/Users/Job Nmadu/Desktop/COVID_DATA/COVID_FORM.xlsx"
xlsx::write.xlsx(COVID, file)
