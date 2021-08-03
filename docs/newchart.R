library(readxl)
cumDaty <- read_excel("cumDat.xlsx")
cumDaty$Date <- as.Date(cumDaty$Date)
options(scipen = 999, digits = 2)
cumDat <- as.data.frame(cumDaty)

library(dplyr)

DDA <- arrange(cumDat, Region)

DDT <- as.data.frame(table(cumDat$Region, cumDat$Month))
colnames(DDT) <- c("Region", "Month", "Cases")
DDT$Month = factor(DDT$Month, levels = month.name)

DDC <- as.data.frame(table(cumDat$County, cumDat$Month))
colnames(DDC) <- c("County", "Month", "Cases")
DDC$Month = factor(DDC$Month, levels = month.name)

DDP <- DDA %>% 
  group_by(Region) %>% 
  mutate(Day = row_number(), 
         Per = Cases/sum(Cases),
         PerCum = cumsum(Per))

DDF <-  filter(cumDat, Date > "2020-05-31")
DDM <-  filter(cumDat, Date < "2020-05-31")
DDL <- filter(cumDat, Region == "Lagos")


library(ggplot2)

ggplot(DDT) +
  aes(x = Month, colour = Month, weight = Cases) +
  geom_bar(position = "dodge", fill = "#0c4c8a") +
  scale_color_brewer(palette = "Accent") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(vars(Region), scales = "free_y")

ggplot(DDC) +
  aes(x = Month, colour = Month, weight = Cases) +
  geom_bar(position = "dodge", fill = "orangered") +
  scale_fill_distiller() +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(vars(County), scales = "free_y")

ggplot(cumDat) +
  aes(x = Date, y = Cases, fill = Region, colour = Region) +
  geom_line(size = 1L) +
  scale_fill_hue() +
  scale_color_hue() +
  scale_y_continuous(trans = "log") +
  labs(x = "date", y = "Daily confimed cases") +
  ggthemes::theme_map() +
  facet_wrap(vars(Region), scales = "free_y", ncol = 3)

ggplot(cumDat) +
  aes(x = Date, y = Cases, fill = Region, colour = Region) +
  geom_line(size = 1L) +
  scale_fill_hue() +
  scale_color_hue() +
  scale_y_continuous(trans = "log") +
  labs(x = "Date", y = "Daily confirmed cases") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(vars(Region), scales = "free_y", ncol = 3)

ggplot(DDP) +
  aes(x = Day, y = Per, fill = Region, colour = Region) +
  geom_line(size = 1L) +
  scale_fill_hue() +
  scale_color_hue() +
  scale_y_continuous() +
  labs(x = "Date", y = "Daily confirmed cases") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(vars(Region), scales = "free_y", ncol = 3)

ggplot(DDF) +
  aes(x = Date, y = cum_Cases, fill = Region, colour = Region) +
  geom_line(size = 1L) +
  scale_fill_hue() +
  scale_color_hue() +
  scale_y_continuous() +
  labs(x = "Date", y = "Daily confirmed cases, June-August") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(vars(Region), scales = "free_y", ncol = 3)

ggplot(DDM) +
  aes(x = Date, y = cum_Cases, fill = Region, colour = Region) +
  geom_line(size = 1L) +
  scale_fill_hue() +
  scale_color_hue() +
  scale_y_continuous() +
  labs(x = "Date", y = "Daily confirmed cases, February - May") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(vars(Region), scales = "free_y", ncol = 3)
 
ggplot(DDL) +
  aes(x = Date, y = Cases) +
  geom_line(size = 1L) +
  scale_fill_hue() +
  scale_color_hue() +
  scale_y_continuous() +
  labs(x = "Date", y = "Daily confirmed cases, Lagos") +
  theme_minimal() +
  theme(legend.position = "right")

ggplot(DDL) +
  aes(x = Date, y = Cases) +
  geom_line(size = 1L, colour = "#00441b") +
  scale_y_continuous() +
  labs(x = "Date", y = "Daily confirmed cases, Lagos") +
  theme_minimal()

ggplot(cumDat) +
  aes(x = Date, y = Cases, group = Region) +
  geom_line(aes(col = Region))+
  theme_minimal() +
  theme(legend.position = "none")

#library(dplyr)
#library(tidyr)
#library(tsibble)
#library(feasts)
#library(lubridate)
#
#cumDat <- cumDat %>%
#  mutate(Date = yday(Date)) %>%
#  as_tsibble(index = Date, key = Region)
#
#cumDat %>%
#ggplot(aes(x = Date, y = Cases, group = Region)) +
#  geom_smooth(aes(col = Region), span = 0.4, se = FALSE, size = .5)

#ggsave("states.png")
#library(tidyr)
#NewDat <- cumDat %>%
#     pivot_wider(names_from = Region, values_from = Cases, values_fill = list(Cases = 0))

library(xlsx)
file <- "C:/Users/Job Nmadu/Dropbox/COVID_DATA/States.xlsx"
xlsx::write.xlsx(NewDat, file)
