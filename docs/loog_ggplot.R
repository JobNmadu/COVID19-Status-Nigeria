library(stringr)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(pander)

# update this file path to point toward appropriate folders on your computer
#results <- "/Users/majerus/Desktop/NJAIS/results/"  

# folder where the data is saved:
#labor <- "/Users/majerus/Desktop/NJAIS/data/dept_labor/age_lvl/"

# folder where you want the graphs to be saved:
results <- "/"  

# folder where the data is saved:
labor <- "/"

# create list of all .csv files in folder 
file_list <- list.files(path=labor, pattern="*.csv") 

# read in each .csv file in file_list and rbind them into a data frame called data.labor 
data.labor <- 
  do.call("rbind", 
          lapply(file_list, 
                 function(x) 
                   cbind(year = as.numeric(str_sub(x, 1, 4)),
                         read.csv(paste(labor, x, sep=''), 
                                  stringsAsFactors = FALSE))))

# remove commas from numeric variables
data.labor[,c(3:12)] <- lapply(
  data.labor[,c(3:12)], 
  function(x) {as.numeric( 
    gsub(",", "", x))})

# drop 2010 from data then data and projections will occur in 5 year intervals 
data.labor <- subset(data.labor, data.labor$year!=2010)

# rename cols 
colnames(data.labor) <- c("Year",   "County", "Total",  
                          "Under 5",  '5 to 9 years', '10 to 14 years', '15 to 19 years', 
                          "X20.24", "X25.29", "X30.34", "X35.39", "X40.44")

# select columns of interest
keep <- c("Year", "County", "Total", 'Under 5', 
          '5 to 9 years', '10 to 14 years', '15 to 19 years')

data.labor <- data.labor[keep]

# melt data to long format 
data.labor.long <- melt(data.labor, id.vars=c("County", "Year"), variable.name="category")

# remove total projections and state level projections from data
data.labor.long <- subset(data.labor.long, data.labor.long$category!='Total')
data.labor.long <- subset(data.labor.long, data.labor.long$County!='New Jersey')

# create graphing function
county.graph <- function(df, na.rm = TRUE, ...){
  
  # create list of counties in data to loop over 
  county_list <- unique(df$County)
  
  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(county_list)) { 
    
    # create plot for each county in df 
    plot <- 
      ggplot(subset(df, df$County==county_list[i]),
             aes(Year, value/1000, group = County, colour = category)) + 
      
      geom_line(size=2) +
      facet_wrap( ~  category, ncol=2) +
      
      theme_pander() +
      theme(legend.position="none") + 
      
      scale_y_continuous("County Population within Age Categories (thousands)", 
                         limits=c(0, max(df$value[df$County==county_list[i]]))/1000) +
      scale_x_continuous("Year") +
      
      ggtitle(paste(county_list[i], ' County, New Jersey \n', 
                    "County Population Projection within Age Categories (thousands) \n",
                    sep=''))
    
    # save plots as .png
    # ggsave(plot, file=paste(results, 
    #                        'projection_graphs/county_graphs/',
    #                        county_list[i], ".png", sep=''), scale=2)
    
    # save plots as .pdf
    # ggsave(plot, file=paste(results, 
    #                        'projection_graphs/county_graphs/',
    #                        county_list[i], ".pdf", sep=''), scale=2)
    
    # print plots to screen
    print(plot)
  }
}

# run graphing function on long df
county.graph(data.labor.long)


x_1 <- c("A1", "A1","A1", "B10", "B10", "B10","B10", "C100", "C100", "C100")
z_1 <- rnorm(10, 70) 
z_2 <- rnorm(10, 1.7)
A <- data.frame(x_1, z_1, z_2)

for (cat in unique(x_1)){
  d <- subset(A, x_1 == cat)
  plot(d$z_1, d$z_2)
}

library(ggplot2)
library(reshape2)

melted_a <- melt(A)


ggplot(melted_a, aes(variable, value)) +
  geom_jitter() +
  facet_grid(. ~ x_1)

ggplot(melted_a, aes(variable, value)) +
  geom_jitter() +
  facet_grid(variable ~ x_1)

ggplot_fun <- function(data, x, y, rowfacet, colfacet, ...){
  p <- ggplot(data, aes_string(x, y))
  p <- p + geom_jitter()
  p <- p + facet_grid(as.formula(sprintf("%s ~ %s",
                                         rowfacet, colfacet)))
  }

ggplot_fun(data=melted_a, variable, value, x_1)

for ( i in 1:length(unique(wheeldata$Date)) ){
  d <- subset( wheeldata, Date == unique ( wheeldata$Date )[i] )
  plot(d$X, d$Y, xlab = "X", ylab = "Y", main = paste0("Date: ",  unique(d$Date)) )
}

