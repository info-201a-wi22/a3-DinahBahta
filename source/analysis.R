library(dplyr)
library(tidyr)
library(stringr)
library(leaflet)
library(ggplot2)
library(mapproj)

# loading data
data_incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)


# sum of Black prison admission rate in the year 2006
black_admisson_rate <- data_incarceration %>%
  group_by(year,state) %>%
  filter(state == "TN", year == 2006 ) %>%
  select(state,year,black_prison_adm_rate,county_name) %>%
  summarise(sum = sum(black_prison_adm_rate,is.finite = FALSE,na.rm = TRUE)) %>%
  pull(sum)

# sum of Black prison admission rate in the year 2007
black_admisson_rate2 <- data_incarceration %>%
  group_by(year,state) %>%
  filter(state == "TN", year == 2007 ) %>%
  select(state,year,black_prison_adm_rate,county_name) %>%
  summarise(sum = sum(black_prison_adm_rate,is.finite = FALSE,na.rm = TRUE)) %>%
  pull(sum)

# sum of Black prison admission rate in the year 2008
black_admisson_rate3 <- data_incarceration %>%
  group_by(year,state) %>%
  filter(state == "TN", year == 2008 ) %>%
  select(state,year,black_prison_adm_rate,county_name) %>%
  summarise(sum = sum(black_prison_adm_rate,is.finite = FALSE,na.rm = TRUE)) %>%
  pull(sum)

# sum of Black prison admission rate in the year 2009
black_admisson_rate4 <- data_incarceration %>%
  group_by(year,state) %>%
  filter(state == "TN", year == 2009 ) %>%
  select(state,year,black_prison_adm_rate) %>%
  summarise(sum = sum(black_prison_adm_rate,is.finite = FALSE,na.rm = TRUE)) %>%
  pull(sum)

# sum of Black prison admission rate in the year 2010
black_admisson_rate5 <- data_incarceration %>%
  group_by(year,state) %>%
  filter(state == "TN", year == 2010 ) %>%
  select(state,year,black_prison_adm_rate) %>%
  summarise(sum = sum(black_prison_adm_rate,is.finite = FALSE,na.rm = TRUE)) %>%
  pull(sum)


# totalBlack male imprisonment throughout the years in New York vs California
black_male_imprisonment <- data_incarceration %>%
  group_by(year,state) %>%
  filter(state == "NY" || state == "CA") %>%
  select(state,year,state,black_male_prison_pop,county_name) %>%
  summarise(sum = sum(black_male_prison_pop,is.finite = FALSE,na.rm = TRUE))
  View(black_male_imprisonment)
  
#line chart for black_imprisonment of New York vs California
line_chart <- ggplot(black_male_imprisonment, aes(x = year, y = sum, color = state)) +
  scale_color_manual(
    values=c('Purple','Blue')
    ) +
  geom_line(
               lwd = 1,
              linetype = "dashed"
    ) +
  labs(
    title = "Black Male Imprisonment In New York & California",
         subtitle = "(1970-2018)",
         x = "Year",
         y = "Total number of Imprisonment"
    ) +
  
  theme(
    plot.title = element_text(face ="bold", size=15, color = "blue")
    ) +
  theme(
    axis.title = element_text(face ="bold", size=13)
    )
        
# comparison between latinx women vs Black women 
 female_imprisonment <- data_incarceration %>%
    group_by(state) %>%
    filter(state == "WA"|| state == "AL" || state == "TX" || state == "FL"|| state == "CA") %>%
    select(state,black_female_prison_pop,latinx_female_prison_pop) %>%
    summarise_all(funs(sum), na.rm = TRUE) %>% 
    select(state,black_female_prison_pop, latinx_female_prison_pop) %>%
    gather(key = Race, value = population, -state)
# bar chart of black and Latin women
  the_chart <- ggplot(female_imprisonment, aes(x = state, y = population, fill = Race)) +
    geom_bar(
      position = "dodge", stat = "identity"
      ) +
    labs(
      title = "Total Population of Black and Latinx Women Imprisonment",
      x = "State",
      y = "Count"
      ) + 
    scale_fill_manual(
      values = c("Red", "Green"),labels = c("Black Women", "Latinx Women")
      ) +
    theme(
      plot.title = element_text(face ="bold", size=14, color = "darkorange3")
      ) +
    theme(
      axis.title = element_text(face ="bold", size=11)
      )
    
# Map of county jail admission rates in the state of New York in the year 2018
    admission_rate <- data_incarceration %>%
      select(state,year,county_name, total_jail_adm_rate,fips)
    
#Joining data
      map_chart <- map_data("county") %>%
        unite(polyname,region,subregion,sep =",") 
        the_count <- left_join(map_chart,county.fips)
        
      the_map <- left_join(admission_rate, the_count) %>%
        filter(state == "AL") %>%
        drop_na()
#creating map
     county_map <- ggplot(the_map) +
      geom_polygon(mapping = aes(x = long, y = lat, group = group,fill = total_jail_adm_rate),
        color = "white", 
        size = .1
        ) +
      scale_fill_continuous(limits = c(0, max(the_map$total_jail_adm_rate)),low = "blue", high = "red", name = "Jail Admission Rates") +
      labs(
        title = "Jail Admission Rates of Each County In Alabama"
          ) +
       theme(
         axis.line = element_blank(),        # remove axis lines
         axis.text = element_blank(),        # remove axis labels
         axis.ticks = element_blank(),       # remove axis ticks
         axis.title = element_blank(),       # remove axis titles
         plot.background = element_blank(),  # remove gray background
         panel.grid.major = element_blank(), # remove major grid lines
         panel.grid.minor = element_blank(), # remove minor grid lines
         panel.border = element_blank(), 
         plot.title = element_text(face ="bold", size=14, color = "grey39")
       )
     

         