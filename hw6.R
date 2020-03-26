
library(tidyverse)
library(cowplot)
library(corrplot)
library(highcharter)
library("ggplot2")
library("GGally")  
require(scales)

 


 
df <- read.csv('C:/Users/onepi/Documents/NEU/IE6600/hw6 data/top10s.csv')

 
## Describe Data  {.tabset .tabset-fade .tabset-pills}

### Types of Data
 


df <- df[2:15]
colnames(df)
sapply(df, class)
df %>% glimpse()

 
### Describe stats
 

df %>% summary()

 
### missing values
 
dim(df)
sum(is.na(df))

cat('completely filled rows -> ', sum(complete.cases(df)))
 


## Overview {.tabset .tabset-fade .tabset-pills}

### Popularity
 


hchart(df$pop, type = "column")

      


### Gerne
 

hchart(df$top.genre, type = "column")

 


 

hpolar <- function(x, a, c, z) { 
  
  highchart() %>% 
    hc_chart(polar = TRUE,
             backgroundColor = "#ff0099" 	#ff0099
    ) %>% 
    hc_title(text = x) %>% 
    hc_xAxis(categories = a,
             tickmarkPlacement = "on",
             lineWidth = 0,
             color = "#'FF0000") %>% 
    hc_yAxis(gridLineInterpolation = "polygon",
             lineWidth = 0,
             min = 0,
             color = "#'FF0000") %>% 
    hc_series(
      list(
        name = z,
        data = c,
        pointPlacement = "on",
        type = "column",
        color = '#000000'
      )
    )
  #%>%
  #   hc_add_theme(
  #  hc_theme_merge(
  #  hc_theme_flatdark(),
  # hc_theme_null(chart = list(backgroundColor = "#9999ff"))
  # )
  #)
  
}   



    

## new 





## Top 20 songs for each feature {.tabset .tabset-fade .tabset-pills}

### Popularity
 
# Popularity - Top 10 songs

popular_df <- df %>%
  select(title, pop, artist) %>%
  group_by(artist, title, pop)%>%
  summarise(n = n())%>%
  arrange(desc(pop))%>%
  head(20) 

hpolar('Popularity - Top 20 songs', popular_df$title, popular_df$pop,  'popularity')

popular_df[,-4]


      





### 2015
 
top_2015 <- df %>%
  select(year, artist, pop, top.genre, title) %>%
  group_by(year)%>%
  filter(!is.na(year))%>%
  filter(!is.na(artist))%>%
  filter(!is.na(pop))%>%
  filter(!is.na(top.genre))%>%
  filter(!is.na(title))%>%
  filter(year == 2015)%>%
  arrange(desc(pop))%>%
  head(n = 20)

hpolar('2015 - Top 20 artist', top_2015$artist, top_2015$pop,  'popularity ')
top_2015
 

### 2016
 
top_2016 <- df %>%
  select(year, artist, pop, top.genre, title) %>%
  group_by(year)%>%
  filter(!is.na(year))%>%
  filter(!is.na(artist))%>%
  filter(!is.na(pop))%>%
  filter(!is.na(top.genre))%>%
  filter(!is.na(title))%>%
  filter(year == 2016)%>%
  arrange(desc(pop))%>%
  head(n = 20)
hpolar('2016 - Top 20 artist', top_2016$artist, top_2016$pop,  'popularity ')
top_2016
 

### 2017
 
top_2017 <- df %>%
  select(year, artist, pop, top.genre, title) %>%
  group_by(year)%>%
  filter(!is.na(year))%>%
  filter(!is.na(artist))%>%
  filter(!is.na(pop))%>%
  filter(!is.na(top.genre))%>%
  filter(!is.na(title))%>%
  filter(year == 2017)%>%
  arrange(desc(pop))%>%
  head(n = 20)

hpolar('2017 - Top 20 artist', top_2017$artist, top_2017$pop,  'popularity ')
top_2017
 

### 2018
 
top_2018 <- df %>%
  select(year, artist, pop, top.genre, title) %>%
  group_by(year)%>%
  filter(!is.na(year))%>%
  filter(!is.na(artist))%>%
  filter(!is.na(pop))%>%
  filter(!is.na(top.genre))%>%
  filter(!is.na(title))%>%
  filter(year == 2018)%>%
  arrange(desc(pop))%>%
  head(n = 20)
hpolar('2018 - Top 20 artist', top_2018$artist, top_2018$pop,  'popularity ')
top_2018
 


### 2019
 
top_2019 <- df %>%
  select(year, artist, pop, top.genre, title) %>%
  group_by(year)%>%
  filter(!is.na(year))%>%
  filter(!is.na(artist))%>%
  filter(!is.na(pop))%>%
  filter(!is.na(top.genre))%>%
  filter(!is.na(title))%>%
  filter(year == 2019)%>%
  arrange(desc(pop))%>%
  head(n = 20)
hpolar('2019 - Top 20 artist', top_2019$artist, top_2019$pop,  'popularity ')
top_2019
 


### 2016 - 2017
 
# Popularity - Top 40 artists
df %>%
  select(year, title, pop, top.genre) %>%
  group_by(year)%>%
  filter(!is.na(year))%>%
  filter(!is.na(title))%>%
  filter(!is.na(pop))%>%
  filter(!is.na(top.genre))%>%
  filter(year < 2018 & year >= 2016)%>%
  arrange(desc(pop))%>%
  head(n = 30)%>%
  ggplot(mapping = aes(x = title, y = pop, size=pop, color=top.genre))+
  geom_point()+
  coord_polar()+
  facet_wrap(~year, ncol = 3)+
  theme_minimal()+
  labs(x = 'Songs', y = 'Popularity', title = 'Popularity Songs~ year 2016 and 2017')+
  theme(plot.title = element_text(hjust=0.5),legend.position ='bottom')   

 

### 2018 - 2019
 
# Popularity - Top 40 artists

df %>%
  select(year, title, pop, top.genre) %>%
  group_by(year)%>%
  filter(!is.na(year))%>%
  filter(!is.na(title))%>%
  filter(!is.na(pop))%>%
  filter(!is.na(top.genre))%>%
  filter(year >= 2018)%>%
  arrange(desc(pop))%>%
  head(n = 30)%>%
  ggplot(mapping = aes(x = title, y = pop, size=pop, color=top.genre))+
  geom_point()+
  coord_polar()+
  facet_wrap(~year, ncol = 3)+
  theme_minimal()+
  labs(x = 'Songs', y = 'Popularity', title = 'Popularity Songs ~ year 2018 and 2019')+
  theme(plot.title = element_text(hjust=1.5),legend.position ='bottom')    

 


# **TOP Songs based on Genre**

### Genre
 

top_genre <- df %>%
  select(top.genre, pop, year) %>% filter(year %in% c(2015,2016,2017,2018,2019))


library(treemap)
library(viridisLite)


tm <- treemap(top_genre, index = c("year", "top.genre"),
              vSize = "pop", vColor = 'year', #palette =heat.colors(4),
              border.col=c("black","white"),             # Color of borders of groups, of subgroups, of subsubgroups ....
              border.lwds=c(3,2)
              # Width of colors
)



hctreemap(tm)
 


