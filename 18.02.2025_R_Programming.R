#line graph using ggplot with r programming


library(tidyverse)
data()
?msleep
view(msleep)
names(msleep)
glimpse(msleep)
msleep %>% 
  drop_na(vore) %>% 
  ggplot(aes(fct_infreq(vore)))+
  geom_bar(fill="#97B3c6")+
  coord_flip()+
  theme_bw()


msleep %>% 
  drop_na(vore) %>% 
  ggplot(aes(fct_infreq(vore)))+
  geom_bar(fill="#97B3c6")+
  theme_bw()

view(Orange) %>%
  filter(Tree!="2") %>%
  ggplot(aes(age,circumference))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Tree)+
  theme_bw()+
  labs(title="Tree age and circumference")

Orange %>% 
  filter(Tree!="1"&
         Tree!="2") %>% 
  ggplot(aes(age,circumference,colour = Tree))+
  geom_point(size=5, alpha=.3)+
  geom_line(size=1)+
  theme_minimal()+
  labs(title="Tree age and circumference")


Orange %>% 
  filter(Tree!="1"&
         Tree!="2") %>% 
  ggplot(aes(age,circumference,colour = Tree))+
  geom_point(size=5,alpha=.5)+
  geom_line(size=1)+
  theme_minimal()+
  labs(Title="Tree age and circumference")


#scatter plot using R programming

library(tidyverse)
data()
?msleep
view(msleep)
names(msleep)
glimpse(msleep)
summary(msleep$brainwt)


# single categorical variable


msleep %>% 
  drop_na(vore) %>% 
  ggplot(aes(fct_infreq(vore)))+
  geom_bar(fill="#97B3c6")+
  theme_bw()+
  labs(x="who eats what?",
       y=NULL,
       title="Number of observations per order")


# single numeric variable

msleep %>% 
  ggplot(aes(x=awake))+
  geom_histogram(binwidth = 1,fill="#97B3c6")+
  theme_bw()+
  labs(x="Total sleep",
       y=NULL,
       title="Histogram of total sleep")


msleep %>% 
  ggplot(aes(x=awake))+
  geom_histogram(binwidth = 1, fill="#97B3c6")+
  theme_bw()+
  labs(x="Total sleep",
       y=NULL,
       title="Histogram of total sleep")

# two or more numeric variable


msleep %>% 
  filter(bodywt<2) %>% 
  ggplot(aes(bodywt,brainwt))+
  geom_point(aes(color=sleep_total,
                 size=awake))+
  geom_smooth(method=lm,
              se=F)+
  labs(x="Body weight",
       y="Brain weight",
       title="Brain and Body weight")+
  theme_minimal()

# ggplot using geometries

view(mtcars)
mtcars %>% 
  ggplot(aes(x=wt,y=mpg))+
  geom_point(size=3,
             colour="steelblue",
             alpha=.6)+
  labs(title="Weight vs Miles per gallon",
       x="wight",
       y='Miles per Gallon')+
  theme_bw()


mtcars %>% 
  ggplot(aes(x=wt,y=mpg))+
  geom_point(size=3,
             colour="steelblue",
             alpha=.6)+
  labs(title="Weight vs Miles per Gallon",
       x="Weight",
       y="Miles per Gallon")+
  theme_bw()


Orange %>% 
  ggplot(aes(x=age,
             y=circumference,
             color=Tree))+
  geom_line()+
  labs(title="Circumference as a function of age",
       x="Age",
       y="Circumference")+
  theme_bw()


mpg %>% 
  ggplot(mapping=aes(x=class))+
  geom_bar(fill="steelblue",
           alpha=.8)+
  labs(title="Number of cars in each class",
       x="",
       y="")+
  theme_bw()


mpg %>% 
  ggplot(mapping=aes(x=class))+
  geom_bar(fill="steelblue",
           alpha=.8)+
  labs(title="Numbers of car in each class",
       x="",
       y="")+
  theme_bw()

mpg %>% 
  ggplot(aes(x=cty))+
  geom_histogram(binwidth = 2,
                 fill="red",
                 alpha=.8)+
  labs(title="Fuel efficiency of cars in the city",
       x="Miles per gallon",
       y="Number of cars")
  theme_bw()

  
 mpg %>% 
   ggplot(aes(x=cty))+
   geom_histogram(binwidth = 2,
                  fill="steelblue",
                  alpha=.8)+
   labs(title="Fuel efficiency of cars in the city",
        x="Miles per gallon",
        y="Number of cars")+
   theme_bw()

 
 mpg %>% 
   ggplot(aes(x=cty))+
   geom_density(fill="steelblue",
                alpha=.8)+
   labs(title="Fuel efficiency of cars in the city",
        x="Miles per gallon",
        y="")+
   theme_bw()

 
mpg %>% 
  ggplot(aes(x=cty))+
  geom_density(fill="steelblue",
               alpha=.8)+
  labs(title="Fuel effociency of the cars in the city",
       x="Miles per gallon",
       y="")+
  theme_bw()

mpg %>% 
  filter(drv %in% c("f", "r")) %>% 
  ggplot(aes(x=cty,
             fill=drv,
             color=drv))+
  geom_density(alpha=.3)+
  labs(title="Fuel efficiency of cars in the city",
       x="Miles per gallon",
       y="")+
  theme_bw()



mpg %>% 
  filter(drv%in% c("f","r")) %>% 
  ggplot(aes(x=cty,
             fill=drv,
             color=drv))+
  geom_density(alpha=.3)+
  labs(title="Fuel efficiency of the cars in the city",
       x="Miles per gallon",
       y="")+
  theme_bw()
 

mpg %>% 
  filter(cty<25) %>%
  ggplot(aes(x=cty,
             fill=drv))+
  geom_boxplot(alpha=.5)+
  labs(title="Fuel efficiency of the cars in the city",
       x="Miles per gallon",
       fill="Drive")+
  theme_bw()


mpg %>% 
  filter(cty<25) %>% 
  ggplot(aes(x=cty,
             fill=drv))+
  geom_boxplot(alpha=.5)+
  labs(title="Fuel efficiency of cars in the city",
       x="Miles per gallon",
       fill="Drive")+
  theme_bw()

ggplot(mpg,aes(x=displ))+
  geom_area(aes(y=hwy,fill="Highway"))+
  geom_area(aes(y=cty,fill="City"))+
  labs(title="Highway vs city driving",
       x="Engine displacement(L)",
       y="Miles per gallon",
       fill="")+
  theme_bw()


ggplot(mpg,aes(x=displ))+
  geom_area(aes(y=hwy,fill="highway"))+
  geom_area(aes(y=cty,fill="City"))+
  labs(title="Highway vs City driving",
       x="Engine Displacement(L)",
       y="Miles per gallon",
       fill="")+
  theme_bw()



ggplot(mpg,aes(x=displ))+
  geom_area(aes(y=hwy,fill="Highway"))+
  geom_area(aes(y=cty,fill = "City"))+
  labs(title="Highway vs City Driving",
       x="Engine Displacement(L)",
       y="Miles per gallon",
       fill="")+
  theme_bw()

ggplot(mpg,aes(x=displ))+
  geom_area(aes(y=hwy,fill="Highway"))+
  geom_area(aes(y=cty,fill="City"))+
  labs(title="Highway vs City driving",
       x="Engine Displacement(L)",
       y="Miles per gallon",
       fill="")+
  theme_bw()


view(faithfuld)  
glimpse(faithfuld)
faithfuld %>% 
  ggplot(aes(x=waiting,
             y=eruptions,
             fill=density))+
  labs(title="Old faithful Geyser",
       x="Waiting time between erruptios",
       y="Duration of erruptions",
       fill="Density")+
  theme_bw()



# data visualization by using ggplot
#Explore
#Clean
#Manipulate
#Describe and Summaries
#Visualize
#Analyse

## Data visualization##


#tell story with data
#Types and combinations of data
# The grammar of graphics
    #Data
    #Mapping
    #Geometry

view(gapminder)
gapminder %>% 
  filter(continent %in% c("Africa","Europe")) %>% 
  filter(gdpPercap<30000) %>% 
  ggplot(aes(x=gdpPercap,
             y=lifeExp,
             size=pop,
             color=year))+
  geom_point()+
  facet_wrap(~continent)+
  labs(title="Life expectancy explained by GDP",
       x="GDP per capita",
       y="Life expectancy")


gapminder %>% 
  filter(continent %in% c("Africa","Europe")) %>% 
  filter(gdpPercap<30000) %>% 
  ggplot(aes(x=gdpPercap,
             y=lifeExp,
             size=pop,
             color=year))+
  geom_point()+
  facet_wrap(~continent)+
  labs(title="Life expectancy explained by GDP",
       x="GDP per capita",
       y="Life expectancy")


#describe and summaries

# Range/ spread
# Centrality
# Variance
# Summarize your Data
# Create tables

data()
view(msleep)
# Have a quick look
glimpse(msleep)

# Describe the spread, centrality and variance of the dataset
min(msleep$awake)
max(msleep$awake)
range(msleep$awake)
IQR(msleep$awake)
mean(msleep$awake)
median(msleep$awake)
var(msleep$awake)


#summaries selected variables
summary(msleep)
summary(msleep$sleep_total)


msleep %>% 
  select(sleep_total,brainwt) %>% 
  summary()


msleep %>% 
  select(sleep_total,brainwt) %>% 
  summary()

# Create a summary table
## For each category of "vore"
## Show the min, max, difference, average "sleep_total"
## arrange data by the average

msleep %>% 
  select(name, vore, sleep_total)

msleep %>% 
  drop_na(vore) %>% 
  group_by(vore) %>% 
  summarise(Lower=min(sleep_total),
            Average=mean(sleep_total),
            Upper=max(sleep_total),
            Difference=max(sleep_total)-min(sleep_total)) %>% 
  arrange(Average) %>% 
  view()

msleep %>% 
  drop_na(vore) %>% 
  group_by(vore) %>% 
  summarise(Lower=min(sleep_total),
            Average=mean(sleep_total),
            Upper=max(sleep_total),
            Difference=max(sleep_total)-min(sleep_total)) %>% 
  arrange(Average) %>% 
  view()


#Creating contingency tables
install.packages("MASS")
library(MASS)
attach(Cars93)
view(Cars93)
glimpse(Cars93)
table(Origin)

unique(Cars93$Origin)  

table(AirBags,Origin)

?Cars93
table(AirBags,Origin)
addmargins(table(AirBags,Origin),1)
addmargins(table(AirBags,Origin),2)
addmargins(table(AirBags,Origin))

table(Origin)
table(AirBags,Origin)
prop.table(table(AirBags,Origin),2)*100
round(prop.table(table(AirBags,Origin),2)*100) 


Cars93 %>% 
  group_by(Origin,AirBags) %>% 
  summarise(number=n()) %>% 
  pivot_wider(names_from = Origin,
              values_from = number)

Cars93 %>% 
  group_by(Origin,AirBags) %>% 
  summarise(number=n()) %>% 
  pivot_wider(names_from = Origin,
              values_from = number)
  

