setwd("./")

if(!("tidyverse" %in% installed.packages())){
  install.packages("tidyr", dependencies = TRUE)
}

if(!("ggplot2" %in% installed.packages())){
  install.packages("ggplot2", dependencies = TRUE)
}

if(!("summarytools" %in% installed.packages())){
  install.packages("summarytools", dependencies = TRUE)
}

library(tidyverse)
library(ggplot2)
library(summarytools)
library(lubridate)

options(scipen = 100)

dane <- read_csv("Walmart.csv",show_col_types = FALSE)

dane <- dane %>%
  mutate(Date = dmy(Date))

descr(dane)
view(dfSummary(dane))

options(repr.plot.width = 15, repr.plot.height = 5)
ggplot(dane, aes(x=Weekly_Sales, color='Weekly Sales')) + 
  geom_histogram(bins=50)+
  xlab("Sprzedaż tygodniowa [USD]")+
  ylab("Ilość wystąpień")+
  ggtitle("Sprzedaż tygodniowa")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),plot.title = element_text(hjust = 0.5))

Maximum_Sales <- dane %>% 
  group_by(Store) %>%
  summarize(Total_Sales=sum(Weekly_Sales))

head(arrange(Maximum_Sales,desc(Total_Sales)))

options(repr.plot.width = 15, repr.plot.height = 5)
ggplot(Maximum_Sales,aes(x=factor(Store),y=Total_Sales,fill=Total_Sales)) +
  geom_col()+
  xlab("Sklep")+
  ylab("Sprzedaż [USD]")+
  ggtitle("Sprzedaż tygodniowa")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),plot.title = element_text(hjust = 0.5))+
  scale_fill_gradient(low="orange", high="blue")


Diff <- dane %>% 
  group_by(Store) %>% 
  summarize(sd= sd(Weekly_Sales),mean= mean(Weekly_Sales),CV= sd/mean) %>%
  arrange(desc(sd))
head(Diff)

options(repr.plot.width = 15, repr.plot.height = 5)
dane %>%
  filter(Store == 14) %>%
  ggplot(aes(y=Weekly_Sales)) + 
  geom_boxplot(colour="black",fill="orchid")+ 
  ylab("Sprzedaż [USD]")+
  ggtitle("Odchylenie standardowe dla sklepu 14") +
  theme(plot.title = element_text(hjust = 0.5))

Super_Bowl <- c('2010-02-12', '2011-02-11', '2012-02-10')
Super_Bowl <- ymd(Super_Bowl)

Labour_Day <- c('2010-08-10', '2011-09-09', '2012-09-07')
Labour_Day <- ymd(Labour_Day)

Thanksgiving <- c('2010-11-26', '2011-11-25', '2012-11-23')
Thanksgiving <- ymd(Thanksgiving)

Christmas <- c('2010-12-31', '2011-12-30', '2012-12-28')
Christmas <- ymd(Christmas)

holiday_sales <- dane %>%
  mutate(Holiday = case_when(Date %in% Super_Bowl ~ "Super Bowl",
                             Date %in% Labour_Day ~ "Święto pracy",
                             Date %in% Thanksgiving ~ "Święto dziękczynienia",
                             Date %in% Christmas ~ "Boże Narodzenie",
                             TRUE ~ "Zwykły dzień"))

holiday_sales %>%
  select(Weekly_Sales,Holiday) %>%
  group_by(Holiday) %>%
  summarise(average_sales = mean(Weekly_Sales)) %>%
  arrange(desc(average_sales))


options(repr.plot.width = 15, repr.plot.height = 5)
holiday_sales %>%
  select(Weekly_Sales,Holiday) %>%
  group_by(Holiday) %>%
  summarise(average_sales = mean(Weekly_Sales)) %>%
  ggplot(aes(x=Holiday,y=average_sales,fill=Holiday)) + 
  geom_col() + 
  ggtitle("Sprzedaż według świąt") + 
  theme(plot.title = element_text(hjust = 0.5))

y2010_sales <- dane %>%
  group_by(Store) %>%
  filter(Date >= "2010-01-01", Date <= "2010-12-31") %>%
  summarise(y2010_sales = sum(Weekly_Sales))

y2011_sales <- dane %>%
  group_by(Store) %>%
  filter(Date >= "2011-01-01", Date <= "2011-12-31") %>%
  summarise(y2011_sales = sum(Weekly_Sales))

y2012_sales <- dane %>%
  group_by(Store) %>%
  filter(Date >= "2012-01-01", Date <= "2012-12-31") %>%
  summarise(y2012_sales = sum(Weekly_Sales))

year_sales <- left_join(y2010_sales, y2011_sales, by="Store") %>%
  left_join(., y2012_sales, by="Store")

options(repr.plot.width = 15, repr.plot.height = 7.5)
year_sales %>%
  gather("Id","Sales",2:4) %>%
  ggplot(aes(fill=Id,x=factor(Store),y=Sales)) + 
  geom_col(position="dodge") + xlab("Store") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),plot.title = element_text(hjust = 0.5))+
  xlab("Sklep")+
  ylab("Sprzedaż [USD]")+
  ggtitle("Sprzedaż roczna sklepów")

monthly_data <- dane %>%
  mutate(Month = month(Date,label = TRUE),Year = year(Date),Quarter = as.character(quarter(Date,with_year = TRUE)))

options(repr.plot.width = 15, repr.plot.height = 7.5)
monthly_data %>%
  filter(Year == 2010) %>%
  group_by(Month) %>%
  summarise(Monthly_sale = sum(Weekly_Sales)) %>% 
    ggplot(aes(x=Month,y=Monthly_sale,fill=Month)) + 
    geom_col() + 
    xlab("Mesiąc")+
    ylab("Sprzedaż miesięczna [USD]")+
    ggtitle("Sprzedaż miesięczna w 2010 roku") +
    theme(plot.title = element_text(hjust = 0.5))

options(repr.plot.width = 15, repr.plot.height = 7.5)
monthly_data %>%
  filter(Year == 2011) %>%
  group_by(Month) %>%
  summarise(Monthly_sale = sum(Weekly_Sales)) %>% 
    ggplot(aes(x=Month,y=Monthly_sale,fill=Month)) + 
    geom_col() + 
    xlab("Mesiąc")+
    ylab("Sprzedaż miesięczna [USD]")+
    ggtitle("Sprzedaż miesięczna w 2011 roku") +
    theme(plot.title = element_text(hjust = 0.5))

options(repr.plot.width = 15, repr.plot.height = 7.5)
monthly_data %>%
  filter(Year == 2012) %>%
  group_by(Month) %>%
  summarise(Monthly_sale = sum(Weekly_Sales)) %>% 
    ggplot(aes(x=Month,y=Monthly_sale,fill=Month)) + 
    geom_col() + 
    xlab("Mesiąc")+
    ylab("Sprzedaż miesięczna [USD]")+
    ggtitle("Sprzedaż miesięczna w 2012 roku") +
    theme(plot.title = element_text(hjust = 0.5))

options(repr.plot.width = 15, repr.plot.height = 7.5)
monthly_data %>% 
  group_by(Quarter) %>%
  summarise(Quarter_sales = sum(Weekly_Sales)) %>%
  ggplot(aes(x=Quarter,y=Quarter_sales,fill=Quarter)) + 
  geom_col() + 
  xlab("Kwartał")+
  ylab("Sprzedaż kwartalna [USD]")+
  ggtitle("Sprzedaż kwartalna") +
  theme(plot.title = element_text(hjust = 0.5))

y2010_fuel <- dane %>%
  filter(Date >= "2010-01-01", Date <= "2010-12-31") %>%
  select(Fuel_Price, Store)

y2011_fuel <- dane %>%
  filter(Date >= "2011-01-01", Date <= "2011-12-31") %>%
  select(Fuel_Price, Store)

y2012_fuel <- dane %>%
  filter(Date >= "2012-01-01", Date <= "2012-12-31") %>%
  select(Fuel_Price, Store)

total_fuel <- left_join(y2010_fuel, y2011_fuel, by="Store") %>%
  left_join(., y2012_fuel, by="Store") %>%
  select(Fuel_Price.x, Fuel_Price.y, Fuel_Price)

names(total_fuel)[1] <- 'Fuel_2010'
names(total_fuel)[2] <- 'Fuel_2011'
names(total_fuel)[3] <- 'Fuel_2012'

descr(total_fuel)


year_fuel <- dane %>%
  select(Fuel_Price, Date)

options(repr.plot.width = 15, repr.plot.height = 7.5)
ggplot(year_fuel, aes(x=Date, y=Fuel_Price)) + 
  geom_point() +
  geom_smooth(se=F, color="red") +
  geom_smooth(method='lm', color='blue') +
  xlab("Czas")+
  ylab("Cena paliwa [USD]") +
  ggtitle("Cena paliwa 2010 - 2012 ") +
  theme(plot.title = element_text(hjust = 0.5))

y2010_unemployment <- dane %>%
  filter(Date >= "2010-01-01", Date <= "2010-12-31") %>%
  select(Unemployment, Store)

y2011_unemployment <- dane %>%
  filter(Date >= "2011-01-01", Date <= "2011-12-31") %>%
  select(Unemployment, Store)

y2012_unemployment <- dane %>%
  filter(Date >= "2012-01-01", Date <= "2012-12-31") %>%
  select(Unemployment, Store)

total_unemployment <- left_join(y2010_unemployment, y2011_unemployment, by="Store") %>%
  left_join(., y2012_unemployment, by="Store") %>%
  select(Unemployment.x, Unemployment.y, Unemployment)

names(total_unemployment)[1] <- 'Unemployment_2010'
names(total_unemployment)[2] <- 'Unemployment_2011'
names(total_unemployment)[3] <- 'Unemployment_2012'

descr(total_unemployment)


year_unemployment <- dane %>%
  select(Unemployment, Date)

options(repr.plot.width = 15, repr.plot.height = 7.5)
ggplot(year_unemployment, aes(x=Date, y=Unemployment)) + 
  geom_point() +
  geom_smooth(se=F, color='red') +
  geom_smooth(method='lm', color='blue') +
  xlab("Czas")+
  ylab("Wskaźnik bezrobocia") +
  ggtitle("Bezrobocie 2010 - 2012 ") +
  theme(plot.title = element_text(hjust = 0.5))

y2010_df <- dane %>%
  filter(Date >= "2010-01-01", Date <= "2010-12-31")

y2011_df <- dane %>%
  filter(Date >= "2011-01-01", Date <= "2011-12-31")

y2012_df <- dane %>%
  filter(Date >= "2012-01-01", Date <= "2012-12-31")


options(repr.plot.width = 15, repr.plot.height = 5)
ggplot(y2010_df,aes(x=Temperature,y=Weekly_Sales)) + 
geom_point() + 
geom_smooth(color="orange") +
ggtitle("Sprzedaż vs. Temperatura w 2010 roku") + 
xlab("Temperatura") +
ylab("Sprzedaż [USD]") +
theme(plot.title = element_text(hjust = 0.5))
cat("Korelacja sprzedaży do temperatury w 2010 roku wyniosła:", cor(y2010_df$Weekly_Sales,y2010_df$Temperature))


options(repr.plot.width = 15, repr.plot.height = 5)
ggplot(y2011_df,aes(x=Temperature,y=Weekly_Sales)) + 
geom_point() + 
geom_smooth(color="blue") +
ggtitle("Sprzedaż vs. Temperatura w 2011 roku") + 
xlab("Temperatura") +
ylab("Sprzedaż [USD]") +
theme(plot.title = element_text(hjust = 0.5))
cat("Korelacja sprzedaży do temperatury w 2011 roku wyniosła:", cor(y2011_df$Weekly_Sales,y2011_df$Temperature))


options(repr.plot.width = 15, repr.plot.height = 5)
ggplot(y2012_df,aes(x=Temperature,y=Weekly_Sales)) + 
geom_point() + 
geom_smooth(color="green") +
ggtitle("Sprzedaż vs. Temperatura w 2012 roku") + 
xlab("Temperatura") +
ylab("Sprzedaż [USD]") +
theme(plot.title = element_text(hjust = 0.5))
cat("Korelacja sprzedaży do temperatury w 2012 roku wyniosła:", cor(y2012_df$Weekly_Sales,y2012_df$Temperature))

y2010_df <- dane %>%
  filter(Date >= "2010-01-01", Date <= "2010-12-31")

y2011_df <- dane %>%
  filter(Date >= "2011-01-01", Date <= "2011-12-31")

y2012_df <- dane %>%
  filter(Date >= "2012-01-01", Date <= "2012-12-31")


options(repr.plot.width = 15, repr.plot.height = 5)
ggplot(y2010_df,aes(x=Unemployment,y=Weekly_Sales)) + 
geom_point() + 
geom_smooth(color="orange") +
ggtitle("Sprzedaż vs. bezrobocie w 2010 roku") + 
xlab("Bezrobocie") +
ylab("Sprzedaż [USD]") +
theme(plot.title = element_text(hjust = 0.5))
cat("Korelacja sprzedaży do bezrobocia w 2010 roku wyniosła:", cor(y2010_df$Weekly_Sales,y2010_df$Unemployment))


options(repr.plot.width = 15, repr.plot.height = 5)
ggplot(y2011_df,aes(x=Unemployment,y=Weekly_Sales)) + 
geom_point() + 
geom_smooth(color="blue") +
ggtitle("Sprzedaż vs. bezrobocie w 2011 roku") + 
xlab("Bezrobocie") +
ylab("Sprzedaż [USD]") +
theme(plot.title = element_text(hjust = 0.5))
cat("Korelacja sprzedaży do bezrobocia w 2011 roku wyniosła:", cor(y2011_df$Weekly_Sales,y2011_df$Unemployment))


options(repr.plot.width = 15, repr.plot.height = 5)
ggplot(y2012_df,aes(x=Unemployment,y=Weekly_Sales)) + 
geom_point() + 
geom_smooth(color="green") +
ggtitle("Sprzedaż vs. bezrobocie w 2012 roku") + 
xlab("Bezrobocie") +
ylab("Sprzedaż [USD]") +
theme(plot.title = element_text(hjust = 0.5))
cat("Korelacja sprzedaży do bezrobocia w 2012 roku wyniosła:", cor(y2012_df$Weekly_Sales,y2012_df$Unemployment))

y2010_df <- dane %>%
  filter(Date >= "2010-01-01", Date <= "2010-12-31")

y2011_df <- dane %>%
  filter(Date >= "2011-01-01", Date <= "2011-12-31")

y2012_df <- dane %>%
  filter(Date >= "2012-01-01", Date <= "2012-12-31")


options(repr.plot.width = 15, repr.plot.height = 5)
ggplot(y2010_df,aes(x=CPI,y=Weekly_Sales)) + 
geom_point() + 
geom_smooth(color="orange") +
ggtitle("Sprzedaż vs. CPI w 2010 roku") + 
xlab("CPI - Index cen konsumpcyjnych") +
ylab("Sprzedaż [USD]") +
theme(plot.title = element_text(hjust = 0.5))
cat("Korelacja sprzedaży do CPI w 2010 roku wyniosła:", cor(y2010_df$Weekly_Sales,y2010_df$CPI))


options(repr.plot.width = 15, repr.plot.height = 5)
ggplot(y2011_df,aes(x=CPI,y=Weekly_Sales)) + 
geom_point() + 
geom_smooth(color="blue") +
ggtitle("Sprzedaż vs. CPI w 2011 roku") + 
xlab("CPI - Index cen konsumpcyjnych") +
ylab("Sprzedaż [USD]") +
theme(plot.title = element_text(hjust = 0.5))
cat("Korelacja sprzedaży do CPI w 2011 roku wyniosła:", cor(y2011_df$Weekly_Sales,y2011_df$CPI))


options(repr.plot.width = 15, repr.plot.height = 5)
ggplot(y2012_df,aes(x=CPI,y=Weekly_Sales)) + 
geom_point() + 
geom_smooth(color="orange") +
ggtitle("Sprzedaż vs. CPI w 2012 roku") + 
xlab("CPI - Index cen konsumpcyjnych") +
ylab("Sprzedaż [USD]") +
theme(plot.title = element_text(hjust = 0.5))
cat("Korelacja sprzedaży do CPI w 2012 roku wyniosła:", cor(y2012_df$Weekly_Sales,y2012_df$CPI))

y2010_df <- dane %>%
  filter(Date >= "2010-01-01", Date <= "2010-12-31")

y2011_df <- dane %>%
  filter(Date >= "2011-01-01", Date <= "2011-12-31")

y2012_df <- dane %>%
  filter(Date >= "2012-01-01", Date <= "2012-12-31")


options(repr.plot.width = 15, repr.plot.height = 5)
ggplot(y2010_df,aes(x=Fuel_Price,y=Weekly_Sales)) + 
geom_point() + 
geom_smooth(color="orange") +
ggtitle("Sprzedaż vs. cena paliwa w 2010 roku") + 
xlab("Cena paliwa [USD]") +
ylab("Sprzedaż [USD]") +
theme(plot.title = element_text(hjust = 0.5))
cat("Korelacja sprzedaży do kosztów paliwa w 2010 roku wyniosła:", cor(y2010_df$Weekly_Sales,y2010_df$Fuel_Price))


options(repr.plot.width = 15, repr.plot.height = 5)
ggplot(y2011_df,aes(x=Fuel_Price,y=Weekly_Sales)) + 
geom_point() + 
geom_smooth(color="blue") +
ggtitle("Sprzedaż vs. cena paliwa w 2011 roku") + 
xlab("Cena paliwa [USD]") +
ylab("Sprzedaż [USD]") +
theme(plot.title = element_text(hjust = 0.5))
cat("Korelacja sprzedaży do kosztów paliwa w 2011 roku wyniosła:", cor(y2011_df$Weekly_Sales,y2011_df$Fuel_Price))


options(repr.plot.width = 15, repr.plot.height = 5)
ggplot(y2012_df,aes(x=Fuel_Price,y=Weekly_Sales)) + 
geom_point() + 
geom_smooth(color="green") +
ggtitle("Sprzedaż vs. cena paliwa w 2012 roku") + 
xlab("Cena paliwa [USD]") +
ylab("Sprzedaż [USD]") +
theme(plot.title = element_text(hjust = 0.5))
cat("Korelacja sprzedaży do kosztów paliwa w 2012 roku wyniosła:", cor(y2012_df$Weekly_Sales,y2012_df$Fuel_Price))

sklep22 <- dane %>%
  filter(Store==22)

cor.test(sklep22$Weekly_Sales,sklep22$Temperature)

cor(dane$Weekly_Sales,dane$Temperature)

cor.test(sklep22$Weekly_Sales,sklep22$Unemployment)

cor(dane$Weekly_Sales,dane$Unemployment)

cor.test(sklep22$Weekly_Sales,sklep22$CPI)

cor(dane$Weekly_Sales,dane$CPI)

cor.test(sklep22$Weekly_Sales,sklep22$Fuel_Price)

cor(dane$Weekly_Sales,dane$Fuel_Price)

cor.test(sklep22$Fuel_Price,sklep22$Temperature)

cor(dane$Fuel_Price,dane$Temperature)

cor.test(sklep22$Fuel_Price,sklep22$Unemployment)

cor(dane$Fuel_Price,dane$Unemployment)

t.test(sklep22$Weekly_Sales, mu=1000000)

mean(dane$Weekly_Sales)

t.test(sklep22$Unemployment, mu=8)

mean(dane$Unemployment)

t.test(sklep22$Fuel_Price, mu=3.5)

mean(dane$Fuel_Price)

t.test(sklep22$Weekly_Sales ~ sklep22$Holiday_Flag)

dane %>%
  filter(Holiday_Flag == 0) %>%
  summarise(srednia_zwykly_dzien=mean(Weekly_Sales))

dane %>%
  filter(Holiday_Flag == 1) %>%
  summarise(srednia_swieto=mean(Weekly_Sales))


shapiro.test(sklep22$Weekly_Sales)

ks.test(dane$Weekly_Sales, "pnorm", mean=mean(dane$Weekly_Sales), sd=sd(dane$Weekly_Sales))

options(repr.plot.width = 15, repr.plot.height = 7.5)
ggplot(dane,aes(sample=Weekly_Sales))+
  stat_qq(color="red") +
  stat_qq_line(color="green")+
  xlab("Wartość rozkładu")+
  ylab("Tygodniowa wielkość sprzedaży [USD]") +
  ggtitle("Rozkład emipiryczny tygodniowej sprzedaży dla całej populacji") +
  theme(plot.title = element_text(hjust = 0.5))

print("Rozkład poniższej cechy dla całej badanej populacji nie jest normalny")


options(repr.plot.width = 15, repr.plot.height = 7.5)
ggplot(sklep22,aes(sample=Weekly_Sales))+
  stat_qq(color="red") +
  stat_qq_line(color="green")+
  xlab("Wartość rozkładu")+
  ylab("Tygodniowa wielkość sprzedaży [USD]") +
  ggtitle("Rozkład emipiryczny tygodniowej sprzedaży dla próby") +
  theme(plot.title = element_text(hjust = 0.5))

print("Rozkład poniższej cechy dla całej badanej populacji nie jest normalny")

options(repr.plot.width = 15, repr.plot.height = 7.5)
ggplot(dane,aes(sample=CPI))+
  stat_qq(color="red") +
  stat_qq_line(color="green")+
  xlab("Wartość rozkładu")+
  ylab("Index CPI") +
  ggtitle("Rozkład emipiryczny wskaźnika cen konsumpcyjnych dla całej populacji") +
  theme(plot.title = element_text(hjust = 0.5))

print("Rozkład poniższej cechy dla całej badanej populacji nie jest normalny")


options(repr.plot.width = 15, repr.plot.height = 7.5)
ggplot(sklep22,aes(sample=CPI))+
  stat_qq(color="red") +
  stat_qq_line(color="green")+
  xlab("Wartość rozkładu ")+
  ylab("Index CPI") +
  ggtitle("Rozkład emipiryczny cen konsumpcyjnych dla próby") +
  theme(plot.title = element_text(hjust = 0.5))

print("Rozkład poniższej cechy dla całej badanej populacji nie jest normalny")

options(repr.plot.width = 15, repr.plot.height = 7.5)
ggplot(dane,aes(sample=Fuel_Price))+
  stat_qq(color="red") +
  stat_qq_line(color="green")+
  xlab("Wartość rozkładu")+
  ylab("Cena paliwa [USD]") +
  ggtitle("Rozkład emipiryczny cen paliwa dla całej populacji") +
  theme(plot.title = element_text(hjust = 0.5))

print("Rozkład poniższej cechy dla całej badanej populacji nie jest normalny")


options(repr.plot.width = 15, repr.plot.height = 7.5)
ggplot(sklep22,aes(sample=Fuel_Price))+
  stat_qq(color="red") +
  stat_qq_line(color="green")+
  xlab("Wartość rozkładu")+
  ylab("Cena paliwa [USD]") +
  ggtitle("Rozkład emipiryczny cenp aliwa dla próby") +
  theme(plot.title = element_text(hjust = 0.5))

print("Rozkład poniższej cechy dla całej badanej populacji nie jest normalny")

wilcox.test(sklep22$Weekly_Sales ~sklep22$Holiday_Flag)


