
rm(list = ls())

# load libraries
require(tidyverse)
require(readxl)
require(lubridate)
require(caret)
require(scales)
require(lme4)
require(merTools)
require(broom)
require(rgl)
require(ggthemes)
require(ggExtra)
require(RColorBrewer)



# load file

file.path <- "C:/Users/mocta/Desktop/Urgency assignment/Case_Study_Urgency_Message_Data.xlsx"
df <- excel_sheets(file.path) %>% 
  map_df(~read_xlsx(file.path, .))

# functions
# mode given a vector
mode.cust <- function(vec) {
  unique_vec <- unique(vec)
  unique_vec[which.max(tabulate(match(vec, unique_vec)))]
}

# clean dataset
df <- df %>% 
  mutate(type = factor(ifelse(is.na(accommadation_type_name), accommodation_type_name, accommadation_type_name)),
         city = factor(case_when(city_id == 9395 ~ "A",
                                 city_id == 17193 ~ "B",
                                 city_id == 5085 ~ "C",
                                 city_id == 16808 ~ "D",
                                 city_id == 8584 ~ "E")),
         chain_hotel = factor(chain_hotel),
         star_rating = factor(star_rating),
         hotel_id = factor(hotel_id))

# create calculated variables, casewise elimination of date inconsistencies
df <- df %>% 
  mutate(month_booked = factor(month(df$booking_date)),
         dayweek_booked = factor(substr(weekdays(booking_date), 1, 3)),
         dayweek_checkin = factor(substr(weekdays(checkin_date), 1, 3)),
         stay_days = as.integer(ymd(df$checkout_date) - ymd(df$checkin_date)),
         total_cost = ADR_USD * stay_days,
         book_2_checkin = as.integer(ymd(df$booking_date) - ymd(df$checkin_date)))
df <- df[-(which(df$book_2_checkin > 0)),] # 3 cases removed



# drop accommodation/accommadation and city code variables, re-arrange vars
df <- df[,-c(4, 6, 11)]
df <- df[,c(1, 2, 10, 9, 3, 4, 5, 6, 7, 8, 11, 12, 13, 14, 15, 16)]

#write.csv(df, file = "C:/Users/mocta/Desktop/Urgency assignment/cleaned_data.csv")

## 1: Overview

graph.1 <- ggplot(df, aes(x = book_2_checkin, y = ADR_USD))+
  geom_jitter(alpha = 0.07)+
  theme_bw()+
  scale_x_continuous(labels = comma)+
  scale_y_continuous(labels = comma)+
  labs(y = "Average Dairly Rate",
       x = "Time from booking to check-in (days)")+
  theme(text = element_text(size=15))

ggMarginal(graph.1, type = "histogram")


test <- df %>% 
  mutate(latecheaper = ifelse((ADR_USD <= 90 & book_2_checkin >= -5), 1, 0))
table(test$latecheaper)[2]/sum(table(test$latecheaper))




# ADR histogram + key facts
table.11 <- ggplot(df, aes(ADR_USD))+
  geom_histogram(bins = 100)+
  scale_x_continuous(labels = comma)+
  scale_y_continuous(labels = comma)+
  geom_vline(xintercept = 113.66, color = "blue")+ # median
  geom_vline(xintercept = 148.093, color = "red")+ # mean
  theme_bw()+
  labs(title = "Histogram of Average Daily Rate",
       x = element_blank(),
       y = element_blank())

summary(df$ADR_USD)

# 15.7% of all bookings (+/- 1.2%) are under an average of 50 dollars a night and 44.6% of all bookings are under 100$ a night
nrow(df[df$ADR_USD < 50,])/nrow(df)
nrow(df[df$ADR_USD < 100,])/nrow(df)

# B2C histogram + key facts
# B2C hist
table.2 <- ggplot(df, aes(book_2_checkin))+
  geom_histogram(bins = 60)+
  scale_y_continuous(labels = comma)+
  geom_vline(xintercept = -8, color = "blue")+ # median
  geom_vline(xintercept = -14.53, color = "red")+ # mean
  theme_bw()+
  labs(title = "Histogram of Book-to-Checkin Time",
       x = element_blank(),
       y = element_blank())

summary(df$book_2_checkin)

# 22.9% of all bookings are made one day in advance or on the day-of, and half of all bookings are made 8 days or fewer away from check-in. In contrast, only 17.3% of bookings are made more than 1 month out
nrow(df[df$book_2_checkin > -2,])/nrow(df)
nrow(df[df$book_2_checkin < -30,])/nrow(df)

# correlation: a weak negative correlation
cor(df$ADR_USD, df$book_2_checkin)

# regression (simple)
summary(lm(ADR_USD ~ book_2_checkin, data = df))
summary(lm(log(ADR_USD) ~ book_2_checkin, data = df))


# 2: On average ADR goes down with B2C
library(sjPlot) # plot marginal effects, interactions
# ADR/B2C trendline (3rd order polynomial)
ggplot(df, aes(x = book_2_checkin, y = ADR_USD))+
  # ylim(0, 300)+
  # geom_point()+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), size = 1, se = F)+
  theme_bw()

simplemodelpoly <- lm(log(ADR_USD) ~ book_2_checkin + I(book_2_checkin^2) + I(book_2_checkin^3), data = df)
summary(simplemodelpoly)


graph.2 <- plot_model(simplemodelpoly, type = "pred", terms = c("book_2_checkin"), data = df,
                      title = "",
                      axis.title = c("Booking to check-in (days)", "Average daily rate"))

graph.2 + theme_bw() + theme(text = element_text(size=15)) + ylim(75, 300) + geom_line(size = 1.2)


# By-city trendline (3rd order poly)
ggplot(df, aes(x = book_2_checkin, y = ADR_USD, color = city))+
  # geom_smooth(method = "lm", se = T)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), size = 1, se = T)+
  theme_bw()+
  scale_colour_Publication()

# if there is a different slope on interaction of city * B2C, that means that cities do have different relationships with ADR over time/B2C

citymodel <- lm(log(ADR_USD) ~ city + book_2_checkin + I(book_2_checkin^2) + I(book_2_checkin^3) + city* book_2_checkin, data = df)
summary(citymodel)
# in a reg model interacting cities w/ b2c we find that B and C move downwards compared to A, that E is indistinguishable from A and that D has a higher slope than A on the interaction term but still indistinguishable from 0 for the whole regression.
# INSIGHT: At the city level, some go down with B2C while others stay steady.

graph.3 <- plot_model(ci.lvl = 0.95, citymodel, type = "pred", terms = c("book_2_checkin", "city"), data = df,
           title = "", se = F,
           axis.title = c("Booking to check-in (days)"))

graph.3 + theme_bw()+
  theme(text = element_text(size=15))+
  scale_color_manual(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506"))+
  ylim(75, 300)+
  geom_line(size = 1.2)




# city bookings breakdown
graphtable.1 <- data.frame(prop.table(table(df$city)))
names(graphtable.1) <- c("City", "Percentage_bookings")
graphtable.1$Percentage_bookings <- graphtable.1$Percentage_bookings*100
graph.4 <- ggplot(graphtable.1, aes(x = City, y = Percentage_bookings))+
  geom_col(aes(y = Percentage_bookings, fill = City))+
  # scale_fill_Publication()+
  theme_bw()+
  labs(y = "Percentage of all bookings")


# variation between city explains % of the total variation in ADR
summary(lm(ADR_USD ~ city, data = df))


# are last min bookings done in different properties?




# 3: One explanation is related not to the change in price of the same rooms/properties/cities, but of a change in the type of booking as B2C shrinks. People in a hurry are more likely to make different booking choices: which ones?


cor(df$ADR_USD, df$book_2_checkin)
cor(log(df$ADR_USD), df$book_2_checkin)
# a) regression

quantile(df$book_2_checkin, .8)


summary(lm(book_2_checkin ~ log(ADR_USD) + city + type + star_rating + chain_hotel + booking_date + dayweek_booked + stay_days, data = df))




df_times1 <- df[df$book_2_checkin > -2 | df$book_2_checkin < -13,]
df_times1 <- df %>% 
  mutate(last_min = ifelse(book_2_checkin > -2, 1, 0),
         well_planned = ifelse(book_2_checkin < -13, 1, 0),
         both = factor(case_when(last_min == 1 ~ "last_minute",
                                 well_planned == 1 ~ "well_planned",
                                 last_min == 0 & well_planned == 0 ~ "Other"))) 

df_times2 <- df_times1 %>% 
  group_by(both) %>% 
  summarise(Med_ADR = median(ADR_USD),
            Med_tot_cost = median(total_cost),
            avg_star = mean(as.double(star_rating)),
            mode_dayweek_check = mode.cust(dayweek_checkin),
            mode_dayweek_book = mode.cust(dayweek_booked),
            avg_stay = mode.cust(stay_days),
            mode_type = mode.cust(type),
            city = mode.cust(city))

# last-minute bookings go more to city A and E, and less to city C
ggplot(df_times1, aes(x = both, fill = city))+
  geom_bar(position = "fill")+
  coord_flip()

# a broadly similar breakdown in terms of type of property, including resorts!
ggplot(df_times1, aes(x = both, fill = type))+
  geom_bar(position = "fill")+
  coord_flip()

# last-minute bookings are more likely to get rooms with higher star ratings
ggplot(df_times1, aes(x = both, fill = (star_rating)))+
  geom_bar(position = "fill")+
  coord_flip()


df_con <- df_times1 %>% 
  group_by(hotel_id) %>% 
  summarise(perc_lastmin = length(last_min[last_min == 1])/length(both),
            tot_lastmin = length(last_min[last_min == 1]))

length(df_con$perc_lastmin[df_con$perc_lastmin > .7]) / nrow(df_con)
sum(df_con$tot_lastmin[df_con$perc_lastmin > .5])


ggplot(df_con, aes(x = perc_lastmin, y = tot_lastmin))+
  geom_point()



# comparison of city bookings
prop_city <- prop.table(table(df_times1$city,df_times1$both), 2)
mat_city <- table(df_times1$city,df_times1$both)
sums_city <- colSums(mat_city)
#city A
prop.test(mat_city[1,], n = sums_city)
#city B
prop.test(mat_city[2,], n = sums_city)
#city C
prop.test(mat_city[3,], n = sums_city)
#city D
prop.test(mat_city[4,], n = sums_city)
#city E
prop.test(mat_city[5,], n = sums_city)


prop.test(x = c(1536, 1818), n = c(11269, 19138)) # 2-proportion z-test, here for city B, the most-similar proportion in the 2 groups

# comparison of property types
prop_prop <- prop.table(table(df_times1$type, df_times1$both), 2)
mat_prop <- table(df_times1$type, df_times1$both)
sums_prop <- colSums(mat_prop)
#hotel test
prop.test(x = mat_prop[8,], n = sums_prop)
#hostel test
prop.test(x = mat_prop[7,], n = sums_prop)
#resort test
prop.test(x = mat_prop[12,], n = sums_prop)
# serviced apartment test 15
prop.test(x = mat_prop[15,], n = sums_prop)
#B&B test 4
prop.test(x = mat_prop[4,], n = sums_prop)

# comparison of stay days
prop_stay <- prop.table(table(df_times1$stay_days, df_times1$both), 2)
mat_stay <- table(df_times1$stay_days, df_times1$both)
sums_stay <- colSums(mat_stay)
#hotel test
prop.test(x = mat_stay[1,], n = sums_stay)
#hostel test
prop.test(x = mat_stay[2,], n = sums_stay)
#resort test
prop.test(x = mat_stay[3,], n = sums_stay)



##############REWRITE the notes here and the code to reflect framework decision changes

# 2 factors to examine: late_booking and 'over-priced' bookings


df$booklate <- 1.2^df$book_2_checkin # higher is later


ggplot(df, aes(x = booklate))+
  geom_histogram()+
  theme_bw()+
  labs(x = "Book Late Measure (higher is later booking)",
       y = element_blank())+
  scale_y_continuous(labels = comma)





#~~~~
mod.log<- lm(log(ADR_USD) ~ city + star_rating, data = df)
summary(mod.x)


df$deal_seek_log <- df$ADR_USD - as.double(exp(predict((mod.log))))



hist(df$deal_seek_log)


ggplot(df, aes(x = deal_seek_log))+
  geom_histogram(bins = 60)+
  theme_bw()+
  theme(text = element_text(size=15))+
  scale_x_continuous(labels = comma)+
  scale_y_continuous(labels = comma)+
  xlim(-350, 400)+
  labs(x = "Deal-seeking bookings",
       y = element_blank())
  
  


df$deal_seek_log <- as.double(exp(predict((mod.log))))


# double scatter

graph.doublescatter <- ggplot(df, aes(x = deal_seek_log, y = booklate))+
  geom_jitter(height = 0.05, width = 20, alpha = 0.05)+
  theme_bw()+
  theme(text = element_text(size=15))+
  labs(x = "Deal seeking",
       y = "Later booking")+
  xlim(min(df$deal_seek_log), 400)
ggMarginal(graph.doublescatter, type = "histogram")


# scale the 2 factors
df <- df %>% 
  mutate(booklate.norm = (booklate-min(booklate)) / (max(booklate) - min(booklate)),
         deal.seek.norm = (deal_seek_log - min(deal_seek_log)) / (max(deal_seek_log) - min(deal_seek_log)),
         framework.1 = (booklate.norm + deal.seek.norm) / 2)

hist(df$framework.1)
summary(df$booklate.norm)
summary(df$deal_orient.norm)
summary(df$framework.1)

graph.5 <- ggplot(df, aes(x = deal.seek.norm, y = booklate.norm))+
  geom_jitter(height= 0.05, width = 0.05, alpha = 0.05)+
  theme_bw()+
  theme(text = element_text(size=15))+
  labs(x = "Deal seeking",
       y = "Later booking")
ggMarginal(graph.5, type = "histogram")


graph.framework <- ggplot(df, aes(framework.1))+
  geom_histogram(bins = 100)+
  theme_bw()+
  theme(text = element_text(size=15))+
  scale_x_continuous(labels = comma)+
  scale_y_continuous(labels = comma)+
  labs(x = "Predisposed to urgency messaging (higher is more predisposed)",
       y = element_blank())



# which bookings are not worth it?
ggplot(df, aes(x = star_rating, y = ADR_USD, fill = star_rating))+
  geom_boxplot()+
  ylim(0, 300)+
  scale_fill_Publication()+
  theme_bw()+
  theme(text = element_text(size=15))+
  labs(x = "Star Rating",
       y = "Average Daily Rate")
  
plot(lm(ADR_USD ~ city + relevel(star_rating, ref = "3"), data = df))





# deal seeking measure
deal.seek <- lm(ADR_USD ~ star_rating + month_booked + dayweek_checkin + type + book_2_checkin, data = df)
df$fitted <- predict(deal.seek)
df$deal.seek <- df$ADR_USD - df$fitted

hist(df$deal.seek)
length(df$deal.seek[df$deal.seek >0])/nrow(df)




ggplot(df, aes(deal.seek))+
  geom_histogram(bins = 50)+
  theme_bw()+
  theme(text = element_text(size=15))+
  scale_x_continuous(labels = comma)+
  scale_y_continuous(labels = comma)+
  labs(x = "Deal-seeking bookings",
       y = element_blank())+
  xlim(min(df$deal.seek), 600)










# 5: new section: now getting the trend in the properties as B2C grows/comes from -60 to 0.


##################
# 1: Random slopes model
# This measures, at the property level, the slope of each property when pitted against ADR on B2C. This gives BLUPS + better precision than comparing all regressions and assumes that properties are part of a distribution of properties--> gives us insight into how properties might be distributed on these two variables.
# issues with this approach: we are looking at properties, not rooms. Rooms would enable us to control for within-property variation in ADR. variation between properties accounts for ~77% of variation in ADR, so that's good but not enough. If there is any correlation between the B2C and the type of room, and the ADR and the type of room (ex.patterns of people reserving more expensive rooms earlier on) then our estimates of the slopes are biased.
# other issues


# casewise elimination of properties with 9 or fewer bookings

df_prop <- df %>% 
  group_by(hotel_id) %>% 
  filter(n() >= 10) %>% 
  ungroup() %>% 
  mutate(ADR_USD = scale(ADR_USD),
         book_2_checkin = scale(book_2_checkin)) # 880 unique properties in total in dataset. We choose those properties with at least 10 bookings: total 455 properties.

write.csv(df_prop, "C:/Users/mocta/Desktop/Urgency assignment/df_prop.csv")

property_mixed <- lmer(ADR_USD ~ book_2_checkin + (book_2_checkin|hotel_id) + city + star_rating + type + month_booked + dayweek_checkin + dayweek_booked + stay_days, data = df_prop, REML = F)




summary(property_mixed)

# eval the model

# get slopes and CIs. Some code from here: https://stat.ethz.ch/pipermail/r-sig-mixed-models/2014q3/022486.html
rslopes <- coef(property_mixed)$hotel_id[,"book_2_checkin"]
varfix <- vcov(property_mixed)["book_2_checkin","book_2_checkin"]
re <- ranef(property_mixed,condVar=TRUE)
varcm <- attr(re$hotel_id,"postVar")[2,2,]
vartot <- varfix+varcm
slope_upperCI <- 2*sqrt(vartot)

property_mixed_slopes <- data.frame(cbind(hotel_id = REsim(property_mixed)[1:(0.5*nrow(REsim(property_mixed))),2], slope = rslopes, lwr = (rslopes - slope_upperCI), upr = (rslopes + slope_upperCI)))
property_mixed_slopes <- map_df(property_mixed_slopes, as.numeric)

property_mixed_slopes <- property_mixed_slopes %>% 
  mutate(hotel_id = factor(hotel_id))
str(property_mixed_slopes)

graph.7 <- property_mixed_slopes %>% 
  mutate(Property = fct_reorder(hotel_id, slope)) %>% 
ggplot(aes(y = slope, x = fct_reorder(hotel_id, slope)))+
  geom_point(aes(alpha = 0.5))+
  geom_errorbar(aes(ymin = lwr, ymax = upr, alpha = 0.5))+
  theme_bw()+
  theme(axis.text.x=element_blank(),
        text = element_text(size=15))+
  geom_hline(yintercept = 0, color = "darkred")+
  labs(y = "Slope coefficient", 
       x = "Properties")


str(property_mixed_slopes$hotel_id)
# Number of stat sig slopes:
test <- property_mixed_slopes %>%
  mutate(stat_sig = ifelse(lwr * upr > 0, 1, 0))
sum(test$stat_sig) # 61 out of 455 slopes are statistically significant (13.4%)


framework.2 <- property_mixed_slopes


#455 by-hotel 'regular' regressions for each property

hotel_reg <- df %>%
  group_by(hotel_id) %>% 
  filter(n() >= 10) %>%
  do(fitsimple = tidy(lm(ADR_USD ~ book_2_checkin + month_booked + dayweek_checkin + dayweek_booked + stay_days, data = .))) %>% 
  unnest(fitsimple)














# 6: new section: variability
# normally we would try for some seasonality analysis, and measure the extent of the waves. In this case though there is 'missing data' in the sense that not every property has a booking on every day.
# as such we choose a simple measure: standard deviation of ADR by property. Again we are likely OVERESTIMATING standard deviation in general and putting too much weight on properties that have high differences in the price of their different kinds of rooms. We do this as a placeholder, to be done better if better data is made available.


df_fr3 <- df %>% 
  group_by(hotel_id) %>% 
  filter(n() >= 10) %>% 
  summarise(sd_ADR = sd(ADR_USD),
            hotel_id = hotel_id)

framework.3 <- df_fr3



graph.8 <- ggplot(df_fr3, aes(sd_ADR))+
  geom_histogram()+
  theme_bw()+
  theme(text = element_text(size=15))+
  scale_y_continuous(labels = comma)+
  labs(x = "Standard Deviation of ADR",
       y = element_blank())
  







#### new idea # like 300
# As B2C declines, which hotel type is more booked?
df_type <- df %>% 
  mutate(type = case_when(type == "Hotel" ~ "Hotel",
                          type == "Resort" ~ "Resort",
                          type == "Serviced Apartment" ~ "Apartment",
                          type == "Hostel" ~ "Hostel",
                          type == "Guest House / Bed & Breakfast" ~ "B&B"),
         type = ifelse(is.na(type), "Other", type))

df_type$type <- factor(df_type$type)

ggplot(df_type, aes(book_2_checkin, ADR_USD))+
  geom_jitter(alpha = 0.1)+
  geom_smooth(method = lm, se = F)+
  facet_wrap(~ type, scales = 'free_y')


df_type %>% 
  group_by(type, book_2_checkin) %>% 
  summarise(med_adr = median(ADR_USD)) %>% 
  ggplot(aes(x = book_2_checkin, y = med_adr, color = type))+
  geom_smooth(method = 'lm', se = F)



# at each level of B2C what percentage of each property type is booked?
tbl_type <- df_type %>% 
  dplyr::mutate(book_2_checkin = factor(book_2_checkin)) %>% 
  dplyr::group_by(book_2_checkin, type) %>% 
  dplyr::summarize(total= n())



type_vec <- data.frame(cbind(c(length(df_type$type[df_type$type == "Hotel"]), 
length(df_type$type[df_type$type == "Resort"]),
length(df_type$type[df_type$type == "Apartment"]),
length(df_type$type[df_type$type == "Hostel"]),
length(df_type$type[df_type$type == "B&B"]),
length(df_type$type[df_type$type == "Other"])), type = c("Hotel", "Resort", "Apartment", "Hostel", "B&B", "Other")))


df_test <- merge(tbl_type[,c("book_2_checkin", "type", "total")], type_vec[,c("V1", "type")])


df_test <- df_test %>% 
  mutate(V1 = as.double(V1),
         perc = total/V1)

df_test %>% 
  group_by(book_2_checkin, type) %>% 
  summarise()

write.csv(df_test, file = "C:/Users/mocta/Desktop/perc.csv")
perc <- read.csv("C:/Users/mocta/Desktop/perc.csv")


perc$X <- perc$X *100
ggplot(perc, aes(x = book_2_checkin, y = X, color = factor(type)))+
  geom_line()+
  theme_bw()+
  scale_colour_Publication()+
  theme(text = element_text(size=15))+
  labs(y = "Percentage of Bookings",
       x = "Time from Booking to Check-in (days)")

#
ggplot(df_type, aes(x = type, y = ADR_USD, fill = type))+
  geom_boxplot()+
  ylim(0, 300)+
  scale_fill_Publication()+
  theme_bw()+
  theme(text = element_text(size=15))+
  labs(y = "Average Daily Rate",
       x = "Property Type",
       legend = "Type")
  

ggplot(df_type, aes(x = type, fill = type))+
  geom_bar()+
  theme_minimal()+
  scale_fill_Publication()+
  scale_y_continuous(labels = comma)+
  labs(x = element_blank(),
       y = element_blank())


# month of booking
ggplot(df, aes(month_booked))+
  geom_histogram()

df_month <- df %>% 
  group_by(month_booked) %>% 
  summarise(med_b2c = median(book_2_checkin),
            med_adr = median(ADR_USD))


fin <- df_type %>% 
  group_by(month_booked, type) %>% 
  summarise(n())



ggplot(df, aes(x = checkin_date, fill = df$month_booked))+
  geom_histogram()+
  scale_y_continuous(labels = comma)+
  facet_grid(rows = df$month_booked)+
  theme_bw()+
  scale_fill_Publication()+
  labs(y = element_blank(),
       x = "Check-in Date")





write.csv(fin, file = "C:/Users/mocta/Desktop/fin.csv")
fin <- read.csv("C:/Users/mocta/Desktop/fin.csv")


### do weekend trippers book late or not?
# people whose stay days are on both sat and sunday (i.e.on Sat for 2 days)

df_weekend <- df %>% 
  mutate(weekend = ifelse(dayweek_checkin == "Sat" & (stay_days == 2 | stay_days == 1), "Weekend ", "Non-weekend"))

ggplot(df_weekend, aes(book_2_checkin))+
  geom_histogram()+
  facet_wrap(~weekend, scales = 'free_y')+
  theme_bw()+
  theme(text = element_text(size = 15))+
  scale_y_continuous(labels = comma)+
  labs(x = "Booking to Check-in (days)",
       y = element_blank())


summary(lm(book_2_checkin ~ weekend, data = df_weekend))


df_business <- df %>% 
  mutate(business = ifelse(dayweek_checkin %in% c("Mon" , "Tue" ,"Wed" ,"Thu", "Fri") & (stay_days == 1), "Business ", "Non-business"))


ggplot(df_business, aes(book_2_checkin))+
  geom_histogram()+
  facet_wrap(~business, scales = 'free_y')+
  theme_bw()+
  theme(text = element_text(size = 15))+
  scale_y_continuous(labels = comma)+
  labs(x = "Booking to Check-in (days)",
       y = element_blank())


summary(lm(book_2_checkin ~ business, data = df_business))
summary(lm(ADR_USD ~ business + city, data = df_business))









# 7: visual of all three dimensions together
# combine dimensions into df
blank <- df[, c(1, 3, 5, 16)]


write.csv(df, file = "C:/Users/mocta/Desktop/Urgency assignment/df.csv")
write.csv(framework.2, file = "C:/Users/mocta/Desktop/Urgency assignment/framework2.csv")



df_framework <- read.csv("C:/Users/mocta/Desktop/Urgency assignment/df.csv")
df_framework$f1 <- as.numeric(df_framework$framework.1)
df_framework$f2 <- as.numeric(df_framework$framework.2)

hist(df_framework$f1)
summary(df_framework$framework.2)

sum(is.na(df_framework$f1))
sum(is.na(df_framework$f2))


# framework final scatterplot
ggplot(df_framework, aes(x = f1, y = f2))+
  geom_jitter(width = 0.005, height = 0.05,alpha = 0.08)+
  theme_bw()+
  xlim(0, 0.7)+
  theme(text = element_text(size = 15))+
  scale_y_continuous(labels = comma)+
  labs(x = "Predisposition",
       y = "Property trend")



# cutoffs: above 0.45 for f1, above 0 for f2

#low-low

df_framework <- df_framework %>% 
  filter(!is.na(f2))
  
sum(df_framework$f1 < 0.45 & df_framework$f2 < 0) / nrow(df_framework)


#low-high
sum(df_framework$f1 > 0.45 & df_framework$f2 < 0) / nrow(df_framework)


#high-high
sum(df_framework$f1 > 0.45 & df_framework$f2 > 0) / nrow(df_framework)

#high-low
sum(df_framework$f1 < 0.45 & df_framework$f2 > 0) / nrow(df_framework)



df_framework <- df_framework[df_framework$f3 < 100,]



par(mar = c(0, 0, 0, 0))
plot3d(
  x = df_framework$f1,
  y = df_framework$f2,
  z = df_framework$f3,
  xlab = "Susceptibility", 
  ylab = "Trend",
  zlab = "Variability"
)

writeWebGL( filename="C:/Users/mocta/Desktop/Urgency assignment/framework.html" ,  width=600, height=600)























