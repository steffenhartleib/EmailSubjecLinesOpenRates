
library('RCurl')
library('rjson')
library('dplyr')
library('stringr')
library('ggplot2')
library('reshape2')


## READ DATA


setwd("/Users/steffen/Google Drive/ds_projects/subject_lines")

emails <- read.csv("data/email_campaigns_report.csv", header = TRUE, stringsAsFactors = FALSE)

products  = read.csv("data/products_export.csv", header = TRUE, stringsAsFactors = FALSE)
products$UPC = str_replace_all(products$UPC, "'","")
products$Artist = trimws(products$Artist, which = c("right"))

lines_items = read.csv("data/line_items_final_export.csv", header = TRUE, stringsAsFactors = FALSE)
pm_lines = filter(lines_items, Store_ID %in% c('1', '5693042'))
pm_line_art = inner_join(pm_lines, products, by = c("Lineitem_sku" = "UPC" ))



## Create a list of artists with LTD spending per artist - to use as weight of artist name as a feature in the model

artist_rank = select(pm_line_art, Artist, Lineitem_quantity, Lineitem_price)%>%
              mutate(Rev = Lineitem_quantity * Lineitem_price)%>%
              mutate(Artist = str_replace(Artist,"The ",""))%>%
              mutate(Artist = ifelse(Artist == "Bob Dylan", "Dylan", Artist))%>%
              group_by(Artist)%>%
              summarize(TL_Rev = sum(Rev))%>%
              arrange(desc(TL_Rev))
           
artist_rank <-mutate(artist_rank,Artist = str_replace(artist_rank$Artist,"The ",""))%>%
              mutate(Artist = ifelse(Artist == "Bob Dylan", "Dylan", Artist))%>%
              mutate(Artist = ifelse(Artist == "Rolling Stones", "Stones", Artist))%>%
              mutate(Artist = ifelse(Artist == "David Bowie", "Bowie", Artist))%>%
              mutate(Artist = ifelse(Artist == "Led Zeppelin", "Zeppelin", Artist))%>%
              mutate(Artist = ifelse(Artist == "Ozzy Osbourne", "Ozzy", Artist))%>%
              mutate(Artist = ifelse(Artist == "ACDC", "AC/DC", Artist))%>%
              mutate(Artist = ifelse(Artist == "Bruce Springsteen", "Springsteen", Artist))%>%
              mutate(Artist = ifelse(Artist == "Black Sabbath", "Sabbath", Artist))%>%
              mutate(Artist = ifelse(Artist == "Jimi Hendrix", "Hendrix", Artist))%>%
              mutate(Artist = ifelse(Artist == "Paul McCartney", "McCartney", Artist))%>%
              mutate(Artist = ifelse(Artist == "John Coltrane", "Coltrane", Artist))%>%
              mutate(Artist = ifelse(Artist == "Elvis Presley", "Elvis", Artist))%>%
              filter(Artist != "The")%>%
              filter(TL_Rev > 0)
              filter(Artist != "popmarket")%>%
head(artist_rank)



## Feature Engineering

#### Extract, dates, times, and key terms from subject lines, and create variables from them


camp <- filter(emails, sends > 100000)%>%
        select(id,last_run_date, name, subject, sends, opens, clicks)%>%
        mutate(Date = as.Date(last_run_date))

camp$date  <- as.Date(substr(camp$last_run_date,1,10))
camp$day <- as.factor(weekdays(camp$date))
camp$time <- substr(camp$last_run_date,11,25)
camp$hour <- as.numeric(substr(camp$last_run_date,12,13))
camp$or <- round(camp$opens/camp$sends,2)
camp$ctr <- round(camp$clicks/camp$sends,4)
camp$length = nchar(camp$subject)

camp$perc_off <-  str_extract(camp$subject,'..%')
camp$perc_off <- as.numeric(str_replace(camp$perc_off,'%',""))
camp$perc_off[is.na(camp$perc_off) == TRUE] <- 0

camp$amt_off <-  str_extract(camp$subject,'\\$...')
camp$amt_off <-  as.numeric(str_replace(camp$amt_off,'\\$',""))
camp$amt_off[is.na(camp$amt_off) == TRUE] <- 0

camp$off = as.numeric(grepl("Off|off|OFF",camp$subject))
camp$dis = as.numeric(grepl("Discount",camp$subject))
camp$sav = as.numeric(grepl("[S,s]avings|[S,s]ave",camp$subject))
camp$lim_ed = as.numeric(grepl('Limited Edition',camp$subject))
camp$vinyl = as.numeric(grepl('Vinyl|LP',camp$subject))
camp$box_set = as.numeric(grepl('Box Set',camp$subject))
camp$deluxe = as.numeric(grepl('Deluxe',camp$subject))
camp$exclusive = as.numeric(grepl('Exclusive',camp$subject))
camp$new = as.numeric(grepl('New',camp$subject))
camp$blow = as.numeric(grepl('Blowout',camp$subject))
camp$sale = as.numeric(grepl('Sale',camp$subject))
camp$steal = as.numeric(grepl('[S,s]teal', camp$subject))
camp$garage = as.numeric(grepl('[G,g]arage', camp$subject))
camp$surprise = as.numeric(grepl('[S,s]urprise', camp$subject))



#### Loop over artist list and check if each artist is in each subject line. If so add the Value, and concat the Artist Name


# list of artists names
artists = artist_rank$Artist

# initialize output variables 
output = data.frame(1,2)
names(output) <-  c('artists', "value")
row = 0

# loop through subjects and check each for artist names 
for(s in camp$subject) {
  a_list = c()
  a_value = 0
  row = row +1
    for(a in artists) {
      
      if(grepl(a,s) == TRUE){
        a_list = paste0(a_list," ",a)
        value = artist_rank$TL_Rev[artist_rank$Artist == a]
        a_value = a_value + value
   }
   else{
     a_list = a_list
     a_value = a_value
   }
    }
  
  if(is.null(a_list) == TRUE) { 
    a_list <- "" 
  }

line <-  c(a_list,a_value)
output <-  rbind(output, line)

}


# join artist name and value columns to 'camp' data frame 

artist_value <- output[-1,]
camp = cbind(camp,artist_value)
camp$value = round(as.numeric(camp$value),0)


## A few quick plots



# OPEN RATES vs. amt_off, perc_off, value



camp_melt_or_cont <- select(camp, or, amt_off, perc_off, value)%>%
  melt(id = "or")


plot_or_cont <- ggplot(camp_melt_or_cont, aes(x = value, y = or)) + 
  facet_wrap(~variable, scales = "free") + 
  geom_point() + geom_smooth(method = "lm")

plot_or_cont


#### => Amount and percentage off seem to have a slight negative impact or open rates - didn't expect that.


# Open rate vs. categorical variables

camp_melt_or_cat <- select(camp, or, off, dis, sav, lim_ed, vinyl, box_set,
                           deluxe, exclusive, new, blow, sale, steal, garage,
                           surprise)%>%
  melt(id = 'or')

camp_melt_or_cat <- select(camp, or, garage, surprise, off, dis, sav, lim_ed, vinyl,
                           box_set, deluxe, exclusive, new, blow, sale, steal, garage,
                           surprise)%>%
  mutate(garage = factor(garage))%>%
  melt(id = 'or')

plot_or_cat <- ggplot(camp_melt_or_cat, aes(x = value, y = or)) + 
  facet_wrap(~variable, scales = "free") + 
  geom_boxplot()

plot_or_cat

# => terms that convey value ("garage" [sale], "surprise", [$ or%] "off" etc) are associated with a lower
# open rate. "Savings" is the exception.
# => "Box set", "vinyl", "exclusive", and "deluxe" are also not asscociated with lower open rates. 



#  CLICK THROUGH RATES vs continuos and discrete variables: amt_off, perc_off, value.
#  (click through = clicks/sends, measures how many opened and acted, i.e. qualifed opens,
# 

camp_melt_ctr_cont <- select(camp, ctr, amt_off, perc_off, value)%>%
  melt(id = "ctr")


plot_or_cont <- ggplot(camp_melt_ctr_cont, aes(x = value, y = ctr)) + 
  facet_wrap(~variable, scales = "free") + 
  geom_point() + geom_smooth(method = "lm")

plot_or_cont


# => perc_off now is has a positive relationship, value has a stronger relationship (steeper slope)




# click through rates vs continuos and discrete variables

camp_melt_ctr_cat <- select(camp, ctr, off, dis, sav, lim_ed, vinyl, box_set,
                            deluxe, exclusive, new, blow, sale, steal, garage,
                            surprise)%>%
  melt(id = 'ctr')

camp_melt_ctr_cat <- select(camp, ctr, garage, surprise, off, dis, sav, lim_ed, vinyl,
                            box_set, deluxe, exclusive, new, blow, sale, steal, garage,
                            surprise)%>%
  mutate(garage = factor(garage))%>%
  melt(id = 'ctr')

plot_ctr_cat <- ggplot(camp_melt_ctr_cat, aes(x = value, y = ctr)) + 
  facet_wrap(~variable, scales = "free") + 
  geom_boxplot()

plot_ctr_cat

# => "surprise, "savings", "steal', and "blow [out]" have a positive
#     impat on click through rates. So these terms do work. The higher click through
#     rates more than compensate for the lower open rates associated with these terms.

# open and click throuh rates for days of the week

plot_or_day <- ggplot(camp, aes(day, or)) + geom_boxplot()
plot_ctr_day <- ggplot(camp, aes(day, ctr)) + geom_boxplot()

plot_ctr_day 

# open and click throuh rates for time of day

camp$hour = as.factor(camp$hour)
plot_or_day <- ggplot(camp, aes(factor(hour), or)) + geom_boxplot()
plot_or_day
plot_ctr_day <- ggplot(camp, aes(factor(hour), ctr)) + geom_boxplot()
plot_ctr_day
plot_camp_per_hour <- ggplot(camp, aes(hour)) + geom_histogram(stat = 'count')

# => 6 pm appears to be the best time for click throuh rates - probably not statistically significant though.

# open and click through rates over time

plot_or_time <- ggplot(camp, aes(date, or)) + geom_point() + geom_smooth(method = 'lm')
plot_or_time

plot_ctr_time <- ggplot(camp, aes(date, ctr)) + geom_point() + geom_smooth(method = 'lm')
plot_ctr_time





# Linear models #####################################################################################


# Setting Open Rate (or) as the dependent variable (looking at click through later)

# select variables
camp_vars_or <- select(camp, or, hour, day, length, perc_off, amt_off, off, dis, sav, lim_ed, vinyl,
                    box_set, deluxe, exclusive, new, blow, sale, steal, garage, surprise, value)

# model all  variables
mod_or_all = lm(or ~., camp_vars_or)
summary(mod_or_all)

# compile resultsd
mod_or_all_R_squared <- summary(mod_or_all)$r.squared 
mod_or_all_F_value <-   summary(mod_or_all)$f[1]
mod_or_all_res <- c(mod_or_all_R_squared,mod_or_all_F_value)


# model with significant variables only
mod_or_2 = lm(or ~ day + off + dis + vinyl + sale + surprise +  value, camp_vars_or)
summary(mod_or_2)

# compling results
mod_or_2_R_squared <- summary(mod_or_2)$r.squared 
mod_or_2_F_value <-   summary(mod_or_2)$f[1]
mod_or_2_res <- c(mod_or_2_R_squared, mod_or_2_F_value)


# model without the 'sale' variable
mod_or_3 = lm(or ~ day + off + dis + vinyl + surprise + value, camp_vars_or)
summary(mod_or_3)

# compiling results
mod_or_3_R_squared <- summary(mod_or_3)$r.squared 
mod_or_3_F_value <-   summary(mod_or_3)$f[1]
mod_or_3_res <- c(mod_or_3_R_squared, mod_or_3_F_value)

# compiling results from all 3 open rate - models
mod_or_res <- rbind(mod_or_all_res, mod_or_2_res, mod_or_3_res)
mod_or_res



# Click Through Rate (ctr) ~ all

camp_vars_ctr <- select(camp, ctr, hour, day, length, perc_off, amt_off, off, dis, sav, lim_ed, vinyl,
                    box_set, deluxe, exclusive, new, blow, sale,  steal, garage, surprise, value)

mod_ctr_all = lm(ctr ~., camp_vars_ctr)
summary(mod_ctr_all)

# compiling results
mod_ctr_all_R_squared <- summary(mod_ctr_all)$r.squared 
mod_ctr_all_F_value <-   summary(mod_ctr_all)$f[1]
mod_ctr_all_re <- c(mod_ctr_all_R_squared, mod_ctr_all_F_value)



# model after removing insignificant variables

mod_ctr_1 = lm(ctr ~ day + length + perc_off + amt_off + off + dis + vinyl + new + blow + value + steal + garage + surprise, camp_vars_ctr)
summary(mod_ctr_1)

# compiling results
mod_ctr_1_R_squared <- summary(mod_ctr_1)$r.squared 
mod_ctr_1_F_value <-   summary(mod_ctr_1)$f[1]
mod_ctr_1_res = c(mod_ctr_1_R_squared, mod_ctr_1_F_value)



# model without the insignificant variables

mod_ctr_2 = lm(ctr ~  length + perc_off + amt_off + off + dis  + blow + value + steal + surprise, camp_vars_ctr)
summary(mod_ctr_2)


mod_ctr_2_R_squared <- summary(mod_ctr_2)$r.squared 
mod_ctr_2_F_value <-   summary(mod_ctr_2)$f[1]
mod_ctr_2_res <-  c(mod_ctr_2_R_squared, mod_ctr_2_F_value)

# removing discoung variable that's now insignificant

mod_ctr_3 = lm(ctr ~  length + perc_off + amt_off + off +  blow + value + steal + surprise, camp_vars_ctr)
summary(mod_ctr_3)


mod_ctr_3_R_squared <- summary(mod_ctr_3)$r.squared 
mod_ctr_3_F_value <-   summary(mod_ctr_3)$f[1]
mod_ctr_3_res <-  c(mod_ctr_3_R_squared, mod_ctr_3_F_value)
mod_ctr_3_res

# Comparing all three Click Through Rate models:
mod_ctr_res = rbind(mod_ctr_all_re, mod_ctr_1_res,mod_ctr_2_res, mod_ctr_3_res)
mod_ctr_res


# Comparing results from al 6 models - CTR and OR

mod_or_ctr_res <- as.data.frame(rbind(mod_or_res, mod_ctr_res))
names(mod_or_ctr_res) <- c("R_Squared", "F_Statistic")
mod_or_ctr_res

# Best Model:
summary(mod_ctr_3)


# looking at artist vs generic campaigns...
camp_artist = camp[is.null(camp$Artists) == FALSE,]
camp_artist_vars_or <- select(camp_artist, or, hour, day, length, perc_off, amt_off, off, dis, sav, lim_ed, vinyl,
                              box_set, deluxe, exclusive, new, blow, sale, steal, garage, surprise, value)






##############################################################################################################




