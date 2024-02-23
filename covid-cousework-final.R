#-------------------------------------------------------------------------------
#   Check for installed packages 
#-------------------------------------------------------------------------------
installed.packages()
#-------------------------------------------------------------------------------
#   Install packages if not already installed
#-------------------------------------------------------------------------------
install.packages("tidyverse")
install.packages('dplyr')
install.packages('funModeling')
install.packages('data.table')
install.packages('ggplot2')
install.packages("corrplot")
install.packages('ggcorrplot')
#-------------------------------------------------------------------------------
# Load required libraries
#-------------------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(WDI)
library(funModeling)
library(data.table)
library(ggplot2)
library(corrplot)
library(ggcorrplot)
#-------------------------------------------------------------------------------
# 1. Read the covid csv file to a dataframe
#-------------------------------------------------------------------------------

covid_df <-read.csv( "owid-covid-data.csv", header = TRUE) 
str(covid_df)
count(covid_df)
View(covid_df)
#-------------------------------------------------------------------------------
# 2. Transform the Date field from character to date format
#-------------------------------------------------------------------------------

covid_df$date = as.Date(covid_df$date ) 
xyz = as.Date("2020-02-24") 
class(xyz)

covid_india <- covid_df %>% filter(location == 'India' & date >= '2020/01/01' & date <= '2020/12/30')
covid_USA <- covid_df %>% filter(location == 'United States' & date >= '2021/01/01' & date <= '2021/12/30')
View(covid_USA)
count(covid_india)
ggplot(covid_india, aes( date, new_cases_per_million))+ geom_point() +
        geom_smooth(method = "lm", se=FALSE) 
#-------------------------------------------------------------------------------
# 3. Select only the required variables for 2021 year data from covid dataset
#-------------------------------------------------------------------------------

covid_2021_df <- covid_df %>%      
  select( continent,
                location, 
                date, 
          total_cases_per_million,
                new_cases_per_million,
          total_deaths_per_million,
                new_deaths_per_million,
                hosp_patients_per_million,
                total_vaccinations_per_hundred,
            #    people_vaccinated_per_hundred,
                stringency_index,
                population_density
  ) 
%>%
  #filter(date >= "2020/01/01" & date <= "2020/12/31")

count(covid_2021_df)


#-------------------------------------------------------------------------------
# 4. Get the World Bank data WDIcache() using WDI library  
#-------------------------------------------------------------------------------

new_wdi_cache <- WDIcache()  

#-------------------------------------------------------------------------------
# 5. Get the GDP 2021 data
#-------------------------------------------------------------------------------

GDP <- WDI( start = 2021,
            end = 2021,
            extra = TRUE,
            cache = new_wdi_cache)

#-------------------------------------------------------------------------------
# 6. Get the high income countries from the GDP
#-------------------------------------------------------------------------------

high_income <- GDP %>%
  select(country, income) %>%
  filter( income == "High income")

head(high_income)
count(high_income)

#-------------------------------------------------------------------------------
# 7. Get the high income countries list 
#------------------------------------------------------------------------------- 

country_list <- intersect(covid_2021_df$location, high_income$country)
country_list
#-------------------------------------------------------------------------------
# 8. Get the covid data only for the high income countries 
#-------------------------------------------------------------------------------

covid_hig_inc_2021_df <- covid_2021_df %>%
  filter(location %in% country_list )

count(covid_hig_inc_2021_df)
#-------------------------------------------------------------------------------
# 9. check for missing values
#-------------------------------------------------------------------------------
summary(covid_hig_inc_2021_df)
which(is.na(covid_hig_inc_2021_df))
plot_num(covid_hig_inc_2021_df) #
profiling_num(covid_hig_inc_2021_df  ) 
qqnorm(covid_hig_inc_2021_df$new_deaths_per_million);qqline(covid_hig_inc_2021_df$new_deaths_per_million, col = 2)
shapiro.test(covid_hig_inc_2021_df$total_cases_per_million)
View(covid_hig_inc_2021_df)
#-------------------------------------------------------------------------------
# 10. Remove missing values
#-------------------------------------------------------------------------------

covid_clean_df <- na.omit(covid_hig_inc_2021_df)
count(covid_clean_df)
View(covid_clean_df)

#-------------------------------------------------------------------------------
# 11. Rename column names for easy use
#-------------------------------------------------------------------------------


covid_clean_df <- covid_clean_df %>% 
  rename(
   "tot_case_pm"  = "total_cases_per_million",
    "new_case_pm"  = "new_cases_per_million",
   "tot_death_pm" = "total_deaths_per_million",
    "new_death_pm" = "new_deaths_per_million",
    "hosp_pat_pm"  = "hosp_patients_per_million",
    "tot_vacc_ph"  = "total_vaccinations_per_hundred",
 #   "ppl_vacc_ph"  = "people_vaccinated_per_hundred",
   # "ppl_ful_vacc_ph" = "people_fully_vaccinated_per_hundred",
  #  "tot_boost_ph"    = "total_boosters_per_hundred",
    "str_idx"      = "stringency_index" ,
    "pop_den"      = "population_density"
  )

View(covid_clean_df)
count(covid_clean_df)
#-------------------------------------------------------------------------------
# 12. Do a descriptive analysis 
#-------------------------------------------------------------------------------
plot_num(covid_clean_df) #
ggplot(covid_clean_df, aes( new_case_pm, new_death_pm)) + geom_point() + geom_smooth(method = "lm", se=FALSE) 
ggplot(covid_clean_df, aes( pop_den, new_death_pm)) + geom_point() + geom_smooth(method = "lm", se=FALSE) 
ggplot(covid_clean_df, aes( str_idx, new_death_pm)) + geom_point() + geom_smooth(method = "lm", se=FALSE) 
profiling_num(covid_clean_df$new_deaths_per_million   ) 
qqnorm(covid_clean_df$new_deaths_per_million);qqline(covid_clean_df$new_deaths_per_million, col = 2)




#-------------------------------------------------------------------------------
# 12. check for normalisation 
#-------------------------------------------------------------------------------
ks.test(covid_clean_df$new_deaths_per_million, 'pnorm')
ks.test(covid_clean_df$new_cases_per_million, 'pnorm')
#ks.test(covid_clean_df$total_deaths_per_million, 'pnorm')
ks.test(covid_clean_df$hosp_patients_per_million, 'pnorm')
ks.test(covid_clean_df$total_vaccinations_per_hundred, 'pnorm')
ks.test(covid_clean_df$people_vaccinated_per_hundred, 'pnorm')
ks.test(covid_clean_df$stringency_index, 'pnorm')
ks.test(covid_clean_df$population_density, 'pnorm')

#-------------------------------------------------------------------------------
# 12. Normalise the data
#-------------------------------------------------------------------------------

covid_clean_df$ndpm_sc <- scale(covid_clean_df$new_deaths_per_million)
covid_clean_df$ncpm_sc <- scale(covid_clean_df$new_cases_per_million)
#covid_clean_df$tdpm_sc <- scale(covid_clean_df$total_deaths_per_million)
covid_clean_df$hppm_sc <- scale(covid_clean_df$hosp_patients_per_million)
covid_clean_df$tvpm_sc <- scale(covid_clean_df$total_vaccinations_per_hundred)
covid_clean_df$pvph_sc <- scale(covid_clean_df$people_vaccinated_per_hundred)
covid_clean_df$si_sc <- scale(covid_clean_df$stringency_index)
covid_clean_df$pd_sc <- scale(covid_clean_df$population_density)

#-------------------------------------------------------------------------------
# 12. Create a dataframe with only continuous variables
#-------------------------------------------------------------------------------
covid_clean_num_df <- covid_clean_df %>%
        select( new_death_pm,
                tot_death_pm,
                new_case_pm,
               tot_case_pm,
                hosp_pat_pm,
                tot_vacc_ph, 
              #  ppl_vacc_ph,
               # ppl_ful_vacc_ph,
              #  tot_boost_ph,
                str_idx,	
                pop_den
                
  )

covid_clean_sc_df <- covid_clean_df %>%
  select( ndpm_sc,
          ncpm_sc,
          hppm_sc,
          tvpm_sc,
          pvph_sc,
          si_sc, 
          pd_sc
          
  )
summary(covid_clean_sc_df)
ks.test(covid_clean_sc_df$ndpm_sc, 'pnorm')
ks.test(covid_clean_df$ncpm_sc, 'pnorm')
ks.test(covid_clean_df$hppm_sc, 'pnorm')
ks.test(covid_clean_df$tvpm_sc, 'pnorm')
ks.test(covid_clean_df$people_vaccinated_per_hundred, 'pnorm')
ks.test(covid_clean_df$stringency_index, 'pnorm')
ks.test(covid_clean_df$population_density, 'pnorm')

pairs(covid_clean_sc_df, pch = 18, col = "steelblue")
pairs(covid_clean_num_df, pch = 18, col = "steelblue")
install.packages("GGally")
library(GGally)
ggpairs(covid_clean_sc_df)
ggpairs(covid_clean_num_df)
#-------------------------------------------------------------------------------
# 12. check for Multicollinearity
#-------------------------------------------------------------------------------

corr_covid <- cor( covid_clean_num_df)
ggcorrplot( corr_covid,   
            type = "lower",
            lab = TRUE)

corr_covid_sc <- cor( covid_clean_sc_df)
ggcorrplot( corr_covid_sc,   
            type = "lower",
            lab = TRUE)

#-------------------------------------------------------------------------------
# 12. Multi-linear model
#-------------------------------------------------------------------------------

lm_model1 = lm( new_death_pm ~
                tot_death_pm +
                new_case_pm +    
                hosp_pat_pm +
                tot_vacc_ph +
                str_idx+
                pop_den , covid_clean_df )

lm_model1
summary(lm_model1)
hist(residuals(lm_model1), col = "steelblue")
plot(fitted(lm_model1), residuals(lm_model1))
abline(h = 0, lty = 2)

lm_model2 = lm(  ndpm_sc ~
                  ncpm_sc +
                  hppm_sc +
                  pvph_sc 
                #  si_sc+
                 #   pd_sc
                 ,covid_clean_sc_df )

lm_model2
summary(lm_model2)
hist(residuals(lm_model2), col = "steelblue")
plot(fitted(lm_model2), residuals(lm_model2))
#add horizontal line at 0
abline(h = 0, lty = 2)

#-------------------------------------------------------------------------------
# 12. Multi-linear model
#-------------------------------------------------------------------------------
