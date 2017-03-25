#############################
# Author: Prem Prakash
# Data Mining Project
#############################

# setwd('/Users/prem/Desktop/mldm/2_sem/data_mining/project_DM/data')
# read.table("myfile.csv",skip=3, header=T)
# read.delim('filename', comment.char = '#')
# -1 => Age not specified
# +100 => for 100+ years
# 101 =>  All ages


ld <- read.csv("1_literacy_data_all.csv", header = TRUE, comment.char = '#')
s_gdp <- read.csv('2_state_gdp.csv', header = TRUE, comment.char = '#')
s_health <- read.csv('3_states_health_expenses.csv', header = TRUE, comment.char = '#')
s_education <- read.csv('4_exprenses_on_education.csv', header = TRUE, comment.char = '#')
woman_parity_in_education <- read.csv('5_state_wise_woman_going_to_higher_education_compared_man_18-23.csv', header = TRUE, comment.char = '#')
unemployment_rate <- read.csv('6_UnemploymentRates2015-16.csv', header = TRUE, comment.char = '#')

# Extra data could be used for association of the results
# et0 <- read.csv('et0_literacy_rate.csv', header = TRUE, comment.char = '#')
# et1 <- read.csv('et1_Sector-Wise Expenditure on Education by Central.csv', header = TRUE, comment.char = '#') # et1_Sector-Wise Expenditure on Education by Central.csv
# et2 <- read.csv('et2_Sector-Wise_Expenditure_On_Education_By_Central_And_State_Governments_Revenue_Account.csv', header = TRUE, comment.char = '#')


# Extracting the relevant features from literacy data i.e <ld(1_literacy_data_all.csv)>
# [1..6]Table.Name, State.code, Dist.Code, Area.Name, Literate.Illiterate, Age,
# [7..9]Persons.All, Males.All, Females.All,
# [10..12]Persons.Rural, Males.Rural, Females.Rural,
# [13..15]Persons.Urban, Males.Urban, Females.Urban

states <- as.character(unique(unlist(ld$Area.Name)))   # as.character(unique(unlist(ld[,4])))
removeNAStates <- c('India', 'Delhi', 'Daman and Diu', 'Dadra and Nagar Haveli', 'Andaman and Nicobar Islands', 'Puducherry', 'Lakshadweep','Chandigarh')
states <- setdiff(states, removeNAStates) # Remove India name from the list
# [1] "Jammu and Kashmir" => JnK "Himachal Pradesh" =>   "Punjab"            "Uttaranchal"       "Haryana"       
# [6] "Rajasthan"         "Uttar Pradesh"     "Bihar"             "Sikkim"            "Arunachal Pradesh"
# [11] "Nagaland"          "Manipur"           "Mizoram"           "Tripura"           "Meghalaya"    
# [16] "Assam"             "West Bengal"       "Jharkhand"         "Orrisa"            "Chhattisgarh" 
# [21] "Madhya Pradesh"    "Gujarat"           "Maharashtra"       "Andhra Pradesh"    "Karnataka"    
# [26] "Goa"               "Kerala"            "Tamil Nadu" 
# Separate data for the working
# 2 => state, 4 => state name, 5 => Literate/Illiterat, 6 => Age
all <- c(2, 4, 5, 6, 7, 8, 9)
rural <- c(2, 4, 5, 6, 10, 11, 12)
urban <- c(2, 4, 5, 6, 13, 14, 15)
region <- c(all, rural, urban)


####
# Calculate the literacy of a state for the given range of age
# ld[(ld[,5] == 'Literate' & ld[,4] == 'Bihar'), ],  dim = 103  15
CalculateLitForState <- function(state = 'Bihar', age.range = c(6, 25), type = 'Illiterate') {
  
  allStateForAgeGroup <- ans <- ld[(ld$Literate.Illiterate == type & ld$Area.Name == 'India' & ld$Age >= age.range[1] & ld$Age <= age.range[2]), ]
  
  india.all <- c(sum(ans$Persons.All), sum(ans$Males.All), sum(ans$Females.All)) 
  # print(all)
  india.rural <- c(sum(ans$Persons.Rural), sum(ans$Males.Rural), sum(ans$Females.Rural))
  # print(rural)
  india.urban <- c(sum(ans$Persons.Urban), sum(ans$Males.Urban), sum(ans$Females.Urban))
  
  
  # ld[(ld[,5] == 'Literate' & ld[,4] == 'Bihar' & ld[, 6] >= 11 & ld[, 6] <=20),  ]
  ans <- ld[(ld$Literate.Illiterate == type & ld$Area.Name == state & ld$Age >= age.range[1] & ld$Age <= age.range[2]),]
  
  all <- c(sprintf('%.4f', (sum(ans$Persons.All) * 100) / india.all[1]), 
           sprintf('%.4f', (sum(ans$Males.All) * 100) / india.all[2]),
           sprintf('%.4f', (sum(ans$Females.All) * 100) / india.all[3])) 
  # print(all)
  
  rural <- c(sprintf('%.4f', (sum(ans$Persons.Rural) * 100) / india.rural[1]), 
             sprintf('%.4f', (sum(ans$Males.Rural) * 100 ) / india.rural[2]),
             sprintf('%.4f', (sum(ans$Females.Rural) * 100) / india.rural[3]))
  # print(rural)
  
  urban <- c(sprintf('%.4f', (sum(ans$Persons.Urban) * 100) / india.urban[1]), 
             sprintf('%.4f', (sum(ans$Males.Urban) * 100) / india.urban[2]),
             sprintf('%.4f', (sum(ans$Females.Urban) * 100) / india.urban[3]))
  # print(urban)
  
  return(c(all, rural, urban))
}

# age.range <- as.data.frame(setNames(replicate(5,numeric(0), simplify = F), letters[1:5]))
# For creating new data set
generated.names <- c('State', 'Age.Range', 'Literate.Illiterate', 'All.Person', 'All.Male', 'All.Female', 'Rural.Person', 'Rural.Male', 'Rural.Female', 'Urban.Person', 'Urban.Male', 'Urban.Female')

# Age groups on whome the mining will be done
#age.groups <- rbind(c(7, 18), c(19, 25), c(26, 35), c(36, 45), c(46, 60), c(7, 25), c(26, 40), c(41, 60), c(61, 100))
age.groups <- rbind(c(7, 25), c(26, 50), c(51, 75), c(76, 100))

# Generate data for the different age group.
# states <- as.character(unique(unlist(ld[,4])))
# lit.data.generated <- setNames(sample, generated.names)
lit.names <- c('Literate', 'Illiterate')
GenerateAgeRangeData <- function() {
  age.groups.string <<- c()
  groups <- age.groups
  lit.data.generated <- rbind() # Initialized to NULL
  for(i in seq(1, dim(groups)[1])) {
    group <- groups[i,]
    age.group <- paste(toString(group[1]), '-', toString(group[2]), sep = '')
    age.groups.string <<- c(age.groups.string, age.group) # '<<-' makes <age.groups.string> global 
    for(state in states) {
      # sample <-c(c('Bihar', '6-25', 'Illiterate'), CalculateLitForState())
      row.lit <- c(c(state, age.group, lit.names[1]), CalculateLitForState(state, group,  lit.names[1]))
      row.illit <- c(c(state, age.group, lit.names[2]), CalculateLitForState(state, group, lit.names[2]))
      if (is.null(lit.data.generated)) { # when lit.data.generated <- Flase
        lit.data.generated <- rbind(row.lit, row.illit, deparse.level = 0)
      } else {
        lit.data.generated <- rbind(lit.data.generated, row.lit, row.illit, deparse.level = 0)
      }
    } # End of state in states
  }
  return(lit.data.generated)
}

# Generate the data for mining
lit.data.generated <- GenerateAgeRangeData()
lit.data.generated <- as.data.frame(lit.data.generated)
colnames(lit.data.generated) <- generated.names

# Dimension of generated data
dim(lit.data.generated)
# 630  12
generated.data.ordered <- lit.data.generated[order(lit.data.generated$State),]
# age.groups.string => [1] "7-18"   "19-25"  "26-35"  "36-45"  "46-60"  "7-25"   "26-40"  "41-60"  "61-100"

# To see total sum add up to 100 
temp <- generated.data.ordered
age_group_sum <- sum(as.numeric(as.character(temp[(temp$Literate.Illiterate == 'Literate' & temp$Age.Range == '19-25'), 5])))
age_group_7_18



library(sp)
library(ggplot2)
# library(maptools)
library(RColorBrewer)
library(rJava)
library(knitr)
suppressPackageStartupMessages(library(googleVis))

# Help at  http://gadm.org/country
# india <- readRDS("IND_adm1.rds")
# india <- india[-(32:33),]
# generated.names <- c('State', 'Age.Range', 'Literate.Illiterate', 'All.Person', 'All.Male', 'All.Female', 'Rural.Person', 'Rural.Male', 'Rural.Female', 'Urban.Person', 'Urban.Male', 'Urban.Female')



PlotData <- function(type = 'Illiterate', age.range = '7-18') {
  print(paste('age.range = ', age.range))
  plot_data <- generated.data.ordered
  plot_data <- plot_data[(plot_data$Literate.Illiterate == type & plot_data$Age.Range == age.range), ]
  plot_data$All.Person <- as.numeric(as.character(plot_data$All.Person))
  return(plot_data)
}

# Create a lis of data
temp <- 1
plotDataList <- list()
for(ageGroup in age.groups.string) {
  ageGroupPlot = PlotData(age.range = ageGroup)
  plotDataList [[temp]] = ageGroupPlot
  temp <- temp + 1
}

ageWiseLitDataList <- plotDataList
#  Plotting Age groups => "7-25"   "26-50"  "51-75"  "76-100"
plot_data <- plotDataList[[1]] # 
ageGroupPlot7 <- gvisGeoChart(data = plot_data, locationvar = 'State',
                             colorvar = 'All.Person', options = list(region = 'IN', domain = 'IN', displayMode = 'regions', resolution = 'provinces', width = 600, height = 400), chartid = 'AllPersonOfAgeGroups7_25')
plot(ageGroupPlot7)

# 26-50
plot_data <- plotDataList[[2]] # 
ageGroupPlot26 <- gvisGeoChart(data = plot_data, locationvar = 'State',
                             colorvar = 'All.Person', options = list(region = 'IN', domain = 'IN', displayMode = 'regions', resolution = 'provinces', width = 600, height = 400), chartid = 'AllPersonOfAgeGroups26_50')
plot(ageGroupPlot26)

#  "51-75"
plot_data <- plotDataList[[3]] # 
ageGroupPlot51 <- gvisGeoChart(data = plot_data, locationvar = 'State',
                              colorvar = 'All.Person', options = list(region = 'IN', domain = 'IN', displayMode = 'regions', resolution = 'provinces', width = 600, height = 400), chartid = 'AllPersonOfAgeGroups51_75')
plot(ageGroupPlot51)

# "76-100"
plot_data <- plotDataList[[2]] # 
ageGroupPlot76 <- gvisGeoChart(data = plot_data, locationvar = 'State',
                               colorvar = 'All.Person', options = list(region = 'IN', domain = 'IN', displayMode = 'regions', resolution = 'provinces', width = 600, height = 400), chartid = 'AllPersonOfAgeGroups76_100')
plot(ageGroupPlot76)



# Draw map graph of gdp
# Convert gdp in percent
# 2. c(sprintf('%.4f', (sum(ans$Persons.All) * 100) / india.all[1]), 
s_gdp$NominalGDP_USD <- as.numeric(as.character(s_gdp$NominalGDP_USD))
s_gdp$NominalGDP_USD <- (s_gdp$NominalGDP_USD * 100) / s_gdp$NominalGDP_USD[36] 

state_gdp_plot <- gvisGeoChart(data = s_gdp, locationvar = 'States',
                               colorvar = 'NominalGDP_USD', options = list(region = 'IN', domain = 'IN', displayMode = 'regions', resolution = 'provinces', width = 600, height = 400), chartid = 'GDP_States')
plot(state_gdp_plot)



# 3. Draw the map of expenses on health
# plot_data$All.Person <- as.numeric(as.character(plot_data$All.Person))
s_health$percent2012 <- as.numeric(as.character(s_health$percent2012))
state_health_expense_plot <- gvisGeoChart(data = s_health, locationvar = 'States',
                               colorvar = 'percent2012', options = list(region = 'IN', domain = 'IN', displayMode = 'regions', resolution = 'provinces', width = 600, height = 400), chartid = 'State_health_expenditure')
plot(state_health_expense_plot)


# 4. State expenditure on education of their toatl expenditure
s_education$year2012_13 <- as.numeric(as.character(s_education$year2012_13))
state_education_expense <- gvisGeoChart(data = s_education, locationvar = 'States',
                                        colorvar = 'year2012_13', options = list(region = 'IN', domain = 'IN', displayMode = 'regions', resolution = 'provinces', width = 600, height = 400), chartid = 'State_expenditure_on_education_of_their_total_expense')
plot(state_education_expense)

# 5. Woman in higher education compared to man
woman_parity_in_education$AllCategories <- as.numeric(as.character(woman_parity_in_education$AllCategories))
state_woman_parity_in_higher_education <- gvisGeoChart(data = woman_parity_in_education, locationvar = 'States',
                                         colorvar = 'AllCategories', options = list(region = 'IN', domain = 'IN', displayMode = 'regions', resolution = 'provinces', width = 600, height = 400), chartid = 'Woman_parity_in_higher_education_compared_to_man')

plot(state_woman_parity_in_higher_education)


# 6. Unemployment rates 
unemployment_rate$Total <- as.numeric(as.character(unemployment_rate$Total))
state_unemployment_rate <- gvisGeoChart(data = unemployment_rate, locationvar = 'States',
                                                       colorvar = 'Total', options = list(region = 'IN', domain = 'IN', displayMode = 'regions', resolution = 'provinces', width = 600, height = 400), chartid = 'Unemployment_rate_state_wise_per_thousand')
plot(state_unemployment_rate)






# ageWiseLitDataList
# Comparision plotting to see relation between differnt factors taken into consideration 
age_group_7_25 <- ageWiseLitDataList[[1]]
barplot(height = age_group_7_25$All.Person,col=rainbow(20),ylim = c(0.000,25.000), names.arg = age_group_7_25$State, las = 2, space = 1.5, main = "Illiteracy rate across states for age group 7-25 ", xlab = "States", ylab = "Percentage", border = "green", horiz = FALSE)


# "7-25"   "26-50"  "51-75"  "76-100"
age_group_26_50 <- ageWiseLitDataList[[2]]
barplot(height = age_group_26_50$All.Person,col=rainbow(20),ylim = c(0.000,25.000), names.arg = age_group_26_50$State, las = 2, space = 1.5, main = "Illiteracy rate across states for age group 26-50 ", xlab = "States", ylab = "Percentage", border = "green", horiz = FALSE)



# "7-25"   "26-50"  "51-75"  "76-100"
age_group_51_75 <- ageWiseLitDataList[[3]]
barplot(height = age_group_51_75$All.Person,col=rainbow(20),ylim = c(0.000,25.000), names.arg = age_group_51_75$State, las = 2, space = 1.5, main = "Illiteracy rate across states for age group 51-75 ", xlab = "States", ylab = "Percentage", border = "green", horiz = FALSE)

# Age group 76-100
age_group_76_100 <- ageWiseLitDataList[[3]]
barplot(height = age_group_76_100$All.Person,col=rainbow(20),ylim = c(0.000,25.000), names.arg = age_group_76_100$State, las = 2, space = 1.5, main = "Illiteracy rate across states for age group 76-100 ", xlab = "States", ylab = "Percentage", border = "green", horiz = FALSE)



# 2 gdp plot
s_gdp_temp <- s_gdp[1:35,]
s_gdp_temp <- s_gdp_temp[order(s_gdp_temp$States), ]

barplot(height = s_gdp_temp$NominalGDP_USD,col=rainbow(10),ylim = c(0.000,25.000), names.arg = s_gdp_temp$State, las = 2, space = 1.5, main = "GDP share of states", xlab = "States", ylab = "Percentage", border = "green", horiz = FALSE)



# 3
s_health_temp <- s_health[1:35,]
s_health_temp <- s_health_temp[order(s_health_temp$States), ]

barplot(height = s_health_temp$percent2012,col=rainbow(10), ylim = c(0.000,15.000), names.arg = s_health_temp$State, las = 2, space = 1.5, main = "Public Expenditure in Health by States", xlab = "States", ylab = "Percentage", border = "green", horiz = FALSE)

# 4
s_education_temp <- s_education[1:35, ]
s_education_temp <- s_education_temp[order(s_education_temp$States)]
barplot(height = s_education_temp$year2012_13,col=rainbow(10), ylim = c(0.000,25.000), names.arg = s_education_temp$State, las = 2, space = 1.5, main = "Expenditure on education by states", xlab = "States", ylab = "Percentage", border = "green", horiz = FALSE)

# 5
woman_parity_in_education_temp <- woman_parity_in_education[1:35,]
woman_parity_in_education_temp <- woman_parity_in_education_temp[order(woman_parity_in_education$States)]
barplot(height = woman_parity_in_education_temp$AllCategories,col=rainbow(10), names.arg = woman_parity_in_education_temp$States, las = 2, space = 1.5, main = "Woman parity in higher education", xlab = "States", ylab = "Parity", border = "green", horiz = FALSE)


# 6:
unemployment_rate_temp <- unemployment_rate[1:35, ]
unemployment_rate_temp <- unemployment_rate_temp[order(unemployment_rate_temp$States)]
barplot(height = unemployment_rate_temp$Total,col=rainbow(10), ylim = c(0.000,250.000), names.arg = unemployment_rate_temp$States, las = 2, space = 1.5, main = "Unemployment rate per 1000 people", xlab = "States", ylab = "Unemployment", border = "green", horiz = FALSE)

# # Tester code:
# states <- as.character(unique(unlist(ld$Area.Name)))
# ch <- states[order(states)]
# test <- unemployment_rate[, 1]
# states_test <- as.character(unique(unlist(test)))
# states_test_sorted <- states_test[order(states_test)]
# states_test_sorted == ch















