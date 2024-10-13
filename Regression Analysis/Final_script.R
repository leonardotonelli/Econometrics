## IMPORT THE LIBRARIES
library(stats)
library(lmtest)
library(data.table)
library(car)
library(nlme)
library("reshape2")


### DATA CLEANING ###

# CONVERT DATAFRAMES FROM WIDE TO LONG FORMAT
melted_pop = melt(POP_GROW, id.vars = 1:4)
melted_saving = melt(Saving_rate, id.vars = 1:4)
melted_gdppc = melt(GDP_per_capita, id.vars = 1:4)
melted_res = melt(Research_and_development_expenditure, id.vars = 1:4)

# CHANGE THE NAME OF OUR VALUES' COLUMNS
colnames(melted_pop)[colnames(melted_pop) == "value"] <- "pop_growth"
colnames(melted_saving)[colnames(melted_saving) == "value"] <- "sav_rate"
colnames(melted_gdppc)[colnames(melted_gdppc) == "value"] <- "gdp_pc"
colnames(melted_res)[colnames(melted_res) == "value"] <- "rnd_exp"

# DELETE USELESS COLUMNS
columns_to_delete <- c("Country Code", "Indicator Name", "Indicator Code")
clean_pop <- melted_pop[, !names(melted_pop) %in% columns_to_delete]
clean_sav <- melted_saving[, !names(melted_saving) %in% columns_to_delete]
clean_gdppc <- melted_gdppc[, !names(melted_gdppc) %in% columns_to_delete]
clean_res <- melted_res[, !names(melted_res) %in% columns_to_delete]


# MERGE THE DATAFRAMES IN ONE 
merged = merge(merge(merge(clean_pop, clean_sav, by = 1:2), clean_gdppc, by = 1:2), clean_res, by = 1:2)

# CHANGE NAME OF DATE'S COLUMN
colnames(merged)[colnames(merged) == "variable"] <- "Year"

# DROP GENERIC LISTINGS (E.G. EU, ASIA...)

to_be_dropped<- c(
  "Arab World",
  "Africa Eastern and Southern",
  "Africa Western and Central",
  "Europe & Central Asia (excluding high income)",
  "Europe & Central Asia (IDA & IBRD countries)",
  "Europe & Central Asia",
  "European Union",
  "Fragile and conflict affected situations",
  "Heavily indebted poor countries (HIPC)",
  "High income",
  "IBRD only",
  "IDA & IBRD total",
  "IDA blend",
  "IDA only",
  "IDA total",
  "Lao PDR",
  "Late-demographic dividend",
  "Latin America & Caribbean (excluding high income)",
  "Latin America & Caribbean",
  "Latin America & the Caribbean (IDA & IBRD countries)",
  "Least developed countries: UN classification",
  "Low & middle income",
  "Low income",
  "Lower middle income",
  "Micronesia, Fed. Sts.",
  "Middle East & North Africa (excluding high income)",
  "Middle East & North Africa (IDA & IBRD countries)",
  "Middle East & North Africa",
  "Middle income",
  "Not classified",
  "OECD members",
  "Other small states",
  "Post-demographic dividend",
  "Pre-demographic dividend",
  "South Africa",
  "South Asia (IDA & IBRD)",
  "South Asia",
  "Sub-Saharan Africa (excluding high income)",
  "Sub-Saharan Africa (IDA & IBRD countries)",
  "Sub-Saharan Africa",
  "Upper middle income",
  "World",
  "Caribbean small states" ,
  "Central Europe and the Baltics",
  "Early-demographic dividend",
  "East Asia & Pacific (excluding high income)",
  "East Asia & Pacific (IDA & IBRD countries)" ,
  "East Asia & Pacific",
  "Euro area", 
  "North America",
  "Pacific island small states", 
  "Russian Federation" ,
  "Small states"
)

merged_v2 <- subset(merged, !(merged[, "Country Name"] %in% to_be_dropped))


#FILTRO: REMOVE NO OIL COUNTRY

no_oil = c("Bahrain",
           "Gabon",
           "Iran, Islamic Rep.", 
           "Iraq",
           "Kuwait",
           "Oman", 
           "Saudi Arabia", 
           "United Arab Emirates")

merged_v3 <- subset(merged_v2, !(merged_v2[, "Country Name"] %in% no_oil))

countries <- c(
  "Afghanistan", "Angola", "Albania", "United Arab Emirates", "Argentina", 
  "Armenia", "Australia", "Austria", "Azerbaijan", "Burundi", 
  "Belgium", "Benin", "Burkina Faso", "Bangladesh", "Bulgaria", 
  "Bosnia and Herzegovina", "Belarus", "Bolivia", "Brazil", "Botswana", 
  "Central African Republic", "Canada", "Switzerland", "Chile", "China", 
  "Cote d'Ivoire", "Cameroon", "Congo, Dem. Rep.", "Congo, Rep.", 
  "Colombia", "Costa Rica", "Cuba", "Czechia", "Germany", "Denmark", 
  "Dominican Republic", "Algeria", "Ecuador", "Egypt, Arab Rep.", "Eritrea", 
  "Spain", "Estonia", "Ethiopia", "Finland", "France", "Gabon", 
  "United Kingdom", "Georgia", "Ghana", "Guinea", "Gambia, The", 
  "Guinea-Bissau", "Greece", "Guatemala", "Hong Kong SAR, China", 
  "Honduras", "Croatia", "Haiti", "Hungary", "Indonesia", "India", 
  "Ireland", "Iran, Islamic Rep.", "Iraq", "Israel", "Italy", 
  "Jamaica", "Jordan", "Japan", "Kazakhstan", "Kenya", "Kyrgyz Republic", 
  "Cambodia", "Korea, Rep.", "Kuwait", "Lebanon", "Liberia", 
  "Libya", "Sri Lanka", "Lesotho", "Lithuania", "Latvia", "Morocco", 
  "Moldova", "Madagascar", "Mexico", "North Macedonia", "Mali", 
  "Myanmar", "Mongolia", "Mozambique", "Mauritania", "Mauritius", 
  "Malawi", "Malaysia", "Namibia", "Niger", "Nigeria", "Nicaragua", 
  "Netherlands", "Norway", "Nepal", "New Zealand", "Oman", "Pakistan", 
  "Panama", "Peru", "Philippines", "Papua New Guinea", "Poland", 
  "Puerto Rico", "Korea, Dem. People's Rep.", "Portugal", "Paraguay", 
  "Romania", "Rwanda", "Saudi Arabia", "Sudan", "Senegal", "Singapore", 
  "Sierra Leone", "El Salvador", "Somalia", "Serbia", "South Sudan", 
  "Slovak Republic", "Slovenia", "Sweden", "Syrian Arab Republic", 
  "Chad", "Togo", "Thailand", "Tajikistan", "Turkmenistan", "Trinidad and Tobago", 
  "Tunisia", "Turkiye", "Tanzania", "Uganda", "Ukraine", "Uruguay", 
  "United States", "Uzbekistan", "Venezuela, RB", "Viet Nam", "Kosovo", 
  "Yemen, Rep.", "Zambia"
)


merged_v4 = subset(merged_v3, (merged_v3[, "Country Name"] %in% countries))

# RENAME THE DATAFRAME
data = merged_v4

# # EXPORT THE DATAFRAME AS CSV FILE IN THE WORKING DIRECTORY
# write.csv(data, "cleaned_final.csv", row.names = FALSE)
# getwd()




### TASK 1 ###

## ORDER THE DATASET
data <- data[order(data$Year),]


## COUNT THE NEGATIVE VALUES 
na_count <- colSums(is.na(data))
na_count
# a lot of nas for all the variables
negative_count <- colSums(data < 0, na.rm = TRUE)
negative_count
# consistent amount of negative values for population growth, cannot take the log


## REMOVE NEGATIVE VALUES TO RUN LOG-REGRESSION (justify the dropping or other methods)
data_ng <- data[data$pop_growth > 0 & data$sav_rate > 0, ]


## RUN NORMAL OLS REGRESSION
model_OLS = lm(log(gdp_pc) ~ log(sav_rate) + log(pop_growth), data = data_ng)
summary(model_OLS)


## RUN ASSUMPTIONS' TESTS ON THIS REGRESSION
durbinWatsonTest(model_OLS) 
bgtest(model_OLS)
bptest(model_OLS)
resettest(model_OLS)


## PLOT THE OLS RESIDUALS OF WHOLE DATASET
residuals_ols = residuals(model_OLS)
residual_index = 1:length(residuals_ols)
plot(residual_index, residuals_ols, type = "p", pch = 16, cex = 0.5, main = "Residuals Plot with Regression Line", xlab = "Observation", ylab = "Residuals")
# HORIZONTAL LINE FOR REFERENCE
abline(h = 0, col = "red", lwd=3)
# LINEAR REGRESSION FOR REFERENCE
residual_index <- 1:length(residuals_ols)
residual_model <- lm(residuals_ols ~ residual_index)
# PLOT THE FITTED CURVE TO PROVE SERIAL CORRELATION
abline(residual_model, col = "blue", lwd=3)


## DO OLS REGRESSION FOR ONE SINGLE YEAR
sin_data = data.table(data_ng)
single = sin_data[Year == 2016]
model_single = lm(log(gdp_pc) ~ log(sav_rate) + log(pop_growth), data = single)


## RUN TESTS AND PLOT THE RESIDUALS
durbinWatsonTest(model_single) 
bgtest(model_single)
bptest(model_single)
resettest(model_single)


## PLOT RESIDUALS
single_residuals = residuals(model_single)
single_residual_index = 1:length(single_residuals)
plot(single_residual_index, single_residuals, type = "p", pch = 16, cex = 0.5, main = "Residuals Plot with Regression Line", xlab = "Observation", ylab = "Residuals")
abline(h = 0, col = "red", lwd=3)
single_residual_model <- lm(single_residuals ~ single_residual_index)
abline(single_residual_model, col = "blue", lwd=3)


## FGLS
data_gls = data_ng[complete.cases(data_ng$sav_rate, data_ng$pop_growth, data_ng$gdp_pc), ]
fgls_model = gls(log(gdp_pc) ~ log(sav_rate) + log(pop_growth), data = data_gls, correlation = corAR1())
summary(fgls_model)
# very similar results


## COMPARE RESIDUALS OLS v. GLS
gls_residuals = resid(fgls_model)
plot(residuals_ols, type = "l", col = "blue", xlab = "Sample Index", ylab = "Residuals", main = "Residuals for Standard Linear Regression")
plot(gls_residuals, type = "l", col = "red", xlab = "Sample Index", ylab = "Residuals", main = "Residuals for GLS Regression")



### TASK 2 ###

#FGLS:
data_gls_2 = na.omit(data_ng)
fgls_model_2 = gls(log(gdp_pc) ~ log(sav_rate) + log(pop_growth) + log(rnd_exp), data = data_gls_2, correlation = corAR1())
summary(fgls_model_2)


#CORRELATION WITHOUT LOG:
cor(data_gls_2$pop_growth, data_gls_2$rnd_exp)
#Negative correlated
cor(data_gls_2$sav_rate, data_gls_2$rnd_exp)
#Positive correlated

# WITH LOG
cor(log(data_gls_2$pop_growth), log(data_gls_2$rnd_exp))
#Negative correlated
cor(log(data_gls_2$sav_rate), log(data_gls_2$rnd_exp))
#Positive correlated







