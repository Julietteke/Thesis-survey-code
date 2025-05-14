# ---- Libraries ----
library(broom)
library(tidyr)
library(flextable)
library(officer)
library(glmnet)
library(caret)
library(dplyr)
library(readxl)
library(pwr)
library(gapminder)
library(tidyverse)
library(MASS)
library(mlogit)
library(ordinal)
library(kableExtra) 
library(stargazer)  
library(magrittr)
library(VGAM)  
library(stats) 
library(brant)
library(car)
library(ggplot2)

# ---- Cleaning data ----

df = read_excel("survey.xlsx")

df_cleaned = df[df$age != "< 18", ]
df_high_risk = df_cleaned[df_cleaned$high_risk == "Yes", ]
data = df_cleaned[df_cleaned$high_risk == "No", ]

data = data %>%
  mutate(annual_income = case_when(
    annual_income %in% c("<20 000", "20 000 - 30 000") ~ "Low",
    annual_income %in% c("30 000 - 50 000", "50 000 - 80 000") ~ "Middle",
    annual_income %in% c("80 000 - 100 000", ">100 000") ~ "High"
  ))

data = data %>%
  mutate(education = case_when(
    education %in% c("Secondary_education","Primary_education") ~ "Low",
    education %in% c("graduaat_opleiding","Bachelor") ~ "Medium",
    education %in% c("Master","phd") ~ "High"
  ))

data$vaccinated = ifelse(data$vaccinated == "Yes", 1, 0)
data$had_covid = ifelse(data$had_covid == "Yes", 1, 0)
data$covid_medical_care = ifelse(data$covid_medical_care == "Yes", 1, 0)

data$Tijdstempel = NULL
data$willingness_use_preventive_measures = round(rowMeans(data[, c("willingness_use_preventive_measures_masks", "willingness_social_distancing")], na.rm = TRUE))

write_excel_csv(data,file = "data_set.csv")
# ---- Load data ----
setwd("~/Desktop/THESIS")
df = read.csv("data_set.csv")
# ---- Power test ----
pwr.t.test(n = 65, d = 0.5, sig.level = 0.05, power = NULL, type = "two.sample")
# ---- Making factors -----
df$annual_income=factor(df$annual_income,levels = c("Low","Middle","High"),ordered = TRUE)
df$education=factor(df$education,levels = c("Low","Medium","High"),ordered = TRUE)
df$age=factor(df$age,levels = c("young","young-middle","middle-old","old"),ordered = TRUE)
df$gender=factor(df$gender,levels = c("male","female")) #female reference
df$marital_status=factor(df$marital_status,levels=c("relationship","Single","divorced"))
df$employment_status=factor(df$employment_status,levels=c("employed","retired","student"))

df$willingness_vaccination = factor(df$willingness_vaccination, levels = c(0,1,2,3,4,5),labels = c("Not at all willing", "strongly unwilling", "unwilling", "Neutral", "willing", "very willing"),ordered = TRUE)
df$influence_side_effects_on_vaccination = factor(df$influence_side_effects_on_vaccination,levels = c(0,1,2,3,4,5),labels = c("no influence","minimal influence","slight influence","moderate influence",'strong influence',"very strong influence"),ordered = TRUE)
df$effectivness_vaccines = factor(df$effectivness_vaccines,levels = c(0,1,2,3,4,5),labels = c("no influence","minimal influence","slight influence","moderate influence",'strong influence',"very strong influence"),ordered = TRUE)
df$personal_health_concerns_influence_willingness_vaccinated = factor(df$personal_health_concerns_influence_willingness_vaccinated,levels = c(0,1,2,3,4,5),labels = c("no influence","minimal influence","slight influence","moderate influence",'strong influence',"very strong influence"),ordered = TRUE)
df$influence_family_friends_on_vaccination = factor(df$influence_family_friends_on_vaccination,levels = c(0,1,2,3,4,5),labels = c("no influence","minimal influence","slight influence","moderate influence",'strong influence',"very strong influence"),ordered = TRUE)
df$vaccination_because_health_family = factor(df$vaccination_because_health_family,levels = c(0,1,2,3,4,5),labels = c("no influence","minimal influence","slight influence","moderate influence",'strong influence',"very strong influence"),ordered = TRUE)
df$trust_governance_vaccination = factor(df$trust_governance_vaccination,levels = c(0,1,2,3,4,5),labels = c("no influence","minimal influence","slight influence","moderate influence",'strong influence',"very strong influence"),ordered = TRUE)
df$trust_government_recommendations_pandemic = factor(df$trust_government_recommendations_pandemic,levels = c(0,1,2,3,4,5),labels = c("no influence","minimal influence","slight influence","moderate influence",'strong influence',"very strong influence"),ordered = TRUE)
df$willingness_use_preventive_measures_masks = factor(df$willingness_use_preventive_measures_masks, levels = c(0,1,2,3,4,5),labels = c("Not at all willing", "strongly unwilling", "unwilling", "Neutral", "willing", "very willing"),ordered = TRUE)
df$willingness_for_treatment_after_positive_test = factor(df$willingness_for_treatment_after_positive_test, levels = c(0,1,2,3,4,5),labels = c("Not at all willing", "strongly unwilling", "unwilling", "Neutral", "willing", "very willing"),ordered = TRUE)
df$willingness_social_distancing = factor(df$willingness_social_distancing, levels = c(0,1,2,3,4,5),labels = c("Not at all willing", "strongly unwilling", "unwilling", "Neutral", "willing", "very willing"),ordered = TRUE)
df$importance_following_guidelines_during_pandemic = factor(df$importance_following_guidelines_during_pandemic,levels = c(0,1,2,3,4,5),labels = c("Not important at all","Not important", "Minimally important", "Moderately important", "Very important", "Essential"),ordered = TRUE)
df$willingness_use_preventive_measures = factor(df$willingness_use_preventive_measures, levels = c(0,1,2,3,4,5),labels = c("Not at all willing", "strongly unwilling", "unwilling", "Neutral", "willing", "very willing"),ordered = TRUE)
df$likelihood_treatment_symptoms_pandemic = factor(df$likelihood_treatment_symptoms_pandemic, levels = c(0,1,2,3,4,5),labels = c("Not at all likely", "strongly unlikely", "unlikely", "Neutral", "likely", "very likely"),ordered = TRUE)
df$Follow_guidelines = factor(df$Follow_guidelines, levels = c(0,1,2,3,4,5),labels = c("Not at all willing", "strongly unwilling", "unwilling", "Neutral", "willing", "very willing"),ordered = TRUE)
df$informed_about_masks_etc = factor(df$informed_about_masks_etc, levels = c(0,1,2,3,4,5),labels = c("Not at all informed", "strongly uninformed", "uninformed", "Neutral", "informed", "very informed"),ordered = TRUE)
df$effectivness_masks_social_distancing = factor(df$effectivness_masks_social_distancing,levels = c(0,1,2,3,4,5),labels = c("no influence","minimal influence","slight influence","moderate influence",'strong influence',"very strong influence"),ordered = TRUE)
df$following_preventive_measures_because_own_concern = factor(df$following_preventive_measures_because_own_concern, levels = c(0,1,2,3,4,5),labels = c("Not at all willing", "strongly unwilling", "unwilling", "Neutral", "willing", "very willing"),ordered = TRUE)
df$inclined_to_follow_health_guidelines_concern_health_family = factor(df$inclined_to_follow_health_guidelines_concern_health_family, levels = c(0,1,2,3,4,5),labels = c("Not at all willing", "strongly unwilling", "unwilling", "Neutral", "willing", "very willing"),ordered = TRUE)
df$outside_pressure_for_preventive_measures = factor(df$outside_pressure_for_preventive_measures,levels = c(0,1,2,3,4,5),labels = c("no influence","minimal influence","slight influence","moderate influence",'strong influence',"very strong influence"),ordered = TRUE)


# ---- Exploratory data ----
table(df$age)  
table(df$gender)  
table(df$annual_income)  
table(df$sexual_orientation)
table(df$marital_status)
table(df$education)
table(df$household_size)
table(df$employment_status)
table(df$vaccinated)
table(df$willingness_use_preventive_measures_masks)
table(df$willingness_social_distancing)
table(df$willingness_use_preventive_measures)

df %>%
  tbl_summary(statistic = list(all_continuous()~"{mean}"))
mean_vaccination_willingness = mean(df$willingness_vaccination, na.rm = TRUE)
mean_preventive_measures = mean(df$willingness_use_preventive_measures, na.rm = TRUE)

#change per plot
freq_table = table(df$marital_status)
data_pie_chart = as.data.frame(freq_table)
colnames(data_pie_chart) = c("Category", "Count")

kuleuven_colors = c(
  "relationship" = "#52BDEC",       
  "Single" = "#00407A",     
  "divorced" = "#A9C6E8"
  #"old"= '#1D8DB0'
)

ggplot(data_pie_chart, aes(x = "", y = Count, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") + 
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5), size = 5) +
  scale_fill_manual(values = kuleuven_colors) +
  theme_void() +
  ggtitle("Pie Chart for relationship status")

# checking the correlation
library(vcdExtra)
table(df_Q2$willingness_use_preventive_measures_masks, df_Q2$willingness_social_distancing)

GoodmanKruskalGamma(tab)
#Since Gamma ranges from -1 to 1, a value of 0.77 means that as willingness for masks increases, willingness for social distancing also tends to increase.

mask_score = as.numeric(df_Q2$willingness_use_preventive_measures_masks)
distancing_score = as.numeric(df_Q2$willingness_social_distancing)

cor.test(mask_score, distancing_score, method = "kendall")

df$vaccinated <- factor(df$vaccinated, levels = c(0, 1), labels = c("No", "Yes"))
freq <- table(df$vaccinated)

barplot(freq,
        main = "COVID-19 vaccination status",
        xlab = "Vaccination Status",
        ylab = "Frequency",
        col = "#52BDEC")



# ---- One variable regression regressions ----


# willingness to vaccinate #


wtv_1= clm(willingness_vaccination~age,data=df)
summary(wtv_1)
wtv_2= clm(willingness_vaccination~gender,data=df)
summary(wtv_2)
wtv_3= clm(willingness_vaccination~education,data=df)
summary(wtv_3)
wtv_4= clm(willingness_vaccination~annual_income,data=df)
summary(wtv_4)
wtv_5= clm(willingness_vaccination~household_size,data=df)
summary(wtv_5)
wtv_6= clm(willingness_vaccination~had_covid,data=df)
summary(wtv_6)
wtv_7= clm(willingness_vaccination~marital_status,data=df)
summary(wtv_7)
wtv_8= clm(willingness_vaccination~employment_status,data=df)
summary(wtv_8) ## significant
exp(coef(wtv_8))
wtv_9= clm(willingness_vaccination~influence_side_effects_on_vaccination,data=df)
summary(wtv_9) ## significant
exp(coef(wtv_9))
wtv_10= clm(willingness_vaccination~effectivness_vaccines,data=df)
summary(wtv_10) ## significant
exp(coef(wtv_10))
table(df$effectivness_vaccines)
#model = glm(willingness_vaccination ~ effectivness_vaccines, data=df, family=binomial)
#plot(model, which=4)  
#par(mar=c(4,4,2,2))  
#plot(model, which=4)
wtv_11= clm(willingness_vaccination~personal_health_concerns_influence_willingness_vaccinated,data=df)
summary(wtv_11) ## significant op 0,1
exp(coef(wtv_11))
wtv_12= clm(willingness_vaccination~influence_family_friends_on_vaccination,data=df)
summary(wtv_12) ## significant aan de cubed, mogelijke verklaring: lage invloed==> weinig effect, hogere invloed => positief versterkend effect, extreme invloed==> negatief afstotend effect
wtv_13= clm(willingness_vaccination~vaccination_because_health_family,data=df)
summary(wtv_13) ## significant
exp(coef(wtv_13))
wtv_14= clm(willingness_vaccination~info_news_media,data=df)
summary(wtv_14)
wtv_15= clm(willingness_vaccination~info_government_websites,data=df)
summary(wtv_15)
wtv_16= clm(willingness_vaccination~info_social_media,data=df)
summary(wtv_16)
wtv_17= clm(willingness_vaccination~info_health_organization,data=df)
summary(wtv_17) ## significant
exp(coef(wtv_17))
wtv_18= clm(willingness_vaccination~info_family_friends,data=df)
summary(wtv_18)
wtv_19= clm(willingness_vaccination~info_educational_institution,data=df)
summary(wtv_19) ## significant
exp(coef(wtv_19))

wtv_final= clm(willingness_vaccination~employment_status+influence_side_effects_on_vaccination+effectivness_vaccines+personal_health_concerns_influence_willingness_vaccinated+info_health_organization+info_educational_institution,data=df)
summary(wtv_final) #problem when including both effectiveness_vaccines and vaccination_because_family
vif(wtv_20)


# willingness preventive measures #

wpm_1= clm(willingness_use_preventive_measures~age,data=df)
summary(wpm_1) ## significant
exp(coef(wpm_1))
wpm_2= clm(willingness_use_preventive_measures~gender,data=df)
summary(wpm_2)
wpm_3= clm(willingness_use_preventive_measures~education,data=df)
summary(wpm_3) ## significant op 0,1
exp(coef(wpm_3))
wpm_4= clm(willingness_use_preventive_measures~annual_income,data=df)
summary(wpm_4)
wpm_5= clm(willingness_use_preventive_measures~household_size,data=df)
summary(wpm_5)
wpm_6= clm(willingness_use_preventive_measures~had_covid,data=df)
summary(wpm_6)
wpm_7= clm(willingness_use_preventive_measures~marital_status,data=df)
summary(wpm_7) ## significant
exp(coef(wpm_7))
wpm_8= clm(willingness_use_preventive_measures~employment_status,data=df)
summary(wpm_8)
wpm_9= clm(willingness_use_preventive_measures~info_news_media,data=df)
summary(wpm_9)
wpm_10= clm(willingness_use_preventive_measures~info_government_websites,data=df)
summary(wpm_10)
wpm_11= clm(willingness_use_preventive_measures~info_social_media,data=df)
summary(wpm_11)
wpm_12= clm(willingness_use_preventive_measures~info_health_organization,data=df)
summary(wpm_12)## significant
exp(coef(wpm_12))
wpm_13= clm(willingness_use_preventive_measures~info_family_friends,data=df)
summary(wpm_13)
wpm_14= clm(willingness_use_preventive_measures~info_educational_institution,data=df)
summary(wpm_14) 

##collapsed because too few observations
df$informed_about_masks_etc = forcats::fct_collapse(
  df$informed_about_masks_etc,
  "Not informed" = c("Not at all informed", "strongly uninformed", "uninformed"),
  "Neutral" = "Neutral",
  "Informed" = c("informed", "very informed")
)
wpm_15= clm(willingness_use_preventive_measures~informed_about_masks_etc,data=df)
summary(wpm_15) 
exp(coef(wpm_15))
wpm_16= clm(willingness_use_preventive_measures~importance_following_guidelines_during_pandemic,data=df)
summary(wpm_16) ##significant
exp(coef(wpm_16))
wpm_17= clm(willingness_use_preventive_measures~ effectivness_masks_social_distancing,data=df)
summary(wpm_17) ##significant
exp(coef(wpm_17))

##collapsed because too few observations
df$following_preventive_measures_because_own_concern = forcats::fct_collapse(
  df$following_preventive_measures_because_own_concern,
  "Unconcerned" = c("Not at all willing", "strongly unwilling", "unwilling"),
  "Neutral" = "Neutral",
  "Concerned" = c("willing", "very willing")
)
wpm_18= clm(willingness_use_preventive_measures~ following_preventive_measures_because_own_concern,data=df)
summary(wpm_18) ##significant
exp(coef(wpm_18))
wpm_19= clm(willingness_use_preventive_measures~ inclined_to_follow_health_guidelines_concern_health_family,data=df)
summary(wpm_19) ## significant
exp(coef(wpm_19))
wpm_20= clm(willingness_use_preventive_measures~outside_pressure_for_preventive_measures,data=df)
summary(wpm_20) #significant op 0.1
exp(coef(wpm_20))


wpm_final=clm(willingness_use_preventive_measures~age+education+marital_status+info_health_organization,data=df)
summary(wpm_final)


# ---- LASSO APPROACH ----

df$education = as.numeric(factor(df$education,levels = c("Low", "Medium", "High"),ordered = TRUE))
df$annual_income = as.numeric(factor(df$annual_income,levels = c("Low", "Middle", "High"),ordered = TRUE))
df$age = as.numeric(factor(df$age,levels = c("young", "young-middle", "middle-old","old"),ordered = TRUE))
df$ethnicity=NULL
df$high_risk=NULL
df$reason_why_not_to_follow_recommendations=NULL
df$reasons_to_follow_recommendations=NULL
df$trustable_information=NULL
df$information_source=NULL

# only for willingness to use preventive measures
 df$willingness_social_distancing=NULL
 df$willingness_use_preventive_measures_masks=NULL

# make dummy vars for categorical variables
df = dummyVars(" ~ .", data = df) %>% predict(df) %>% as.data.frame()
dummy_cols = colnames(df)[sapply(df, function(x) all(x %in% c(0, 1)))]

# Select only numeric but non-dummy variables
non_dummy_cols = setdiff(names(df), dummy_cols)

# Apply standardization only to non-dummy variables
preProc = preProcess(df[, non_dummy_cols], method = c("center", "scale"))

# Transform only those columns
df_scaled = df
df_scaled[, non_dummy_cols] = predict(preProc, df[, non_dummy_cols])

## willingness to use preventive measures: selection of variables
# x = as.matrix(df_scaled[, -which(names(df_scaled) == "willingness_use_preventive_measures")])
# y = df_scaled$willingness_use_preventive_measures

## willingness to get vaccinated: selection of variables
# x = as.matrix(df_scaled[, -which(names(df_scaled) == "willingness_vaccination")])
# y = df_scaled$willingness_vaccination

set.seed(555)
cv.lasso = cv.glmnet(x, y, alpha = 1)  
best_lambda = cv.lasso$lambda.min  
print(best_lambda)

lasso_model = glmnet(x, y, alpha = 1, lambda = best_lambda)
selected_variables = coef(lasso_model)
print(selected_variables)

predictions = predict(lasso_model, s = best_lambda, newx = x)
cor(predictions, y)  # Correlation between predicted & actual values
mean((predictions - y)^2)  # Mean Squared Error (MSE)


# ---- CLM model transforming ----
## going from a 5 point likert to a 3/2 point likert scale

vars_to_recode_3lev = c(
  "willingness_for_treatment_after_positive_test",
  "effectivness_vaccines",
  "vaccination_because_health_family",
  "outside_pressure_for_preventive_measures",
  "trust_governance_vaccination",
  "willingness_vaccination",
  "influence_side_effects_on_vaccination",
  "likelihood_treatment_symptoms_normal",
  "likelihood_treatment_symptoms_pandemic",
  "willingness_use_preventive_measures_masks",
  "willingness_social_distancing",
  "effectivness_masks_social_distancing",
  "personal_health_concerns_influence_willingness_vaccinated",
  "trust_government_recommendations_pandemic",
  "personal_health_concerns_influence_willingness_vaccinated",
  "influence_family_friends_on_vaccination",
  "influence_trust_governance_preventive_measures"
  
)
df = df %>%
  mutate(across(all_of(vars_to_recode_3lev), ~ case_when(
    . %in% c(0, 1) ~ 1,
    . %in% c(2, 3) ~ 2,
    . %in% c(4, 5) ~ 3
  )))

vars_to_recode_2lev = c(
  "importance_following_guidelines_during_pandemic",
  "informed_about_masks_etc",
  "following_preventive_measures_because_own_concern",
  "inclined_to_follow_health_guidelines_concern_health_family",
  "Follow_guidelines",
  "willingness_use_preventive_measures"
)
df = df %>%
  mutate(across(all_of(vars_to_recode_2lev), ~ case_when(
    . %in% c(0, 1,2,3) ~ 1,
    . %in% c( 4,5) ~ 2,
  )))

# ---- MODEL_LASSO : WILLINGNESS TO VACCINATE ####


df$willingness_vaccination = factor(df$willingness_vaccination,levels = c(1,2,3),labels = c("Not willing", "Neutral", "Willing"),ordered = TRUE)
df$influence_side_effects_on_vaccination = factor(df$influence_side_effects_on_vaccination,levels = c(1,2,3),labels = c("No influence", "Neutral", "Influence"),ordered = TRUE)
df$employment_status=factor(df$employment_status,levels=c("employed","retired","student"))
df$willingness_for_treatment_after_positive_test= factor(df$willingness_for_treatment_after_positive_test,levels = c(1,2,3),labels = c("Not willing", "Neutral", "Willing"),ordered = TRUE)
df$effectivness_vaccines= factor(df$effectivness_vaccines,levels = c(1,2,3),labels = c("Not effective", "Neutral", "Effective"),ordered = TRUE)
df$outside_pressure_for_preventive_measures= factor(df$outside_pressure_for_preventive_measures,levels = c(1,2,3),labels = c("No influence", "Neutral", "Influence"),ordered = TRUE)
df$trust_governance_vaccination= factor(df$trust_governance_vaccination,levels = c(1,2,3),labels = c("Not trust", "Neutral", "Trust"),ordered = TRUE)
df$vaccination_because_health_family= factor(df$vaccination_because_health_family,levels = c(1,2,3),labels = c("No influence", "Neutral", "Influence"),ordered = TRUE)

model_vac= clm(willingness_vaccination~ employment_status + influence_side_effects_on_vaccination + willingness_for_treatment_after_positive_test + effectivness_vaccines + vaccination_because_health_family + outside_pressure_for_preventive_measures +trust_governance_vaccination ,data=df)
summary(model_vac)
cor(model_vac$fitted.values,as.numeric(df$willingness_vaccination))


predicted_class <- predict(model_vac, newdata = df, type = "class")$fit
table(predicted_class)
table(Predicted = predicted_class, Actual = df$willingness_vaccination)

accuracy = (16+10+60)/115


# ---- TABLE MODEL : WILLINGNESS TO VACCINATE #####
# 
# # Extract and format model results
# tidy_results = broom::tidy(model_vac)
# 
# # Compute Odds Ratios (OR) and Confidence Intervals (CIs)
# tidy_results = tidy_results %>%
#   mutate(OR = exp(estimate),  
#          OR_lower = exp(estimate - 1.96 * std.error),  
#          OR_upper = exp(estimate + 1.96 * std.error))  
# 
# # Remove threshold coefficients
# tidy_results = tidy_results %>%
#   filter(!grepl("\\|", term)) %>%  # Exclude "Not willing|Neutral" and "Neutral|Willing"
#   #filter(grepl("\\.L", term)) %>%  # Include only linear terms
#   select(term, estimate, OR, OR_lower, OR_upper, std.error, p.value) %>%
#   rename(Estimate = estimate,
#          `Odds Ratio` = OR,
#          `Lower CI` = OR_lower,
#          `Upper CI` = OR_upper,
#          `Std. Error` = std.error,
#          `P-value` = p.value)
# # Round the relevant columns to 3 decimal places
# tidy_results = tidy_results %>%
#   mutate(
#     Estimate = round(Estimate, 3),
#     `Odds Ratio` = round(`Odds Ratio`, 3),
#     `Lower CI` = round(`Lower CI`, 3),
#     `Upper CI` = round(`Upper CI`, 3),
#     `Std. Error` = round(`Std. Error`, 3),
#     `P-value` = round(`P-value`, 3) 
#   )
# 
# 
# # Format table with flextable
# ft = flextable(tidy_results) %>%
#   theme_booktabs() %>%
#   autofit()
# 
# # Create a new Word document and add the table
# doc = read_docx() %>%
#   body_add_flextable(ft) %>%
#   body_add_par("Table: Ordinal Logistic Regression: Willingness to vaccinate", style = "heading 2")
# 
# # Save the document
# print(doc, target = "Willingness_to_vaccinate.docx")
# 
# ---- MODEL_LASSO : WILLINGNESS TO USE PREVENTIVE MEASURES -----

df$willingness_use_preventive_measures = factor(df$willingness_use_preventive_measures,levels = c(1,2),labels = c("Not willing", "Willing"),ordered = TRUE)
df$willingness_for_treatment_after_positive_test= factor(df$willingness_for_treatment_after_positive_test,levels = c(1,2,3),labels = c("Not willing", "Neutral", "Willing"),ordered = TRUE)
df$likelihood_treatment_symptoms_pandemic= factor(df$likelihood_treatment_symptoms_pandemic,levels = c(1,2,3),labels = c("Weak", "Neutral", "Strong"),ordered = TRUE)
df$importance_following_guidelines_during_pandemic= factor(df$importance_following_guidelines_during_pandemic,levels = c(1,2),labels = c("Weak", "Strong"),ordered = TRUE)
df$informed_about_masks_etc= factor(df$informed_about_masks_etc,levels = c(1,2),labels = c("Weak", "Strong"),ordered = TRUE)
df$following_preventive_measures_because_own_concern= factor(df$following_preventive_measures_because_own_concern,levels = c(1,2),labels = c("Weak", "Strong"),ordered = TRUE)
df$inclined_to_follow_health_guidelines_concern_health_family= factor(df$inclined_to_follow_health_guidelines_concern_health_family,levels = c(1,2),labels = c("Weak", "Strong"),ordered = TRUE)

model_prev= clm(willingness_use_preventive_measures~ willingness_for_treatment_after_positive_test + importance_following_guidelines_during_pandemic +informed_about_masks_etc+ following_preventive_measures_because_own_concern+ inclined_to_follow_health_guidelines_concern_health_family + likelihood_treatment_symptoms_pandemic ,data=df)
summary(model_prev)
predicted_class <- predict(model_prev, newdata = df, type = "class")$fit
table(predicted_class)
table(Predicted = predicted_class, Actual = df$willingness_use_preventive_measures)

accuracy = (14+90)/115


# ---- TABLE MODEL : WILLINGNESS TO USE PREVENTIVE MEASURES ----
# # Extract and format model results
# tidy_results = broom::tidy(model_prev)
# 
# # Compute Odds Ratios (OR) and Confidence Intervals (CIs)
# tidy_results = tidy_results %>%
#   mutate(OR = exp(estimate),
#          OR_lower = exp(estimate - 1.96 * std.error),
#          OR_upper = exp(estimate + 1.96 * std.error))
# 
# # Remove threshold coefficients
# tidy_results = tidy_results %>%
#   filter(!grepl("\\|", term)) %>%  # Exclude "Not willing|Neutral" and "Neutral|Willing"
#   #filter(grepl("\\.L", term)) %>%  # Include only linear terms
#   select(term, estimate, OR, OR_lower, OR_upper, std.error, p.value) %>%
#   rename(Estimate = estimate,
#          `Odds Ratio` = OR,
#          `Lower CI` = OR_lower,
#          `Upper CI` = OR_upper,
#          `Std. Error` = std.error,
#          `P-value` = p.value)
# # Round the relevant columns to 3 decimal places
# tidy_results = tidy_results %>%
#   mutate(
#     Estimate = round(Estimate, 3),
#     `Odds Ratio` = round(`Odds Ratio`, 3),
#     `Lower CI` = round(`Lower CI`, 3),
#     `Upper CI` = round(`Upper CI`, 3),
#     `Std. Error` = round(`Std. Error`, 3),
#     `P-value` = round(`P-value`, 3)
#   )
# 
# 
# # Format table with flextable
# ft = flextable(tidy_results) %>%
#   theme_booktabs() %>%
#   autofit()
# 
# # Create a new Word document and add the table
# doc = read_docx() %>%
#   body_add_flextable(ft) %>%
#   body_add_par("Table: Ordinal Logistic Regression: Willingness to use preventive measures", style = "heading 2")
# 
# # Save the document
# print(doc, target = "Willingness_to_use_preventive_measures.docx")

# ---- Literature-driven selection + CLM model ----
vars_to_recode_3lev = c(
  "willingness_vaccination",
  "influence_side_effects_on_vaccination",
  "trust_governance_vaccination"
)
df = df %>%
  mutate(across(all_of(vars_to_recode_3lev), ~ case_when(
    . %in% c(0, 1) ~ 1,
    . %in% c(2, 3) ~ 2,
    . %in% c(4, 5) ~ 3
  )))

vars_to_recode_2lev = c(
  "willingness_use_preventive_measures",
  "trust_government_recommendations_pandemic"
)
df = df %>%
  mutate(across(all_of(vars_to_recode_2lev), ~ case_when(
    . %in% c(0, 1,2,3) ~ 1,
    . %in% c( 4,5) ~ 2,
  )))

df$annual_income=factor(df$annual_income,levels = c("Low","Middle","High"),ordered = TRUE)
df$education=factor(df$education,levels = c("Low","Medium","High"),ordered = TRUE)
df$age=factor(df$age,levels = c("young","young-middle","middle-old","old"),ordered = TRUE)
df$gender=factor(df$gender,levels = c("male","female")) #female reference
df$employment_status=factor(df$employment_status,levels=c("employed","retired","student"))
df$willingness_use_preventive_measures = factor(df$willingness_use_preventive_measures, levels = c(1,2),labels = c("Weak","Strong"),ordered = TRUE)
df$willingness_vaccination = factor(df$willingness_vaccination, levels = c(1,2,3),labels = c("Weak","Neutral","Strong"),ordered = TRUE)
df$influence_side_effects_on_vaccination = factor(df$influence_side_effects_on_vaccination,levels = c(1,2,3),labels = c("Weak","Neutral","Strong"),ordered = TRUE)
df$trust_governance_vaccination = factor(df$trust_governance_vaccination,levels = c(1,2,3),labels = c("Weak","Neutral","Strong"),ordered = TRUE)
df$trust_government_recommendations_pandemic = factor(df$trust_government_recommendations_pandemic,levels = c(1,2),labels = c("Weak","Strong"),ordered = TRUE)


wtv_model= clm(willingness_vaccination~age + gender + annual_income + trust_governance_vaccination + education + had_covid + influence_side_effects_on_vaccination + employment_status ,data=df)
summary(wtv_model)

predicted_class <- predict(wtv_model, newdata = df, type = "class")$fit
table(predicted_class)
table(Predicted = predicted_class, Actual = df$willingness_vaccination)
accuracy = (11+6+61)/115



wpm_model= clm(willingness_use_preventive_measures~age + gender + annual_income + trust_government_recommendations_pandemic + education + had_covid + employment_status,data=df)
summary(wpm_model)
predicted_class <- predict(wpm_model, newdata = df, type = "class")$fit
table(predicted_class)
table(Predicted = predicted_class, Actual = df$willingness_use_preventive_measures)
accuracy = (22+41)/115

### do with numerical

test=subset(df,employment_status=='student')
mean(test$willingness_vaccination) ##4.35 high willingness

kruskal.test(willingness_vaccination ~ employment_status, data = df)
ggplot(df, aes(x = employment_status, y = willingness_vaccination,fill = employment_status)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "employed" = "#52BDEC",
    "student" = "#00407A",
    "retired" = "#A9C6E8"
  )) +
  labs(title = "Willingness to vaccinate per employment status",
       x = "Employment Status", y = "Willingness to Vaccinate (0-5)")
+ theme_minimal()

# ---- Information sources ----
model_sources = clm(willingness_vaccination ~ info_news_media + info_government_websites + info_social_media + info_health_organization + info_family_friends + info_educational_institution+ info_employer  , data = df, link = "logit")
summary(model_sources)
predicted_class <- predict(model_sources, newdata = df, type = "class")$fit
table(predicted_class)
table(Predicted = predicted_class, Actual = df$willingness_vaccination)

accuracy_vac = (5+0+32+6)/115


model_sources =  clm(willingness_use_preventive_measures ~ info_news_media + info_government_websites + info_social_media + info_health_organization + info_family_friends + info_educational_institution + info_employer  , data = df, link = "logit")
summary(model_sources)
table(df$info_social_media,df$willingness_social_distancing) 

predicted_class <- predict(model_sources, newdata = df, type = "class")$fit
table(predicted_class)
table(Predicted = predicted_class, Actual = df$willingness_use_preventive_measures)
accuracy_prev = (45+17)/115


# ---- Table sources ----
# Extract and format model results
tidy_results <- broom::tidy(model_Q4)

# Compute Odds Ratios (OR) and Confidence Intervals (CIs)
tidy_results <- tidy_results %>%
  mutate(OR = exp(estimate),
         OR_lower = exp(estimate - 1.96 * std.error),
         OR_upper = exp(estimate + 1.96 * std.error))

# Remove threshold coefficients
tidy_results <- tidy_results %>%
  filter(!grepl("\\|", term)) %>%  # Exclude "Not willing|Neutral" and "Neutral|Willing"
  #filter(grepl("\\.L", term)) %>%  # Include only linear terms
  select(term, estimate, OR, OR_lower, OR_upper, std.error, p.value) %>%
  rename(Estimate = estimate,
         `Odds Ratio` = OR,
         `Lower CI` = OR_lower,
         `Upper CI` = OR_upper,
         `Std. Error` = std.error,
         `P-value` = p.value)
# Round the relevant columns to 3 decimal places
tidy_results <- tidy_results %>%
  mutate(
    Estimate = round(Estimate, 3),
    `Odds Ratio` = round(`Odds Ratio`, 3),
    `Lower CI` = round(`Lower CI`, 3),
    `Upper CI` = round(`Upper CI`, 3),
    `Std. Error` = round(`Std. Error`, 3),
    `P-value` = round(`P-value`, 3)
  )


# Format table with flextable
ft <- flextable(tidy_results) %>%
  theme_booktabs() %>%
  autofit()

# Create a new Word document and add the table
doc <- read_docx() %>%
  body_add_flextable(ft) %>%
  body_add_par("Table: Ordinal Logistic Regression: Willingness to use preventive measures", style = "heading 2")

# Save the document
print(doc, target = "Willingness_to_use_preventive_measures_info_sources2.docx")

# ---- Tables + explanatory graphs ----
# 
# tidy_results = broom::tidy(wpm_model)
# 
# # Compute Odds Ratios (OR) and Confidence Intervals (CIs)
# tidy_results = tidy_results %>%
#   mutate(OR = exp(estimate),
#          OR_lower = exp(estimate - 1.96 * std.error),
#          OR_upper = exp(estimate + 1.96 * std.error))
# 
# # Remove threshold coefficients
# tidy_results = tidy_results %>%
#   filter(!grepl("\\|", term)) %>%  # Exclude "Not willing|Neutral" and "Neutral|Willing"
#   #filter(grepl("\\.L", term)) %>%  # Include only linear terms
#   select(term, estimate, OR, OR_lower, OR_upper, std.error, p.value) %>%
#   rename(Estimate = estimate,
#          `Odds Ratio` = OR,
#          `Lower CI` = OR_lower,
#          `Upper CI` = OR_upper,
#          `Std. Error` = std.error,
#          `P-value` = p.value)
# # Round the relevant columns to 3 decimal places
# tidy_results = tidy_results %>%
#   mutate(
#     Estimate = round(Estimate, 3),
#     `Odds Ratio` = round(`Odds Ratio`, 3),
#     `Lower CI` = round(`Lower CI`, 3),
#     `Upper CI` = round(`Upper CI`, 3),
#     `Std. Error` = round(`Std. Error`, 3),
#     `P-value` = round(`P-value`, 3)
#   )
# 
# 
# # Format table with flextable
# ft = flextable(tidy_results) %>%
#   theme_booktabs() %>%
#   autofit()
# 
# # Create a new Word document and add the table
# doc = read_docx() %>%
#   body_add_flextable(ft) %>%
#   body_add_par("Table: Ordinal Logistic Regression: Willingness to use preventive measures", style = "heading 2")
# 
# # Save the document
# print(doc, target = "Willingness_to_use_preventive_measures_1.docx")
# 
# ## graph annual income :
# df$predicted_probs = predict(wpm_model, type = "class")
# income_plot_data = df %>%
#   group_by(annual_income) %>%
#   summarise(mean_willingness = mean(as.numeric(willingness_use_preventive_measures)))
# 
# ggplot(income_plot_data, aes(x = as.numeric(annual_income), y = mean_willingness)) +
#   geom_point() +
#   geom_smooth(method = "loess", se = TRUE, color = "#52BDEC") +
#   labs(
#     title = "Annual income vs Willingness to use preventive measures",
#     x = "Annual Income (ordinal scale)",
#     y = "Average Willingness to Use Preventive Measures"
#   ) + theme_minimal()
# 
# ## graph trust:
# # Gemiddelde willingness per niveau van vertrouwen
# trust_plot_data = df %>%
#   group_by(trust_government_recommendations_pandemic) %>%
#   summarise(mean_willingness = mean(as.numeric(willingness_use_preventive_measures)))
# 
# # Plot
# ggplot(trust_plot_data, aes(x = as.numeric(trust_government_recommendations_pandemic), y = mean_willingness)) +
#   geom_point(size = 3) +
#   geom_smooth(method = "loess", se = TRUE, color = "#52BDEC") +
#   labs(
#     title = "Relationship between trust in government and willingness to use preventive measures",
#     x = "Trust in Government Recommendations (ordinal scale)",
#     y = "Average Willingness to Use Preventive Measures"
#   )  + theme_minimal() 






