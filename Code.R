
##=============================================================================
##
## This code was written in  RStudio 2022.07.1+554 "Spotted Wakerobin"
## release. Functions and package compatibility may be subject to
## change.
##
##=============================================================================

## Libraries
pkgs <- c(
  "foreign","readr","lavaan","semPlot","psych","apaTables","memisc",
  "car","readxl","knitr","e1071","effsize","ltm","mice", "ggsci",
  "naniar","finalfit","writexl","semTools","MASS","simsem","simr",
  "ordinal","dplyr","tidyr","Hmisc","weights","ggplot2", "viridis", "lme4",
  "lmtest", "nnet", "clinfun", "rcompanion"
)
for (p in pkgs) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

df  <- read_excel("Data_1.xlsx", col_names = TRUE)
df2 <- read_excel("Data_2.xlsx", 
                  col_names = TRUE)

##=============================================================================
##
## Descriptive statistics & plots
##
##=============================================================================

table(df$Gender, df$InTime)
table(df$Gender, df$Dropout)
prop.table(table(df$Gender,  df$InTime),  margin = 1)
prop.table(table(df$Gender,  df$Dropout), margin = 1)
aggregate(df$Alder_opptak ~ Gender, data = df, FUN = mean, na.rm = TRUE)

## Bar plot: completion rate by gender
ggplot(df, aes(x = Gender, fill = InTime)) +
  geom_bar(position = "fill") +
  labs(
    title = "",
    x = "Sex",
    y = "Rate",
    fill = "Completed within the nominal timeframe"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(labels = c("F" = "Female", "M" = "Male")) +
  scale_fill_manual(
    values = c("J" = "#4C72B0", "N" = "#999999"), 
    labels = c("J" = "Yes", "N" = "No")
  ) +
  theme_minimal()

## Bar plot, for Dropout by gender (395)
ggplot(df, aes(x = Gender, fill = Dropout)) +
  geom_bar(position = "fill") +
  labs(
    ##title = "Title",
    x = "Sex",
    y = "Rate",
    fill = "Completed the program"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(labels = c("F" = "Female", "M" = "Male")) +
  scale_fill_manual(
    ##values = c("J" = "#4C72B0", "N" = "#999999"), 
    values = c("J" = "#999999", "N" = "#4C72B0"), 
    labels = c("J" = "No", "N" = "Yes")
  ) +
  theme_minimal()

## Completion and dropout rates over time, by gender
df <- df %>%
  mutate(
    InTime_bin  = ifelse(InTime  == "J", 1, 0),
    Dropout_bin = ifelse(Dropout == "J", 1, 0)
  )

## Summarize by year and gender
rates_by_year_gender <- df %>%
  group_by(AdmissionYear, Gender) %>%
  summarise(
    n = n(),
    InTime_rate  = mean(InTime_bin,  na.rm = TRUE),
    Dropout_rate = mean(Dropout_bin, na.rm = TRUE)
  ) %>%
  ungroup()

## Linear models
m_age <- lm(df2$Average_age ~ df2$AdmissionYear)
## Normality of residuals
shapiro.test(residuals(m_age))
## p > .88, great -> Null hypothesis is "The residuals are normally 
## distributed", we don't have grounds to dismiss this. 

## Homoscedasticity
bptest(m_age)
## The residual variance is constant (homoscedasticity holds), no evidence
## of heteroscedasticity

## Independence
durbinWatsonTest(m_age)
## Durbin-Watson test (independence test): Residuals are independent 
## (no autocorrelation), p = .45 -> fail to reject H0, No evidence of 
## autocorrelation. 
summary(m_age) 
confint(m_age)

## Plot - Alder vs kull
ggplot(df2, aes(x = AdmissionYear, y = Average_age)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "cyan3") +
  labs(
    x = "Cohort",
    y = "Average age"
  ) +
  theme_minimal()
df$AdmissionYear <- factor(df$AdmissionYear, ordered = TRUE)
jonckheere.test(df$Alder_opptak, df$AdmissionYear) ## All good

## Plot - Semester over normert tid vs kull
ggplot(df2, aes(x = df2$AdmissionYear, y = df2$`Semester_over_(average)`)) +
  geom_line(size = 1) +
  labs(
    title = "",
    x = "Cohort",
    y = "Semesters beyond the nominal timeframe"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 4))

## Plot - Fullforing paa normert tid vs kull
ggplot(df2, aes(x = df2$AdmissionYear, y = df2$Completion_rate)) +
  geom_line(size = 1) +
  labs(
    title = "",
    x = "Cohort",
    y = "Rate"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal()

## Noen OLS-modeller
m_intime <- lm(df2$Completion_rate ~ df2$AdmissionYear)
summary(m_intime)
confint(m_intime)
m_over <- lm(df2$`Semester_over_(average)` ~ df2$AdmissionYear)
summary(m_over)
confint(m_over)

##=============================================================================
##
## Proportion of students finishing in time
##
##=============================================================================

## Clear cache and re-load the datasets before proceeding, some wonky shit
## happens if not (weird loop)
table(df$InTime, df$Gender)
prop.table(table(df$InTime, df$Gender), margin = 2)

df$InTime_bin <- ifelse(df$InTime == "J", 1, 0)
df$Gender_bin <- ifelse(df$Gender == "F", 1, 0)  

chisq.test(table(df$InTime, df$Gender))
cramerV(table(df$InTime, df$Gender))

chisq.test(table(df$Semester_over, df$Gender))
cramerV(table(df$Semester_over, df$Gender))

df$Outcome <- with(df, ifelse(Dropout == "J", "Dropout",
                              ifelse(InTime == "J", "InTime", "Late")))
df$Outcome <- factor(df$Outcome, levels = c("InTime", "Late", "Dropout"))
model4 <- multinom(Outcome ~ Gender_bin + Alder_opptak + AdmissionYear, 
                   data = df)
levels(df$Outcome)
summary(model4)

exp(c(-0.2841, 0.3509))
## [1] 0.7525 (Late vs InTime) - Non-significant
## [2] 1.4202 (Dropout vs Gender_bin)
## INTERPRETATION: Females - 42% higher chance of dropping out!

## p-values
## Get coefficients and standard errors
coefs <- summary(model4)$coefficients
ses   <- summary(model4)$standard.errors
## Compute z and p-values
zvals <- coefs / ses
pvals <- 2 * (1 - pnorm(abs(zvals)))
round(pvals, 4)
## Holds after adjusting for age at admission and year of admission! Very, very
## important finding, p = 0.0004.

##=============================================================================
##
## END
##
##=============================================================================