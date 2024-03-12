library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)

setwd('~/Documents/DATA/Data 332/files given/Patient Data')
#reading 
df_visits  <- read_excel('~/Documents/DATA/Data 332/files given/Patient Data/Visit.xlsx',.name_repair = 'universal')
df_billing  <- read_excel('~/Documents/DATA/Data 332/files given/Patient Data/Billing.xlsx',.name_repair = 'universal')
df_patient  <- read_excel('~/Documents/DATA/Data 332/files given/Patient Data/Patient.xlsx',.name_repair = 'universal')

#Left Joining data

df_patient_visit_information <- df_patient %>%
  left_join(df_visits, by = "PatientID") %>%
  left_join(df_billing, by = "VisitID")

df_patient_visit_information <- df_patient_visit_information %>%
  mutate(
    Reason = case_when(
      Reason == 'UTI follow-up' ~ 'UTI',
      Reason == 'Rhinitis follow-up' ~ 'Rhinitis',
      Reason == 'Migraine follow-up' ~ 'Migraine',
      grepl('Laceration', Reason) ~ 'Laceration',
      Reason == 'Hypotension monitoring' ~ 'Hypotension',
      Reason == 'Hypertension monitoring' ~ 'Hypertension monitoring',
      Reason == 'Dermatitis follow-up' ~ 'Dermatitis',
      Reason == 'Cyst removal follow-up' ~ 'Cyst removal',
      Reason == 'Bronchitis follow-up' ~ 'Bronchitis',
      Reason == 'Allergic reaction follow-up' ~ 'Allergic reaction',
      TRUE ~ Reason
    )
  )
# 1st visit by month of year chart 
df$VisitDate <- as.Date(df$VisitDate)
monthly_patients <- df %>%
  group_by(PatientID, month = format(VisitDate, "%B")) %>%
  summarise(count = n())
ggplot(monthly_patients, aes(x = month, y = count, fill = month)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of visits per month", x = "Month", y = "Number of Patients") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#2nd Reason for visit based on walk in or not. 
reason_by_walkin <- df %>%
  group_by(WalkIn, Reason) %>%
  summarise(count = n())
ggplot(reason_by_walkin, aes(x = Reason, y = count, fill = WalkIn)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7, color = "white") +
  labs(title = "Reasons for Visit by Walk-In Status",
       x = "Reason for Visit",
       y = "Number of Visits",
       fill = "Walk-In Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top") +
  scale_fill_discrete(name = "Walk-In Status")

#3 reasons for locations by cities

df_patient_visit_information %>%
  group_by(Reason, City) %>%
  summarise(count_by_city = n()) %>%
  ggplot(aes(x = City, y = count_by_city, fill = Reason)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Reasons for Hospital Visits by City",
       x = "City",
       y = "Number of Visits") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#4 Total invoice amount based on reason for visit. Segmented (stacked bar chart) with it was paid. 
total_invoice_summary <- df %>%
  group_by(Reason, InvoicePaid) %>%
  summarise(total_invoice = sum(InvoiceAmt, na.rm = TRUE))
ggplot(total_invoice_summary, aes(x = Reason, y = total_invoice, fill = InvoicePaid)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Invoice Amount by Reason for Visit and Payment Status",
       x = "Reason for Visit",
       y = "Total Invoice Amount",
       fill = "Payment Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#5 Analysis for reasons that that patients commnly vist the hospitals for 
common_condition <- df %>%
  group_by(month = format(VisitDate, "%B"), Reason) %>%
  summarise(TotalVisits = n())
ggplot(common_condition, aes(x = month, fill = Reason)) +
  geom_bar(stat = "count") +
  labs(title = "Common reasons for visit",
       x = "Month",
       y = "Total Visit Amount",
       fill = "Reason for Visit") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

