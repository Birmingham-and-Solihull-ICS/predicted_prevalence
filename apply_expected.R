library(tidyverse)
library(DBI)
con <- dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "MLCSU-BI-SQL", 
                 Database = "EAT_Reporting_BSOL", Trusted_Connection = "True")


sql1 <- 
  "select GPPracticeCode_Current,
  GPPracticeName_Current,
  case when age like '95%' then '75+'
  when cast(age as int) < 35 then '16-34'
  when cast(age as int)  < 45 then '35-44'
  when cast(age as int)  < 55 then '45-54'
  when cast(age as int)  < 65 then '55-64'
  when cast(age as int)  < 75 then '65-74'
  else '75+' end as Age_grp,
  sum(Total_Patients) as patients,
  sum(Total_MalePatients) as male_patients,
  sum(Total_FemalePatients) as female_patients
  from [Reference].[vwPop_GPPractice_Patient_SYOA_Pivotted]
  where (Age = '95+' or Age > 15 )
  and Effective_SnapShot_Date in 
  (select MAX(Effective_SnapShot_Date) from [Reference].[vwPop_GPPractice_Patient_SYOA_Pivotted])
  group by GPPracticeCode_Current,
  GPPracticeName_Current,
  case when age like '95%' then '75+'
  when cast(age as int) < 35 then '16-34'
  when cast(age as int)  < 45 then '35-44'
  when cast(age as int)  < 55 then '45-54'
  when cast(age as int)  < 65 then '55-64'
  when cast(age as int)  < 75 then '65-74'
  else '75+' end"

list_size <- 
  dbGetQuery(con, sql1)


#### HYPERTENSION
# Age / sex estimates manually extracted, Table 13, from:
# https://digital.nhs.uk/data-and-information/publications/statistical/health-survey-for-england/2022-part-2#data-sets

hse_estimate <- read_csv("./data/hse_estimate.csv"
                         , col_types = "ccnnnnnn"
                         , na = "0")

# PIvot to long to join
hse_estimate <-
  hse_estimate |> 
  filter(Group == 'All with hypertension') |> 
  select(-Group) |> 
  pivot_longer(-Sex, values_to = "exp_perc", values_transform = function(x) x/100,
               names_transform = as.character, names_to = "Age_grp") |> 
  filter(Sex != "Adults") |> 
  mutate(Sex = ifelse(Sex == "Men", "male", "female"))


# JOin together and calcuate predicted numbers by aplying percentage to list groups
practice_age_sex_figures <-
  list_size |> 
  pivot_longer(cols = c(patients, male_patients, female_patients)
               , names_to = "Sex", values_to = "Patients") |> 
  filter(Sex != "patients") |> 
  mutate(Sex = ifelse(Sex == "male_patients", "male", "female")) |> 
  inner_join(hse_estimate) |> 
  mutate(expected_patients = Patients * exp_perc)

#Sum up groups to practice level
practice_figures <-
  practice_age_sex_figures |> 
  group_by(GPPracticeCode_Current, GPPracticeName_Current) |> 
  summarise(List_size = sum(Patients),
         expected_patients = sum(expected_patients)) |> 
  filter(GPPracticeCode_Current != 'Y01057') # Exclude Homeless Health Exchange, as no QOF

# Check the list size sum is the same faster join
list_size |> 
  group_by(GPPracticeCode_Current, GPPracticeName_Current) |> 
  summarise(sum(patients))


# Get recent reported prevalence
sql2 <-
  " SELECT  
        T1.FinancialYear
        ,T2.GPPracticeCode_Original
		,T2.GPPracticeCode_Current
        ,T2.PCN
		,T2.Locality as Locality_Reg
		,T1.DiseaseRegisterSize
		,T1.PracticeListsize
	
   FROM [AnalystGlobal].[Performance].[QOFIndicatorsAndPrevalence] T1
  INNER JOIN EAT_Reporting_BSOL.Reference.BSOL_ICS_PracticeMapped T2
     ON T1.PracticeCode = T2.GPPracticeCode_Original
  WHERE T2.ICS_2223 = 'BSOL'
    AND IndicatorCode = 'HYP001'
    AND DiseaseRegisterSize IS NOT NULL
	AND FinancialYear = '2023-24'"

reported_prev <- 
  dbGetQuery(con, sql2)


# Temp table to plot
tmp_figs <- 
  practice_figures |> 
  left_join(reported_prev, by = c("GPPracticeCode_Current" = "GPPracticeCode_Original"))


library(FunnelPlotR)

hyp_funnel <-
  funnel_plot(tmp_figs, DiseaseRegisterSize, expected_patients, GPPracticeName_Current
              , data_type = "RC", draw_adjusted = TRUE
              , draw_unadjusted = FALSE
              , title = "Ratio of QOF prevelence to predicted prevalence of hypertension"
              , x_label = "Expected Prevalence"
              , y_label = "QOF prevelence / Expected Prevalence"
            )

hyp_funnel$plot +
  geom_hline(col = "red", yintercept=0.666666666) +
  annotate("text", x = 8500, y = 0.7, label = "National average", colour="red") +
  labs(subtitle = "Expected prevaelence based on age and sex-adjusted national data")

tmp_figs$expected_patients
tmp_figs$DiseaseRegisterSize
