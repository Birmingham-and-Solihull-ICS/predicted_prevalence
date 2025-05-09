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
  sum(Total_MalePatients) as males_patients,
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
  pivot_longer(-Sex, values_to = "patients", names_transform = as.character, names_to = "Age_grp")



practice_figures <-
  list_size |> 
  select(-)
  pivot_longer()
  inner_join(hse_estimate)

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
