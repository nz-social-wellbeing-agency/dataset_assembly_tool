/**************************************************************************************************
Title: Employment infered from EMS
Author: Simon Anastasiadis

Inputs & Dependencies:
- [IDI_Clean].[ir_clean].[ird_ems]
Outputs:
- [IDI_UserCode].[DL-MAA2020-01].[defn_employed]

Description:
A spell where wages or salaries are reported to IRD as evidence of employment.

Intended purpose:
Creating indicators of when/whether a person was employed.
Counting number of employers.
Total income from Wages and Salaries.
 
Notes:
1) Self employment does not appear in this definition.
2) Not suited to counting days in employment as people working multiple jobs will
   count the same day twice.
3) A placeholder identity exists where the encrypted IRD number [snz_ird_uid] is equal to
   zero. Checking across refreshes suggests this is people without an IRD number. We exclude
   this identity.

Parameters & Present values:
  Current refresh = 20191020
  Prefix = defn_
  Project schema = [DL-MAA2020-01]
 
Issues:
 
History (reverse order):
2020-05-12 SA v1
**************************************************************************************************/

/* Set database for writing views */
USE IDI_UserCode
GO

/* Clear existing view */
IF OBJECT_ID('[DL-MAA2020-01].[defn_employed]','V') IS NOT NULL
DROP VIEW [DL-MAA2020-01].[defn_employed];
GO

CREATE VIEW [DL-MAA2020-01].[defn_employed] AS
SELECT snz_uid
	,DATEFROMPARTS(YEAR([ir_ems_return_period_date]), MONTH([ir_ems_return_period_date]), 1) AS period_start_date
	,[ir_ems_return_period_date]
	,ir_ems_gross_earnings_amt
	,snz_employer_ird_uid
FROM [IDI_Clean_20191020].[ir_clean].[ird_ems]
WHERE [ir_ems_income_source_code]= 'W&S' -- income from wages and salaries
AND [snz_ird_uid] <> 0; -- exclude placeholder person without IRD number
GO
