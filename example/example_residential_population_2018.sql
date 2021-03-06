/**************************************************************************************************
Title: 2018 Residential population
Author: Simon Anastasiadis

Inputs & Dependencies:
- [IDI_Clean].[data].[snz_res_pop]
Outputs:
- [IDI_UserCode].[DL-MAA2020-01].[defn_2018_residents]

Description:
List of snz_uid values for those identities that are part of the
Estimated Residential Population (ERP) of New Zealand in 2018.

Intended purpose:
Producing summary statistics for the entire population.

Notes:

Parameters & Present values:
  Current refresh = 20191020
  Prefix = defn_
  Project schema = [DL-MAA2020-01]
   
Issues:
 
History (reverse order):
2020-03-03 SA v1
**************************************************************************************************/

/* Establish database for writing views */
USE IDI_UserCode
GO

IF OBJECT_ID('[DL-MAA2020-01].[defn_2018_residents]','V') IS NOT NULL
DROP VIEW [DL-MAA2020-01].[defn_2018_residents];
GO

CREATE VIEW [DL-MAA2020-01].[defn_2018_residents] AS
SELECT [snz_uid]
      ,[srp_ref_date]
FROM [IDI_Clean_20191020].[data].[snz_res_pop]
WHERE YEAR(srp_ref_date) = 2018;
GO
