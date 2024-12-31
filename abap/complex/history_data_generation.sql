-- combinations of
--  3 plants from 2 different branches 2002, 2005, 2015
--  3 banknotes (1,50,100) and 2 coins (50H, 1SAR)
--  several business partners
--  move_type - ALL
--  mblnr - any
--  DCINDIC (S,H)

select 'Selected Person' As "Statement", floor(20*Rand()+1) from dummy;

SELECT ROUND(TO_DECIMAL(1 + (999-1)*RAND()),0) FROM DUMMY

(SELECT ROUND(TO_DECIMAL(1 + (999999-1)*RAND()),0) FROM DUMMY)

(SELECT 'IBT' as move_type from dummy union all
SELECT 'MMC' from dummy union all
SELECT 'PRO' from dummy union all
SELECT 'DEP' from dummy union all
SELECT 'WIT' from dummy union all
--SELECT 'CTF' from dummy union all
SELECT 'CTT' from dummy union all
SELECT 'EXI' from dummy union all
SELECT 'MTO' from dummy union all
SELECT 'EXO' from dummy union all
SELECT 'PRO' from dummy union all
SELECT 'DSN' from dummy union all
SELECT 'DSF' from dummy)


SELECT plant, "/BIC/ZCBRANCH", "/BIC/ZCVAULT" 
  from "SAPBWP"."/BI0/PPLANT"
 where objvers = 'A'
   AND "/BIC/ZCBRANCH" <> ''
   AND plant in ('2002', '2005', '2015')
  ORDER BY 2
  
SELECT bpartner, bp_type 
  from "SAPBWP"."/BI0/PBPARTNER" 
 WHERE objvers = 'A'
   AND bp_type IN ('Z005','Z007','Z001','Z002','Z003')
   AND bpartner IN ('7100000121','7200000115','0000005041','8200000303','8200000303')
  ORDER BY 2 DESC
  
SELECT matl_group, "/BIC/ZCMATNO","/BIC/ZCDENOM" ,"/BIC/ZCMGRDEN","/BIC/ZCSTATUS",
    map(matl_group,'C001',"/BIC/ZCDENOM",'C002',"/BIC/ZCDENOM" / 100 ) as denom
  FROM "SAPBWP"."/BIC/PZCMATNO"
 WHERE objvers = 'A'
   AND "/BIC/ZCDENOM" IN (1,50,100)
   AND matl_group IN ('C001','C002')
   AND "/BIC/ZCMATNO" IN ('C06010001F','C06010001U','C05010001F','B04010000F','B04010001N','B05000101F','B06000101U')
  ORDER BY 1, "/BIC/ZCMGRDEN", "/BIC/ZCSTATUS"


SELECT *, cppvlc / quant_b as denom
  FROM "MIS_SAMA"."IT_CYM_MTRLMOVEMENT" 
 WHERE "ZCMATNO" IN ('C05010001F')

SELECT date0 FROM "SAPBWP"."/BI0/SDATE"
 WHERE date0 in ('20160301','20160302','20160303')
-- **********************************************************

SELECT * FROM "MIS_SAMA"."IT_CYM_MTRLMOVEMENT" 

DELETE FROM "MIS_SAMA"."IT_CYM_MTRLMOVEMENT" 
 WHERE calday IN ('20160301','20160302','20160303');
 
 
SELECT * FROM "MIS_SAMA"."IT_CYM_MTRLMOVEMENT" 
 WHERE calday IN ('20160301','20160302','20160303')
  AND prnt_cost <> 0;

UPDATE  "MIS_SAMA"."IT_CYM_MTRLMOVEMENT"  SET  prnt_currcy = 'SAR'
  
COMMIT;

INSERT INTO "MIS_SAMA"."IT_CYM_MTRLMOVEMENT" 
SELECT plant, zcmatno, bpartner, bp_type, calday, movetype, mblnr
, CASE WHEN movetype = 'CTT' THEN
     map(plant,'2002','2005','2005','2015','2015','2002','2002')
   ELSE    ''
  END as move_plant
 , ebeln 
 , map(movetype,'WIT','H','EXO','H','MTO','H','DSN','H','DSF','H','S') as shkzg 
 , denom*quant_b as cppvlc, loc_currcy, quant_b, base_uom, quant_b as curr_grqty
 , po_unit
 , ROUND(TO_DECIMAL(1 + (999-1)*RAND()),0) as prnt_cost
 , 'SAR' as prnt_currcy
  FROM (
SELECT plant, "/BIC/ZCMATNO" as zcmatno, bpartner, bp_type, calday, movetype,
       to_char(400000000+ROUND(TO_DECIMAL(1 + (999999-1)*RAND()),0)) as MBLNR, '' as move_plant, '' as ebeln
       , 'S' as shkzg, 0 as cppvlc, 'SAR' as loc_currcy, v1.denom, ROUND(TO_DECIMAL(1 + (999-1)*RAND()),0) as quant_b, 'ST' as base_uom,
       0 as curr_grqty, 'ST' as po_unit
  FROM 
(SELECT "/BIC/ZCMATNO",  map(matl_group,'C001',"/BIC/ZCDENOM",'C002',"/BIC/ZCDENOM" / 100 ) as denom FROM "SAPBWP"."/BIC/PZCMATNO"
   WHERE "/BIC/ZCMATNO" IN ('C06010001F','C06010001U','C05010001F','B04010000F','B04010001N','B05000101F','B06000101U')
     AND objvers = 'A') v1,
(SELECT plant FROM "SAPBWP"."/BI0/SPLANT"
  WHERE plant in ('2002', '2005', '2015')) v2,
(SELECT date0 as calday FROM "SAPBWP"."/BI0/SDATE"
 WHERE date0 in ('20160301','20160302','20160303')) v3,
   (SELECT 'IBT' as movetype from dummy union all
	SELECT 'MMC' from dummy union all
	SELECT 'PRO' from dummy union all
	SELECT 'DEP' from dummy union all
	SELECT 'WIT' from dummy union all
--SELECT 'CTF' from dummy union all
	SELECT 'CTT' from dummy union all
	SELECT 'EXI' from dummy union all
	SELECT 'EXO' from dummy union all
    SELECT 'MTO' from dummy union all	
	SELECT 'PRO' from dummy union all
	SELECT 'DSN' from dummy union all
    SELECT 'ICC' from dummy union all	
	SELECT 'CFS' from dummy union all
	SELECT 'CCS' from dummy union all	
	SELECT 'DSF' from dummy) v4,
 (SELECT bpartner, bp_type from "SAPBWP"."/BI0/PBPARTNER" 
 WHERE objvers = 'A'
--   AND bp_type IN ('Z005','Z007','Z001')
   AND bpartner IN ('7100000121','7200000115','0000005041','8200000303','8200000303')) v5 );

INSERT INTO "MIS_SAMA"."IT_CYM_MTRLMOVEMENT"
SELECT 
  move_plant as plant
 ,zcmatno, bpartner, bp_type
 ,calday, 'CTF', mblnr, plant as move_plant, ebeln, 'H' as shkzg
 ,cppvlc, loc_currcy, quant_b, base_uom, curr_grqty, po_unit, 0,  prnt_currcy
FROM "MIS_SAMA"."IT_CYM_MTRLMOVEMENT" 
 WHERE movetype = 'CTT' AND calday IN ('20160301','20160302','20160303');
 
INSERT INTO "MIS_SAMA"."IT_CYM_MTRLMOVEMENT"
SELECT 
  plant
 ,zcmatno, bpartner, bp_type
 ,calday, movetype, mblnr, move_plant, ebeln, 'H' as shkzg
 ,cppvlc, loc_currcy, quant_b, base_uom, curr_grqty, po_unit,  0,  prnt_currcy
FROM "MIS_SAMA"."IT_CYM_MTRLMOVEMENT" 
 WHERE movetype = 'MMC' AND calday IN ('20160301','20160302','20160303') ;

UPDATE "MIS_SAMA"."IT_CYM_MTRLMOVEMENT" SET bpartner = '', bp_type = ''
 WHERE movetype IN ('MMC','CTF','CTT','DSN','DSF') AND calday IN ('20160301','20160302','20160303');

SELECT * FROM "MIS_SAMA"."IT_CYM_MTRLMOVEMENT"
 WHERE movetype IN ('CTT','CTF') AND calday IN ('20160301','20160302','20160303')
 ORDER BY mblnr;
 
 -- **************** STOCK ********************

(SELECT 'E0001' as STATUS from dummy union all
SELECT 'VALUE' as STATUS from dummy union all
SELECT 'PHEX' as STATUS from dummy )

UPDATE "MIS_SAMA"."IT_CYM_MTRLSTOCK" SET base_uom = 'ST'

SELECT * FROM "MIS_SAMA"."IT_CYM_MTRLSTOCK"
 where calday IN ('20160301','20160302','20160303')
 order by 3



DELETE FROM "MIS_SAMA"."IT_CYM_MTRLSTOCK"
-- WHERE status NOT IN ('BOX','LOOSE','STOCK')
 WHERE calday IN ('20160301','20160302','20160303')

INSERT INTO "MIS_SAMA"."IT_CYM_MTRLSTOCK"
SELECT calday, plant, zcmatno, status,  map(status,'BOX',zcmatverp,'') as zcmatverp, 
 '' as zcexidv,quant_b, base_uom, loc_currcy,
 denom*quant_b as deb_cre_lc
FROM (
SELECT  calday, plant, "/BIC/ZCMATNO" as zcmatno, denom, v4.status, 'VNHU1' as zcmatverp,
  100*ROUND(TO_DECIMAL(1 + (999-1)*RAND()),0) as quant_b,
 'ST' as base_uom, 
 'SAR' as loc_currcy
FROM
(SELECT "/BIC/ZCMATNO", map(matl_group,'C001',"/BIC/ZCDENOM",'C002',"/BIC/ZCDENOM" / 100 ) as denom FROM "SAPBWP"."/BIC/PZCMATNO"
   WHERE "/BIC/ZCMATNO" IN ('C06010001F','C06010001U','C05010001F','B04010000F','B04010001N','B05000101F','B06000101U')
     AND objvers = 'A') v1,
(SELECT plant FROM "SAPBWP"."/BI0/SPLANT"
 WHERE plant in ('2002', '2005', '2015')) v2,
(SELECT date0 as calday FROM "SAPBWP"."/BI0/SDATE"
 WHERE date0 in ('20160301','20160302','20160303')) v3,
(SELECT 'LOOSE' as STATUS from dummy union all
SELECT 'STOCK' as STATUS from dummy union all
SELECT 'BOX' as STATUS from dummy ) v4 )

ORDER BY calday, plant, zcmatno 

=====================

SELECT * FROM "MIS_SAMA"."IT_CYM_MTRLCIRC"
 WHERE calday IN ('20160301','20160302','20160303');

SELECT * FROM "MIS_SAMA"."IT_CYM_MTRLCIRC"
 WHERE calday IN ('20000101');

DELETE FROM "MIS_SAMA"."IT_CYM_MTRLCIRC"
 WHERE calday in ('20000101', '20160301','20160302','20160303')
 
insert into "MIS_SAMA"."IT_CYM_MTRLCIRC" 
SELECT date0 as calday, v1."/BIC/ZCMATNO",  ROUND(TO_DECIMAL(1 + (999-1)*RAND()),0)*denom as deb_cre_lc, 'SAR' as loc_currcy
 FROM
(SELECT matl_group, "/BIC/ZCMATNO","/BIC/ZCDENOM" ,"/BIC/ZCMGRDEN","/BIC/ZCSTATUS",
    map(matl_group,'C001',"/BIC/ZCDENOM",'C002',"/BIC/ZCDENOM" / 100 ) as denom
  FROM "SAPBWP"."/BIC/PZCMATNO"
 WHERE objvers = 'A'
   AND "/BIC/ZCDENOM" IN (1,50,100)
   AND matl_group IN ('C001','C002')
   AND "/BIC/ZCMATNO" IN ('C06010001F','C06010001U','C05010001F','B04010000F','B04010001N','B05000101F','B06000101U')
  ORDER BY 1, "/BIC/ZCMGRDEN", "/BIC/ZCSTATUS") v1,
(SELECT date0 FROM "SAPBWP"."/BI0/SDATE"
 WHERE date0 in ('20180101', '20160301','20160302','20160303') ) v2
   
===========================

===========================

insert into "MIS_SAMA"."IT_CYM_INVESTMENTS" 
SELECT date0 as calday
 , -ROUND(TO_DECIMAL(1 + (99999999-1)*RAND()),0) as inv_gold
 , -ROUND(TO_DECIMAL(1 + (99999999-1)*RAND()),0) as inv_secur
 , -ROUND(TO_DECIMAL(1 + (99999999-1)*RAND()),0) as inv_portf
 , 'SAR' as loc_currcy
 FROM  
(SELECT date0 FROM "SAPBWP"."/BI0/SDATE"
 WHERE date0 in ('20160301','20160302','20160303') ) v2
 
 
SELECT * FROM "MIS_SAMA"."IT_CYM_INVESTMENTS"
 WHERE   calday in ('20160301','20160302','20160303')
 
INSERT INTO "MIS_SAMA"."IT_CYM_INVESTMENT_ACC" 
SELECT calday, '0101010001' as account, inv_gold as amount, 'SAR' as currency, 'G' as invcov_type 
  FROM "MIS_SAMA"."IT_CYM_INVESTMENTS"
 WHERE calday in ('20160301','20160302','20160303')
UNION ALL
 SELECT calday, '0000013001' as account, inv_secur as amount, 'SAR' as currency, 'S' as invcov_type 
  FROM "MIS_SAMA"."IT_CYM_INVESTMENTS"
 WHERE calday in ('20160301','20160302','20160303')
UNION ALL
 SELECT calday, '0000014001' as account, inv_portf as amount, 'SAR' as currency, 'P' as invcov_type 
  FROM "MIS_SAMA"."IT_CYM_INVESTMENTS"
 WHERE calday in ('20160301','20160302','20160303') 
 
SELECT * FROM "MIS_SAMA"."IT_CYM_INVESTMENT_ACC" 