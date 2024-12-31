SET SCHEMA "SAPBWP";
 
--Трансформации
SELECT TRANID, SOURCENAME, TARGETNAME
FROM "RSTRAN"
WHERE TSTPNM = 'DDIC'
  AND OBJVERS = 'A'
  AND OBJSTAT = 'INA'
  AND TO_DATE(TIMESTMP) >= ADD_DAYS(CURRENT_DATE ,-1);
   
--Композитные провайдеры
SELECT HCPRNM
FROM "RSOHCPR"
WHERE TSTPNM = 'DDIC'
  AND OBJVERS = 'A'
  AND OBJSTAT = 'INA'
  AND TO_DATE(TIMESTMP) >= ADD_DAYS(CURRENT_DATE ,-1);
  
--ADSO
SELECT main.ADSONM FROM "RSOADSO" AS main
JOIN "RSOADSOLOC" AS dop
ON main.ADSONM = dop.ADSONM
WHERE main.OBJVERS = 'A'
  AND main.TSTPNM = 'DDIC'
  AND TO_DATE(main.TIMESTMP) >= ADD_DAYS(CURRENT_DATE ,-1)
  AND dop.OBJSTAT = 'INA';
  
--Инфообъекты
SELECT IOBJNM
FROM "RSDIOBJ"
WHERE TSTPNM = 'DDIC'
  AND OBJVERS = 'A'
  AND OBJSTAT = 'INA'
  AND TSTPNM = 'DDIC'
  AND TO_DATE(TIMESTMP) >= ADD_DAYS(CURRENT_DATE ,-1);
В случае обнаружения разактивированных объектов, активировать их с помощью программ:

RSDG_TRFN_ACTIVATE - для трансформаций;
RSDG_HCPR_ACTIVATE - для композитных провайдеров;
RSDG_ADSO_ACTIVATE - для ADSO;
RSDG_IOBJ_ACTIVATE - для инфо-объектов.
========================================
