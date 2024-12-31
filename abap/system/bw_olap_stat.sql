SET SCHEMA "SAPBW3";

with X as (
SELECT 
   substring(to_char(starttime),9,2)  as hrsGMT
 , substring(to_char(starttime),11,2) as minGMT
 , substring(to_char(starttime),13,2) as secGMT
 , v1.*
 , v2.db_time, v2.sel_cnt, v2.trans_cnt, v2.olapi_time, v2.olapt_time FROM
(SELECT t1.sessionuid, t1.stepuid, t1.handleid, t1.calday, t1.starttime, t1.uname, max(objname) as obj_name, round(SUM(t1.evtime),1) as total_time
  FROM RSDDSTAT_OLAP t1,
 (SELECT sessionuid, stepuid, handleid
    FROM RSDDSTAT_OLAP 
   WHERE 1 = 1
--   AND eventid = '000003500'
     AND steptp <> 'JAVA'
     AND handletp = 'OLAP'
     AND objname = 'DSDVCP27_Q001'
     AND calday = '20210511'
 --    AND uname = 'EX_SCHRAND' -- 
--     AND uname = 'EX_SHABLYS'
   GROUP BY sessionuid, stepuid, handleid) v1
 WHERE t1.sessionuid = v1.sessionuid AND t1.stepuid = v1.stepuid AND t1.handleid = v1.handleid
 GROUP BY t1.sessionuid, t1.stepuid, t1.handleid, t1.calday, t1.starttime, t1.uname /*HAVING SUM(t1.evtime) > 10*/) v1,
(SELECT sessionuid, stepuid, handleid, SUM(evcount_sel) as sel_cnt, SUM(evcount_trans) as trans_cnt, round(SUM(db_time),1) as db_time
       , round(SUM(olapi_time),1) as olapi_time, round(SUm(olapt_time),1) as olapt_time
  FROM (
	SELECT t1.sessionuid, t1.stepuid, t1.handleid, 0 as evcount_sel, evcount as evcount_trans, 0 as db_time, 0 as olapi_time, 0 as olapt_time
	  FROM RSDDSTAT_OLAP t1
	 WHERE eventid = '000009010'
--	   AND steptp <> 'JAVA'
	   AND handletp = 'OLAP'	 
	UNION ALL
	SELECT t1.sessionuid, t1.stepuid, t1.handleid, evcount as evcount_sel, 0 as evcount_trans, 0 as db_time, 0 as olapi_time, 0 as olapt_time
	  FROM RSDDSTAT_OLAP t1
	 WHERE eventid = '000009011'
--	   AND steptp <> 'JAVA'
	   AND handletp = 'OLAP'	 
    UNION ALL
   SELECT t1.sessionuid, t1.stepuid, t1.handleid, 0 as evcount_sel, 0 as evcount_trans, evtime as db_time, 0 as olapi_time, 0 as olapt_time
	 FROM RSDDSTAT_OLAP t1
	WHERE eventid = '000009000'
--	   AND steptp <> 'JAVA'
	   AND handletp = 'OLAP'
    UNION ALL
   SELECT t1.sessionuid, t1.stepuid, t1.handleid, 0 as evcount_sel, 0 as evcount_trans, 0 as db_time, evtime as olapi_time, 0 as olapt_time
	 FROM RSDDSTAT_OLAP t1
	WHERE eventid = '000003500'
--	   AND steptp <> 'JAVA'
	   AND handletp = 'OLAP'
    UNION ALL
   SELECT t1.sessionuid, t1.stepuid, t1.handleid, 0 as evcount_sel, 0 as evcount_trans, 0 as db_time, 0 as olapi_time, evtime as olapt_time
	 FROM RSDDSTAT_OLAP t1
	WHERE eventid = '000003200'
--	   AND steptp <> 'JAVA'
	   AND handletp = 'OLAP'	   )	   
GROUP BY  sessionuid, stepuid, handleid) v2
WHERE v1.sessionuid = v2.sessionuid AND v1.stepuid = v2.stepuid AND v1.handleid = v2.handleid)
 SELECT * FROM x ORDER BY starttime DESC
 

SELECT * FROM RSDDSTAT_OLAP 
 WHERE stepuid = '00O2TMLZNZJNNKAKFO8BYEDIS'
   AND handleid = 0004
  ORDER BY eventid;
  
/*
3200
During a data transfer to the front end, exception aggregations and simple currency translations are carried out, 
formulas are calculated, and the correct number of decimal places for the data cells is determined. 
In addition, the result set is sorted according to the interface settings. The number of cells that have been read is in the counter.  

3500
This event measures the time taken to start the query, read the query definition, execute the variable exists, and replace the variables.