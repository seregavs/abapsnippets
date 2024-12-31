SET SCHEMA "SAPBWP";


-- MiniCheck все процессы цепочек, стартовавших за последние 24h
SELECT
   -SECONDS_BETWEEN(c1.end_ts, c1.st_ts) as seconds
  ,c1.*
 FROM (
	SELECT t2.chain_id
	     , t2.analyzed_status, t5.analyzed_status_text
	     , utctolocal(to_timestamp(t2.datum || t2.zeit,'YYYYMMDDHHMISS')) as pc_stts
	     , t1.log_id
--     , variante
	     , type
--	     , t1.batchdate, t1.batchtime
	     , t1.state, t4.state_text
	     , t1.actual_state, t3.actual_state_text
	     , t1.variante || '__' || t1.type as variant_ext
	 --    , starttimestamp, endtimestamp 
	     , case when t1.starttimestamp = 0 then to_timestamp('19000101000001','YYYYMMDDHHMISS')
	            else utctolocal(to_timestamp(substr(t1.starttimestamp,1,14)  ,'YYYYMMDDHHMISS')) 
	            end as st_ts
	     , case when ( t1.endtimestamp = 0 ) and ( (t1.actual_state = 'A') OR (t3.actual_state_text = 'Не определено' ) ) then now()    
	            when ( t1.endtimestamp = 0 ) and (t1.state <> 'A' ) then to_timestamp('19000101000001','YYYYMMDDHHMISS')
	            when ( t1.endtimestamp = 0 ) and (t1.state = 'A' ) then now()
	            else utctolocal(to_timestamp(substr(t1.endtimestamp,1,14)  ,'YYYYMMDDHHMISS'))
	            end as end_ts
	  FROM rspcprocesslog t1, rspclogchain t2
	    , (SELECT domvalue_l as actual_state, ddtext as actual_state_text  from dd07T WHERE domname = 'RSPC_STATE' AND ddlanguage = 'R') t3
	    , (SELECT domvalue_l as state, ddtext as state_text  from dd07T WHERE domname = 'RSPC_STATE' AND ddlanguage = 'R') t4
	    , (SELECT domvalue_l as analyzed_status, ddtext as analyzed_status_text  from dd07T WHERE domname = 'RSPC_STATE' AND ddlanguage = 'R') t5
	 WHERE 1 = 1
	   AND t1.log_id = t2.log_id
	  -- AND t2.chain_id in ('COMPRESS_ADSO','Z_SALES_FCST_ZPM_LOG','ZSKIF_EXPORT')
--	   AND t1.batchdate = '20240706'
	   AND t1.actual_state = t3.actual_state
	   AND t1.state = t4.state
	   AND t2.analyzed_status = t5.analyzed_status
	   AND t1.type NOT IN ('AND','OR','EXOR', 'PLSWITCHP', 'PLSWITCHL', 'TRIGGER','INTERRUPT','DECISION','CHAIN')
	     ) c1
  WHERE  1 = 1
    AND state not in ('S')
-- AND add_Seconds(now(), -24*60*60 ) >= c1.pc_stts -- старше последних 24h
    AND add_Seconds(now(), -24*60*60 ) < c1.pc_stts -- за последние 24h
    AND chain_id NOT IN ('Z_CHEK_PUSH')
--    AND c1.analyzed_status <> 'G'
 ORDER BY c1.pc_stts desc, c1.chain_id, variant_ext, c1.pc_stts,  c1.log_id;
 
 -- MiniCheck все активные цепочки, стартовавшие за последние 48h
SELECT
   -SECONDS_BETWEEN(c1.end_ts, c1.st_ts) as seconds
  ,c1.*
  ,c2.dtp_name
 FROM (
	SELECT t2.chain_id
	     , t2.analyzed_status
--	     , t5.analyzed_status_text
	     , utctolocal(to_timestamp(t2.datum || t2.zeit,'YYYYMMDDHHMISS')) as pc_stts
	     , t1.log_id
--     , variante
	     , type
--	     , t1.batchdate, t1.batchtime
	     , t1.state
	     , t4.state_text
--	     , t1.actual_state
	     , t3.actual_state_text
	     , t1.variante
	     , t1.variante || '__' || t1.type as variant_ext
	 --    , starttimestamp, endtimestamp 
	     , case when t1.starttimestamp = 0 then to_timestamp('19000101000001','YYYYMMDDHHMISS')
	            else utctolocal(to_timestamp(substr(t1.starttimestamp,1,14)  ,'YYYYMMDDHHMISS')) 
	            end as st_ts
	     , case when ( t1.endtimestamp = 0 ) and ( (t1.actual_state = 'A') OR (t3.actual_state_text = 'Не определено' ) ) then now()    
	            when ( t1.endtimestamp = 0 ) and (t1.state <> 'A' ) then to_timestamp('19000101000001','YYYYMMDDHHMISS')
	            when ( t1.endtimestamp = 0 ) and (t1.state = 'A' ) then now()	            
	            else utctolocal(to_timestamp(substr(t1.endtimestamp,1,14)  ,'YYYYMMDDHHMISS'))
	            end as end_ts
	  FROM rspcprocesslog t1, rspclogchain t2
	    , (SELECT domvalue_l as actual_state, ddtext as actual_state_text  from dd07T WHERE domname = 'RSPC_STATE' AND ddlanguage = 'R') t3
	    , (SELECT domvalue_l as state, ddtext as state_text  from dd07T WHERE domname = 'RSPC_STATE' AND ddlanguage = 'R') t4
	    , (SELECT domvalue_l as analyzed_status, ddtext as analyzed_status_text  from dd07T WHERE domname = 'RSPC_STATE' AND ddlanguage = 'R') t5
	 WHERE 1 = 1
	   AND t1.log_id = t2.log_id
	  -- AND t2.chain_id in ('COMPRESS_ADSO','Z_SALES_FCST_ZPM_LOG','ZSKIF_EXPORT')
--	   AND t1.batchdate = '20240706'
	   AND t1.actual_state = t3.actual_state
	   AND t1.state = t4.state
	   AND t2.analyzed_status = t5.analyzed_status
	   AND t1.type NOT IN ('AND','OR','EXOR', 'PLSWITCHP', 'PLSWITCHL', 'TRIGGER','INTERRUPT','DECISION','CHAIN')
	     ) c1 LEFT OUTER JOIN (SELECT dtp, '(' || updmode || ') ' || replace(src,' ','.') || ' -> ' || tgt as dtp_name  from rsbkdtp where objvers = 'A') as c2 ON c1.variante = c2.dtp
  WHERE  1 = 1
-- AND add_Seconds(now(), -24*60*60 ) >= c1.pc_stts -- старше последних 24h
    AND add_Seconds(now(), -24*60*60*2 ) < c1.pc_stts -- за последние 48h
    AND chain_id NOT IN ('Z_CHEK_PUSH')
    AND c1.analyzed_status = 'A'
 --   AND chain_id = 'ZFIXX_MD_PC'
    AND state = 'A'
 ORDER BY c1.pc_stts asc, c1.chain_id, variant_ext, c1.pc_stts,  c1.log_id;
 
  -- MiniCheck все ошибочные цепочки, стартовавшие за последние 48h
SELECT
   -SECONDS_BETWEEN(c1.end_ts, c1.st_ts) as seconds
  ,c1.*
  ,c2.dtp_name
 FROM (
	SELECT t2.chain_id
	     , t2.analyzed_status, t5.analyzed_status_text
	     , utctolocal(to_timestamp(t2.datum || t2.zeit,'YYYYMMDDHHMISS')) as pc_stts
	     , t1.log_id
--     , variante
	     , type
--	     , t1.batchdate, t1.batchtime
	     , t1.state, t4.state_text
	     , t1.actual_state, t3.actual_state_text
	     , t1.variante
	     , t1.variante || '__' || t1.type as variant_ext
	 --    , starttimestamp, endtimestamp 
	     , case when t1.starttimestamp = 0 then to_timestamp('19000101000001','YYYYMMDDHHMISS')
	            else utctolocal(to_timestamp(substr(t1.starttimestamp,1,14)  ,'YYYYMMDDHHMISS')) 
	            end as st_ts
	     , case when ( t1.endtimestamp = 0 ) and ( (t1.actual_state = 'A') OR (t3.actual_state_text = 'Не определено' ) ) then now()    
	            when ( t1.endtimestamp = 0 ) and (t1.state <> 'A' ) then to_timestamp('19000101000001','YYYYMMDDHHMISS')
	            when ( t1.endtimestamp = 0 ) and (t1.state = 'A' ) then now()	            
	            else utctolocal(to_timestamp(substr(t1.endtimestamp,1,14)  ,'YYYYMMDDHHMISS'))
	            end as end_ts
	  FROM rspcprocesslog t1, rspclogchain t2
	    , (SELECT domvalue_l as actual_state, ddtext as actual_state_text  from dd07T WHERE domname = 'RSPC_STATE' AND ddlanguage = 'R') t3
	    , (SELECT domvalue_l as state, ddtext as state_text  from dd07T WHERE domname = 'RSPC_STATE' AND ddlanguage = 'R') t4
	    , (SELECT domvalue_l as analyzed_status, ddtext as analyzed_status_text  from dd07T WHERE domname = 'RSPC_STATE' AND ddlanguage = 'R') t5
	 WHERE 1 = 1
	   AND t1.log_id = t2.log_id
	  -- AND t2.chain_id in ('COMPRESS_ADSO','Z_SALES_FCST_ZPM_LOG','ZSKIF_EXPORT')
--	   AND t1.batchdate = '20240706'
	   AND t1.actual_state = t3.actual_state
	   AND t1.state = t4.state
	   AND t2.analyzed_status = t5.analyzed_status
	   AND t1.type NOT IN ('AND','OR','EXOR', 'PLSWITCHP', 'PLSWITCHL', 'TRIGGER','INTERRUPT','DECISION','CHAIN')
	     ) c1 LEFT OUTER JOIN (SELECT dtp, '(' || updmode || ') ' || replace(src,' ','.') || ' -> ' || tgt as dtp_name  from rsbkdtp where objvers = 'A') as c2 ON c1.variante = c2.dtp
  WHERE  1 = 1
-- AND add_Seconds(now(), -24*60*60 ) >= c1.pc_stts -- старше последних 24h
    AND add_Seconds(now(), -24*60*60*2 ) < c1.pc_stts -- за последние 48h
    AND chain_id NOT IN ('Z_CHEK_PUSH')
 --   AND (( c1.analyzed_status = 'R' ))
 --   AND chain_id = 'ZFIXX_MD_PC'
   AND ( state = 'R' )
 ORDER BY c1.pc_stts desc, c1.chain_id, variant_ext;
 
-- статистика по завершенным процессам
SELECT variant_ext, count(*) as cnt, min(secs) as min_secs, max(secs) as max_secs
     , round(avg(secs),0) as avg_secs, median(secs) as med_secs
     , round(stddev(secs),2) as stdev_secs
  FROM (
SELECT
   -SECONDS_BETWEEN(c1.end_ts, c1.st_ts) as secs
  , c1.variant_ext
 FROM (
	SELECT t2.chain_id
	     , t2.analyzed_status, t5.analyzed_status_text
	     , utctolocal(to_timestamp(t2.datum || t2.zeit,'YYYYMMDDHHMISS')) as pc_stts
	     , t1.log_id
	     , type
	     , t1.state, t4.state_text
	     , t1.actual_state, t3.actual_state_text
	     , t1.variante || '__' || t1.type as variant_ext
	     , case when t1.starttimestamp = 0 then to_timestamp('19000101000001','YYYYMMDDHHMISS')
	            else utctolocal(to_timestamp(substr(t1.starttimestamp,1,14)  ,'YYYYMMDDHHMISS')) 
	            end as st_ts
	     , case when ( t1.endtimestamp = 0 ) and ( (t1.actual_state = 'A') OR (t3.actual_state_text = 'Не определено' ) ) then now()    
	            when ( t1.endtimestamp = 0 ) and (t1.state <> 'A' ) then to_timestamp('19000101000001','YYYYMMDDHHMISS')
	            when ( t1.endtimestamp = 0 ) and (t1.state = 'A' ) then now()
	            else utctolocal(to_timestamp(substr(t1.endtimestamp,1,14)  ,'YYYYMMDDHHMISS'))
	            end as end_ts
	  FROM rspcprocesslog t1, rspclogchain t2
	    , (SELECT domvalue_l as actual_state, ddtext as actual_state_text  from dd07T WHERE domname = 'RSPC_STATE' AND ddlanguage = 'R') t3
	    , (SELECT domvalue_l as state, ddtext as state_text  from dd07T WHERE domname = 'RSPC_STATE' AND ddlanguage = 'R') t4
	    , (SELECT domvalue_l as analyzed_status, ddtext as analyzed_status_text  from dd07T WHERE domname = 'RSPC_STATE' AND ddlanguage = 'R') t5
	 WHERE 1 = 1
	   AND t1.log_id = t2.log_id
	   AND t1.actual_state = t3.actual_state
	   AND t1.state = t4.state
	   AND t2.analyzed_status = t5.analyzed_status
	   AND t1.type NOT IN ('AND','OR','EXOR', 'PLSWITCHP', 'PLSWITCHL', 'TRIGGER','INTERRUPT','DECISION','CHAIN')
	     ) c1
  WHERE  1 = 1
    AND state in ('F','G')
    AND chain_id NOT IN ('Z_CHEK_PUSH'))
 group by variant_ext
    ORDER BY 1;

-- сравнение со статистикой

WITH stat as (
SELECT variant_ext, count(*) as cnt, min(secs) as min_secs, max(secs) as max_secs
     , round(avg(secs),0) as avg_secs, median(secs) as med_secs
 --    , min(pc_stts) as min_pc_stts
 --    , round(stddev(secs),2) as stdev_secs
  FROM (
SELECT
   -SECONDS_BETWEEN(c1.end_ts, c1.st_ts) as secs
  , c1.variant_ext
 FROM (
	SELECT t2.chain_id
	     , t2.analyzed_status, t5.analyzed_status_text
	     , utctolocal(to_timestamp(t2.datum || t2.zeit,'YYYYMMDDHHMISS')) as pc_stts
	     , t1.log_id
	     , type
	     , t1.state, t4.state_text
	     , t1.actual_state, t3.actual_state_text
	     , t1.variante || '__' || t1.type as variant_ext
	     , case when t1.starttimestamp = 0 then to_timestamp('19000101000001','YYYYMMDDHHMISS')
	            else utctolocal(to_timestamp(substr(t1.starttimestamp,1,14)  ,'YYYYMMDDHHMISS')) 
	            end as st_ts
	     , case when ( t1.endtimestamp = 0 ) and ( (t1.actual_state = 'A') OR (t3.actual_state_text = 'Не определено' ) ) then now()    
	            when ( t1.endtimestamp = 0 ) and (t1.state <> 'A' ) then to_timestamp('19000101000001','YYYYMMDDHHMISS')
	            when ( t1.endtimestamp = 0 ) and (t1.state = 'A' ) then now()
	            else utctolocal(to_timestamp(substr(t1.endtimestamp,1,14)  ,'YYYYMMDDHHMISS'))
	            end as end_ts
	  FROM rspcprocesslog t1, rspclogchain t2
	    , (SELECT domvalue_l as actual_state, ddtext as actual_state_text  from dd07T WHERE domname = 'RSPC_STATE' AND ddlanguage = 'R') t3
	    , (SELECT domvalue_l as state, ddtext as state_text  from dd07T WHERE domname = 'RSPC_STATE' AND ddlanguage = 'R') t4
	    , (SELECT domvalue_l as analyzed_status, ddtext as analyzed_status_text  from dd07T WHERE domname = 'RSPC_STATE' AND ddlanguage = 'R') t5
	 WHERE 1 = 1
	   AND t1.log_id = t2.log_id
	   AND t1.actual_state = t3.actual_state
	   AND t1.state = t4.state
	   AND t2.analyzed_status = t5.analyzed_status
	   AND t1.type NOT IN ('AND','OR','EXOR', 'PLSWITCHP', 'PLSWITCHL', 'TRIGGER','INTERRUPT','DECISION','CHAIN')
	     ) c1
  WHERE  1 = 1
    AND state in ('F','G')
    AND chain_id NOT IN ('Z_CHEK_PUSH'))
 group by variant_ext
), processes as (
SELECT
   -SECONDS_BETWEEN(c1.end_ts, c1.st_ts) as seconds
  ,c1.*
 FROM (
	SELECT t2.chain_id
	     , t2.analyzed_status, t5.analyzed_status_text
	     , utctolocal(to_timestamp(t2.datum || t2.zeit,'YYYYMMDDHHMISS')) as pc_stts
	     , t1.log_id
	     , type
	     , t1.state, t4.state_text
	     , t1.actual_state, t3.actual_state_text
	     , t1.variante || '__' || t1.type as variant_ext
	     , case when t1.starttimestamp = 0 then to_timestamp('19000101000001','YYYYMMDDHHMISS')
	            else utctolocal(to_timestamp(substr(t1.starttimestamp,1,14)  ,'YYYYMMDDHHMISS')) 
	            end as st_ts
	     , case when ( t1.endtimestamp = 0 ) and ( (t1.actual_state = 'A') OR (t3.actual_state_text = 'Не определено' ) ) then now()    
	            when ( t1.endtimestamp = 0 ) and (t1.state <> 'A' ) then to_timestamp('19000101000001','YYYYMMDDHHMISS')
	            when ( t1.endtimestamp = 0 ) and (t1.state = 'A' ) then now()
	            else utctolocal(to_timestamp(substr(t1.endtimestamp,1,14)  ,'YYYYMMDDHHMISS'))
	            end as end_ts
	  FROM rspcprocesslog t1, rspclogchain t2
	    , (SELECT domvalue_l as actual_state, ddtext as actual_state_text  from dd07T WHERE domname = 'RSPC_STATE' AND ddlanguage = 'R') t3
	    , (SELECT domvalue_l as state, ddtext as state_text  from dd07T WHERE domname = 'RSPC_STATE' AND ddlanguage = 'R') t4
	    , (SELECT domvalue_l as analyzed_status, ddtext as analyzed_status_text  from dd07T WHERE domname = 'RSPC_STATE' AND ddlanguage = 'R') t5
	 WHERE 1 = 1
	   AND t1.log_id = t2.log_id
	   AND t1.actual_state = t3.actual_state
	   AND t1.state = t4.state
	   AND t2.analyzed_status = t5.analyzed_status
	   AND t1.type NOT IN ('AND','OR','EXOR', 'PLSWITCHP', 'PLSWITCHL', 'TRIGGER','INTERRUPT','DECISION','CHAIN')
	     ) c1
  WHERE  1 = 1
    AND state not in ('S')
    AND add_Seconds(now(), -24*60*60*2 ) < c1.pc_stts -- за последние 24*2h
    AND chain_id NOT IN ('Z_CHEK_PUSH')
 ORDER BY c1.pc_stts desc, c1.chain_id, variant_ext, c1.pc_stts,  c1.log_id)
select t1.chain_id, t1.pc_stts, t1.analyzed_status_text, t1.variant_ext, t1.state_text, t1.seconds, t2.med_secs, t2.cnt--, t2.avg_Secs
      from processes as t1, stat as t2
 where t1.variant_ext = t2.variant_ext
   AND t2.med_secs > 0
   AND t1.seconds > 1000
   AND (( t1.seconds - t2.med_secs ) / t2.med_secs ) * 100 >= 20
 order by (( t1.seconds - t2.med_secs ) / t2.med_secs );
 order by t1.pc_stts desc, t1.chain_id asc;


-- DTP names 
SELECT dtp
--, src, tgt, updmode
     , '(' || updmode || ') ' || replace(src,' ','.') || ' -> ' || tgt as dtpname
  from rsbkdtp
 where objvers = 'A'
   and dtp = 'DTP_ARXABGLIEOU33AM8SKDL7NOTI'
 
