class ZCL_BW_AVAIL_ST1 definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !I_DATE type /BI0/OICALDAY
      !I_JOBCNT type RSINT1 default 8
      !I_CLEAR type CHAR1 optional
      !I_PBEGP type /BI0/OIPLANT optional
      !I_PENDP type /BI0/OIPLANT optional
      !I_PINGRP type RSINT1 default 6
      !I_VARI type CHAR30 default 'PROD' .
  methods BKGR_PROC_FACTORY .
  class-methods FILL_PLANT
    importing
      !IN_DATE type /BI0/OICALDAY
      !IN_PLIST type CHAR100
      !IN_PLIST2 type CHAR100 .
  class-methods PUT_MESSAGE
    importing
      !IN_MESS type CHAR255
      !IN_MESSTYPE type CHAR1 default 'S' .
protected section.
private section.

  types:
    BEGIN OF t_plant_int,
        begp TYPE /bi0/oiplant,
      END OF t_plant_int .
  types:
    tt_plant_int TYPE STANDARD TABLE OF t_plant_int WITH DEFAULT KEY .
  types:
    BEGIN OF t_plant_grp,
        item  TYPE char100,
        item2 TYPE char100,
      END OF t_plant_grp .
  types:
    tt_plant_grp TYPE STANDARD TABLE OF t_plant_grp WITH DEFAULT KEY .

  data I_DATE type /BI0/OICALDAY .
  data I_JOBCNT type RSINT1 .
  data LO_CONN type ref to CL_SQL_CONNECTION .
  data LO_STATEMENT type ref to CL_SQL_STATEMENT .
  data LX_SQL type ref to CX_SQL_EXCEPTION .
  data LT_RES type ref to DATA .
  data I_PBEGP type /BI0/OIPLANT .
  data I_PENDP type /BI0/OIPLANT .
  data I_VARI type CHAR30 .
  data I_PINGRP type RSINT1 .
  data LT_PLANT_GRP type TT_PLANT_GRP .
  data LT_PLANT_INT type TT_PLANT_INT .

  methods TRUNCATE_ZBW_AVAIL_ST1 .
  methods SET_PLANT_GRP .
  methods SET_PLANT_GRP2 .
ENDCLASS.



CLASS ZCL_BW_AVAIL_ST1 IMPLEMENTATION.


  METHOD bkgr_proc_factory.

    DATA:
      lv_repname   TYPE edpline.

    lv_repname = 'ZBW_AVAIL_ST1_FILL_PLANT'.
    DATA(lo_ran) = cl_abap_random_int=>create(
                                        seed = CONV i( sy-uzeit )
                                        min  = 1
                                        max = 100 ).
    DATA(lv_jind) = lo_ran->get_next( ).

    TYPES:
      BEGIN OF t_job,
        jobname   TYPE btcjob,
        jobnumber TYPE btcjobcnt,
      END OF t_job,
      tt_job TYPE STANDARD TABLE OF t_job.

    DATA: rspar            TYPE TABLE OF rsparams,
          lv_jobname       TYPE btcjob,
          lv_jobnumber     TYPE btcjobcnt,
          print_parameters TYPE pri_params,
          lt_job           TYPE tt_job,
          lt_code          TYPE TABLE OF rssource-line.

*    me->set_plant_grp( ). Try to load same way as DTP does
    me->set_plant_grp2( ).

    LOOP AT lt_plant_grp ASSIGNING FIELD-SYMBOL(<fs>).
      rspar = VALUE #(
       ( selname = 'PPLIST'
         kind = 'P'
         low  = <fs>-item )
       ( selname = 'PPLIST2'
         kind = 'P'
         low  = <fs>-item2 )
       ( selname = 'PDATE'
         kind = 'P'
         low  = me->i_date )
         ).
      "      REPLACE ALL OCCURRENCES OF REGEX '''' IN <fs>-item WITH ''.
      zcl_bw_avail_st1=>put_message( in_mess = |Group { sy-tabix } for { <fs>-item } is run| ).
      lv_jobname = |ZBW_AVAIL_ST1_{ lv_jind }_{ sy-tabix WIDTH = 4 ALIGN = RIGHT PAD = '0' }|.
      CALL FUNCTION 'JOB_OPEN'
        EXPORTING
          jobname          = lv_jobname
        IMPORTING
          jobcount         = lv_jobnumber
        EXCEPTIONS
          cant_create_job  = 1
          invalid_job_data = 2
          jobname_missing  = 3
          OTHERS           = 4.
      IF sy-subrc = 0.
        APPEND VALUE #( jobname = lv_jobname jobnumber = lv_jobnumber ) TO lt_job.
        SUBMIT (lv_repname) TO SAP-SPOOL
                        SPOOL PARAMETERS print_parameters
                        WITHOUT SPOOL DYNPRO
                        VIA SELECTION-SCREEN
                        WITH SELECTION-TABLE rspar
                        VIA JOB lv_jobname NUMBER lv_jobnumber
                        AND RETURN.
        IF sy-subrc = 0.
          zcl_bw_avail_st1=>put_message( in_mess = |Job { lv_jobname } started| ).
          WAIT UP TO 1 SECONDS.
          CALL FUNCTION 'JOB_CLOSE'
            EXPORTING
              jobcount             = lv_jobnumber
              jobname              = lv_jobname
              strtimmed            = 'X'
            EXCEPTIONS
              cant_start_immediate = 1
              invalid_startdate    = 2
              jobname_missing      = 3
              job_close_failed     = 4
              job_nosteps          = 5
              job_notex            = 6
              lock_failed          = 7
              OTHERS               = 8.
          IF sy-subrc <> 0.
            zcl_bw_avail_st1=>put_message( in_mess = |Error closing job { lv_jobname } subrc = { sy-subrc }| ).
          ENDIF.
        ELSE.
          zcl_bw_avail_st1=>put_message( in_mess = |Error submitting { lv_repname } subrc = { sy-subrc }| ).
        ENDIF.
      ELSE.
        zcl_bw_avail_st1=>put_message( in_mess = |Error opening job { lv_jobname } subrc = { sy-subrc }| ).
      ENDIF.

      "check if it is OK to schedule next job
      DO.
        IF lt_job IS NOT INITIAL.
          SELECT COUNT(*) AS cnt FROM tbtco
            FOR ALL ENTRIES IN @lt_job
           WHERE jobname  = @lt_job-jobname
             AND jobcount = @lt_job-jobnumber
             AND ( status IN ('P','R') )
            INTO ( @DATA(lv_cnt1) ).
          WAIT UP TO 2 SECONDS.
          IF lv_cnt1 < me->i_jobcnt.
            EXIT.
          ENDIF.
        ENDIF.
      ENDDO.
    ENDLOOP.

    " do check in almost infinite cycle of job execution
    DO.
      IF lt_job IS NOT INITIAL.
        SELECT COUNT(*) AS cnt FROM tbtco
          FOR ALL ENTRIES IN @lt_job
          WHERE jobname  = @lt_job-jobname
            AND jobcount = @lt_job-jobnumber
          AND ( status IN ('A','F') )
         INTO ( @DATA(lv_cnt) ).
        IF lv_cnt = lines( lt_job ).
          zcl_bw_avail_st1=>put_message( in_mess = |All { lines( lt_job ) } jobs were completed| ).
          EXIT.
        ELSE.
          zcl_bw_avail_st1=>put_message( in_mess = |Only { lv_cnt } of { lines( lt_job ) } jobs were completed| ).
        ENDIF.
        WAIT UP TO 5 SECONDS.
      ENDIF.
    ENDDO.
    zcl_bw_avail_st1=>put_message( in_mess = | ************** Final statistics [plant: rows] ************** | ).
    SELECT plant, COUNT(*) AS cnt
      FROM zbw_avail_st1
     GROUP BY plant ORDER BY plant
      INTO TABLE @DATA(lt_plant_calc).

    LOOP AT lt_plant_int ASSIGNING FIELD-SYMBOL(<fs2>).
      READ TABLE lt_plant_calc ASSIGNING FIELD-SYMBOL(<fs_calc>) WITH KEY plant = <fs2>-begp BINARY SEARCH.
      IF sy-subrc = 0.
        zcl_bw_avail_st1=>put_message( in_mess = |{ <fs_calc>-plant }: { <fs_calc>-cnt WIDTH = 15 ALIGN = RIGHT }| ).
      ELSE.
        zcl_bw_avail_st1=>put_message( in_mess = |{ <fs2>-begp }: now rows inserted| ).
      ENDIF.
    ENDLOOP.

*" то, что вызывается в start routine трансформации
*    SORT SOURCE_PACKAGE
*      BY CALDAY PLANT MATERIAL ZAVAILTYP VENDOR
*         ZSTR_IND CREATEDON DATEFROM.
*    DELETE ADJACENT DUPLICATES FROM SOURCE_PACKAGE
*      COMPARING CALDAY PLANT MATERIAL ZAVAILTYP.
*Но!
*SELECT CALDAY, PLANT, MATERIAL, "/BIC/ZAVAILTYP", count(*) as cnt
*  FROM "SAPBWP"."ZBW_AVAIL_ST1"
* GROUP BY CALDAY, PLANT, MATERIAL, "/BIC/ZAVAILTYP" HAVING count(*) > 1;
* returns 0 records

*Values for TBTCO-STATUS:
*
*A - Cancelled
*F - Completed
*P - Scheduled
*R - Active
*S - Released

  ENDMETHOD.


  METHOD constructor.
    " me->i_date = i_date. " commented because we take data from VARI of the program
    me->i_jobcnt = i_jobcnt.
    me->i_pbegp = i_pbegp.
    me->i_pendp = i_pendp.
    me->i_pingrp = i_pingrp.
    me->i_vari = i_vari.

    IF me->i_jobcnt > 20. " protection from high consumption
      me->i_jobcnt = 20.
    ENDIF.

    IF me->i_pingrp > 6. " protection from large string in JOB parameter
      me->i_pingrp = 6.
    ENDIF.

    "zcl_bw_avail_st1=>put_message( in_mess = |Date                = {  me->i_date }| ).
    zcl_bw_avail_st1=>put_message( in_mess = |Parallel processes <= {  me->i_jobcnt }| ).
    zcl_bw_avail_st1=>put_message( in_mess = |Plants in group <= {  me->i_pingrp }| ).
    zcl_bw_avail_st1=>put_message( in_mess = |Plants in [{  me->i_pbegp }, { me->i_pendp }]| ).

    TRY.
        lo_conn = cl_sql_connection=>get_connection( ).
        lo_statement = lo_conn->create_statement( ).
      CATCH cx_sql_exception INTO lx_sql.
        WRITE: / lx_sql->get_text( ).
        WRITE: / lx_sql->sql_code.
        WRITE: / lx_sql->sql_message.
        RETURN.
    ENDTRY.

    IF i_clear = 'X'.
      me->truncate_zbw_avail_st1( ).
    ELSE.
      WAIT UP TO 7 SECONDS. " In parallel execution it is required to wait while table will be truncated
    ENDIF.
  ENDMETHOD.


  METHOD fill_plant.
    DATA: " s means STATIC
      lso_conn      TYPE REF TO cl_sql_connection,
      lso_statement TYPE REF TO cl_sql_statement,
      lsx_sql       TYPE REF TO cx_sql_exception,
      lst_res       TYPE REF TO data,

      lv_ts         TYPE timestamp.
    CONSTANTS:
      lv_dpsize TYPE p VALUE 1000000.
    GET TIME STAMP FIELD lv_ts.

    DATA: lv_pext TYPE p LENGTH 6 DECIMALS 0.
    TRY. " To provide unique combination of values (ZREQTSN,ZDATAPAKID)
*        lv_pext = in_pbegp. " expect PLANT code contains only digits !!
        lv_pext = in_plist+1(4). " take 1st plant from the list
        lv_pext = lv_pext * 100.
      CATCH cx_root.
        lv_pext = 100.
    ENDTRY.

** added DISTINCT keyword. And commented. beacuse decided to implement DISTINCT at the transformation level
*    DATA(lv_stmt) =
*    | INSERT INTO "ZBW_AVAIL_ST1" (ZREQTSN,ZDATAPAKID,ZRECORD | &
*    | ,"CALDAY","PLANT","MATERIAL","/BIC/ZSTR_IND","/BIC/ZAVAILTYP"| &
*    | ,"BASE_UOM","VENDOR","CREATEDON","MRP_CONTRL"| &
*    | ,"RT_STRNR","DATEFROM","RT_DEPARTM","/BIC/FLAG2","/BIC/FLAG4"| &
*    | ,"RT_PROMO","/BIC/ZBIGMEDIA","/BIC/COUNTER1","/BIC/COUNTER2","/BIC/COUNTER3"| &
*    | ,"/BIC/COUNTER4","/BIC/COUNTER5","/BIC/COUNTER6","/BIC/COUNTER8","/BIC/COUNTER9"| &
*    | ,"CPTGOREVBU","/BIC/CLSS","/BIC/ZCLSS_PRV","/BIC/ZCLSS_IKB","/BIC/ZCLSS_DV")| &
*    | SELECT { lv_ts } as ZREQTSN, { lv_pext }+to_integer(row_number() over () / { lv_dpsize } + 1) as ZDATAPAKID, | &
*    |         mod (  row_number() over (), { lv_dpsize })+1 as ZRECORD, t1.* FROM ( SELECT DISTINCT | &
*    | "CALDAY",t1."PLANT","MATERIAL", ifnull("ZSTR_IND",'') as "/BIC/ZSTR_IND", ifnull("ZAVAILTYP",'') as "/BIC/ZAVAILTYP"| &
*    | ,"BASE_UOM", "VENDOR", ifnull("CREATEDON",'00000000') as "CREATEDON", ifnull("MRP_CONTRL",'') as "MRP_CONTRL"| &
*    | ,ifnull("RT_STRNR",'') as "RT_STRNR",ifnull("DATEFROM",'00000000') as "DATEFROM", ifnull("RT_DEPARTM",'') as "RT_DEPARTM", ifnull("FLAG2",'') as "/BIC/FLAG2", ifnull("FLAG4",'') as "/BIC/FLAG4"| &
*    | ,ifnull("RT_PROMO",'') as "RT_PROMO", ifnull("ZBIGMEDIA",'') as "/BIC/ZBIGMEDIA", ifnull("COUNTER1",0) as "/BIC/COUNTER1", ifnull("COUNTER2",0) as "/BIC/COUNTER2", ifnull("COUNTER3",0) as "/BIC/COUNTER3"| &
*    | ,ifnull("COUNTER4",0) as "/BIC/COUNTER4" ,ifnull("COUNTER5",0) as "/BIC/COUNTER5",ifnull("COUNTER6",0) as "/BIC/COUNTER6", ifnull("COUNTER8",0) as "/BIC/COUNTER8",ifnull("COUNTER9",0) as "/BIC/COUNTER9"| &
*    | ,ifnull("CPTGOREVBU",0) as "CPTGOREVBU", ifnull("CLSS",0) as "/BIC/CLSS", ifnull("ZCLSS_PRV",0) as "/BIC/ZCLSS_PRV", ifnull("ZCLSS_IKB",0) as "/BIC/ZCLSS_IKB", ifnull("ZCLSS_DV",0) as "/BIC/ZCLSS_DV"| &
*    | FROM "_SYS_BIC"."lenta.bw.lspec.log_report.avail/AVAIL_CALCULATION_STEP1_CV02"('PLACEHOLDER' = ('$$I_DATE$$', '{ in_date }'), 'PLACEHOLDER' = ('$$I_PLANT$$','{ in_plist2 }')) t1, "/BI0/SPLANT" t2 | &
*    | WHERE t1.plant = t2.plant AND t2.plant IN ({ in_plist })) as t1;|.

"
    DATA(lv_stmt) =
    | INSERT INTO "ZBW_AVAIL_ST1" (ZREQTSN,ZDATAPAKID,ZRECORD | &
    | ,"CALDAY","PLANT","MATERIAL","/BIC/ZSTR_IND","/BIC/ZAVAILTYP"| &
    | ,"BASE_UOM","VENDOR","CREATEDON","MRP_CONTRL"| &
    | ,"RT_STRNR","DATEFROM","RT_DEPARTM","/BIC/FLAG2","/BIC/FLAG4"| &
    | ,"RT_PROMO","/BIC/ZBIGMEDIA"| &
     | ,"/BIC/ZBIGMEDV", "/BIC/Z_BM_FROM", "/BIC/Z_BM_TO"| &
    | ,"/BIC/COUNTER1", "/BIC/COUNTER2", "/BIC/COUNTER3"| &
    | ,"/BIC/COUNTER4", "/BIC/COUNTER5", "/BIC/COUNTER6", "/BIC/COUNTER8", "/BIC/COUNTER9"| &
    | ,"CPTGOREVBU","/BIC/CLSS","/BIC/ZCLSS_PRV","/BIC/ZCLSS_IKB","/BIC/ZCLSS_DV")| &
    | SELECT { lv_ts } as ZREQTSN, { lv_pext }+to_integer(row_number() over () / { lv_dpsize } + 1) as ZDATAPAKID, | &
    |         mod (  row_number() over (), { lv_dpsize })+1 as ZRECORD, | &
    | "CALDAY",t1."PLANT","MATERIAL", ifnull("ZSTR_IND",'') as "/BIC/ZSTR_IND", ifnull("ZAVAILTYP",'') as "/BIC/ZAVAILTYP"| &
    | ,"BASE_UOM", "VENDOR", ifnull("CREATEDON",'00000000') as "CREATEDON", ifnull("MRP_CONTRL",'') as "MRP_CONTRL"| &
    | ,ifnull("RT_STRNR",'') as "RT_STRNR",ifnull("DATEFROM",'00000000') as "DATEFROM", ifnull("RT_DEPARTM",'') as "RT_DEPARTM", ifnull("FLAG2",'') as "/BIC/FLAG2", ifnull("FLAG4",'') as "/BIC/FLAG4"| &
    | ,ifnull("RT_PROMO",'') as "RT_PROMO", ifnull("ZBIGMEDIA",'') as "/BIC/ZBIGMEDIA" | &
     | ,ifnull("ZBIGMEDV",'') as "/BIC/ZBIGMEDV", ifnull("Z_BM_FROM",'') as "/BIC/Z_BM_FROM", ifnull("Z_BM_TO",'') as "/BIC/Z_BM_TO"| &
    | ,ifnull("COUNTER1",0) as "/BIC/COUNTER1", ifnull("COUNTER2",0) as "/BIC/COUNTER2", ifnull("COUNTER3",0) as "/BIC/COUNTER3"| &
    | ,ifnull("COUNTER4",0) as "/BIC/COUNTER4" ,ifnull("COUNTER5",0) as "/BIC/COUNTER5",ifnull("COUNTER6",0) as "/BIC/COUNTER6", ifnull("COUNTER8",0) as "/BIC/COUNTER8",ifnull("COUNTER9",0) as "/BIC/COUNTER9"| &
    | ,ifnull("CPTGOREVBU",0) as "CPTGOREVBU", ifnull("CLSS",0) as "/BIC/CLSS", ifnull("ZCLSS_PRV",0) as "/BIC/ZCLSS_PRV", ifnull("ZCLSS_IKB",0) as "/BIC/ZCLSS_IKB", ifnull("ZCLSS_DV",0) as "/BIC/ZCLSS_DV"| &
    | FROM "_SYS_BIC"."lenta.bw.lspec.log_report.avail/AVAIL_CALCULATION_STEP1_CV02"('PLACEHOLDER' = ('$$I_DATE$$', '{ in_date }'), 'PLACEHOLDER' = ('$$I_PLANT$$','{ in_plist2 }')) t1, "/BI0/SPLANT" t2 | &
    | WHERE t1.plant = t2.plant AND t2.plant IN ({ in_plist });|.

* commented 2023-09-11 because changes in underlying tables were not transferred to Q and P systems yet.
*    DATA(lv_stmt) =
*    | INSERT INTO "ZBW_AVAIL_ST1" (ZREQTSN,ZDATAPAKID,ZRECORD | &
*    | ,"CALDAY","PLANT","MATERIAL","/BIC/ZSTR_IND","/BIC/ZAVAILTYP"| &
*    | ,"BASE_UOM","VENDOR","CREATEDON","MRP_CONTRL"| &
*    | ,"RT_STRNR","DATEFROM","RT_DEPARTM","/BIC/FLAG2","/BIC/FLAG4"| &
*    | ,"RT_PROMO","/BIC/ZBIGMEDIA","/BIC/ZBIGMEDV","/BIC/Z_BM_FROM","/BIC/Z_BM_TO","/BIC/COUNTER1","/BIC/COUNTER2","/BIC/COUNTER3"| &
*    | ,"/BIC/COUNTER4","/BIC/COUNTER5","/BIC/COUNTER6","/BIC/COUNTER8","/BIC/COUNTER9"| &
*    | ,"CPTGOREVBU","/BIC/CLSS","/BIC/ZCLSS_PRV","/BIC/ZCLSS_IKB","/BIC/ZCLSS_DV")| &
*    | SELECT DISTINCT { lv_ts } as ZREQTSN, { lv_pext }+to_integer(row_number() over () / { lv_dpsize } + 1) as ZDATAPAKID, | &
*    |         mod (  row_number() over (), { lv_dpsize })+1 as ZRECORD, | &
*    | "CALDAY",t1."PLANT","MATERIAL", ifnull("ZSTR_IND",'') as "/BIC/ZSTR_IND", ifnull("ZAVAILTYP",'') as "/BIC/ZAVAILTYP"| &
*    | ,"BASE_UOM", "VENDOR", ifnull("CREATEDON",'00000000') as "CREATEDON", ifnull("MRP_CONTRL",'') as "MRP_CONTRL"| &
*    | ,ifnull("RT_STRNR",'') as "RT_STRNR",ifnull("DATEFROM",'00000000') as "DATEFROM", ifnull("RT_DEPARTM",'') as "RT_DEPARTM", ifnull("FLAG2",'') as "/BIC/FLAG2", ifnull("FLAG4",'') as "/BIC/FLAG4"| &
*    | ,ifnull("RT_PROMO",'') as "RT_PROMO", ifnull("ZBIGMEDIA",'') as "/BIC/ZBIGMEDIA", ifnull("ZBIGMEDV",'') as "/BIC/ZBIGMEDV", ifnull("Z_BM_FROM",'') as "/BIC/Z_BM_FROM", ifnull("Z_BM_TO",'') as "/BIC/Z_BM_TO"| &
*    | ,ifnull("COUNTER1",0) as "/BIC/COUNTER1", ifnull("COUNTER2",0) as "/BIC/COUNTER2", ifnull("COUNTER3",0) as "/BIC/COUNTER3"| &
*    | ,ifnull("COUNTER4",0) as "/BIC/COUNTER4" ,ifnull("COUNTER5",0) as "/BIC/COUNTER5",ifnull("COUNTER6",0) as "/BIC/COUNTER6", ifnull("COUNTER8",0) as "/BIC/COUNTER8",ifnull("COUNTER9",0) as "/BIC/COUNTER9"| &
*    | ,ifnull("CPTGOREVBU",0) as "CPTGOREVBU", ifnull("CLSS",0) as "/BIC/CLSS", ifnull("ZCLSS_PRV",0) as "/BIC/ZCLSS_PRV", ifnull("ZCLSS_IKB",0) as "/BIC/ZCLSS_IKB", ifnull("ZCLSS_DV",0) as "/BIC/ZCLSS_DV"| &
*    | FROM "_SYS_BIC"."lenta.bw.lspec.log_report.avail/AVAIL_CALCULATION_STEP1_CV02"('PLACEHOLDER' = ('$$I_DATE$$', '{ in_date }'), 'PLACEHOLDER' = ('$$I_PLANT$$','{ in_plist2 }')) t1, "/BI0/SPLANT" t2 | &
*    | WHERE t1.plant = t2.plant AND t2.plant IN ({ in_plist });|.

 "   | WHERE plant BETWEEN '{ in_pbegp }' AND '{ in_pendp }';|.
"I028159/YAVAIL_CALCULATION_STEP1_CV01
"lenta.bw.lspec.log_report.avail/AVAIL_CALCULATION_STEP1_CV01"
    TRY.
        lso_conn = cl_sql_connection=>get_connection( ).
        lso_statement = lso_conn->create_statement( ).
        DATA(l_row_cnt) = lso_statement->execute_update( lv_stmt ).
        COMMIT WORK.
        zcl_bw_avail_st1=>put_message( in_mess = | Inserted for date {  in_date } and  [{ in_plist }], rows={ l_row_cnt }| ).
      CATCH cx_sql_exception INTO lsx_sql.
        zcl_bw_avail_st1=>put_message( in_mess = |{ lsx_sql->get_text( ) }| ).
        zcl_bw_avail_st1=>put_message( in_mess = |{ lsx_sql->sql_code }| ).
        zcl_bw_avail_st1=>put_message( in_mess = |{ lsx_sql->sql_message }| ).
        zcl_bw_avail_st1=>put_message( in_mess = | Error inserting for date {  in_date } and [{ in_plist }]|  ).
        zcl_bw_avail_st1=>put_message( in_mess = | Error inserting for date {  in_date } and [{ in_plist }]| in_messtype = 'E' ).
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD put_message.
    WRITE: / in_mess.
    MESSAGE in_mess TYPE in_messtype.
  ENDMETHOD.


  method TRUNCATE_ZBW_AVAIL_ST1.
      TRY.
        DATA(l_row_cnt) = lo_statement->execute_update( |TRUNCATE TABLE ZBW_AVAIL_ST1;| ).
        me->put_message( |Truncation of the table ZBW_AVAIL_ST1 was successful| ).
      CATCH cx_sql_exception INTO lx_sql.
        me->put_message( in_mess = |truncation of table ZBW_AVAIL_ST1 got error - { lx_sql->sql_code }:{ lx_sql->get_text( ) }| in_messtype =  'E').
        me->put_message( in_mess = |{ lx_sql->sql_message }| in_messtype =  'E').
        me->put_message( in_mess = |Delete started from ABAP command..'| in_messtype =  'S').
        DELETE FROM ZBW_AVAIL_ST1.
        me->put_message( in_mess = |Deletion ended| in_messtype =  'S'). .
    ENDTRY.
  endmethod.


  METHOD set_plant_grp.
    DATA: ls_plant_int TYPE t_plant_int,
          "lt_plant_grp TYPE tt_plant_grp,
          ls_plant_grp TYPE t_plant_grp,
          lt_valtab    TYPE STANDARD TABLE OF rsparams,
          lt_valutabl  TYPE STANDARD TABLE OF rsparamsl.

    CLEAR: me->lt_plant_int, me->lt_plant_grp.

*    IF ( me->i_pbegp <> '' ) AND ( me->i_pendp = '' ).
*      me->i_pendp  = me->i_pbegp.
*    ENDIF.
*    IF ( me->i_pbegp = '' ) AND ( me->i_pendp <> '' ).
*      me->i_pbegp  = me->i_pendp.
*    ENDIF.
*    IF ( me->i_pbegp = '' ) AND ( me->i_pendp = '' ).
*      SELECT plant AS begp ", plant AS endp
*        FROM /bi0/mplant
*       WHERE objvers = 'A'
*         AND dateto = '99991231'
*         AND plantcat = 'A'
*         AND /bic/zfrmttyp <> ''
*         AND gln <> '0'
*        INTO CORRESPONDING FIELDS OF TABLE @me->lt_plant_int.
*    ELSE.
*      SELECT plant AS begp", plant AS endp
*        FROM /bi0/mplant
*       WHERE objvers = 'A'
*         AND dateto = '99991231'
*         AND plantcat = 'A'
*         AND /bic/zfrmttyp <> ''
*         AND gln <> '0'
*         AND plant BETWEEN @me->i_pbegp AND @me->i_pendp
*        INTO CORRESPONDING FIELDS OF TABLE @me->lt_plant_int.
*    ENDIF.
*    SORT me->lt_plant_int BY begp ASCENDING.

    CALL FUNCTION 'RS_VARIANT_CONTENTS'
      EXPORTING
        report   = 'Z_LOG_AVAILABILITY_CALC_PARAMS'
        variant  = 'PROD'
*       MOVE_OR_WRITE               = 'W'
*       NO_IMPORT                   = ' '
*       EXECUTE_DIRECT              = ' '
* IMPORTING
*       SP       =
      TABLES
*       L_PARAMS =
*       L_PARAMS_NONV               =
*       L_SELOP  =
*       L_SELOP_NONV                =
        valutab  = lt_valtab
        valutabl = lt_VALUTABL
*       OBJECTS  =
*       VARIVDATS                   =
*       FREE_SELECTIONS_DESC        =
*       FREE_SELECTIONS_VALUE       =
*       FREE_SELECTIONS_OBJ         =
* EXCEPTIONS
*       VARIANT_NON_EXISTENT        = 1
*       VARIANT_OBSOLETE            = 2
*       OTHERS   = 3
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
      zcl_bw_avail_st1=>put_message( in_mess = |Error reading PROD variant for report Z_LOG_AVAILABILITY_CALC_PARAMS.| ).
      RETURN.
    ENDIF.

    READ TABLE lt_VALUTABL WITH KEY selname = 'P_DATE' ASSIGNING FIELD-SYMBOL(<fs_vt1>).
    IF ( sy-subrc = 0 ).
      IF ( <fs_vt1>-low <> '00.00.0000').
        REPLACE REGEX '(\d\d).(\d\d).(\d\d\d\d)' IN <fs_vt1>-low WITH '$3$2$1'.
        me->i_date = <fs_vt1>-low.
      ELSE.
        me->i_date = sy-datum - 1.
      ENDIF.
    ELSE.
      me->i_date = sy-datum - 1.
    ENDIF.
    zcl_bw_avail_st1=>put_message( in_mess = |Date                = {  me->i_date }| ).
    LOOP AT lt_VALUTABL ASSIGNING FIELD-SYMBOL(<fs_vt>)
      WHERE selname = 'P_PLANT'
        AND option = 'EQ'.
      ls_plant_int-begp = <fs_vt>-low+0(4).
      APPEND ls_plant_int TO me->lt_plant_int.
    ENDLOOP.
    SORT me->lt_plant_int BY begp ASCENDING.
    IF me->i_pbegp <> '' AND me->i_pendp <> ''.
      LOOP AT me->lt_plant_int ASSIGNING FIELD-SYMBOL(<fs1>).
        IF <fs1>-begp < me->i_pbegp OR <fs1>-begp > me->i_pendp.
          DELETE me->lt_plant_int INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.

    LOOP AT me->lt_plant_int ASSIGNING <fs1>.
      AT FIRST.
        ls_plant_grp-item = ''.
        ls_plant_grp-item2 = ''.
      ENDAT.
      ls_plant_grp-item  = |{ ls_plant_grp-item }'{ <fs1>-begp }',|.
      ls_plant_grp-item2 = |{ ls_plant_grp-item2 }{ <fs1>-begp },|.
      IF sy-tabix MOD me->i_pingrp = 0.
        ls_plant_grp-item  = substring( val = ls_plant_grp-item off = 0 len = strlen( ls_plant_grp-item ) - 1 ).
        ls_plant_grp-item2 = substring( val = ls_plant_grp-item2 off = 0 len = strlen( ls_plant_grp-item2 ) - 1 ).
        APPEND ls_plant_grp TO me->lt_plant_grp.
        ls_plant_grp-item = ''.
        ls_plant_grp-item2 = ''.
      ENDIF.
    ENDLOOP.
    IF ls_plant_grp-item <> ''.
      ls_plant_grp-item  = substring( val = ls_plant_grp-item off = 0 len = strlen( ls_plant_grp-item ) - 1 ).
      ls_plant_grp-item2 = substring( val = ls_plant_grp-item2 off = 0 len = strlen( ls_plant_grp-item2 ) - 1 ).
      APPEND ls_plant_grp TO me->lt_plant_grp.
      ls_plant_grp-item  = ''.
      ls_plant_grp-item2 = ''.
    ENDIF.
    zcl_bw_avail_st1=>put_message( in_mess = |Plants are { lines( lt_plant_int ) }. Groups are { lines( lt_plant_grp ) }| ).
  ENDMETHOD.


  METHOD set_plant_grp2.
    DATA: ls_plant_int TYPE t_plant_int,
          ls_plant_grp TYPE t_plant_grp,
          lt_valtab    TYPE STANDARD TABLE OF rsparams,
          lt_valutabl  TYPE STANDARD TABLE OF rsparamsl.

    DATA: l_t_range TYPE STANDARD TABLE OF rssdlrange.

    CLEAR: me->lt_plant_int, me->lt_plant_grp.

    DATA: l_idx    LIKE sy-tabix,
          lr_plant TYPE RANGE OF /bi0/oiplant,
          l_date   TYPE /bi0/oicalday.

    l_idx = sy-tabix.

    IMPORT pltavail = lr_plant FROM DATABASE indx(bp) ID 'AVAIL'.
    l_t_range = VALUE #( FOR <fs_plant> IN lr_plant
                           ( fieldname = 'PLANT'
                             sign = <fs_plant>-sign
                             option = <fs_plant>-option
                             low = <fs_plant>-low
                             high  = <fs_plant>-high ) ).

    IMPORT cdavail = l_date FROM DATABASE indx(bp) ID 'AVAIL'.
    IF l_date IS INITIAL.
      me->i_date = sy-datum - 1.
    ELSE.
      me->i_date = l_date.
    ENDIF.

*    CALL FUNCTION 'RS_VARIANT_CONTENTS'
*      EXPORTING
*        report   = 'Z_LOG_AVAILABILITY_CALC_PARAMS'
*        variant  = 'PROD'
**       MOVE_OR_WRITE               = 'W'
**       NO_IMPORT                   = ' '
**       EXECUTE_DIRECT              = ' '
** IMPORTING
**       SP       =
*      TABLES
**       L_PARAMS =
**       L_PARAMS_NONV               =
**       L_SELOP  =
**       L_SELOP_NONV                =
*        valutab  = lt_valtab
*        valutabl = lt_VALUTABL
**       OBJECTS  =
**       VARIVDATS                   =
**       FREE_SELECTIONS_DESC        =
**       FREE_SELECTIONS_VALUE       =
**       FREE_SELECTIONS_OBJ         =
** EXCEPTIONS
**       VARIANT_NON_EXISTENT        = 1
**       VARIANT_OBSOLETE            = 2
**       OTHERS   = 3
    .
*    IF sy-subrc <> 0.
    IF lines( l_t_range ) = 0.
* Implement suitable error handling here
      zcl_bw_avail_st1=>put_message( in_mess = |Error IMPORT report Z_LOG_AVAILABILITY_CALC_PARAMS.| ).
      RETURN.
    ENDIF.

*    READ TABLE lt_VALUTABL WITH KEY selname = 'P_DATE' ASSIGNING FIELD-SYMBOL(<fs_vt1>).
*    IF ( sy-subrc = 0 ).
*      IF ( <fs_vt1>-low <> '00.00.0000').
*        REPLACE REGEX '(\d\d).(\d\d).(\d\d\d\d)' IN <fs_vt1>-low WITH '$3$2$1'.
*        me->i_date = <fs_vt1>-low.
*      ELSE.
*        me->i_date = sy-datum - 1.
*      ENDIF.
*    ELSE.
*      me->i_date = sy-datum - 1.
*    ENDIF.
    zcl_bw_avail_st1=>put_message( in_mess = |Date                = {  me->i_date }| ).
*    LOOP AT lt_VALUTABL ASSIGNING FIELD-SYMBOL(<fs_vt>)
    LOOP AT l_t_range ASSIGNING FIELD-SYMBOL(<fs_vt>).
*      WHERE selname = 'P_PLANT'
*        AND option = 'EQ'.
      ls_plant_int-begp = <fs_vt>-low+0(4).
      APPEND ls_plant_int TO me->lt_plant_int.
    ENDLOOP.
    SORT me->lt_plant_int BY begp ASCENDING.
    IF me->i_pbegp <> '' AND me->i_pendp <> ''.
      LOOP AT me->lt_plant_int ASSIGNING FIELD-SYMBOL(<fs1>).
        IF <fs1>-begp < me->i_pbegp OR <fs1>-begp > me->i_pendp.
          DELETE me->lt_plant_int INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.

    LOOP AT me->lt_plant_int ASSIGNING <fs1>.
      AT FIRST.
        ls_plant_grp-item = ''.
        ls_plant_grp-item2 = ''.
      ENDAT.
      ls_plant_grp-item  = |{ ls_plant_grp-item }'{ <fs1>-begp }',|.
      ls_plant_grp-item2 = |{ ls_plant_grp-item2 }{ <fs1>-begp },|.
      IF sy-tabix MOD me->i_pingrp = 0.
        ls_plant_grp-item  = substring( val = ls_plant_grp-item off = 0 len = strlen( ls_plant_grp-item ) - 1 ).
        ls_plant_grp-item2 = substring( val = ls_plant_grp-item2 off = 0 len = strlen( ls_plant_grp-item2 ) - 1 ).
        APPEND ls_plant_grp TO me->lt_plant_grp.
        ls_plant_grp-item = ''.
        ls_plant_grp-item2 = ''.
      ENDIF.
    ENDLOOP.
    IF ls_plant_grp-item <> ''.
      ls_plant_grp-item  = substring( val = ls_plant_grp-item off = 0 len = strlen( ls_plant_grp-item ) - 1 ).
      ls_plant_grp-item2 = substring( val = ls_plant_grp-item2 off = 0 len = strlen( ls_plant_grp-item2 ) - 1 ).
      APPEND ls_plant_grp TO me->lt_plant_grp.
      ls_plant_grp-item  = ''.
      ls_plant_grp-item2 = ''.
    ENDIF.
    zcl_bw_avail_st1=>put_message( in_mess = |Plants are { lines( lt_plant_int ) }. Groups are { lines( lt_plant_grp ) }| ).
  ENDMETHOD.
ENDCLASS.