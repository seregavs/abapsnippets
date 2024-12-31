class ZCL_BW_HANAEE_UTILS definition
  public
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IN_TNAME type CHAR40
      !IN_ACTION type CHAR1
      !IN_WHERE type CHAR80 optional .
  methods CALL_ACTION .
  class-methods GET_NIELSEN8_PERIOD_INT
    importing
      !IN_RN type CHAR2 default '1'
      !IN_GRP type CHAR2 default '10'
    exporting
      !OUT_PSTART type INT2
      !OUT_PEND type INT2 .
  class-methods FILL_ZNLSO10 .
private section.

  "  TYPES:
  data LO_CONN type ref to CL_SQL_CONNECTION .
  data LO_STATEMENT type ref to CL_SQL_STATEMENT .
  data LX_SQL type ref to CX_SQL_EXCEPTION .
  data LT_RES type ref to DATA .
  data LV_TNAME type CHAR40 .
  data LV_ACTION type CHAR1 .
  data LV_WHERE type CHAR80 .
  constants:
    lc_hana_ee_schema(10) TYPE c value 'BW_REMOTE' ##NO_TEXT.
  constants LC_CONN_NAME type DBCON_NAME value 'HANAEE' ##NO_TEXT.
  data I_DBSCHEMA type CHAR20 .

  methods MERGE_T
    importing
      !IN_COMP type CHAR1 .
  methods UNLOAD_T .
  methods TRUNCATE_T .
  methods DELETE_T .
  methods DELETE_T_LOCAL .
ENDCLASS.



CLASS ZCL_BW_HANAEE_UTILS IMPLEMENTATION.


  METHOD constructor.
    TRY.
        IF ( in_action = '7' ). " 7 - DELETE data from _local_ table. So, connection must be local
          lo_conn = cl_sql_connection=>get_connection( ).
        ELSE.
          lo_conn = cl_sql_connection=>get_connection( con_name = lc_conn_name ).
        ENDIF.
        lo_statement = lo_conn->create_statement( ).
      CATCH cx_sql_exception INTO lx_sql.
        zcl_bw_avail_st1=>put_message( in_mess = |Error opening connection to HANA EE: { lx_sql->sql_message }| in_messtype =  'E').
        RETURN.
    ENDTRY.

    CALL FUNCTION 'DB_DBSCHEMA_CURRENT'
      IMPORTING
        dbschema = i_dbschema.

    lv_tname = to_upper( in_tname ).
    IF lv_tname IS NOT INITIAL.
      IF NOT ( ( in_tname = 'ZDLNLSO08' )
            OR ( in_tname = 'ZDLNLSO01' )
            OR ( in_tname = 'ZDLMONO03' )
            OR ( in_tname = 'ZDLMONO04' )
            OR ( in_tname = 'ZBW_DLCRMO041')
            OR ( in_tname = 'ZBW_DLCRMO011')
*<<<<<<< BI-1934
            OR ( in_tname = 'ZBW_DLCRMO021')
            OR ( in_tname = 'ZBW_DLCRMO031')
*>>>>>>> BI-1934
*<<<<<<< BI-2056
            OR ( in_tname = 'ZBW_DLRTO631')
*>>>>>>> BI-2056
         ).
        zcl_bw_avail_st1=>put_message( in_mess = |Table { lv_tname } is not allowed to process| in_messtype =  'E').
        CLEAR: lv_tname.
        RETURN.
      ENDIF.
      " Please check call_Action method to see what every action number means
      " Table name provided for merge and delete operations, in both HANA EE and local BW schema
      IF ( ( in_action GE '1' ) AND ( in_action LE '5' ) ) OR ( in_action EQ '7' ).
        lv_action = in_action.
      ELSE.
        zcl_bw_avail_st1=>put_message( in_mess = |Action must be (between 1 and 5) or (eq 7)| in_messtype = 'E' ).
        RETURN.
      ENDIF.
    ELSE.
      IF ( in_action = '6' ).
        lv_action = in_action.
      ELSE.
        zcl_bw_avail_st1=>put_message( in_mess = |if table name is empty, action must be between 6 and 6| in_messtype = 'E' ).
        RETURN.
      ENDIF.
    ENDIF.

    IF lv_action = '4' OR lv_action = '5' OR lv_action = '7'.
      lv_where = in_where.
    ENDIF.
    " The report must handle only hardcoded set of HANA EE tables to truncate.
    " This is done for protection from unintentional purging of data.
    " All tables which are egligable to purge must be listed in CHECK statement with OR exrpression.

  ENDMETHOD.


  METHOD call_Action.
    CASE lv_Action.
      WHEN '1'. merge_t( in_comp = '' ).
      WHEN '2'. merge_t( in_comp = 'X' ).
      WHEN '3'. unload_t( ).
      WHEN '4'. truncate_t( ).
      WHEN '5'. delete_t( ).
      WHEN '6'. fill_znlso10( ).
      WHEN '7'. delete_t_local( ).
    ENDCASE.
  ENDMETHOD.


  METHOD merge_t.
    TRY.
        DATA(l_row_cnt) = lo_statement->execute_update( |MERGE DELTA OF { lc_hana_ee_schema }.{ lv_tname };| ).
        zcl_bw_avail_st1=>put_message( |Merging table { lc_hana_ee_schema }.{ lv_tname } was successful| ).
        IF in_Comp = 'X'.
          l_row_cnt = lo_statement->execute_update( |UPDATE { lc_hana_ee_schema }.{ lv_tname } WITH PARAMETERS ('OPTIMIZE_COMPRESSION' = 'FORCE');| ).
          zcl_bw_avail_st1=>put_message( |Compression optimization of table { lc_hana_ee_schema }.{ lv_tname } was successful| ).
        ENDIF.
      CATCH cx_sql_exception INTO lx_sql.
        zcl_bw_avail_st1=>put_message( in_mess = |Merging table { lc_hana_ee_schema }.{ lv_tname } got error - { lx_sql->sql_code }:{ lx_sql->get_text( ) }| ).
        zcl_bw_avail_st1=>put_message( in_mess = |{ lx_sql->sql_message }| in_messtype =  'E').
    ENDTRY.
  ENDMETHOD.


  METHOD unload_t.
    TRY.
        DATA(l_row_cnt) = lo_statement->execute_update( |UNLOAD { lc_hana_ee_schema }.{ lv_tname };| ).
        zcl_bw_avail_st1=>put_message( |Unloading of table { lc_hana_ee_schema }.{ lv_tname } was successful| ).
      CATCH cx_sql_exception INTO lx_sql.
        zcl_bw_avail_st1=>put_message( in_mess = |Unloading of  table { lc_hana_ee_schema }.{ lv_tname } got error - { lx_sql->sql_code }:{ lx_sql->get_text( ) }| ).
        zcl_bw_avail_st1=>put_message( in_mess = |{ lx_sql->sql_message }| in_messtype =  'E').
    ENDTRY.
  ENDMETHOD.


  METHOD truncate_t.
    TRY.
        zcl_bw_avail_st1=>put_message( |Truncation of table was disables temporarily | ).
        DATA(l_row_cnt) = lo_statement->execute_update( |TRUNCATE TABLE { lc_hana_ee_schema }.{ lv_tname };| ).
        zcl_bw_avail_st1=>put_message( |Truncation of the table { lc_hana_ee_schema }.{ lv_tname } was successful| ).
      CATCH cx_sql_exception INTO lx_sql.
        zcl_bw_avail_st1=>put_message( in_mess = |Truncation of table { lc_hana_ee_schema }.{ lv_tname } got error - { lx_sql->sql_code }:{ lx_sql->get_text( ) }| ).
        zcl_bw_avail_st1=>put_message( in_mess = |{ lx_sql->sql_message }| in_messtype =  'E').
    ENDTRY.
  ENDMETHOD.


  METHOD delete_t.
    TRY.
        IF lv_where IS INITIAL.
          zcl_bw_avail_st1=>put_message( | WHERE-condition must be provided. Deleting rows from table { lc_hana_ee_schema }.{ lv_tname } was not done. | ).
        ELSE.
          DATA(l_row_cnt) = lo_statement->execute_update( |DELETE from { lc_hana_ee_schema }.{ lv_tname } WHERE { lv_where };| ).
          zcl_bw_avail_st1=>put_message( |Deleting rows from table { lc_hana_ee_schema }.{ lv_tname } was successful. WHERE-condition was { lv_where }. | ).
        ENDIF.
      CATCH cx_sql_exception INTO lx_sql.
        zcl_bw_avail_st1=>put_message( in_mess = |Deleting rows from table { lc_hana_ee_schema }.{ lv_tname } got error - { lx_sql->sql_code }:{ lx_sql->get_text( ) }| ).
        zcl_bw_avail_st1=>put_message( in_mess = |{ lx_sql->sql_message }| in_messtype =  'E').
    ENDTRY.

  ENDMETHOD.


  METHOD delete_t_local.
    " Method for deleting data in local database schema (SAPBWD|Q|P)
    TRY.
        IF lv_where IS INITIAL.
          zcl_bw_avail_st1=>put_message( | WHERE-condition must be provided. Deleting rows from local table { i_dbschema }.{ lv_tname } was not done. | ).
        ELSE.
          DATA(l_row_cnt) = lo_statement->execute_update( |DELETE FROM "{ i_dbschema }"."{ lv_tname }" WHERE { lv_where };| ).
          zcl_bw_avail_st1=>put_message( |Deleting rows from local table "{ i_dbschema }"."{ lv_tname }" was successful. WHERE-condition was { lv_where }. | ).
        ENDIF.
      CATCH cx_sql_exception INTO lx_sql.
        zcl_bw_avail_st1=>put_message( in_mess = |Deleting rows from local table "{ i_dbschema }"."{ lv_tname }" got error - { lx_sql->sql_code }:{ lx_sql->get_text( ) }| ).
        zcl_bw_avail_st1=>put_message( in_mess = |{ lx_sql->sql_message }| in_messtype =  'E').
    ENDTRY.

  ENDMETHOD.


  METHOD fill_znlso10.
    DATA:
      lx_sql          TYPE REF TO cx_sql_exception,
      lv_dbschema(10) TYPE c.

    CALL FUNCTION 'DB_DBSCHEMA_CURRENT'
      IMPORTING
        dbschema = lv_dbschema.

    DATA(lv_stmt1) = |delete from "BW_REMOTE"."ZNLSO10";|.
    DATA(lv_stmt2) = |insert into "BW_REMOTE"."ZNLSO10" (SELECT cast(period_id as INTEGER) as period_id, calmonth, name FROM "HDB_BW"."{ lv_dbschema }"."/BIC/AZNLSO102" );|.
    DATA(lv_stmt3) = |MERGE DELTA OF "BW_REMOTE"."ZNLSO10";|.
    TRY.
        DATA(lo_conn) = cl_sql_connection=>get_connection( con_name = lc_conn_name ).
        DATA(lo_statement) = lo_conn->create_statement( ).
        DATA(l_row_cnt) = lo_statement->execute_update( lv_stmt1 ).
        l_row_cnt = lo_statement->execute_update( lv_stmt2 ).
        l_row_cnt = lo_statement->execute_update( lv_stmt3 ).
        zcl_bw_avail_st1=>put_message( in_mess = |"BW_REMOTE"."ZNLSO10" updated with new data| ).
      CATCH cx_sql_exception INTO lx_sql.
        zcl_bw_avail_st1=>put_message( in_mess = |Error filling "BW_REMOTE"."ZNLSO10": { lx_sql->sql_message }| in_messtype =  'E').
    ENDTRY.
  ENDMETHOD.


  METHOD get_nielsen8_period_int.
    TYPES:
      BEGIN OF t_period,
        period_from TYPE integer,
        period_to   TYPE integer,
        rn          TYPE integer,
      END OF t_period,
      tt_period TYPE STANDARD TABLE OF t_period.
    DATA:
      lt_period TYPE tt_period,
      lt_res    TYPE REF TO data,
      lx_sql    TYPE REF TO cx_sql_exception.

    DATA(lv_stmt) =
    | SELECT period_from, period_to, row_number() over (order by period_from ASC) AS rn FROM (| &
    | SELECT DISTINCT period_from, period_to FROM ( | &
    | SELECT v1.* | &
    |      , FIRST_VALUE(period_id) OVER (PARTITION BY grp ORDER BY rn ASC ) AS period_from | &
    |      , FIRST_VALUE(period_id) OVER (PARTITION BY grp ORDER BY rn DESC ) AS period_to | &
    |   FROM (SELECT period_id, ROW_NUMBER () OVER (ORDER BY period_id ASC) AS rn | &
    |      , round(ndiv0( ( row_number() over (order by period_id ASC) )-1, { in_grp }),0, ROUND_DOWN) AS grp | &
    |   FROM (SELECT DISTINCT PERIOD_ID  FROM "BW_REMOTE".ZDLNLSO08 )) AS v1 ) AS v2) | &
    |   ORDER BY period_from;|.
    out_pstart = 0.
    out_pend = 0.

    TRY.
        DATA(lo_conn) = cl_sql_connection=>get_connection( con_name = lc_conn_name ).
        DATA(lo_statement) = lo_conn->create_statement( ).
        GET REFERENCE OF lt_period INTO lt_res.
        DATA(l_res_ref) = lo_statement->execute_query( lv_stmt ).
        l_res_ref->set_param_table( lt_res ).
        l_res_ref->next_package( ).
        l_res_ref->close( ).
        IF lt_period IS NOT INITIAL.
          READ TABLE lt_period WITH KEY rn = CONV integer( in_rn ) ASSIGNING FIELD-SYMBOL(<fs_period>).
          IF sy-subrc = 0.
            out_pstart = <fs_period>-period_from.
            out_pend = <fs_period>-period_to.
          ENDIF.
        ENDIF.
      CATCH cx_sql_exception INTO lx_sql.
        zcl_bw_avail_st1=>put_message( in_mess = |Error getting period interval: { lx_sql->sql_message }| in_messtype =  'E').
    ENDTRY.
  ENDMETHOD.
ENDCLASS.