*&---------------------------------------------------------------------*
*& Report ZDTIS_AVAIL_FILL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZDTIS_AVAIL_FILL.

PARAMETERS:
  ppglo  TYPE /bi0/oipur_group DEFAULT 'P00',
  ppghi  TYPE /bi0/oipur_group DEFAULT 'P20',
  pclear TYPE c.

CLASS lcl_dtis_avail DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING in_pglo  TYPE /bi0/oipur_group OPTIONAL
                            in_pghi  TYPE /bi0/oipur_group OPTIONAL
                            in_clear TYPE char1,
      fill_dtis.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_pur_group,
        pur_group TYPE /bi0/oipur_group,
      END OF t_pur_group,
      tt_pur_group TYPE STANDARD TABLE OF t_pur_group,
      BEGIN OF t_pgplant,
        pur_group TYPE /bi0/oipur_group,
        loplant   TYPE /bi0/oimaterial,
        hiplant   TYPE /bi0/oimaterial,
      END OF t_pgplant,
      tt_pgplant TYPE STANDARD TABLE OF t_pgplant.
    DATA:
      lo_conn      TYPE REF TO cl_sql_connection,
      lo_statement TYPE REF TO cl_sql_statement,
      lx_sql       TYPE REF TO cx_sql_exception,
      lt_res       TYPE REF TO data,
      lv_month(6)  TYPE c,
      lt_pur_group TYPE tt_pur_group,
      lv_ts        TYPE timestamp,
      lv_pgplant   TYPE char250_d,
      lt_pgplant   TYPE tt_pgplant.
    CONSTANTS:
      lv_dpsize TYPE p VALUE 1000000.

    METHODS:
      fill_dtis_pg      IMPORTING in_pg      TYPE /bi0/oipur_group
                                  in_loplant TYPE /bi0/oimaterial OPTIONAL
                                  in_hiplant TYPE /bi0/oimaterial OPTIONAL,
      get_ins_stmt_p1   IMPORTING in_pg      TYPE /bi0/oipur_group RETURNING VALUE(o_stmt) TYPE string,
      get_ins_stmt_p0   RETURNING VALUE(o_stmt) TYPE string,
      truncate_dtis_avail.



ENDCLASS.

CLASS lcl_dtis_avail IMPLEMENTATION.
  METHOD constructor.
    IF in_pglo <> '' AND in_pghi = ''.
      DATA(lv_where_pg) = | AND pur_group = '{ in_pglo }'|.
    ELSEIF in_pglo <> '' AND in_pghi <> ''.
      lv_where_pg = | AND pur_group BETWEEN '{ in_pglo }' AND '{ in_pghi }'|.
    ENDIF.
    DATA(lv_mess) = |pur_group in ({ in_pglo }, { in_pghi }), truncate_dtis261 = '{ pclear }'|.
    WRITE: / lv_mess.
    MESSAGE lv_mess TYPE 'S'.

    DATA(lv_stmt) =
       |SELECT DISTINCT PUR_GROUP as SPLIT_VALUE FROM "/BI0/PPUR_GROUP" | &
       | WHERE PUR_GROUP LIKE_REGEXPR 'P[0-9][0-9]' { lv_where_pg } ORDER BY 1 ASC| .
    GET TIME STAMP FIELD lv_ts.

    TRY.
        lo_conn = cl_sql_connection=>get_connection( ).
        lo_statement = lo_conn->create_statement( ).
        GET REFERENCE OF lt_pur_group INTO lt_res.
        DATA(l_res_ref) = lo_statement->execute_query( lv_stmt ).
        l_res_ref->set_param_table( lt_res ).
        l_res_ref->next_package( ).
        l_res_ref->close( ).
      CATCH cx_sql_exception INTO lx_sql.
        WRITE: / lx_sql->get_text( ).
        WRITE: / lx_sql->sql_code.
        WRITE: / lx_sql->sql_message.
        RETURN.
    ENDTRY.

    DATA: lv_date        TYPE d.
*          lv_tvarv_month TYPE rvari_val_255.

*  SELECT SINGLE low
*      FROM tvarvc INTO lv_tvarv_month BYPASSING BUFFER
*     WHERE name = 'BONUS_MONTH_CALC'.
*
*    lv_date = sy-datum.
*    lv_date = |{ lv_date(6) }01|.
*    lv_date = lv_date - 1.
*    IF lv_tvarv_month IS INITIAL.
*      lv_month = lv_date(6).
*    ELSE.
*      lv_month = lv_tvarv_month.
*    ENDIF.

      lv_date = sy-datum - 120.
      lv_month = lv_date(6).

    IF in_clear = 'X'.
      me->truncate_dtis_avail( ).
    ELSE.
      WAIT UP TO 5 SECONDS.
    ENDIF.

  ENDMETHOD.

  METHOD fill_dtis.
    WRITE: / |calmonth={ lv_month }|.
    LOOP AT lt_pur_group ASSIGNING FIELD-SYMBOL(<fs_pg>).
      me->fill_dtis_pg( in_pg = <fs_pg>-pur_group ).
    ENDLOOP.
  ENDMETHOD.

  METHOD fill_dtis_pg.
    GET TIME STAMP FIELD lv_ts.
    DATA(lv_stmt) = me->get_ins_stmt_p1( in_pg = in_pg ).
    TRY.
        DATA(l_row_cnt) = lo_statement->execute_update( lv_stmt ).
        DATA(lv_mess) = |ts={ lv_ts }, pur_group={ in_pg }, rows={ l_row_cnt }|.
        WRITE: / lv_mess.
        MESSAGE lv_mess TYPE 'S'.
        COMMIT WORK.
      CATCH cx_sql_exception INTO lx_sql.
        WRITE: / |pur_group { in_pg }, - { lx_sql->get_text( ) }|.
        WRITE: / |calmonth { lv_month } - { lx_sql->sql_code }|.
        WRITE: / lx_sql->sql_message.
        RETURN.
    ENDTRY.
  ENDMETHOD.
  METHOD get_ins_stmt_p1.
    o_stmt = me->get_ins_stmt_p0( ).
    o_stmt = |{ o_stmt })|.
  ENDMETHOD.
  METHOD get_ins_stmt_p0.
    o_stmt = |INSERT INTO "zditisavail_aggr" (| &
              |  ZREQTSN,ZDATAPAKID,ZRECORD, | &
              |  FLAG4, FLAG2, ZBIGMEDIA, | &
              |  ZMATVENDC, AVREASON, CALQUARTER, CALYEAR, | &
              |  RT_PROMO, VENDOR, MAT_PLANT, CALMONTH, | &
              |  MATERIAL, PLANT, COUNTER1_AVG, COUNTER2_AVG, | &
              |  COUNTER3_AVG, COUNTER4_AVG, COUNTER5_AVG, | &
              |  COUNTER6_AVG, COUNTER7_AVG, COUNTER8_AVG, | &
              |  COUNTER9_AVG, VOLSTOCK_AVG, ZSALES6_AVG, | &
              |  ZSALES9_AVG, BASE_UOM, LOC_CURRCY, | &
              |  KOEFF, COUNTER4_AVG_NEW) | &
              |(SELECT { lv_ts } as ZREQTSN, to_integer(row_number() over () / { lv_dpsize } + 1) as ZDATAPAKID, | &
              | mod (  row_number() over (), { lv_dpsize })+1 as ZRECORD, | &

              | IFNULL("MATERIAL",char(0)) as MATERIAL, | &
              | IFNULL("PLANT",char(0)) as MATERIAL, | &
              | IFNULL("BASE_UOM",char(0)) as MATERIAL, | &
              | IFNULL("LOC_CURRCY",char(0)) as MATERIAL, | &
              | IFNULL("CALMONTH",char(0)) as MATERIAL, | &
              | IFNULL("FLAG2",char(0)) as MATERIAL, | &
              | IFNULL("FLAG4",char(0)) as MATERIAL, | &
              | IFNULL("ZBIGMEDIA",char(0)) as MATERIAL, | &
              | IFNULL("ZMATVENDC",char(0)) as MATERIAL, | &
              | IFNULL("CALQUARTER",char(0)) as MATERIAL, | &
              | IFNULL("CALYEAR",char(0)) as MATERIAL, | &
              | IFNULL("VENDOR",char(0)) as MATERIAL, | &
              | IFNULL("AVREASON",char(0)) as MATERIAL, | &
              | IFNULL("COUNTER4_AVG_NEW",char(0)) as MATERIAL, | &
              | IFNULL("COUNTER8",char(0)) as MATERIAL, | &
              | IFNULL("COUNTER9",char(0)) as MATERIAL, | &
              | IFNULL("VOLSTOCK",char(0)) as MATERIAL, | &
              | IFNULL("ZSALES6",char(0)) as MATERIAL, | &
              | IFNULL("ZSALES9",char(0)) as MATERIAL, | &
              | IFNULL("COUNTER1",char(0)) as MATERIAL, | &
              | IFNULL("COUNTER2",char(0)) as MATERIAL, | &
              | IFNULL("COUNTER3",char(0)) as MATERIAL, | &
              | IFNULL("COUNTER4",char(0)) as MATERIAL, | &
              | IFNULL("COUNTER5",char(0)) as MATERIAL, | &
              | IFNULL("COUNTER6",char(0)) as MATERIAL, | &
              | IFNULL("COUNTER1_AVG",char(0)) as MATERIAL, | &
              | IFNULL("COUNTER2_AVG",char(0)) as MATERIAL, | &
              | IFNULL("COUNTER3_AVG",char(0)) as MATERIAL, | &
              | IFNULL("COUNTER4_AVG",char(0)) as MATERIAL, | &
              | IFNULL("COUNTER5_AVG",char(0)) as MATERIAL, | &
              | IFNULL("COUNTER6_AVG",char(0)) as MATERIAL, | &
              | IFNULL("COUNTER7_AVG",char(0)) as MATERIAL, | &
              | IFNULL("COUNTER8_AVG",char(0)) as MATERIAL, | &
              | IFNULL("COUNTER9_AVG",char(0)) as MATERIAL, | &
              | IFNULL("VOLSTOCK_AVG",char(0)) as MATERIAL, | &
              | IFNULL("ZSALES6_AVG",char(0)) as MATERIAL, | &
              | IFNULL("ZSALES9_AVG",char(0)) as MATERIAL, | &
              | IFNULL("KOEFF",char(0)) as MATERIAL | &
              | FROM "_SYS_BIC"."lenta.bw.lspec.log_report.avail/AVALC01_CV04" |.


  ENDMETHOD.
  METHOD truncate_dtis_avail.
    TRY.
        DATA(l_row_cnt) = lo_statement->execute_update( |TRUNCATE TABLE zditisavail_aggr;| ).
      CATCH cx_sql_exception INTO lx_sql.
        WRITE: / |truncation of table zditisavail_aggr got error - { lx_sql->get_text( ) }|.
        WRITE: / |truncation of table zditisavail_aggr  got error - { lv_month } - { lx_sql->sql_code }|.
        WRITE: / lx_sql->sql_message.
        WRITE: / |Delete started from ABAP command..'|.
        DELETE FROM zditisavail_aggr.
        WRITE: / |Delete ended|.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  NEW lcl_dtis_avail(
                   in_pglo = ppglo
                   in_pghi = ppghi
                   in_clear = pclear )->fill_dtis( ).

END-OF-SELECTION.