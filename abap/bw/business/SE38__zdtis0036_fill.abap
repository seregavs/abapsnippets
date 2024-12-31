*&---------------------------------------------------------------------*
*& Report ZDTIS_FILL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdtis00361_fill.
PARAMETERS:
  ppgplant TYPE char250_d DEFAULT 'P21-1-569120;P21-569121-9999999',
  ppglo   TYPE /bi0/oipur_group DEFAULT 'P00',
  ppghi   TYPE /bi0/oipur_group DEFAULT 'P20',
  pclear  TYPE c.

CLASS lcl_dtis361 DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING in_pgplant TYPE char250_d
                            in_pglo TYPE /bi0/oipur_group OPTIONAL
                            in_pghi TYPE /bi0/oipur_group OPTIONAL
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
      get_lt_pgplant,
      get_ins_stmt_p0   RETURNING VALUE(o_stmt) TYPE string,
      get_ins_stmt_p1   IMPORTING in_pg         TYPE /bi0/oipur_group RETURNING VALUE(o_stmt) TYPE string,
      get_ins_stmt_p2   IMPORTING in_pg         TYPE /bi0/oipur_group
                                  in_loplant    TYPE /bi0/oimaterial
                                  in_hiplant    TYPE /bi0/oimaterial
                        RETURNING VALUE(o_stmt) TYPE string,
      check_pg_in_table IMPORTING in_pg      TYPE /bi0/oipur_group RETURNING VALUE(o_pg) TYPE char1,
      fill_dtis_pg      IMPORTING in_pg      TYPE /bi0/oipur_group
                                  in_loplant TYPE /bi0/oimaterial OPTIONAL
                                  in_hiplant TYPE /bi0/oimaterial OPTIONAL,
      truncate_dtis261.
ENDCLASS.

CLASS lcl_dtis361 IMPLEMENTATION.
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
    lv_pgplant = in_pgplant.

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

    DATA: lv_date        TYPE d,
          lv_tvarv_month TYPE rvari_val_255.
    SELECT SINGLE low
      FROM tvarvc INTO lv_tvarv_month BYPASSING BUFFER
     WHERE name = 'BONUS_IFRS_MONTH'.

    lv_date = sy-datum.
    lv_date = |{ lv_date(6) }01|.
    lv_date = lv_date - 1.
    IF lv_tvarv_month IS INITIAL.
      lv_month = lv_date(6).
    ELSE.
      lv_month = lv_tvarv_month.
    ENDIF.

    IF in_clear = 'X'.
      me->truncate_dtis261( ).
    ELSE.
      WAIT UP TO 5 SECONDS.
    ENDIF.

  ENDMETHOD.

  METHOD fill_dtis.
    WRITE: / |calmonth={ lv_month }|.
    me->get_lt_pgplant( ).
    LOOP AT lt_pur_group ASSIGNING FIELD-SYMBOL(<fs_pg>).

      IF me->check_pg_in_table( in_pg = <fs_pg>-pur_group ) = 'X'.
        LOOP AT lt_pgplant ASSIGNING FIELD-SYMBOL(<fs_pgplant>).
          me->fill_dtis_pg( in_pg = <fs_pgplant>-pur_group
                            in_loplant = <fs_pgplant>-loplant
                            in_hiplant = <fs_pgplant>-hiplant ).
        ENDLOOP.
      ELSE.
        me->fill_dtis_pg( in_pg = <fs_pg>-pur_group ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_lt_pgplant.
    DATA: lt_t       TYPE STANDARD TABLE OF string,
          ls_pgplant TYPE t_pgplant.

    CLEAR: lt_pgplant.
    SPLIT lv_pgplant AT ';' INTO TABLE lt_t.
    LOOP AT lt_t ASSIGNING FIELD-SYMBOL(<fs_t>).
      CLEAR: ls_pgplant.
      SPLIT <fs_t> AT '-' INTO ls_pgplant-pur_group ls_pgplant-loplant ls_pgplant-hiplant.
      ls_pgplant-loplant = |{ ls_pgplant-loplant WIDTH = 18 ALIGN = RIGHT PAD = '0' }|.
      ls_pgplant-hiplant = |{ ls_pgplant-hiplant WIDTH = 18 ALIGN = RIGHT PAD = '0' }|.
      APPEND ls_pgplant TO lt_pgplant.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_ins_stmt_p0.
    o_stmt = |INSERT INTO "ZDTIS000000361" (| &
              |  ZREQTSN,ZDATAPAKID,ZRECORD, | &
              |  CALMONTH,ZGR_BRAND,APUR_GRP, | &
              |  ZINV_PTY,MATERIAL,LOC_CURRCY, | &
              |  VIRT_STOCK_BONUS_PREV_VOL,STOCK_BONUS_PREV_VOL,ZSIN_STKP_PREV, | &
              |  VIRT_STOCK_PREV,ZMARK_INC,ZVOL_BON, | &
              |  ZSIN_STKU,VIRT_STOCK_BONUS_VOL,VIRT_STOCK, | &
              |  STOCK_BONUS_VOL,ZSIN_STKP,VENDOR, | &
              |  VIRT_STOCK_BONUS_PREV_MARK,STOCK_BONUS_PREV_MARK,VIRT_STOCK_BONUS_MARK, | &
              |  STOCK_BONUS_MARK,STOCK_BONUS,VIRT_STOCK_BONUS, | &
              |  VIRT_STOCK_BONUS_PREV,STOCK_BONUS_PREV,ZMARK_INC_PREV, | &
              |  ZVOL_BON_PREV,VIRT_IFRS_VOL,IFRS_VOL, | &
              |  REAL_IFRS_VOL,VIRT_IFRS_MARK,REAL_IFRS_MARK, | &
              |  IFRS_MARK,IFRS,VIRT_IFRS, | &
              |  REAL_IFRS,IFRS_CORR_ALLOCATED,PLANT, | &
              |  EMD_FLAG) | &
*            |(SELECT { lv_ts } as ZREQTSN, to_integer(row_number() over () / { lv_dpsize } + 1) as ZDATAPAKID, | &
*            | mod (  row_number() over (), { lv_dpsize })+1 as ZRECORD, | &
*            | IFNULL(CALMONTH,char(0)) as CALMONTH, | &
*            | IFNULL(ZGR_BRAND,char(0)) as ZGR_BRAND,| &
*            | IFNULL(APUR_GRP,char(0)) as APUR_GRP,| &
*            | IFNULL(ZINV_PTY,char(0)) as ZINV_PTY,| &
*            | IFNULL(MATERIAL,char(0)) as MATERIAL,| &
*            | IFNULL(LOC_CURRCY, char(0)) as LOC_CURRCY,| &
*            | IFNULL(SUM(VIRT_STOCK_BONUS_PREV_VOL),0) AS VIRT_STOCK_BONUS_PREV_VOL, | &
*            | IFNULL(SUM(STOCK_BONUS_PREV_VOL),0) as STOCK_BONUS_PREV_VOL, | &
*            | IFNULL(SUM(ZSIN_STKP_PREV),0) as ZSIN_STKP_PREV, | &
*            | IFNULL(SUM(VIRT_STOCK_PREV),0) as VIRT_STOCK_PREV, | &
*            | IFNULL(SUM(ZMARK_INC),0) as ZMARK_INC, | &
*            | IFNULL(SUM(ZVOL_BON),0) as ZVOL_BON, | &
*            | IFNULL(SUM(ZSIN_STKU),0) as ZSIN_STKU, | &
*            | IFNULL(SUM(VIRT_STOCK_BONUS_VOL),0) as VIRT_STOCK_BONUS_VOL, | &
*            | IFNULL(SUM(VIRT_STOCK),0) as VIRT_STOCK, | &
*            | IFNULL(SUM(STOCK_BONUS_VOL),0) as STOCK_BONUS_VOL, | &
*            | IFNULL(SUM(ZSIN_STKP),0) as ZSIN_STKP, | &
*            | IFNULL(VENDOR,char(0)) as VENDOR,| &
*            | IFNULL(SUM(VIRT_STOCK_BONUS_PREV_MARK),0) as VIRT_STOCK_BONUS_PREV_MARK, | &
*            | IFNULL(SUM(STOCK_BONUS_PREV_MARK),0) as STOCK_BONUS_PREV_MARK, | &
*            | IFNULL(SUM(VIRT_STOCK_BONUS_MARK),0) as IRT_STOCK_BONUS_MARK, | &
*            | IFNULL(SUM(STOCK_BONUS_MARK),0) as STOCK_BONUS_MARK, | &
*            | IFNULL(SUM(STOCK_BONUS),0) as STOCK_BONUS, | &
*            | IFNULL(SUM(VIRT_STOCK_BONUS),0) as VIRT_STOCK_BONUS, | &
*            | IFNULL(SUM(VIRT_STOCK_BONUS_PREV),0) as VIRT_STOCK_BONUS_PREV, | &
*            | IFNULL(SUM(STOCK_BONUS_PREV),0) as STOCK_BONUS_PREV, | &
*            | IFNULL(SUM(ZMARK_INC_PREV),0) as ZMARK_INC_PREV, | &
*            | IFNULL(SUM(ZVOL_BON_PREV),0) as ZVOL_BON_PREV, | &
*            | IFNULL(SUM(VIRT_IFRS_VOL),0) as VIRT_IFRS_VOL, | &
*            | IFNULL(SUM(IFRS_VOL),0) as IFRS_VOL, | &
*            | IFNULL(SUM(REAL_IFRS_VOL),0) as REAL_IFRS_VOL, | &
*            | IFNULL(SUM(VIRT_IFRS_MARK),0) as VIRT_IFRS_MARK, | &
*            | IFNULL(SUM(REAL_IFRS_MARK),0) as REAL_IFRS_MARK, | &
*            | IFNULL(SUM(IFRS_MARK),0) as IFRS_MARK, | &
*            | IFNULL(SUM(IFRS),0) as IFRS, | &
*            | IFNULL(SUM(VIRT_IFRS),0) as VIRT_IFRS, | &
*            | IFNULL(SUM(REAL_IFRS),0) as REAL_IFRS, | &
*            | IFNULL(SUM(IFRS_CORR_ALLOCATED),0) as IFRS_CORR_ALLOCATED, | &
*            | IFNULL(PLANT,char(0)) as PLANT, | &
*            | IFNULL(EMD_FLAG, char(0)) as EMD_FLAG | &
*            |FROM "_SYS_BIC"."lenta.bw.co.bonuses/IFRS_CALC_CV03"('PLACEHOLDER' = ('$$I_CALMONTH$$', '{ lv_month }') | &
*            |   ,'PLACEHOLDER' = ('$$I_PUR_GRP$$','''{ <fs_pg>-pur_group }''')  )  | &
*            |GROUP BY "ZGR_BRAND", "APUR_GRP", "LOC_CURRCY", "VENDOR", "PLANT", "EMD_FLAG", "ZINV_PTY", "MATERIAL",  "CALMONTH");|.
              |(SELECT { lv_ts } as ZREQTSN, to_integer(row_number() over () / { lv_dpsize } + 1) as ZDATAPAKID, | &
              | mod (  row_number() over (), { lv_dpsize })+1 as ZRECORD, | &
              | IFNULL(CALMONTH,char(0)) as CALMONTH, | &
              | IFNULL(ZGR_BRAND,char(0)) as ZGR_BRAND,| &
              | IFNULL(APUR_GRP,char(0)) as APUR_GRP,| &
              | IFNULL(ZINV_PTY,char(0)) as ZINV_PTY,| &
              | IFNULL(MATERIAL,char(0)) as MATERIAL,| &
              | IFNULL(LOC_CURRCY, char(0)) as LOC_CURRCY,| &
              | IFNULL(VIRT_STOCK_BONUS_PREV_VOL,(0)) AS VIRT_STOCK_BONUS_PREV_VOL, | &
              | IFNULL(STOCK_BONUS_PREV_VOL,(0)) as STOCK_BONUS_PREV_VOL, | &
              | IFNULL(ZSIN_STKP_PREV,(0)) as ZSIN_STKP_PREV, | &
              | IFNULL(VIRT_STOCK_PREV,(0)) as VIRT_STOCK_PREV, | &
              | IFNULL(ZMARK_INC,(0)) as ZMARK_INC, | &
              | IFNULL(ZVOL_BON,(0)) as ZVOL_BON, | &
              | IFNULL(ZSIN_STKU,(0)) as ZSIN_STKU, | &
              | IFNULL(VIRT_STOCK_BONUS_VOL,(0)) as VIRT_STOCK_BONUS_VOL, | &
              | IFNULL(VIRT_STOCK,(0)) as VIRT_STOCK, | &
              | IFNULL(STOCK_BONUS_VOL,(0)) as STOCK_BONUS_VOL, | &
              | IFNULL(ZSIN_STKP,(0)) as ZSIN_STKP, | &
              | IFNULL(VENDOR,char(0)) as VENDOR,| &
              | IFNULL(VIRT_STOCK_BONUS_PREV_MARK,(0)) as VIRT_STOCK_BONUS_PREV_MARK, | &
              | IFNULL(STOCK_BONUS_PREV_MARK,(0)) as STOCK_BONUS_PREV_MARK, | &
              | IFNULL(VIRT_STOCK_BONUS_MARK,(0)) as VIRT_STOCK_BONUS_MARK, | &
              | IFNULL(STOCK_BONUS_MARK,(0)) as STOCK_BONUS_MARK, | &
              | IFNULL(STOCK_BONUS,(0)) as STOCK_BONUS, | &
              | IFNULL(VIRT_STOCK_BONUS,(0)) as VIRT_STOCK_BONUS, | &
              | IFNULL(VIRT_STOCK_BONUS_PREV,(0)) as VIRT_STOCK_BONUS_PREV, | &
              | IFNULL(STOCK_BONUS_PREV,(0)) as STOCK_BONUS_PREV, | &
              | IFNULL(ZMARK_INC_PREV,(0)) as ZMARK_INC_PREV, | &
              | IFNULL(ZVOL_BON_PREV,(0)) as ZVOL_BON_PREV, | &
              | IFNULL(VIRT_IFRS_VOL,(0)) as VIRT_IFRS_VOL, | &
              | IFNULL(IFRS_VOL,(0)) as IFRS_VOL, | &
              | IFNULL(REAL_IFRS_VOL,(0)) as REAL_IFRS_VOL, | &
              | IFNULL(VIRT_IFRS_MARK,(0)) as VIRT_IFRS_MARK, | &
              | IFNULL(REAL_IFRS_MARK,(0)) as REAL_IFRS_MARK, | &
              | IFNULL(IFRS_MARK,(0)) as IFRS_MARK, | &
              | IFNULL(IFRS,(0)) as IFRS, | &
              | IFNULL(VIRT_IFRS,(0)) as VIRT_IFRS, | &
              | IFNULL(REAL_IFRS,(0)) as REAL_IFRS, | &
              | IFNULL(IFRS_CORR_ALLOCATED,(0)) as IFRS_CORR_ALLOCATED, | &
              | IFNULL(PLANT,char(0)) as PLANT, | &
              | IFNULL(EMD_FLAG, '') as EMD_FLAG | &
              |FROM "_SYS_BIC"."lenta.bw.co.bonuses/IFRS_CALC_CV03"('PLACEHOLDER' = ('$$I_CALMONTH$$', '{ lv_month }') |.
  ENDMETHOD.

  METHOD get_ins_stmt_p1.
    o_stmt = me->get_ins_stmt_p0( ).
    o_stmt = |{ o_stmt } ,'PLACEHOLDER' = ('$$I_PUR_GRP$$','''{ in_pg }''')))|.
  ENDMETHOD.

  METHOD get_ins_stmt_p2.
    o_stmt = me->get_ins_stmt_p0( ).
    o_stmt = |{ o_stmt } ,'PLACEHOLDER' = ('$$I_PUR_GRP$$','''{ in_pg }'''))|.
    o_stmt = |{ o_stmt } WHERE material BETWEEN '{ in_loplant }' AND '{ in_hiplant }');|.
  ENDMETHOD.

  METHOD check_pg_in_table.
    o_pg = ''.
    READ TABLE lt_pgplant WITH KEY pur_group = in_pg TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      o_pg = 'X'.
    ENDIF.
  ENDMETHOD.

  METHOD fill_dtis_pg.
    GET TIME STAMP FIELD lv_ts.
    IF in_loplant = '' OR in_hiplant = ''.
      DATA(lv_stmt) = me->get_ins_stmt_p1( in_pg = in_pg ).
    ELSE.
      lv_stmt = me->get_ins_stmt_p2( in_pg = in_pg in_loplant = in_loplant in_hiplant = in_hiplant ).
      DATA(lv_lmat) = in_loplant.
      DATA(lv_hmat) = in_hiplant.
      SHIFT lv_lmat LEFT DELETING LEADING '0'.
      SHIFT lv_hmat LEFT DELETING LEADING '0'.
    ENDIF.

    TRY.
        DATA(l_row_cnt) = lo_statement->execute_update( lv_stmt ).
        DATA(lv_mess) = |ts={ lv_ts }, pur_group={ in_pg }, mat=({ lv_lmat },{ lv_hmat }), rows={ l_row_cnt }|.
        WRITE: / lv_mess.
        MESSAGE lv_mess TYPE 'S'.
        COMMIT WORK.
      CATCH cx_sql_exception INTO lx_sql.
        WRITE: / |pur_group { in_pg }, mat=({ lv_lmat },{ lv_hmat }), - { lx_sql->get_text( ) }|.
        WRITE: / |calmonth { lv_month } - { lx_sql->sql_code }|.
        WRITE: / lx_sql->sql_message.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD truncate_dtis261.
    TRY.
        DATA(l_row_cnt) = lo_statement->execute_update( |TRUNCATE TABLE zdtis000000361;| ).
      CATCH cx_sql_exception INTO lx_sql.
        WRITE: / |truncation of table zdtis000000361 got error - { lx_sql->get_text( ) }|.
        WRITE: / |truncation of table zdtis000000361 got error - { lv_month } - { lx_sql->sql_code }|.
        WRITE: / lx_sql->sql_message.
        WRITE: / |Delete started from ABAP command..'|.
        DELETE FROM zdtis000000361.
        WRITE: / |Delete ended|.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  NEW lcl_dtis361( in_pgplant = ppgplant
                   in_pglo = ppglo
                   in_pghi = ppghi
                   in_clear = pclear )->fill_dtis( ).

END-OF-SELECTION.

*  DATA:
*    lo_conn      TYPE REF TO cl_sql_connection,
*    lo_statement TYPE REF TO cl_sql_statement,
*    lx_sql       TYPE REF TO cx_sql_exception,
*    lt_res       TYPE REF TO data.
*  DATA(lv_dpsize) = 1000000.
*  GET TIME STAMP FIELD DATA(lv_ts).
*
*  TYPES:
*    BEGIN OF t_pur_group,
*      pur_group TYPE /bi0/oipur_group,
*    END OF t_pur_group,
*    tt_pur_group TYPE STANDARD TABLE OF t_pur_group.
*  DATA: lt_pur_group TYPE tt_pur_group.
*
*  DATA(lv_stmt) =
*    |SELECT DISTINCT PUR_GROUP as SPLIT_VALUE FROM "/BI0/PPUR_GROUP" | &
*    | WHERE PUR_GROUP LIKE_REGEXPR 'P[0-9][0-9]' | .
*  TRY.
*      GET REFERENCE OF lt_pur_group INTO lt_res.
*      lo_conn = cl_sql_connection=>get_connection( ).
*      lo_statement = lo_conn->create_statement( ).
*      DATA(l_res_ref) = lo_statement->execute_query( lv_stmt ).
*      l_res_ref->set_param_table( lt_res ).
*      l_res_ref->next_package( ).
*      l_res_ref->close( ).
*
*    CATCH cx_sql_exception INTO lx_sql.
*      WRITE: / lx_sql->get_text( ).
*      WRITE: / lx_sql->sql_code.
*      WRITE: / lx_sql->sql_message.
*      RETURN.
*  ENDTRY.
*
*  DATA: lv_date        TYPE d,
*        lv_tvarv_month TYPE rvari_val_255,
*        lv_month(6)    TYPE c.
*  SELECT SINGLE low
*    FROM tvarvc INTO lv_tvarv_month BYPASSING BUFFER
*   WHERE name = 'BONUS_IFRS_MONTH'.
*
*  lv_date = sy-datum.
*  lv_date = |{ lv_date(6) }01|.
*  lv_date = lv_date - 1.
*  IF lv_tvarv_month IS INITIAL.
*    lv_month = lv_date(6).
*  ELSE.
*    lv_month = lv_tvarv_month.
*  ENDIF.

*  TRY.
*      lo_conn = cl_sql_connection=>get_connection( ).
*      lo_statement = lo_conn->create_statement( ).
*      DATA(l_row_cnt) = lo_statement->execute_update( |TRUNCATE TABLE zdtis000000361;| ).
*    CATCH cx_sql_exception INTO lx_sql.
*      WRITE: / |truncation of table zdtis000000361 got error - { lx_sql->get_text( ) }|.
*      WRITE: / |truncation of table zdtis000000361 got error - { lv_month } - { lx_sql->sql_code }|.
*      WRITE: / lx_sql->sql_message.
*      WRITE: / |Delete started from ABAP command..'|.
*      DELETE FROM zdtis000000361.
*      WRITE: / |Delete ended'|.
*      WRITE: / |Delete ended|.
*  ENDTRY.

*  WRITE: / |calmonth={ lv_month }|.
*  LOOP AT lt_pur_group ASSIGNING FIELD-SYMBOL(<fs_pg>).
*    GET TIME STAMP FIELD lv_ts.
*    lv_stmt = |INSERT INTO "ZDTIS000000361" (| &
*              |  ZREQTSN,ZDATAPAKID,ZRECORD, | &
*              |  CALMONTH,ZGR_BRAND,APUR_GRP, | &
*              |  ZINV_PTY,MATERIAL,LOC_CURRCY, | &
*              |  VIRT_STOCK_BONUS_PREV_VOL,STOCK_BONUS_PREV_VOL,ZSIN_STKP_PREV, | &
*              |  VIRT_STOCK_PREV,ZMARK_INC,ZVOL_BON, | &
*              |  ZSIN_STKU,VIRT_STOCK_BONUS_VOL,VIRT_STOCK, | &
*              |  STOCK_BONUS_VOL,ZSIN_STKP,VENDOR, | &
*              |  VIRT_STOCK_BONUS_PREV_MARK,STOCK_BONUS_PREV_MARK,VIRT_STOCK_BONUS_MARK, | &
*              |  STOCK_BONUS_MARK,STOCK_BONUS,VIRT_STOCK_BONUS, | &
*              |  VIRT_STOCK_BONUS_PREV,STOCK_BONUS_PREV,ZMARK_INC_PREV, | &
*              |  ZVOL_BON_PREV,VIRT_IFRS_VOL,IFRS_VOL, | &
*              |  REAL_IFRS_VOL,VIRT_IFRS_MARK,REAL_IFRS_MARK, | &
*              |  IFRS_MARK,IFRS,VIRT_IFRS, | &
*              |  REAL_IFRS,IFRS_CORR_ALLOCATED,PLANT, | &
*              |  EMD_FLAG) | &
**            |(SELECT { lv_ts } as ZREQTSN, to_integer(row_number() over () / { lv_dpsize } + 1) as ZDATAPAKID, | &
**            | mod (  row_number() over (), { lv_dpsize })+1 as ZRECORD, | &
**            | IFNULL(CALMONTH,char(0)) as CALMONTH, | &
**            | IFNULL(ZGR_BRAND,char(0)) as ZGR_BRAND,| &
**            | IFNULL(APUR_GRP,char(0)) as APUR_GRP,| &
**            | IFNULL(ZINV_PTY,char(0)) as ZINV_PTY,| &
**            | IFNULL(MATERIAL,char(0)) as MATERIAL,| &
**            | IFNULL(LOC_CURRCY, char(0)) as LOC_CURRCY,| &
**            | IFNULL(SUM(VIRT_STOCK_BONUS_PREV_VOL),0) AS VIRT_STOCK_BONUS_PREV_VOL, | &
**            | IFNULL(SUM(STOCK_BONUS_PREV_VOL),0) as STOCK_BONUS_PREV_VOL, | &
**            | IFNULL(SUM(ZSIN_STKP_PREV),0) as ZSIN_STKP_PREV, | &
**            | IFNULL(SUM(VIRT_STOCK_PREV),0) as VIRT_STOCK_PREV, | &
**            | IFNULL(SUM(ZMARK_INC),0) as ZMARK_INC, | &
**            | IFNULL(SUM(ZVOL_BON),0) as ZVOL_BON, | &
**            | IFNULL(SUM(ZSIN_STKU),0) as ZSIN_STKU, | &
**            | IFNULL(SUM(VIRT_STOCK_BONUS_VOL),0) as VIRT_STOCK_BONUS_VOL, | &
**            | IFNULL(SUM(VIRT_STOCK),0) as VIRT_STOCK, | &
**            | IFNULL(SUM(STOCK_BONUS_VOL),0) as STOCK_BONUS_VOL, | &
**            | IFNULL(SUM(ZSIN_STKP),0) as ZSIN_STKP, | &
**            | IFNULL(VENDOR,char(0)) as VENDOR,| &
**            | IFNULL(SUM(VIRT_STOCK_BONUS_PREV_MARK),0) as VIRT_STOCK_BONUS_PREV_MARK, | &
**            | IFNULL(SUM(STOCK_BONUS_PREV_MARK),0) as STOCK_BONUS_PREV_MARK, | &
**            | IFNULL(SUM(VIRT_STOCK_BONUS_MARK),0) as IRT_STOCK_BONUS_MARK, | &
**            | IFNULL(SUM(STOCK_BONUS_MARK),0) as STOCK_BONUS_MARK, | &
**            | IFNULL(SUM(STOCK_BONUS),0) as STOCK_BONUS, | &
**            | IFNULL(SUM(VIRT_STOCK_BONUS),0) as VIRT_STOCK_BONUS, | &
**            | IFNULL(SUM(VIRT_STOCK_BONUS_PREV),0) as VIRT_STOCK_BONUS_PREV, | &
**            | IFNULL(SUM(STOCK_BONUS_PREV),0) as STOCK_BONUS_PREV, | &
**            | IFNULL(SUM(ZMARK_INC_PREV),0) as ZMARK_INC_PREV, | &
**            | IFNULL(SUM(ZVOL_BON_PREV),0) as ZVOL_BON_PREV, | &
**            | IFNULL(SUM(VIRT_IFRS_VOL),0) as VIRT_IFRS_VOL, | &
**            | IFNULL(SUM(IFRS_VOL),0) as IFRS_VOL, | &
**            | IFNULL(SUM(REAL_IFRS_VOL),0) as REAL_IFRS_VOL, | &
**            | IFNULL(SUM(VIRT_IFRS_MARK),0) as VIRT_IFRS_MARK, | &
**            | IFNULL(SUM(REAL_IFRS_MARK),0) as REAL_IFRS_MARK, | &
**            | IFNULL(SUM(IFRS_MARK),0) as IFRS_MARK, | &
**            | IFNULL(SUM(IFRS),0) as IFRS, | &
**            | IFNULL(SUM(VIRT_IFRS),0) as VIRT_IFRS, | &
**            | IFNULL(SUM(REAL_IFRS),0) as REAL_IFRS, | &
**            | IFNULL(SUM(IFRS_CORR_ALLOCATED),0) as IFRS_CORR_ALLOCATED, | &
**            | IFNULL(PLANT,char(0)) as PLANT, | &
**            | IFNULL(EMD_FLAG, char(0)) as EMD_FLAG | &
**            |FROM "_SYS_BIC"."lenta.bw.co.bonuses/IFRS_CALC_CV03"('PLACEHOLDER' = ('$$I_CALMONTH$$', '{ lv_month }') | &
**            |   ,'PLACEHOLDER' = ('$$I_PUR_GRP$$','''{ <fs_pg>-pur_group }''')  )  | &
**            |GROUP BY "ZGR_BRAND", "APUR_GRP", "LOC_CURRCY", "VENDOR", "PLANT", "EMD_FLAG", "ZINV_PTY", "MATERIAL",  "CALMONTH");|.
*              |(SELECT { lv_ts } as ZREQTSN, to_integer(row_number() over () / { lv_dpsize } + 1) as ZDATAPAKID, | &
*              | mod (  row_number() over (), { lv_dpsize })+1 as ZRECORD, | &
*              | IFNULL(CALMONTH,char(0)) as CALMONTH, | &
*              | IFNULL(ZGR_BRAND,char(0)) as ZGR_BRAND,| &
*              | IFNULL(APUR_GRP,char(0)) as APUR_GRP,| &
*              | IFNULL(ZINV_PTY,char(0)) as ZINV_PTY,| &
*              | IFNULL(MATERIAL,char(0)) as MATERIAL,| &
*              | IFNULL(LOC_CURRCY, char(0)) as LOC_CURRCY,| &
*              | IFNULL(VIRT_STOCK_BONUS_PREV_VOL,(0)) AS VIRT_STOCK_BONUS_PREV_VOL, | &
*              | IFNULL(STOCK_BONUS_PREV_VOL,(0)) as STOCK_BONUS_PREV_VOL, | &
*              | IFNULL(ZSIN_STKP_PREV,(0)) as ZSIN_STKP_PREV, | &
*              | IFNULL(VIRT_STOCK_PREV,(0)) as VIRT_STOCK_PREV, | &
*              | IFNULL(ZMARK_INC,(0)) as ZMARK_INC, | &
*              | IFNULL(ZVOL_BON,(0)) as ZVOL_BON, | &
*              | IFNULL(ZSIN_STKU,(0)) as ZSIN_STKU, | &
*              | IFNULL(VIRT_STOCK_BONUS_VOL,(0)) as VIRT_STOCK_BONUS_VOL, | &
*              | IFNULL(VIRT_STOCK,(0)) as VIRT_STOCK, | &
*              | IFNULL(STOCK_BONUS_VOL,(0)) as STOCK_BONUS_VOL, | &
*              | IFNULL(ZSIN_STKP,(0)) as ZSIN_STKP, | &
*              | IFNULL(VENDOR,char(0)) as VENDOR,| &
*              | IFNULL(VIRT_STOCK_BONUS_PREV_MARK,(0)) as VIRT_STOCK_BONUS_PREV_MARK, | &
*              | IFNULL(STOCK_BONUS_PREV_MARK,(0)) as STOCK_BONUS_PREV_MARK, | &
*              | IFNULL(VIRT_STOCK_BONUS_MARK,(0)) as VIRT_STOCK_BONUS_MARK, | &
*              | IFNULL(STOCK_BONUS_MARK,(0)) as STOCK_BONUS_MARK, | &
*              | IFNULL(STOCK_BONUS,(0)) as STOCK_BONUS, | &
*              | IFNULL(VIRT_STOCK_BONUS,(0)) as VIRT_STOCK_BONUS, | &
*              | IFNULL(VIRT_STOCK_BONUS_PREV,(0)) as VIRT_STOCK_BONUS_PREV, | &
*              | IFNULL(STOCK_BONUS_PREV,(0)) as STOCK_BONUS_PREV, | &
*              | IFNULL(ZMARK_INC_PREV,(0)) as ZMARK_INC_PREV, | &
*              | IFNULL(ZVOL_BON_PREV,(0)) as ZVOL_BON_PREV, | &
*              | IFNULL(VIRT_IFRS_VOL,(0)) as VIRT_IFRS_VOL, | &
*              | IFNULL(IFRS_VOL,(0)) as IFRS_VOL, | &
*              | IFNULL(REAL_IFRS_VOL,(0)) as REAL_IFRS_VOL, | &
*              | IFNULL(VIRT_IFRS_MARK,(0)) as VIRT_IFRS_MARK, | &
*              | IFNULL(REAL_IFRS_MARK,(0)) as REAL_IFRS_MARK, | &
*              | IFNULL(IFRS_MARK,(0)) as IFRS_MARK, | &
*              | IFNULL(IFRS,(0)) as IFRS, | &
*              | IFNULL(VIRT_IFRS,(0)) as VIRT_IFRS, | &
*              | IFNULL(REAL_IFRS,(0)) as REAL_IFRS, | &
*              | IFNULL(IFRS_CORR_ALLOCATED,(0)) as IFRS_CORR_ALLOCATED, | &
*              | IFNULL(PLANT,char(0)) as PLANT, | &
*              | IFNULL(EMD_FLAG, char(0)) as EMD_FLAG | &
*              |FROM "_SYS_BIC"."lenta.bw.co.bonuses/IFRS_CALC_CV03"('PLACEHOLDER' = ('$$I_CALMONTH$$', '{ lv_month }') | &
*              |   ,'PLACEHOLDER' = ('$$I_PUR_GRP$$','''{ <fs_pg>-pur_group }''')  ));|.
*    TRY.
*        lo_conn = cl_sql_connection=>get_connection( ).
*        lo_statement = lo_conn->create_statement( ).
*        DATA(l_row_cnt) = lo_statement->execute_update( lv_stmt ).
*        WRITE: / |{ sy-tabix } ts={ lv_ts }, pur_group={ <fs_pg>-pur_group }, rows={ l_row_cnt }|.
*        COMMIT WORK.
*      CATCH cx_sql_exception INTO lx_sql.
*        WRITE: / |pur_group { <fs_pg>-pur_group } - { lx_sql->get_text( ) }|.
*        WRITE: / |calmonth { lv_month } - { lx_sql->sql_code }|.
*        WRITE: / lx_sql->sql_message.
*        RETURN.
*    ENDTRY.
*  ENDLOOP.

  " SELECT DISTINCT PUR_GROUP as SPLIT_VALUE FROM "/BI0/PPUR_GROUP" WHERE PUR_GROUP LIKE_REGEXPR 'P[0-9][0-9]'

* '20221210221010' as REQTSN,
* round(row_number() over () / 10 + 1,0,ROUND_FLOOR) as datapakid,
* mod (  row_number() over (), 10)+1 as record,