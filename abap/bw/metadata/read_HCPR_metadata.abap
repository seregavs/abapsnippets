*&---------------------------------------------------------------------*
*& Report ZHCPRCVDOC2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhcprcvdoc2.

TYPES:
  BEGIN OF tt_adso,
    adso_name TYPE char45,
  END OF tt_adso.

DATA:
  lo_conn      TYPE REF TO cl_sql_connection,
  lo_statement TYPE REF TO cl_sql_statement,
  lx_sql       TYPE REF TO cx_sql_exception,
  lt_res       TYPE REF TO data,
  lv_cv        TYPE c LENGTH 100,
  lt_adso      TYPE TABLE OF tt_adso,
  lt_adso1     TYPE TABLE OF tt_adso,
  ls_adso      LIKE LINE OF lt_adso,
  i_dbschema   TYPE char20,
  lv_datefrom  TYPE sy-datum,
  lv_dateto    TYPE sy-datum,
  lv_datedtp   TYPE sy-datum,
  lr_header    TYPE REF TO cl_salv_form_layout_grid,
  lr_grid      TYPE REF TO cl_salv_form_layout_grid,
  lr_salv_alv  TYPE REF TO cl_salv_table,
  lt_cv_adso   TYPE TABLE OF zhcprcvtab2,
  ls_cv_adso   LIKE LINE OF lt_cv_adso,
  lt_excl_adso TYPE TABLE OF zhcprcvtab3,
  lt_rep_adso  TYPE TABLE OF tt_adso,
  ls_tab       TYPE zhcprcvtab2,
  timestamp_in TYPE timestamp,
  c_v          TYPE tvarv_val,
  lv_per       TYPE i.

*список адсо, которые содержатся в композитах
SELECT DISTINCT adso_name
  INTO TABLE lt_adso
  FROM zhcprcvtab
  WHERE adso_name <> ''.
SORT lt_adso.
DELETE ADJACENT DUPLICATES FROM lt_adso.
CHECK lt_adso IS NOT INITIAL.

*список композит - CV - ADSO
SELECT DISTINCT *
  INTO CORRESPONDING FIELDS OF TABLE lt_cv_adso
  FROM zhcprcvtab
  WHERE adso_name <> ''.
SORT lt_cv_adso.
DELETE ADJACENT DUPLICATES FROM lt_cv_adso.

*ADSO исключения
SELECT DISTINCT *
  INTO CORRESPONDING FIELDS OF TABLE lt_excl_adso
  FROM zhcprcvtab3
  WHERE adso_name <> ''.

*Отчеты на ADSO
SELECT DISTINCT r~infocube AS adso_name
  INTO CORRESPONDING FIELDS OF TABLE lt_rep_adso
  FROM rsrrepdir AS r
  JOIN rsoadso AS a ON a~adsonm = r~infocube
  WHERE a~objvers = 'A' AND r~objvers = 'A' AND r~objstat = 'ACT' AND r~infocube <> '' AND r~compuid <> r~compid.
LOOP AT lt_rep_adso ASSIGNING FIELD-SYMBOL(<fs_rep_adso>).
  ls_cv_adso-mandt = sy-mandt.
  ls_cv_adso-hcprnm = <fs_rep_adso>-adso_name.
  ls_cv_adso-view_name = <fs_rep_adso>-adso_name.
  ls_cv_adso-adso_name = <fs_rep_adso>-adso_name.
  APPEND ls_cv_adso TO lt_cv_adso.
  ls_adso-adso_name = <fs_rep_adso>-adso_name.
  APPEND ls_adso TO lt_adso.
ENDLOOP.

LOOP AT lt_excl_adso ASSIGNING FIELD-SYMBOL(<fs_excl_adso>).
*<<<<<<< SLAREPORT-162
  DELETE lt_cv_adso WHERE  adso_name = <fs_excl_adso>-adso_name AND hcprnm = <fs_excl_adso>-hcprnm.
*-------
*  DELETE lt_cv_adso WHERE  adso_name = <fs_excl_adso>-adso_name.
*>>>>>>> SLAREPORT-162
ENDLOOP.

*************************************************************

** выбор adso, которые грузились последние 40 дней и у них по 1 реквесту
CALL FUNCTION 'DB_DBSCHEMA_CURRENT' IMPORTING dbschema = i_dbschema.
lv_datefrom = sy-datum - 40.
lv_dateto = sy-datum.

CLEAR: lt_res.
DATA(lv_stmt2) =
| SELECT distinct "DATATARGET" from ( | &
| SELECT count(distinct "REQUEST_TSN") as cnt_rqst, "DATATARGET", "SYST_DATE" | &
| FROM  "{ i_dbschema }"."RSPMREQUEST" | &
| WHERE "DATATARGET" in (select distinct "ADSO_NAME" from "{ i_dbschema }"."ZHCPRCVTAB" where "ADSO_NAME" <> '' ) and "REQUEST_STATUS" <> 'D' and "STORAGE" = 'AQ' and "SYST_DATE" between '{ lv_datefrom }' and '{ lv_dateto }' | &
| GROUP BY "SYST_DATE", "DATATARGET" ) | &
| WHERE cnt_rqst = 1 |.

TRY.
    lo_conn = cl_sql_connection=>get_connection( ).
    lo_statement = lo_conn->create_statement( ).
  CATCH cx_sql_exception INTO lx_sql.
    WRITE: / lx_sql->get_text( ).
    WRITE: / lx_sql->sql_code.
    WRITE: / lx_sql->sql_message.
    RETURN.
ENDTRY.

TRY.
    GET REFERENCE OF lt_adso1 INTO lt_res.
    DATA(l_res_ref) = lo_statement->execute_query( lv_stmt2 ).
    l_res_ref->set_param_table( lt_res ).
    l_res_ref->next_package( ).
    l_res_ref->close( ).
  CATCH cx_sql_exception INTO lx_sql.
    WRITE: / lx_sql->get_text( ).
    WRITE: / lx_sql->sql_code.
    WRITE: / lx_sql->sql_message.
    RETURN.
ENDTRY.
***************************************************************

*последний реквест ADSO c 1 реквестом
IF lt_adso1 IS NOT INITIAL.
  SELECT request_tsn, storage, datatarget, syst_date, syst_time
    FROM rspmrequest
    INTO TABLE @DATA(lt_rqst)
    FOR ALL ENTRIES IN @lt_adso1
    WHERE datatarget = @lt_adso1-adso_name(30).
ENDIF.
SORT lt_rqst DESCENDING BY datatarget request_tsn.
DELETE ADJACENT DUPLICATES FROM lt_rqst COMPARING datatarget.

*проверяем загрузки за последние 40 дней
CALL FUNCTION 'Z_BIW_GET_TVARVC'
  EXPORTING
    in_name   = 'ZHCPRCVDOC2_PERIOD_NUMBER'
    in_type   = 'P'
  IMPORTING
    out_param = c_v.

IF c_v IS INITIAL.
  lv_per = 40. " по умолчанию
ELSE.
  lv_per = c_v. "период анализа загрузок
ENDIF.

lv_datedtp = sy-datum - lv_per.

*все загрузки в адсо
SELECT DISTINCT d~tgt, c~chain_id, c~starttimestamp, c~endtimestamp
  FROM rsbkdtp          AS d
  JOIN v_rspcprocesslog AS c  ON c~variante  = d~dtp
  JOIN rspclogchain     AS lc ON lc~chain_id = c~chain_id
  INTO TABLE @DATA(lt_dtp)
  FOR ALL ENTRIES IN @lt_adso
  WHERE tgt = @lt_adso-adso_name AND d~objvers = 'A' AND c~type = 'DTP_LOAD' AND datum >= @lv_datedtp.

*<<<<<<< SLAREPORT-147
SORT lt_dtp DESCENDING BY tgt endtimestamp.
DELETE ADJACENT DUPLICATES FROM lt_dtp COMPARING tgt.
*-------
*SORT lt_dtp DESCENDING BY tgt chain_id endtimestamp.
*DELETE ADJACENT DUPLICATES FROM lt_dtp COMPARING tgt chain_id.
*>>>>>>> SLAREPORT-147

*заполняем результат
LOOP AT lt_cv_adso ASSIGNING FIELD-SYMBOL(<fs_cv_adso>).
*      для адсо с 1 запросом записываем последний реквест
  READ TABLE lt_adso1 TRANSPORTING NO FIELDS WITH KEY adso_name(30) = <fs_cv_adso>-adso_name(30).
  IF sy-subrc = 0.
    IF <fs_cv_adso>-last_rqst IS INITIAL.
      READ TABLE lt_rqst ASSIGNING FIELD-SYMBOL(<fs_rqst>) WITH KEY datatarget = <fs_cv_adso>-adso_name.
      IF sy-subrc = 0.
        <fs_cv_adso>-last_rqst = <fs_rqst>-request_tsn.
        <fs_cv_adso>-syst_date = <fs_rqst>-syst_date.
        <fs_cv_adso>-syst_time = <fs_rqst>-syst_time.
      ENDIF.
    ENDIF.
  ELSE.
*      для адсо с не 1 запросом записываем время выполнения шага DTP_LOAD в цп
    READ TABLE lt_dtp ASSIGNING FIELD-SYMBOL(<fs_dtp>) WITH KEY          tgt = <fs_cv_adso>-adso_name.
    IF sy-subrc = 0.
*<<<<<<< SLAREPORT-130
      CONVERT TIME STAMP <fs_dtp>-endtimestamp TIME ZONE sy-zonlo INTO DATE <fs_cv_adso>-syst_date TIME <fs_cv_adso>-syst_time.
*-------
*      timestamp_in = <fs_dtp>-datum && <fs_dtp>-zeit.
*      CONVERT TIME STAMP timestamp_in TIME ZONE sy-zonlo INTO DATE <fs_cv_adso>-syst_date TIME <fs_cv_adso>-syst_time.
*>>>>>>> SLAREPORT-130
      <fs_cv_adso>-chain_id  = <fs_dtp>-chain_id.
    ENDIF.
  ENDIF.
ENDLOOP.

*заполняем таблицу ZHCPRCVTAB2
DELETE FROM zhcprcvtab2.
INSERT zhcprcvtab2 FROM TABLE lt_cv_adso.

* ALV
IF lt_cv_adso IS NOT INITIAL.
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lr_salv_alv
        CHANGING
          t_table = lt_cv_adso ).

      CREATE OBJECT lr_grid.
      CREATE OBJECT lr_header.
      lr_salv_alv->set_top_of_list( lr_grid ).
      lr_salv_alv->display( ).
    CATCH cx_root.
  ENDTRY.
ENDIF.