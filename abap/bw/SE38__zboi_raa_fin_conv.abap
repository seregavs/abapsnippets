*&---------------------------------------------------------------------*
*& Report ZBOI_RAA_FIN_CONV
*&---------------------------------------------------------------------*
*& Report is for uploading currency rates from external table.
*& External table INVESTMENTS.IT_CF_MARKETDATA is used in HANA Calculation View (CV) ZBOI_RAA_EDW.CF/ZCF_R_CV002
*& The CV is queried by the report to get currency pairs, quote dates and rates. Then the data is uploaded to BW standard
*& currency conversion data model via BAPI-calls.
*&
*& The report should be executed from process chain in regular basis (daily, hourly)
*&
*&
*&---------------------------------------------------------------------*
REPORT zboi_raa_fin_conv.

TYPES: BEGIN OF ty_result,
         quote_name  TYPE string,
         quote_date  TYPE string,
         close_quote TYPE string,
         quote_c1    TYPE string,
         quote_c2    TYPE string,
       END OF ty_result,

       BEGIN OF ty_output,
         kurst TYPE kurst,
         fcurr TYPE string,
         tcurr TYPE string,
         gdatu TYPE string,
         ukurs TYPE string,
       END OF ty_output.

DATA:lo_sql                TYPE REF TO cl_sql_statement,
     lv_sql                TYPE string,
     lx_sql                TYPE REF TO cx_sql_exception,
     lo_result             TYPE REF TO cl_sql_result_set,
     lr_data               TYPE REF TO data,
     lt_result             TYPE TABLE OF ty_result,
     msgtxt                TYPE string,
     lt_output             TYPE TABLE OF ty_output,
     lt_bapi               TYPE TABLE OF bapi1093_0,
     ls_bapi               TYPE bapi1093_0,
     ls_output             TYPE ty_output,
     ls_tcurf              TYPE tcurf,
     lt_bapiret            TYPE TABLE OF bapiret2,
     lr_header             TYPE REF TO cl_salv_form_layout_grid,
     lr_grid               TYPE REF TO cl_salv_form_layout_grid,
     lr_salv_alv           TYPE REF TO cl_salv_table,
     lv_last_as_of_date(8) TYPE c VALUE '20190101',
     lv_title              TYPE string,
     ls_tvarvc             TYPE tvarvc.

CONSTANTS: lc_p    TYPE rsscr_kind VALUE 'P',
           lc_name TYPE rvari_vnam VALUE 'CAL_AS_OF_DATE_MD',
           lc_topl TYPE INT4 VALUE 1000. " used to limit maximum number of rows for BAPI call

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS:p_sim   AS CHECKBOX DEFAULT 'X', " if X, nothing updated in BW currency conversion data model
           p_full  AS CHECKBOX, " if X then first 1000 records from CV are quieried regardless if its already loaded or not ("full mode"). If !X, then "delta-mode" is used
           p_pair  AS CHECKBOX, " if X then missing currency pairs are inserted for selected KURST (see below)
           p_kurst TYPE kurst_curr DEFAULT 'CAL' OBLIGATORY. " Currency rate type. You must use CAL.
SELECTION-SCREEN END OF BLOCK b1.

TRY .
  " reading maximum rate update date from app-server parameter CAL_AS_OF_DATE_MD. It is required for "delta-mode" uploading only
    SELECT SINGLE low
           FROM tvarvc
           INTO @DATA(lv_tvarvc_dt)
           WHERE name = @lc_name
           AND   type = @lc_p
           AND   numb = @space.
  " preparing component of SQL-SELECT query depending on upload mode (full/delta)
    DATA: lv_qstr(100) TYPE c.
    IF p_full = 'X'.
      DATA(lv_str) = | ORDER BY "QUOTE_DATE" |.
    ELSE.
      lv_str =  |  WHERE "QUOTE_DATE" > '{ lv_tvarvc_dt }'  ORDER BY "QUOTE_DATE" |.
    ENDIF.

    CREATE OBJECT lo_sql .
    lv_sql = | SELECT TOP { lc_topl } QUOTE_NAME, QUOTE_DATE, CLOSE_QUOTE, QUOTE_C1, QUOTE_C2 | &&
             | FROM "_SYS_BIC"."ZBOI_RAA_EDW.CF/ZCF_R_CV002" | && lv_str.

    lo_result = lo_sql->execute_query( lv_sql ).
    GET REFERENCE OF lt_result INTO lr_data.
    lo_result->set_param_table( lr_data ).
    lo_result->next_package( ).
    lo_result->close( ).
   " now currency rates from CALYPSO table is in lt_result internal table.

    REFRESH:lt_bapi,lt_output.

*    SELECT kurst,
*           fcurr,
*           tcurr
*           FROM tcurr
*           INTO  TABLE @DATA(lt_tcurr)
*           WHERE kurst = 'CAL' ORDER BY kurst ,fcurr , tcurr ASCENDING.

    LOOP AT lt_result INTO DATA(ls_result) .

*      READ TABLE lt_tcurr TRANSPORTING NO FIELDS WITH KEY fcurr = ls_result-quote_name+3(3)
*      tcurr = ls_result-quote_name+7(3) BINARY SEARCH.
*      IF sy-subrc = 0.
      ls_bapi-rate_type = ls_output-kurst = p_kurst.
      ls_bapi-from_curr = ls_output-fcurr = ls_result-quote_name+3(3) .
      ls_bapi-to_currncy = ls_output-tcurr = ls_result-quote_name+7(3) .
      "      ls_bapi-valid_from = ls_output-gdatu = ls_result-quote_date+6(4) && ls_result-quote_date+3(2) && ls_result-quote_date+0(2) .
      ls_bapi-valid_from = ls_output-gdatu = ls_result-quote_date.
      ls_bapi-exch_rate = ls_output-ukurs = ls_result-close_quote .
      ls_bapi-from_factor = 1.
      ls_bapi-to_factor = 1.
      APPEND ls_output TO lt_output.
      APPEND ls_bapi TO lt_bapi.
      CLEAR:ls_bapi, ls_output.
*      ENDIF.
    ENDLOOP.

    SORT lt_bapi BY valid_from DESCENDING.
   " Uploading currency rates to TCURR via BAPI call. BAPI commit is required after it.
    CALL FUNCTION 'BAPI_EXCHRATE_CREATEMULTIPLE'
      EXPORTING
        upd_allow     = 'X'
        chg_fixed     = 'X'
        dev_allow     = '000'
      TABLES
        exchrate_list = lt_bapi
        return        = lt_bapiret.

    IF p_sim IS INITIAL. " if it is not simulation mode ...
      DATA lv_repeat(1) TYPE c.
      IF p_pair IS NOT INITIAL. " if we need to insert missing currency pairs as well
        " we get missing currency pairs from lt_bapiret (after BAPI call)
        LOOP AT lt_bapiret ASSIGNING FIELD-SYMBOL(<fs_bapiret>) WHERE message_v2 = p_kurst.
          IF <fs_bapiret>-message_v3 IS NOT INITIAL
            AND <fs_bapiret>-message_v4 IS NOT INITIAL.
            CLEAR: ls_tcurf.
            ls_tcurf-kurst = p_kurst.
            ls_tcurf-gdatu = 79999898.
            ls_tcurf-ffact = ls_tcurf-tfact = 1.
            ls_tcurf-fcurr = <fs_bapiret>-message_v3.
            ls_tcurf-tcurr = <fs_bapiret>-message_v4.
            INSERT tcurf FROM ls_tcurf.
            WRITE: / | TCURF for CAL { ls_tcurf-fcurr } -> { ls_tcurf-tcurr } inserted|.
            lv_repeat = 'X'.
          ENDIF.
        ENDLOOP.
      ENDIF.
      " if at least one currency pair is inserted, we need to repeat BAPI call. So all source rows will be inserted
      IF lv_repeat = 'X'. " curency pairs were added. Can be updated one more time.
        CLEAR: lt_bapiret.
        WRITE: / | BAPI call repeated|.
        CALL FUNCTION 'BAPI_EXCHRATE_CREATEMULTIPLE'
          EXPORTING
            upd_allow     = 'X'
            chg_fixed     = 'X'
            dev_allow     = '000'
          TABLES
            exchrate_list = lt_bapi
            return        = lt_bapiret.
      ENDIF.
      " The important BAPI commit
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      lv_title = TEXT-001.
      " Updating app-server parameter with maximum (because of sorting above) currenc rate date
      READ TABLE lt_bapi INTO ls_bapi INDEX 1.
      IF sy-subrc = 0.
        ls_tvarvc-name = lc_name.
        ls_tvarvc-type = lc_p.
        ls_tvarvc-numb = space.
        ls_tvarvc-low = ls_bapi-valid_from.
        UPDATE tvarvc FROM ls_tvarvc.
        FREE: ls_tvarvc.
      ENDIF.
    ELSE.
      lv_title = TEXT-002.
    ENDIF.

    "Show the output in ALV
    cl_salv_table=>factory(
      IMPORTING
        r_salv_table = lr_salv_alv
      CHANGING
        t_table = lt_output ).

    CREATE OBJECT lr_grid.
    CREATE OBJECT lr_header.

    lr_grid = lr_header->create_grid( row = 2  column = 1 colspan = 6 ).
    lr_grid->create_label( row = 1  column = 65 text = lv_title ).
    lr_salv_alv->set_top_of_list( lr_grid ).
    lr_salv_alv->display( ).

  CATCH cx_sql_exception INTO lx_sql.
    msgtxt = lx_sql->get_text( ).
ENDTRY.