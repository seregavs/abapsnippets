class ZCL_TRFN_ZFIGL_O20_ZFIGL_D20 definition
  public
  final
  create public .

public section.

  types:
*  types:
*    BEGIN OF ztty_source_record,
**      Field: PLANT Plant.
*        PLANT           TYPE C LENGTH 4,
**      Field: ZCMATNO Material Code.
*        ZCMATNO           TYPE C LENGTH 40,
**      Field: BPARTNER Business Partner.
*        BPARTNER           TYPE C LENGTH 10,
**      Field: BP_TYPE GP: Partner Type.
*        BP_TYPE           TYPE C LENGTH 4,
**      InfoObject: 0CALDAY Calendar day.
*        CALDAY           TYPE /BI0/OICALDAY,
**      Field: MOVE_TYPE Movement Type.
*        MOVE_TYPE           TYPE C LENGTH 3,
**      Field: MBLNR MBLNR.
*        MBLNR           TYPE C LENGTH 10,
**      Field: MOVE_PLANT Receiving Plant/Issuing Plant.
*        MOVE_PLANT           TYPE C LENGTH 4,
**      Field: EBELN EBELN.
*        EBELN           TYPE C LENGTH 40,
**      Field: DCINDIC Debit/credit indicator.
*        DCINDIC           TYPE C LENGTH 1,
**      InfoObject: 0RECORDMODE BW Delta Process: Update Mode.
*        RECORDMODE           TYPE RODMUPDMOD,
**      Field: DMBTR Value, SAR.
*        DMBTR           TYPE P LENGTH 9 DECIMALS 2,
**      InfoObject: 0LOC_CURRCY Local currency.
*        LOC_CURRCY           TYPE /BI0/OILOC_CURRCY,
**      InfoObject: 0CPQUABU Quantity in Base UoM.
*        CPQUABU           TYPE /BI0/OICPQUABU,
**      InfoObject: 0BASE_UOM Base Unit of Measure.
*        BASE_UOM           TYPE /BI0/OIBASE_UOM,
**      InfoObject: 0CURR_GRQTY Quanity in Purchase UoM.
*        CURR_GRQTY           TYPE /BI0/OICURR_GRQTY,
**      InfoObject: 0PO_UNIT Order unit.
*        PO_UNIT           TYPE /BI0/OIPO_UNIT,
**      Field: RECORD.
*        RECORD           TYPE RSARECORD,"
*      END   OF ztty_source_record .
    ztty_source_package TYPE STANDARD TABLE OF /bic/azfigl_o201 .
  types:
    ztty_result_package TYPE STANDARD TABLE OF /bic/azfigl_d201 .

  data MT_SOURCE_PACKAGE type ZTTY_SOURCE_PACKAGE .
  data MT_RESULT_PACKAGE type ZTTY_RESULT_PACKAGE .
  data MT_MONITOR type RSTR_TY_T_MONITORS .

  methods START_ROUTINE
    changing
      !CT_PACKAGE type STANDARD TABLE
      !CT_MONITOR type RSTR_TY_T_MONITORS .
protected section.
private section.

  methods MAIN_TRFN_SR_LOGIC .
ENDCLASS.



CLASS ZCL_TRFN_ZFIGL_O20_ZFIGL_D20 IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_TRFN_ZFIGL_O20_ZFIGL_D20->MAIN_TRFN_SR_LOGIC
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD main_trfn_sr_logic.

    DATA: monitor_rec LIKE LINE OF mt_monitor,
          lv_del(1)   TYPE c.

    SELECT co_area
      FROM /bi0/sco_area
      INTO TABLE @DATA(lt_co_area).

    SELECT chrt_accts
      FROM /bi0/schrt_accts
      INTO TABLE @DATA(lt_chrt_accts).

    SELECT gl_account
      FROM /bi0/sgl_account
      INTO TABLE @DATA(lt_gl_account).


    SELECT profit_ctr
   FROM /bi0/sprofit_ctr
   INTO TABLE @DATA(lt_profit_ctr).

    SELECT currency
   FROM /bi0/scurrency
   INTO TABLE @DATA(lt_currency).



    LOOP AT mt_source_package ASSIGNING FIELD-SYMBOL(<fs_sp>).
      DATA(lv_i) = sy-tabix.
      CLEAR: lv_del.


      READ TABLE lt_co_area TRANSPORTING NO FIELDS
       WITH KEY co_area = <fs_sp>-co_area.
      IF sy-subrc NE 0.
        monitor_rec-msgid = '0CO_AREA'.
        monitor_rec-msgty = 'I'.
        monitor_rec-msgno = '000'.
        monitor_rec-msgv1 = <fs_sp>-co_area.
        APPEND monitor_rec TO mt_monitor.
        lv_del = 'X'.
      ENDIF.


      READ TABLE lt_chrt_accts TRANSPORTING NO FIELDS
       WITH KEY chrt_accts = <fs_sp>-chrt_accnts.
      IF sy-subrc NE 0.
        monitor_rec-msgid = 'CHRT_ACCTS'.
        monitor_rec-msgty = 'I'.
        monitor_rec-msgno = '000'.
        monitor_rec-msgv1 = <fs_sp>-chrt_accnts.
        APPEND monitor_rec TO mt_monitor.
        lv_del = 'X'.
      ENDIF.




      DATA lv_gl_account TYPE char10.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_sp>-gl_account
        IMPORTING
          output = lv_gl_account.


      READ TABLE lt_gl_account TRANSPORTING NO FIELDS
       WITH KEY gl_account = lv_gl_account.
      IF sy-subrc NE 0.
        monitor_rec-msgid = 'GL_ACCOUNT'.
        monitor_rec-msgty = 'I'.
        monitor_rec-msgno = '000'.
        monitor_rec-msgv1 = lv_gl_account.
        APPEND monitor_rec TO mt_monitor.
        lv_del = 'X'.
      ENDIF.

      CLEAR: lv_gl_account.


      DATA lv_profit_ctr TYPE char10.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <fs_sp>-profitctr
        IMPORTING
          output = lv_profit_ctr.


      READ TABLE lt_profit_ctr TRANSPORTING NO FIELDS
       WITH KEY profit_ctr = lv_profit_ctr.
      IF sy-subrc NE 0.
        monitor_rec-msgid = 'PROFIT_CTR'.
        monitor_rec-msgty = 'I'.
        monitor_rec-msgno = '000'.
        monitor_rec-msgv1 = lv_profit_ctr.
        APPEND monitor_rec TO mt_monitor.
        lv_del = 'X'.
      ENDIF.
      CLEAR lv_profit_ctr.


      READ TABLE lt_currency TRANSPORTING NO FIELDS
       WITH KEY currency = <fs_sp>-loc_currcy.
      IF sy-subrc NE 0.
        monitor_rec-msgid = 'CURRENCY'.
        monitor_rec-msgty = 'I'.
        monitor_rec-msgno = '000'.
        monitor_rec-msgv1 = <fs_sp>-loc_currcy.
        APPEND monitor_rec TO mt_monitor.
        lv_del = 'X'.
      ENDIF.


      IF lv_del = 'X'.
        DELETE mt_source_package INDEX lv_i.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_TRFN_ZFIGL_O20_ZFIGL_D20->START_ROUTINE
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CT_PACKAGE                     TYPE        STANDARD TABLE
* | [<-->] CT_MONITOR                     TYPE        RSTR_TY_T_MONITORS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD START_ROUTINE.

    MOVE-CORRESPONDING ct_package[] TO mt_source_package[].
    MOVE-CORRESPONDING ct_monitor[] TO mt_monitor[].
    CLEAR ct_package[].
    me->main_trfn_sr_logic( ).
    MOVE-CORRESPONDING mt_monitor[] TO ct_monitor[].
    MOVE-CORRESPONDING mt_source_package[] TO ct_package[] .

  ENDMETHOD.
ENDCLASS.



Вызов класса


  METHOD start_routine.
*=== Segments ===

    FIELD-SYMBOLS:
      <SOURCE_FIELDS>    TYPE _ty_s_SC_1.

    DATA:
      MONITOR_REC     TYPE rstmonitor.

*$*$ begin of routine - insert your code only below this line        *-*
    ... "insert your code here
*--  fill table "MONITOR" with values of structure "MONITOR_REC"
*-   to make monitor entries
    ... "to cancel the update process
*    raise exception type CX_RSROUT_ABORT.

    NEW zcl_trfn_zfigl_o20_zfigl_d20( )->start_routine(
          CHANGING
            ct_package = SOURCE_PACKAGE
            ct_monitor = MONITOR
        ).



*$*$ end of routine - insert your code only before this line         *-*
  ENDMETHOD.
