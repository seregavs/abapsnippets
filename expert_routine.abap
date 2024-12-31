  METHOD expert_routine.
*=== Segments ===

    FIELD-SYMBOLS:
      <SOURCE_FIELDS>    TYPE _ty_s_SC_1.

    DATA:
      RESULT_FIELDS      TYPE _ty_s_TG_1.

*$*$ begin of routine - insert your code only below this line        *-*

* Note the _M class are not considered for DTP execution.
* ABAP Breakpoints must be set in the generated program instead

**** begin of routine - insert your code only below this line       ****

    DATA(lo_trfn) = NEW zcl_flow_zgtdo23(  ).

    lo_trfn->zgtdo22_expert(
      EXPORTING
        it_in  = CORRESPONDING #( source_package )
      IMPORTING
        et_out = DATA(lt_result)
    ).

    MOVE-CORRESPONDING lt_result TO result_package.


**** end of routine - insert your code only before this line        ****
*$*$ end of routine - insert your code only before this line         *-*
  ENDMETHOD. 
========================
METHOD GLOBAL_EXPERT BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT OPTIONS READ-ONLY USING ZCL_FLOW_ZGTDO22=>ZGTDO21_EXPERT.
-- *** Begin of routine - insert your code only below this line ***

    t_in =
        SELECT material, plant, min( calday ) calday
        from :intab
        group by material, plant
        with hint ( no_inline ) ;


    CALL "ZCL_FLOW_ZGTDO22=>ZGTDO21_EXPERT"( :t_in , :t_out );


    outtab =
        SELECT
           DOC_YEAR,
           MAT_DOC,
           MAT_ITEM,
           CALDAY,
           MATERIAL,
           PLANT ,
           RECORDMODE,
           STORNO,
           MOVE_TYPE,
           MOVE_PLANT,
           LOC_CURRCY,
           BASE_UOM,
           VALUE_LC,
           CPQUABU,
           UPD_TS_INCOME,
           ROW_NUMBER() OVER() RECORD,
           '' SQL__PROCEDURE__SOURCE__RECORD
        FROM :t_out;
==================================
    e_t_range = CORRESPONDING #(
      zcl_md_sel_0vendor=>get_distribution_centers( i_condensed = 'X' )
      MAPPING opt = option
    ).
============================================
  DATA: lv_today_date TYPE timestampl.
  GET TIME STAMP FIELD lv_today_date.

  " Calculate cutoff date for deletion
  DATA(lv_cutoff_date) = cl_abap_tstmp=>SUBTRACTSECS_TO_SHORT( tstmp = lv_today_date
                                            secs  = p_ret * 24 * 60 * 60 ).

  TRY.
      " SQL statement for archiving
      lr_sql_stmt->execute_update( |DELETE FROM ZSDA_REMOTE_STMT WHERE END_TIME < { lv_cutoff_date }| ).

==============
I_T_SELECT TYPE SRSC_S_IF_SIMPLE-T_SELECT
T_SELECT TYPE SRSC_T_SELECT,
SRSC_T_SELECT TYPE SRSC_S_SELECT OCCURS 0,
SRSC_S_SELECT LIKE RSSELECT,
RSSELECT
FIELDNM	Тип	RSFIELDNM	CHAR	30	0	Имя поля
SIGN	Тип	RSSIGN	CHAR	1	0	Критерии выбора: SIGN
OPTION	Тип	RSOPTION	CHAR	2	0	Критерии выбора: ОПЦИЯ
LOW	Тип	RSLOW	CHAR	45	0	Критерии выбора: 'С значения'
HIGH	Тип	RSHIGH	CHAR	45	0	Критерии выбора: 'По значение'

    " Критерии выбора
    ar_pernr = value #( for ls_select in i_t_select where ( fieldnm ='PERNR' ) ( corresponding #( ls_select ) ) ).
    ar_calmon = value #( for ls_select in i_t_select where ( fieldnm ='CALMON' ) ( corresponding #( ls_select ) ) ).

    if ar_calmon is initial.
      "Если не задан месяц, то берём текущий
      ar_calmon = value #( sign = 'I'  option = 'BT' ( low = |{ sy-datum(6) }|  high = |{ sy-datum(6) }| ) ).
    endif.

==================

FUNCTION zbw_rfc_vts_moves_get.
*"----------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  TABLES
*"      I_T_SELECT TYPE  RS_T_SELECT
*"      E_T_MOVES TYPE  ZTT_BW_VTS_MOVES
*"  EXCEPTIONS
*"      INVALID_SEL_FIELD
*"      INVALID_SEL_CALMONTH
*"----------------------------------------------------------------------

  DATA:
    l_t_range_plant TYPE zcl_bw_co_log_segm02=>mty_t_range_plant,
    l_t_range_dc TYPE zcl_bw_co_log_segm02=>mty_t_range_plant,
    l_t_range_vts_type TYPE zcl_bw_co_log_segm02=>mty_t_range_vts_type,
    l_t_range_calmonth TYPE zcl_bw_co_log_segm02=>mty_t_range_calmonth.

  LOOP AT i_t_select REFERENCE INTO DATA(s).
    CASE s->fieldnm.
      WHEN 'PLANT'.
        APPEND CORRESPONDING #( s->* ) TO l_t_range_plant.
      WHEN 'DC'.
        APPEND CORRESPONDING #( s->* ) TO l_t_range_dc.
      WHEN 'VTS_TYPE'.
        APPEND CORRESPONDING #( s->* ) TO l_t_range_vts_type.
      WHEN 'CALMONTH'.
        APPEND CORRESPONDING #( s->* ) TO l_t_range_calmonth.
      WHEN OTHERS.
        RAISE invalid_sel_field.
    ENDCASE.
  ENDLOOP.

  IF l_t_range_calmonth[] IS INITIAL.
    RAISE invalid_sel_calmonth.
  ENDIF.

  e_t_moves[] = zcl_bw_co_log_segm02=>get_moves(
      i_t_plant    = l_t_range_plant[]
      i_t_dc       = l_t_range_dc[]
      i_t_vts_type = l_t_range_vts_type[]
      i_t_calmonth = l_t_range_calmonth[] ).



ENDFUNCTION.
==================
in DTP

form c_I_CALMONTH
  tables l_t_range structure rssdlrange
  using i_r_request type ref to IF_RSBK_REQUEST_ADMINTAB_VIEW
        i_fieldnm type RSFIELDNM
  changing p_subrc like sy-subrc.
*       Insert source code to current selection field
*$*$ begin of routine - insert your code only below this line        *-*
  APPEND VALUE #(
    fieldname = i_fieldnm
    sign   = rs_c_range_sign-including
    option = rs_c_range_opt-equal
    low    = zcl_bw_co_log_segm01=>get_calmonth( )
  ) TO l_t_range.
  p_subrc = 0.

*$*$ end of routine - insert your code only before this line         *-*
endform.
