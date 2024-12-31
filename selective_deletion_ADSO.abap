*&---------------------------------------------------------------------*
*& Report ZSELDEL_FROM_ADSO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zseldel_from_adso.
TABLES:
sscrfields
,/bi0/sdate
,/bi0/splant
,/bi0/siobjnm
,/bi0/spostxt.

DATA:
  l_cnt     TYPE f VALUE 0,
  l_cnt_i   TYPE i VALUE 0,
  l_day     LIKE sy-datum,
  l_s_range TYPE rsdrd_s_range,
  l_sx_sel  TYPE rsdrd_sx_sel,
  l_t_msg   TYPE rs_t_msg,
  l_s_msg   LIKE LINE OF l_t_msg,
  lv_msg1   TYPE string,
  l_thx_sel TYPE rsdrd_thx_sel.


SELECTION-SCREEN BEGIN OF BLOCK b002 WITH FRAME.

  SELECTION-SCREEN BEGIN OF BLOCK b004 WITH FRAME.
    PARAMETERS p_target TYPE  rsddatatarget OBLIGATORY.
  SELECTION-SCREEN   END OF BLOCK b004.

  SELECTION-SCREEN BEGIN OF BLOCK b003 WITH FRAME TITLE TEXT-003.
    PARAMETERS p_direct RADIOBUTTON GROUP drct.
    SELECTION-SCREEN BEGIN OF BLOCK b0078 WITH FRAME TITLE TEXT-008.
      PARAMETERS p_001 TYPE /bi0/siobjnm-iobjnm .
      SELECT-OPTIONS c008 FOR /bi0/spostxt-postxt.
    SELECTION-SCREEN   END OF BLOCK b0078.
    PARAMETERS p_3month RADIOBUTTON GROUP drct.
    SELECTION-SCREEN BEGIN OF BLOCK b007 WITH FRAME TITLE TEXT-007.
      PARAMETERS p_004 TYPE /bi0/siobjnm-iobjnm .
      PARAMETERS p_dayty RADIOBUTTON GROUP timt.
      PARAMETERS p_monty RADIOBUTTON GROUP timt.
      PARAMETERS p_hist TYPE monum OBLIGATORY DEFAULT '03'.
    SELECTION-SCREEN   END OF BLOCK b007.
  SELECTION-SCREEN   END OF BLOCK b003.

  SELECTION-SCREEN BEGIN OF BLOCK b005 WITH FRAME TITLE TEXT-004.
    PARAMETERS p_002 TYPE /bi0/siobjnm-iobjnm.
    SELECT-OPTIONS c010 FOR /bi0/spostxt-postxt.
  SELECTION-SCREEN   END OF BLOCK b005.

  SELECTION-SCREEN BEGIN OF BLOCK b006 WITH FRAME TITLE TEXT-005.
    PARAMETERS p_003 TYPE /bi0/siobjnm-iobjnm.
    SELECT-OPTIONS c012 FOR /bi0/spostxt-postxt.
  SELECTION-SCREEN   END OF BLOCK b006.

SELECTION-SCREEN   END OF BLOCK b002.

INITIALIZATION.

START-OF-SELECTION.

  IF p_001 IS INITIAL AND p_002 IS INITIAL AND p_003 IS INITIAL AND p_004 IS INITIAL.

    MESSAGE 'Не задан ни один критерий выборочного удаления' TYPE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF c008[] IS INITIAL AND c010[] IS INITIAL AND c012[] IS INITIAL.
    MESSAGE 'Не задано ни одно значение для выборочного удаления' TYPE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF p_target IS INITIAL.
    MESSAGE 'Не выбран инфопровайдер' TYPE 'E'.
    LEAVE LIST-PROCESSING.
  ELSE.
    SELECT SINGLE adsonm FROM rsoadso INTO @DATA(lv_exists) WHERE adsonm = @p_target.
    IF sy-subrc <> 0.
      MESSAGE 'Данный инфопровайдер не существует' TYPE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_001 IS NOT INITIAL.
    SELECT SINGLE objnm_dep FROM rsoobjxref INTO @lv_exists WHERE objnm = @p_target AND tlogo = 'ADSO' AND tlogo_dep = 'IOBJ' AND objnm_dep = @p_001.
    IF sy-subrc <> 0.
      MESSAGE | Данного поля { p_001 } нет в ADSO | TYPE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_004 IS NOT INITIAL.
    SELECT SINGLE objnm_dep FROM rsoobjxref INTO @lv_exists WHERE objnm = @p_target AND tlogo = 'ADSO' AND tlogo_dep = 'IOBJ' AND objnm_dep = @p_004.
    IF sy-subrc <> 0.
      MESSAGE | Данного поля { p_004 } нет в ADSO | TYPE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_002 IS NOT INITIAL.
    SELECT SINGLE objnm_dep FROM rsoobjxref INTO @lv_exists WHERE objnm = @p_target AND tlogo = 'ADSO' AND tlogo_dep = 'IOBJ' AND objnm_dep = @p_002.
    IF sy-subrc <> 0.
      MESSAGE | Данного поля { p_002 } нет в ADSO | TYPE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_003 IS NOT INITIAL.
    SELECT SINGLE objnm_dep FROM rsoobjxref INTO @lv_exists WHERE objnm = @p_target AND tlogo = 'ADSO' AND tlogo_dep = 'IOBJ' AND objnm_dep = @p_003.
    IF sy-subrc <> 0.
      MESSAGE | Данного поля { p_003 } нет в ADSO | TYPE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  CASE 'X'.
    WHEN p_direct.
      IF NOT c008[] IS INITIAL AND NOT p_001 IS INITIAL.
        CLEAR l_sx_sel.
        l_sx_sel-iobjnm = p_001.
        LOOP AT c008 .
          CLEAR l_s_range.
          l_s_range = VALUE #( sign = c008-sign option = c008-option low = c008-low high = c008-high keyfl = rs_c_true ).
          APPEND l_s_range TO l_sx_sel-t_range.
        ENDLOOP.
        INSERT l_sx_sel INTO TABLE l_thx_sel.
      ENDIF.
    WHEN p_3month.
      IF NOT p_hist IS INITIAL AND NOT p_004 IS INITIAL.
        CLEAR l_sx_sel.
        l_sx_sel-iobjnm = p_004.
        l_day = sy-datum.
        CALL FUNCTION 'ZBW_CALC_DATE_IN_INTERVAL'
          EXPORTING
            im_date   = l_day
            im_months = p_hist
            im_signum = '-'
          IMPORTING
            ex_date   = l_day.

        CASE 'X'.
          WHEN p_monty.
            l_day = l_day(6).
        ENDCASE.
        l_s_range = VALUE #( sign = 'I' option = 'LT' low = l_day keyfl = rs_c_true ).
        APPEND l_s_range TO l_sx_sel-t_range.
        INSERT l_sx_sel INTO TABLE l_thx_sel.

      ENDIF.
  ENDCASE.

  IF NOT c010[] IS INITIAL AND NOT p_002 IS INITIAL.
    CLEAR l_sx_sel.
    l_sx_sel-iobjnm = p_002.
    LOOP AT c010.
      CLEAR l_s_range.
      l_s_range = VALUE #( sign = c010-sign option = c010-option low = c010-low high = c010-high keyfl = rs_c_true ).
      APPEND l_s_range TO l_sx_sel-t_range.
    ENDLOOP.
    INSERT l_sx_sel INTO TABLE l_thx_sel.
  ENDIF.

  IF NOT c012[] IS INITIAL AND NOT p_003 IS INITIAL.
    CLEAR l_sx_sel.
    l_sx_sel-iobjnm = p_003.
    LOOP AT c012.
      CLEAR l_s_range.
      l_s_range = VALUE #( sign = c012-sign option = c012-option low = c012-low high = c012-high keyfl = rs_c_true ).
      APPEND l_s_range TO l_sx_sel-t_range.
    ENDLOOP.
    INSERT l_sx_sel INTO TABLE l_thx_sel.
  ENDIF.

  IF l_thx_sel IS NOT INITIAL.
    zcl_seldel=>del_from_adso(
      EXPORTING
       i_cube   = p_target
       i_sel    = l_thx_sel
      CHANGING
        c_t_msg = l_t_msg
      ).
    READ TABLE l_t_msg
      TRANSPORTING NO FIELDS
      WITH KEY msgty = 'E'.
    IF sy-subrc = 0.
      MESSAGE TEXT-001 TYPE 'E'.
    ELSE.
      CLEAR lv_msg1.
      LOOP AT l_t_msg INTO l_s_msg
        WHERE msgty = 'I' AND msgno = 120.
        IF lv_msg1 IS INITIAL.
          lv_msg1 = l_s_msg-msgv1.
        ELSE.
          lv_msg1 = | { lv_msg1 } строк удалено из первой таблицы и { l_s_msg-msgv1 } строк из второй. |.
        ENDIF.
      ENDLOOP.
      MESSAGE lv_msg1 TYPE 'I'.
    ENDIF.
  ELSE.
    MESSAGE 'Критерии для удаления не сформированы.' TYPE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

METHOD DEL_FROM_ADSO.

  CALL FUNCTION 'RSDRD_SEL_DELETION'
    EXPORTING
      i_datatarget      = i_cube
      i_thx_sel         = i_sel
      i_del_activ       = rs_c_true
      i_parallel_degree = ' '
      i_no_logging      = rs_c_false
      i_show_report     = ' '
    CHANGING
      c_t_msg           = c_t_msg
    EXCEPTIONS
      OTHERS            = 4.

ENDMETHOD.

