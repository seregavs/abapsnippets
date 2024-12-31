предыдущий месяц
  if p_month is initial.
    l_date = sy-datum - sy-datum+6(2) - 1.
    p_month = l_date(6).
  endif.

----------------------------
Get restriction filters from DTP in end-routine
--
    DATA:
      lt_th_range  TYPE rsbk_th_range
      ,ls_th_range TYPE rsbk_s_range

      .
    FIELD-SYMBOLS:
      <fs_th_range> TYPE  rsbk_s_range
      .

    IF g_datefrom IS INITIAL.
      lt_th_range =
        p_r_request->get_th_range( ).
      LOOP AT lt_th_range ASSIGNING <fs_th_range>
                              WHERE fieldnm = 'CALDAY'.
        IF <fs_th_range>-sign = 'I' AND <fs_th_range>-option = 'BT'.
          g_datefrom = <fs_th_range>-low(8).
          g_dateto = <fs_th_range>-high(8).
        ELSEIF <fs_th_range>-sign = 'I' AND <fs_th_range>-option = 'EQ'.
          g_datefrom = <fs_th_range>-low(8).
          g_dateto = <fs_th_range>-low(8).
        ELSE.
          RAISE EXCEPTION TYPE CX_RSROUT_ABORT.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF g_datefrom IS INITIAL.
      RAISE EXCEPTION TYPE CX_RSROUT_ABORT.
    ENDIF.

    LOOP AT RESULT_PACKAGE ASSIGNING <result_fields>.
      <result_fields>-datefrom = g_datefrom.
      <result_fields>-dateto = g_dateto.
    ENDLOOP.
----------------

  DATA: lt_dc TYPE TABLE OF zrange_type,
        lr_rdc TYPE RANGE OF /bi0/oidistr_chan.

  CALL FUNCTION 'Z_BIW_GET_TVARVC'
    EXPORTING
      in_name         = 'DC_RETSALES'
      in_type         = 'S'
    TABLES
      out_range       = lt_dc
  .
  lr_rdc =
    VALUE #( FOR <fs_dc> IN lt_dc
             ( sign = <fs_dc>-sign
               option = <fs_dc>-option
               low = <fs_dc>-low
               high = <fs_dc>-high
             )
           )
  .
---------------------
  IMPORT PLTAVAIL = lr_plant FROM DATABASE indx(bp) ID 'AVAIL'.
  l_t_range[] = VALUE #( FOR <fs_plant> IN lr_plant
                         ( fieldname = i_fieldnm
                           sign = <fs_plant>-sign
                           option = <fs_plant>-option
                           low = <fs_plant>-low
                           high  = <fs_plant>-high ) ).
=================
