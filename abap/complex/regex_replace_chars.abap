поменять порядок компонентов в дате

        REPLACE REGEX '(\d\d).(\d\d).(\d\d\d\d)' IN <fs_vt1>-low WITH '$3$2$1'.

    TRANSLATE ls_string_tmp USING ';,'. " меняет ; на , 


IF_RSROA_VARIABLES_EXIT_BADI~PROCESS

      WHEN 'VAR_DATE_000'.
        IF i_step = 1.
          l_s_range-low = sy-datum.
          l_s_range-sign = 'I'.
          l_s_range-opt = 'EQ'.
          APPEND l_s_range TO c_t_range.
        ENDIF.
      WHEN 'VAR_DATE_001'.
        IF i_step = 2.
          l_s_range-low = sy-datum.
          READ TABLE i_t_var_range  INTO l_s_var_range WITH KEY vnam = 'VAR_DATE_000'.
          IF sy-subrc = 0.
            l_s_range-low = l_s_var_range-low.
          ENDIF.
          l_s_range-sign = 'I'.
          l_s_range-opt = 'EQ'.
          APPEND l_s_range TO c_t_range.
        ENDIF.
      WHEN 'VAR_DATE_002'.
        IF i_step = 1.
          DATA: lv_d TYPE d.
          lv_d = |{ sy-datum+0(6) }01|.
          lv_d = lv_d - 1.
          l_s_range-low = |{ lv_d+0(6) }01|.
          l_s_range-high = lv_d.
          l_s_range-sign = 'I'.
          l_s_range-opt = 'BT'.
          APPEND l_s_range TO c_t_range.
        ENDIF.
      WHEN 'VAR_CALMONTH_INT'.
        IF i_step = 1.
          l_s_range-low  = |{ sy-datum+0(4) }01|.
          l_s_range-high = |{ sy-datum+0(6) }|.
          l_s_range-low = '201801'. " for design time in Lumira
          l_s_range-high = '201903'. " for design time in Lumira
          l_s_range-sign = 'I'.
          l_s_range-opt = 'BT'.
          APPEND l_s_range TO c_t_range.
        ENDIF.
      WHEN 'VAR_CALMONTH_BEG'.
        IF i_step = 1.
          l_s_range-low  = |{ sy-datum+0(4) }01|.
          l_s_range-low = '201801'. " for design time in Lumira
          l_s_range-sign = 'I'.
          l_s_range-opt = 'EQ'.
          APPEND l_s_range TO c_t_range.
        ENDIF.
      WHEN 'VAR_CALMONTH_END'.
        IF i_step = 1.
          l_s_range-low = |{ sy-datum+0(6) }|.
          l_s_range-low = '201903'. " for design time in Lumira
          l_s_range-sign = 'I'.
          l_s_range-opt = 'EQ'.
          APPEND l_s_range TO c_t_range.
        ENDIF.
      WHEN 'VAR_DATE_BEGDA'.
        IF i_step = 1.
          lv_d = |{ sy-datum+0(6) }01|.
          lv_d = lv_d - 1.
          lv_d = |{ lv_d+0(6) }01|.
          l_s_range-low  = lv_d.
          l_s_range-low  = |{ sy-datum+0(4) }01|.
          l_s_range-low = '20180101'. " for design time in Lumira
          l_s_range-sign = 'I'.
          l_s_range-opt = 'EQ'.
          APPEND l_s_range TO c_t_range.
        ENDIF.
      WHEN 'VAR_DATE_ENDDA'.
        IF i_step = 1.
          lv_d = |{ sy-datum+0(6) }01|.
          lv_d = lv_d - 1.
          l_s_range-low  = lv_d.
          l_s_range-low = '20190331'. " for design time in Lumira
          l_s_range-sign = 'I'.
          l_s_range-opt = 'EQ'.
          APPEND l_s_range TO c_t_range.
        ENDIF.
      WHEN 'VAR_CALQUARTER_INT'.
        IF i_step = 1.
           DATA: lv_cmon(2) TYPE n,
                 lv_q(1)  TYPE n.
           lv_cmon = sy-datum+4(2).
           lv_q = lv_cmon div 4 + 1.
           l_s_range-low  = |{ sy-datum+0(4) }1|.
           l_s_range-high = |{ sy-datum+0(4) }{ lv_q }|.
           l_s_range-low  = '20181'. " for design time in Lumira
           l_s_range-high = '20191'. " for design time in Lumira
           l_s_range-sign = 'I'.
           l_s_range-opt = 'BT'.
           APPEND l_s_range TO c_t_range.
        ENDIF.
      WHEN 'VAR_CALQUARTER_BEG'.
        IF i_step = 1.
           lv_cmon = sy-datum+4(2).
           lv_q = lv_cmon div 4 + 1.
           l_s_range-low  = |{ sy-datum+0(4) }1|.
           l_s_range-low  = '20181'. " for design time in Lumira
           l_s_range-sign = 'I'.
           l_s_range-opt = 'EQ'.
           APPEND l_s_range TO c_t_range.
        ENDIF.
      WHEN 'VAR_CALQUARTER_END'.
        IF i_step = 1.
           lv_cmon = sy-datum+4(2).
           lv_q = lv_cmon div 4 + 1.
           l_s_range-low = |{ sy-datum+0(4) }{ lv_q }|.
           l_s_range-low = '20191'. " for design time in Lumira
           l_s_range-sign = 'I'.
           l_s_range-opt = 'EQ'.
           APPEND l_s_range TO c_t_range.
        ENDIF.

=================================

METHOD replace_not_allowed_chars.

    DATA: i_str TYPE string,
          e_str TYPE string.
    DATA l_rskc_pattern TYPE string.
    DATA l_subrc TYPE sy-subrc.

    i_str = input.
    e_str = i_str.

    CALL FUNCTION 'RSKC_CHAVL_CHECK'
      EXPORTING
        i_chavl     = i_str
      IMPORTING
        e_err_subrc = l_subrc.

    IF l_subrc IS INITIAL.
      " ok
    ELSE.
      " подразумевается, что не используются ALL_CAPITAL*
      CALL FUNCTION 'RSKC_ALLOWED_CHAR_GET'
        IMPORTING
          e_allowed_char = l_rskc_pattern.

      "
      " Экранирование '\' специальных символов внутри класса: ]\^-
      "
      REPLACE ALL OCCURRENCES OF REGEX `[\]\\\^\-]`
        IN l_rskc_pattern WITH `\\$0`.

      "
      " Паттерн для удаления символов - первый символ '!' или '#'
      " и все, не входящие в разрешенные символы
      "
      CONCATENATE '(^([!#])+)|([^' l_rskc_pattern '])' INTO l_rskc_pattern.

      "
      " Удалить все символы, которые не входят в разрешенные
      "
      REPLACE ALL OCCURRENCES OF REGEX l_rskc_pattern
        IN e_str WITH ' ' IGNORING CASE.

    ENDIF.

    output = e_str.

  ENDMETHOD.
=====================

=======================
    DATA : l_t_out_range TYPE rsr_t_rangesid.
    DATA : l_s_out_range LIKE LINE OF l_t_out_range.

    DATA :
      l_r_olap_area      TYPE REF TO cl_rsr_olap_area,

      ld_e_tsx_auths     TYPE rsec_tsx_auths,
      ld_e_t_leaves_sids TYPE rrsv_t_sid.


=================