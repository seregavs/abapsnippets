" считываем полномочия, которые прописаны вручную на ТК - Z_BUDGET_*
" полномочия, присвоенные узлами иерархии - игнорируем
" в результате в l_t_out_range для сотрудников ТК в идеале должна быть 1 запись
" для логинов без роли L*:TK_RSR - l_t_out_range будет пустой

    CALL FUNCTION 'RSEC_GET_AUTH_FOR_USER'
      EXPORTING
        i_iobjnm                    = '0PLANT'
        i_infocube                  = ''
        i_uname                     = sy-uname
        i_no_warnings               = rs_c_false
        i_ignore_hierarchies        = 'X'
        i_separate_leaves           = rs_c_false
        i_complete_answer           = rs_c_false
        i_r_olap_area               = l_r_olap_area
      IMPORTING
        e_t_rangesid                = l_t_out_range
        e_tsx_auths                 = ld_e_tsx_auths
        e_t_leaves_sids             = ld_e_t_leaves_sids
      EXCEPTIONS
        not_authorized              = 1
        wrong_parameter_combination = 2
        internal_error              = 3
        iobj_not_found              = 4
        x_message                   = 5.


    IF sy-subrc = 0. " ok

      IF l_t_out_range IS NOT INITIAL.
        LOOP AT l_t_out_range INTO l_s_out_range.
         " на всякий случай обработка нескольких записей из полномочий
         " переменная оставит все равно только одно - формат выдаст ошибку
          ls_e_range-low  = l_s_out_range-low.
          ls_e_range-sign = 'I'.
          ls_e_range-opt  = 'EQ'.
          APPEND ls_e_range TO e_t_range.
        ENDLOOP.
      ENDIF.

      "RFC-028304
      "IF l_t_out_range IS NOT INITIAL.
        "APPEND l_t_out_range[ 1 ] TO e_t_range.
      "ENDIF.
      "RFC-028304

    ELSE.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.