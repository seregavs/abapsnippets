CLASS zcl_query DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:

           tt_xls TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    TYPES: BEGIN OF ts_range_var,
             sign   TYPE ddsign,
             option TYPE ddoption,
             low    TYPE tvarv_val,
             high   TYPE tvarv_val,
             vnam   TYPE rszvnam,
           END OF ts_range_var,
           tt_range_var TYPE STANDARD TABLE OF ts_range_var WITH EMPTY KEY.

    DATA gr_limit TYPE efg_tab_ranges.

    METHODS constructor
      IMPORTING is_input TYPE zcl_zcomcp01_q006_uparams=>ts_input.

    METHODS get_csv

      IMPORTING ir            TYPE tt_range_var OPTIONAL
      RETURNING VALUE(rt_csv) TYPE tt_xls.

    METHODS upload
      IMPORTING it TYPE zcl_query=>tt_xls
                iv_extension TYPE string OPTIONAL
                iv_folder TYPE string OPTIONAL
                iv_name TYPE string OPTIONAL.
    METHODS get_def
      RETURNING
        value(rt) TYPE zcl_query=>tt_xls.
    METHODS upload_xstring
      IMPORTING
        iv_xsctring  TYPE xstring
        iv_name      TYPE store_label
        iv_extension TYPE string
        iv_folder    TYPE store_label.
    METHODS get_xstring_tab
      IMPORTING
        ir TYPE REF TO data
      RETURNING
        VALUE(rv_xstring) TYPE xstring.
    METHODS create_zip.
    METHODS add_file_to_zip
      IMPORTING
        iv_name    TYPE string
        iv_xstring TYPE xstring.
    METHODS save_zip
      RETURNING
        value(rv) TYPE xstring.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA ms_input         TYPE zcl_zcomcp01_q006_uparams=>ts_input.
    DATA mv_objnm_column  TYPE rsiobjnm.
    DATA mv_objnm_row     TYPE rsiobjnm.
    DATA mt_def           TYPE tt_xls.
    DATA mv_def_create    TYPE abap_bool.
    DATA mo_zip           TYPE REF TO cl_abap_zip.
    DATA mc_encoding_utf8 TYPE abap_encoding VALUE '4110'.

    METHODS get_data
      IMPORTING ir            TYPE tt_range_var OPTIONAL
      RETURNING VALUE(rt_xls) TYPE tt_xls.

    METHODS get_set
      IMPORTING ir            TYPE tt_range_var OPTIONAL
      CHANGING  lo            TYPE REF TO cl_rsr_request
      RETURNING VALUE(ro_set) TYPE REF TO cl_rsr_data_set.

    METHODS get_xls
      IMPORTING io_set                TYPE REF TO cl_rsr_data_set
                iv_mat_frmt           TYPE flag OPTIONAL
      RETURNING VALUE(et_xls_content) TYPE zcl_query=>tt_xls.

    METHODS get_var
      IMPORTING ir            TYPE tt_range_var OPTIONAL
      RETURNING VALUE(rt_var) TYPE rrx1_t_var.

    METHODS get_objnm_key
      IMPORTING lo TYPE REF TO cl_rsr_request.

    METHODS add_columns
      CHANGING !co TYPE REF TO cl_rsr_request.

    METHODS add_column
      IMPORTING i_name   TYPE string
                i_parent TYPE rsiobjnm OPTIONAL
                i_axis   TYPE rrxaxis  DEFAULT 'Y'
      CHANGING  !co      TYPE REF TO cl_rsr_request.

    METHODS change_request
      CHANGING !co TYPE REF TO cl_rsr_request.

    METHODS set_filters
      CHANGING !co TYPE REF TO cl_rsr_request.

    METHODS add_def
      IMPORTING iv TYPE string.

    METHODS finish_def.
    METHODS prepare_mark
      IMPORTING
        iv_caption   TYPE rrws_sx_tuple-caption
        iv_chavl_ext TYPE rrws_sx_tuple-chavl_ext
        iv_chanm     TYPE rrws_sx_tuple-chanm OPTIONAL
        iv_chaprsnt  TYPE rrws_sx_axis_chars-chaprsnt
        iv_mat_frmt  TYPE flag
        iv_text      type flag
      EXPORTING
        es_data_mark TYPE string.
.
ENDCLASS.



CLASS zcl_query IMPLEMENTATION.

  METHOD constructor.
        ms_input = is_input.
           APPEND VALUE #( sign   = 'I'
                    option = 'BT'
                    low    = ' '
                    high   = '~' ) TO gr_limit.
    APPEND VALUE #( sign   = 'I'
                    option = 'BT'
                    low    = 'А'
                    high   = 'я' ) TO gr_limit.
    APPEND VALUE #( sign   = 'I'
                    option = 'EQ'
                    low    = 'ё' ) TO gr_limit.
    APPEND VALUE #( sign   = 'I'
                    option = 'EQ'
                    low    = 'Ё' ) TO gr_limit.
    APPEND VALUE #( sign   = 'I'
                    option = 'EQ'
                    low    = '№' ) TO gr_limit.
    APPEND VALUE #( sign   = 'I'
                    option = 'EQ'
                    low    = '™' ) TO gr_limit.
    APPEND VALUE #( sign   = 'I'
                    option = 'EQ'
                    low    = '“' ) TO gr_limit.
    APPEND VALUE #( sign   = 'I'
                    option = 'EQ'
                    low    = '”' ) TO gr_limit.
    APPEND VALUE #( sign   = 'I'
                    option = 'EQ'
                    low    = '–' ) TO gr_limit.
    APPEND VALUE #( sign   = 'I'
                    option = 'EQ'
                    low    = '—' ) TO gr_limit.
    APPEND VALUE #( sign   = 'I'
                    option = 'EQ'
                    low    = '©' ) TO gr_limit.
    APPEND VALUE #( sign   = 'I'
                    option = 'EQ'
                    low    = '«' ) TO gr_limit.
    APPEND VALUE #( sign   = 'I'
                    option = 'EQ'
                    low    = '»' ) TO gr_limit.
    APPEND VALUE #( sign   = 'I'
                    option = 'EQ'
                    low    = '®' ) TO gr_limit.
    APPEND VALUE #( sign   = 'I'
                    option = 'EQ'
                    low    = '…' ) TO gr_limit.
  ENDMETHOD.

  METHOD get_csv.
   rt_csv = get_data( ir  ).
  ENDMETHOD.




  METHOD get_data.
  DATA(lo) = NEW cl_rsr_request( i_query    = CONV #( ms_input-query )
                                   i_infocube = CONV #( ms_input-infocube ) ).
    DATA(lo_set) = get_set( EXPORTING ir = ir CHANGING lo = lo ).
    rt_xls =  get_xls( lo_set ).
    lo_set->free( ).
    lo->free( ).
    free: lo_set, lo.
  ENDMETHOD.

  METHOD get_set.

*    lo->
    lo->variables_set( i_t_var = get_var( ir = ir ) ).

    lo->variables_submit( EXCEPTIONS bad_value_combination = 1
                                     x_message             = 2
                                     user_not_authorized   = 3
                                     OTHERS                = 4 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lo->check_state( ).

    IF ms_input-flag IS INITIAL. " Обходимся без фильтров на атрибуты
      set_filters( CHANGING co = lo ).
    ENDIF.


    get_objnm_key( lo ).
    add_columns( CHANGING co = lo  ).

    change_request( CHANGING co = lo ).

    lo->read_data( EXCEPTIONS no_processing = 1
                              x_message     = 2
                              OTHERS        = 3 ).
    IF sy-subrc <> 0.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

"was here

    cl_rsr_data_set=>get( EXPORTING  i_r_request     = lo
                          RECEIVING  r_r_data_set    = DATA(lo_set)
                          EXCEPTIONS x_message       = 1
                                     request_unknown = 2
                                     OTHERS          = 3 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lo_set->refresh( cl_rsr_data_set=>c_version_20a_1 ).

    ro_set = lo_set.
  ENDMETHOD.

  METHOD get_xls.
    DATA ls_data_tmp      TYPE string.                      " строка показтелей
    DATA ls_data_sign     TYPE string.                      " строка признаков
    DATA ls_string_tmp    TYPE string.                      " для проверки на число
    DATA lv_count_column1 TYPE i.                           " количество колонок признаков
    DATA lv_count_column2 TYPE i.                           " количество колонок показателей
    DATA lv_count_row2    TYPE i.                           " количество строк названий показателей (и признаков)
    DATA lv_column_tmp    TYPE i.                           " текущий номер колонки
    DATA lv_tuple_ordinal TYPE rrws_sx_tuple-tuple_ordinal. " текущее значение TUPLE_ORDINAL
    DATA lt_data_sign     TYPE TABLE OF rstxtlg.
    DATA lv_char          TYPE text8192.
    data lt_def_pok    type table of rschavl .
    " формируем название колонок показателей
    TRY.
        " считывание названий колонок
        DATA(lt_info2) = io_set->n_sx_version_20a_1-axis_info[ axis = '000' ]-chars.
        DATA(lt_axis0) = io_set->n_sx_version_20a_1-axis_data[ axis = '000' ]-set.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    LOOP AT lt_axis0 ASSIGNING FIELD-SYMBOL(<ls_axis0>).
      lv_count_row2 += 1.

      " подсчитываем количество колонок показателей
      IF <ls_axis0>-tuple_ordinal <> lv_tuple_ordinal OR sy-tabix = 1.
        lv_count_column2 += 1.
      ENDIF.
      lv_tuple_ordinal = <ls_axis0>-tuple_ordinal.

      " текст не переносить на следующую строку
      <ls_axis0>-caption = replace( val  = <ls_axis0>-caption
                                    sub  = cl_abap_char_utilities=>cr_lf
                                    with = ` `
                                    occ  = 0 ).

      " определяем тип, чтобы в дальнейшем определить колонку считывания данных
      ASSIGN lt_info2[ chanm = <ls_axis0>-chanm ] TO FIELD-SYMBOL(<ls_info2>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CASE <ls_info2>-chaprsnt. " представление признаков
        WHEN '0' OR '2'. " ключ и текст или ключ
          IF <ls_axis0>-chavl_ext = 'SUMME'.
            ls_string_tmp = <ls_axis0>-caption.
          ELSE.

            " для удобства чтения из CSV в EXCEL,
            " числовые целые значения переводим в символьные
            IF ( strlen( <ls_axis0>-chanm ) > 7 AND <ls_axis0>-chanm CO '0123456789' )
               OR <ls_axis0>-chanm = '0CALWEEK'.
              IF NOT ( <ls_axis0>-chanm = '0MATERIAL' AND iv_mat_frmt IS NOT INITIAL ).
                ls_string_tmp = |'{ <ls_axis0>-chavl_ext }|.
              ENDIF.
            ELSE.
              ls_string_tmp = <ls_axis0>-chavl_ext.
            ENDIF.
          ENDIF.
        WHEN '1' OR 'F'. " текст
          ls_string_tmp = <ls_axis0>-caption.
        WHEN OTHERS.
          ls_string_tmp = <ls_axis0>-caption.
      ENDCASE.

      append <ls_axis0>-CHAVL to lt_def_pok.
      ls_string_tmp = |{ ls_string_tmp };|.
      APPEND ls_string_tmp TO lt_data_sign.
      CLEAR ls_data_sign.
    ENDLOOP.

    " формируем название колонок признакв
    TRY.
        " считывание названий колонок
        DATA(lt_info1) = io_set->n_sx_version_20a_1-axis_info[ axis = '001' ]-chars.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    LOOP AT lt_info1 ASSIGNING FIELD-SYMBOL(<ls_info1>).
      ls_data_tmp = |{ ls_data_tmp }{ <ls_info1>-caption };|.
      lv_count_column1 += 1.

      add_def( |{ <ls_info1>-chanm }| ).

      IF ms_input-text IS NOT INITIAL.
        " добавляем дополнительную колонку текст
        CASE <ls_info1>-chaprsnt.
          WHEN 'B' OR 'C' OR 'D' OR 'E' OR 'F' OR 'G' OR '3' OR '0'.
            ls_data_tmp = ls_data_tmp && <ls_info1>-caption
                                                 && |.текст;|.
            add_def( |{ <ls_info1>-chanm }_text| ).
        ENDCASE.
      ENDIF.
      " добавляем дополнительные колонки атрибутов
      IF <ls_info1>-attrinm IS NOT INITIAL.
        LOOP AT <ls_info1>-attrinm ASSIGNING FIELD-SYMBOL(<ls_attrinm>).
          ls_data_tmp = |{ ls_data_tmp }{ <ls_attrinm>-caption };|.
          add_def( |{ <ls_info1>-chanm }| ).
          IF ms_input-text IS NOT INITIAL.
            " добавляем дополнительную колонку текст
            CASE <ls_attrinm>-chaprsnt.
              WHEN 'B' OR 'C' OR 'D' OR 'E' OR 'F' OR 'G' OR '3' OR '0'.
                ls_data_tmp = ls_data_tmp && <ls_attrinm>-caption
                                                     && |.текст;|.
                add_def( |{ <ls_info1>-chanm }_text| ).
            ENDCASE.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
    loop at lt_def_pok assigning FIELD-SYMBOL(<ls_pok>).
       add_def( |{ <ls_pok> }| ).
    endloop.

    finish_def( ).

    " определяем количество строк названий показателей
    IF lv_count_column2 IS NOT INITIAL.
      lv_count_row2 = lv_count_row2 DIV lv_count_column2.
    ELSEIF ls_data_tmp IS NOT INITIAL.
      lv_count_row2 = 1.
    ENDIF.

    " Склеиваем название колонок признаков и показателей
    DO lv_count_row2 TIMES. " цикл по строкам названий
      " формируем строку названий показателей
      CLEAR ls_data_sign.
      DATA(lv_row_tmp) = sy-index.
      DO lv_count_column2 TIMES. " цикл по столбцам
        DATA(lv_tab_index) = lv_row_tmp + ( sy-index - 1 ) * lv_count_row2.
        ASSIGN lt_data_sign[ lv_tab_index ] TO FIELD-SYMBOL(<ls_data_sign>).
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
        ls_data_sign = ls_data_sign && <ls_data_sign>.
      ENDDO.

      " склеиваем строку названий признаков со строкой названий показателей
      APPEND INITIAL LINE TO et_xls_content
             ASSIGNING FIELD-SYMBOL(<ls_xls_content>).
      ls_data_sign = ls_data_tmp && ls_data_sign.
      <ls_xls_content> = ls_data_sign.
      lv_column_tmp += 1.
    ENDDO.

    TRY.
        DATA(lt_axis1) = io_set->n_sx_version_20a_1-axis_data[ axis = '001' ]-set.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    " формируем значения показателей
    CLEAR: ls_data_tmp,
           ls_string_tmp,
           lv_column_tmp.
    LOOP AT io_set->n_sx_version_20a_1-cell_data ASSIGNING FIELD-SYMBOL(<ls_cell_data>).
      DATA(lv_tabix) = sy-tabix.

      IF ls_data_tmp IS INITIAL. " начало новой строки
        DO lv_count_column1 TIMES. " записываем значение признаков в стоках
          lv_column_tmp += 1.

          ASSIGN lt_axis1[ lv_column_tmp ] TO FIELD-SYMBOL(<ls_axis1>).
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
          " определяем тип признака, чтобы в дальнейшем
          " определить колонку считывания данных
          ASSIGN lt_info1[ chanm = <ls_axis1>-chanm ] TO <ls_info1>.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

          CLEAR ls_string_tmp.

          prepare_mark( EXPORTING iv_caption   = <ls_axis1>-caption
                                  iv_chavl_ext = <ls_axis1>-chavl_ext
                                  iv_chanm     = <ls_axis1>-chanm
                                  iv_chaprsnt  = <ls_info1>-chaprsnt
                                  iv_mat_frmt  = iv_mat_frmt
                                  iv_text      = ms_input-text
                        IMPORTING es_data_mark = ls_string_tmp ).

          ls_data_tmp = ls_data_tmp && ls_string_tmp.

          " добавляем дополнительные колонки атрибутов
          IF <ls_axis1>-attributes IS NOT INITIAL.
            LOOP AT <ls_axis1>-attributes ASSIGNING FIELD-SYMBOL(<ls_attributes>).
              " определяем тип признака, чтобы в дальнейшем
              " определить колонку считывания данных
              ASSIGN <ls_info1>-attrinm[ attrinm = <ls_attributes>-attrinm ] TO <ls_attrinm>.
              IF sy-subrc <> 0.
                CONTINUE.
              ENDIF.

              CLEAR ls_string_tmp.

              prepare_mark( EXPORTING iv_caption   = <ls_attributes>-caption
                                      iv_chavl_ext = <ls_attributes>-attrivl
                                      iv_chaprsnt  = <ls_attrinm>-chaprsnt
                                      iv_mat_frmt  = iv_mat_frmt
                                      iv_text      = ms_input-text
                            IMPORTING es_data_mark = ls_string_tmp ).

              ls_data_tmp = ls_data_tmp && ls_string_tmp.
            ENDLOOP.
          ELSE.
            ASSIGN lt_info1[ chanm = <ls_axis1>-chanm ] TO <ls_info1>.
            IF sy-subrc <> 0.
              CONTINUE.
            ENDIF.
            LOOP AT <ls_info1>-attrinm ASSIGNING <ls_attrinm>.

              CLEAR ls_string_tmp.
              CALL FUNCTION 'ZBW_PREPARE_MARK'
                EXPORTING iv_caption   = <ls_axis1>-caption
                          iv_chavl_ext = 'SUMME'
                          iv_chaprsnt  = <ls_attrinm>-chaprsnt
                          iv_mat_frmt  = iv_mat_frmt
                IMPORTING es_data_mark = ls_string_tmp.
              ls_data_tmp = ls_data_tmp && ls_string_tmp.
            ENDLOOP.
          ENDIF.
        ENDDO.
      ENDIF.

      DATA(lv_value) = replace( val  = <ls_cell_data>-value
                                sub  = '.'
                                with = ',' ).

      ls_data_tmp = |{ ls_data_tmp }{ lv_value };|.

      " если конец строки по показателям
      IF lv_count_column2 = 0 OR lv_tabix MOD lv_count_column2 = 0.
        APPEND INITIAL LINE TO et_xls_content ASSIGNING <ls_xls_content>.
        <ls_xls_content> = ls_data_tmp.

        " проверка "левых" символов
        DATA(lv_strlen) = strlen( <ls_xls_content> ).
        DO lv_strlen TIMES.
          DATA(lv_i) = sy-index - 1.

          IF <ls_xls_content>+lv_i(1) NOT IN gr_limit.
            lv_char = <ls_xls_content>.
            lv_char+lv_i(1) = `·`.
            <ls_xls_content> = lv_char.
          ENDIF.
        ENDDO.
        CLEAR ls_data_tmp.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_var.
    rt_var = ms_input-var_t.
    IF ir IS SUPPLIED.

      DATA(lt_var) = VALUE rrx1_t_var(
          FOR ls_itab IN ir
          ( sign = ls_itab-sign opt = ls_itab-option low = ls_itab-low high = ls_itab-high  vnam = ls_itab-vnam   ) ).
      APPEND LINES OF lt_var TO rt_var.
    ENDIF.
  ENDMETHOD.



  METHOD get_objnm_key.
    mv_objnm_row = lo->n_s_state-struc1_iobjnm. " Key Figures fo PL
    mv_objnm_column = lo->n_s_state-struc2_iobjnm. " Структура отчета о прибылях и убытках для BW
  ENDMETHOD.

  METHOD add_columns.
    IF lines( ms_input-add_clm ) > 0.
      DATA(lv_index) = lines( ms_input-add_clm ).
      LOOP AT ms_input-add_clm ASSIGNING FIELD-SYMBOL(<ls_clm>).
        add_column( EXPORTING i_name = CONV #( <ls_clm>-low )
                    CHANGING  co     = co  ).
      ENDLOOP.
      add_column( EXPORTING i_name   = CONV #( mv_objnm_column )
                            i_parent = ms_input-add_clm[ lv_index ]-low
                  CHANGING  co       = co  ).
    ENDIF.
  ENDMETHOD.


  METHOD add_column.
    co->set_axis_position( EXPORTING  i_iobjnm      = conv #( i_name )
                                      i_axis        = i_axis " x column Y row
                                      i_parent_iobjnm = i_parent
*                                      i_r_parameter =
                           EXCEPTIONS x_message     = 1
                                      no_processing = 2
                                      OTHERS        = 3 ).
  ENDMETHOD.

  METHOD change_request.
    DATA(lo_mem) = co->n_sx_request-mem.
    IF lines( ms_input-key_clm ) <> 0.
      LOOP AT lo_mem ASSIGNING FIELD-SYMBOL(<ls_mem>) WHERE iobjnm = mv_objnm_row AND hidden = space.
        IF <ls_mem>-chavltxt NOT IN ms_input-key_clm.
          <ls_mem>-hidden = 'X'.
        ENDIF.
      ENDLOOP.
    ENDIF.

    DATA(lo_dim) = co->n_sx_request-dim.
    LOOP AT lo_dim ASSIGNING FIELD-SYMBOL(<ls_dim>) WHERE axis <> space AND nosums = space.
      <ls_dim>-nosums = 'U'.
    ENDLOOP.
    co->set_request( EXPORTING  i_t_dim       = lo_dim
                                i_t_mem       = lo_mem
                                i_t_cel       = co->n_sx_request-cel
                                i_t_atr       = co->n_sx_request-atr
                                i_t_con       = co->n_sx_request-con
                                i_t_fac       = co->n_sx_request-fac
                                i_t_prptys    = co->n_sx_request-prptys
*                                i_t_hry_types =
*                                i_t_drill     =
*      IMPORTING
*                                e_warnings    =
                     EXCEPTIONS no_processing = 1
                                x_message     = 2
                                OTHERS        = 3 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD upload.
    DATA(file) = |{ iv_folder }{ iv_name }.{ iv_extension }|.

    TRY.
        OPEN DATASET file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT WITH SMART LINEFEED.
        IF sy-subrc <> 0.
          WRITE / |Dataset for file { file } has errors while opening|.
        ENDIF.
        LOOP AT it ASSIGNING FIELD-SYMBOL(<fs>).
          TRANSFER <fs> TO file.
        ENDLOOP.
        CLOSE DATASET file.
         WRITE: / | File created: { file }|.
      CATCH cx_root INTO DATA(lx_root).
        DATA(lv_err) = lx_root->kernel_errid.
        WRITE / | Error creating { file }: { lv_err }|.
    ENDTRY.
  ENDMETHOD.

  METHOD set_filters.
    IF lines( ms_input-fil_t ) <= 0.
      RETURN.
    ENDIF.

    LOOP AT ms_input-fil_t ASSIGNING FIELD-SYMBOL(<ls_fil>).

      co->set_filter( EXPORTING  i_iobjnm           = <ls_fil>-iobjnm
*                                 i_niobjnm          = '0CALMONTH'
                                 i_chavl_high       = <ls_fil>-high
                                 i_chavl            = <ls_fil>-low
*                                 i_chavl_high_ext   =
*                                 i_chavl_ext        =
                                 i_opt              = <ls_fil>-opt
                                 i_sign             = <ls_fil>-sign
*                                 i_axis_position    =
*                                 i_sid              =
                                 i_append_to_filter = rs_c_true
                                 i_remove_from_axis = rs_c_false
*                                 i_value_type       = c_filter_value_type_value
                      EXCEPTIONS no_processing      = 1
                                 x_message          = 2
                                 OTHERS             = 3 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_def.
    CHECK mv_def_create IS INITIAL.
    " Создаем структуру для def файла

    cl_rsdha_ut_datatype=>map_abap_descr_to_sqltype(
      EXPORTING i_r_abap_elemdescr = CAST cl_abap_elemdescr( cl_abap_datadescr=>describe_by_name( 'RSTXTLG' ) )
      IMPORTING e_sql_type         = DATA(e_sql_type)
                e_sql_length       = DATA(e_sql_length)
                e_sql_scale        = DATA(e_sql_scale)
      " TODO: variable is assigned but only used in commented-out code (ABAP cleaner)
      " TODO: variable is assigned but never used (ABAP cleaner)
                e_sql_precision    = DATA(e_sql_precision) ).
    APPEND VALUE #( ) TO mt_def ASSIGNING FIELD-SYMBOL(<ls_def>).
    <ls_def> = |{ iv  };{ e_sql_type };{ e_sql_length };{ e_sql_scale }|.

  ENDMETHOD.


  METHOD finish_def.
    mv_def_create = abap_true.
  ENDMETHOD.


  METHOD get_def.
    rt = mt_def.
  ENDMETHOD.

  METHOD upload_xstring.
    DATA(file) = |{ iv_folder }{ iv_name }.{ iv_extension }|.

    TRY.
        OPEN DATASET file FOR OUTPUT IN BINARY MODE .
        IF sy-subrc <> 0.
          WRITE / |Dataset for file { file } has errors while opening|.
        ENDIF.

        TRANSFER iv_xsctring TO file.
        CLOSE DATASET file.
        WRITE / | File created: { file }|.
      CATCH cx_root INTO DATA(lx_root).
        DATA(lv_err) = lx_root->kernel_errid.
        WRITE / | Error creating { file }: { lv_err }|.
    ENDTRY.
  ENDMETHOD.

  METHOD get_xstring_tab.
    FIELD-SYMBOLS <fs_data> TYPE ANY TABLE.

    ASSIGN ir->* TO <fs_data>.
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_table)
                                CHANGING  t_table      = <fs_data> ).

        DATA(lt_fcat) =
          cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = lo_table->get_columns( )
                                                             r_aggregations = lo_table->get_aggregations( ) ).

        DATA(lo_result) =
          cl_salv_ex_util=>factory_result_data_table( r_data         = ir
                                                      t_fieldcatalog = lt_fcat ).

        cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform(
          EXPORTING xml_type      = if_salv_bs_xml=>c_type_xlsx
                    xml_version   = cl_salv_bs_a_xml_base=>get_version( )
                    r_result_data = lo_result
                    xml_flavour   = if_salv_bs_c_tt=>c_tt_xml_flavour_export
                    gui_type      = if_salv_bs_xml=>c_gui_type_gui
          IMPORTING xml           = rv_xstring ).
      CATCH cx_root INTO DATA(lo). " TODO: variable is assigned but never used (ABAP cleaner)
        CLEAR rv_xstring.
        " Скорее всего просто нет структры, так что передадим склейкой
        DATA lv_string TYPE string.
        LOOP AT <fs_data> ASSIGNING FIELD-SYMBOL(<ls>).
          lv_string = lv_string && <ls> && cl_abap_char_utilities=>newline.
        ENDLOOP.

        rv_xstring = cl_abap_conv_codepage=>create_out( codepage = `UTF-8`
                                            )->convert( source = lv_string ).

    ENDTRY.
  ENDMETHOD.

  METHOD create_zip.
    mo_zip = NEW cl_abap_zip( ).

    mo_zip->support_unicode_names = abap_true.

  ENDMETHOD.


  METHOD add_file_to_zip.
    mo_zip->add(
      EXPORTING
        name           = iv_name
        content        = iv_xstring
*        compress_level = 6
    ).
  ENDMETHOD.

  METHOD save_zip.
    rv = mo_zip->save( ).
  ENDMETHOD.

  METHOD prepare_mark.
    DATA ls_string_tmp TYPE string. " для проверки

    CASE iv_chaprsnt.
      WHEN 'B' OR 'C' OR 'D' OR 'E' OR 'F' OR 'G' OR '3' OR '0' OR '2'. " представление
        IF iv_chavl_ext = 'SUMME'.
          ls_string_tmp = iv_caption.
        ELSE.
          ls_string_tmp = iv_chavl_ext.
        ENDIF.
      WHEN OTHERS.
        ls_string_tmp = iv_caption.
    ENDCASE.

    " текст не переносить на следующую строку
    ls_string_tmp = replace( val  = ls_string_tmp
                             sub  = cl_abap_char_utilities=>cr_lf
                             with = ` `
                             occ  = 0 ).

    " для удобства чтения из CSV в EXCEL,
    " числовые целые значения переводим в символьные
    IF ( strlen( ls_string_tmp ) > 7 AND ls_string_tmp CO '0123456789' )
       OR iv_chanm = '0CALWEEK'.
      IF NOT ( iv_chanm = '0MATERIAL' AND iv_mat_frmt IS NOT INITIAL ).
        ls_string_tmp = |'{ ls_string_tmp }|.
      ENDIF.
    ENDIF.
    REPLACE ALL OCCURRENCES OF ';' IN ls_string_tmp WITH ','.
    ls_string_tmp = |{ ls_string_tmp };|.

    IF iv_text IS NOT INITIAL.
      " добавляем дополнителную колонку .текст
      CASE iv_chaprsnt.
        WHEN 'B' OR 'C' OR 'D' OR 'E' OR 'F' OR 'G' OR '3' OR '0'.
          ls_string_tmp = |{ ls_string_tmp }{ iv_caption };|.
      ENDCASE.
    ENDIF.
    es_data_mark = ls_string_tmp.
  ENDMETHOD.

ENDCLASS.