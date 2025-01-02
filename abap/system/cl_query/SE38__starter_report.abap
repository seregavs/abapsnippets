&---------------------------------------------------------------------*
*& Report z_pl06
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_pl06.

PARAMETERS: p_year TYPE gjahr,
            p_name TYPE store_label,
            p_full TYPE flag,
            p_step TYPE syst_tabix.

class lcl_helper definition create private.

  public section.

    class-METHODS create_instance
              RETURNING
                VALUE(lo_help) TYPE REF TO lcl_helper.
    METHODS change_input
      CHANGING
        cs_input TYPE zcl_zcomcp01_q006_uparams=>ts_input.
    METHODS get_plant
      RETURNING
        value(rt) TYPE zcl_query=>tt_range_var.
    METHODS show_process
      IMPORTING
        iv_index TYPE any
        iv_total TYPE any.
  protected section.
  private section.

endclass.

class lcl_helper implementation.
  METHOD create_instance.
    lo_help = NEW lcl_helper( ).
  ENDMETHOD.

  METHOD change_input.
    IF p_year IS NOT INITIAL.
      cs_input-year = p_year.
    ENDIF.

    IF p_name IS NOT INITIAL.
      cs_input-name = p_name.
    ENDIF.

    IF p_full IS NOT INITIAL.
      cs_input-flag = p_full.
    ENDIF.

    IF p_step IS NOT INITIAL.
      cs_input-step = p_step.
    ENDIF.

    LOOP AT CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( cs_input ) )->get_components( )
         INTO DATA(wa_structure_field).
      CASE wa_structure_field-type->kind.
        WHEN 'T'.
          " Табличные значения, не меняем или меняем другим образом
        WHEN OTHERS.
          IF wa_structure_field-type->type_kind <> 'I'.
            " Значения int так же не меняем
            ASSIGN COMPONENT wa_structure_field-name OF STRUCTURE cs_input TO FIELD-SYMBOL(<lv_value>).
            REPLACE ALL OCCURRENCES OF '{sy-datum}' IN <lv_value> WITH sy-datum.
            REPLACE ALL OCCURRENCES OF '{mv_year}' IN <lv_value> WITH sy-datum(4).
          ENDIF.

      ENDCASE.
    ENDLOOP.

    LOOP AT cs_input-var_t ASSIGNING FIELD-SYMBOL(<ls_var>).
      REPLACE ALL OCCURRENCES OF '{mv_year}' IN <ls_var>-low WITH cs_input-year.
    ENDLOOP.
    LOOP AT cs_input-key_clm ASSIGNING FIELD-SYMBOL(<ls_key>).
      REPLACE ALL OCCURRENCES OF '{mv_year}' IN <ls_key>-low WITH cs_input-year.
    ENDLOOP.

    DATA(lv_data) = sy-datum+4(2).
    CASE lv_data.
      WHEN 1.
        DATA(low) = |{ cs_input-year - 1 }12|.
      WHEN OTHERS.
        low = |{ cs_input-year }{ lv_data - 1 }|.
    ENDCASE.
    DATA(high) = |{ cs_input-year }{ lv_data }|.
    cs_input-fil_t = VALUE #( (  iobjnm = '0CALMONTH' sign = 'I' opt = 'BT' low = low high = high   ) ).
  ENDMETHOD.


  METHOD get_plant.
  SELECT 'I'       AS sign,
         'EQ'      AS option,
         plant     AS low,
         'IPLNTCO' AS vnam
    FROM /bi0/pplant
    INTO CORRESPONDING FIELDS OF TABLE @rt
*    UP TO 1 rows
    WHERE plant    BETWEEN '0001' AND '6999'
      AND plantcat       = 'A'
      AND objvers        = 'A'
      AND gln           <> @space
*      and plant = '0015'
      .
  ENDMETHOD.


  METHOD show_process.
  data(lv_txt) = |{ cl_abap_context_info=>get_system_time( ) TIME = USER } |.
      cl_progress_indicator=>progress_indicate( i_text               = | Сделано { iv_index } / { iv_total } время { lv_txt } |
*                                                i_msgid              = i_msgid    " Message Class (If I_TEXT is not transferred)
*                                                i_msgno              = i_msgno    " Message Number (If I_TEXT is not transferred)
*                                                i_msgv1              = i_msgv1    " Message Variable (Maximum of 50 Characters)
*                                                i_msgv2              = i_msgv2    " Message Variable (Maximum of 50 Characters)
*                                                i_msgv3              = i_msgv3    " Message Variable (Maximum of 50 Characters)
*                                                i_msgv4              = i_msgv4    " Message Variable (Maximum of 50 Characters)
                                                i_processed          = iv_index    " Number of Objects Already Processed
                                                i_total              = iv_total    " Total Number of Objects to Be Processed
                                                i_output_immediately = abap_false    " X = Display Progress Immediately
*  importing
*                                                e_progress_sent      = e_progress_sent    " X = Progress Information Was Displayed
).
      IF sy-batch IS NOT INITIAL.
        WRITE / | Сделано { iv_index } / { iv_total } время { lv_txt } |.
      ENDIF.
  ENDMETHOD.

endclass.

START-OF-SELECTION.
  DATA(is_input) = VALUE zcl_zcomcp01_q006_uparams=>ts_input( ).
  is_input-query    = zcl_zcomcp01_q006_uparams=>query.
  is_input-infocube = zcl_zcomcp01_q006_uparams=>infocube.
  is_input-folder   = zcl_zcomcp01_q006_uparams=>folder.
  is_input-name     = zcl_zcomcp01_q006_uparams=>name.
  is_input-flag     = zcl_zcomcp01_q006_uparams=>flag.
  is_input-year     = zcl_zcomcp01_q006_uparams=>year.
  is_input-var_t    = zcl_zcomcp01_q006_uparams=>var_t.
  is_input-add_clm  = zcl_zcomcp01_q006_uparams=>add_clm.
  is_input-key_clm  = zcl_zcomcp01_q006_uparams=>key_clm.
  is_input-fil_t    = zcl_zcomcp01_q006_uparams=>fil_t.
  is_input-name_def = zcl_zcomcp01_q006_uparams=>name_def.
  is_input-step     = zcl_zcomcp01_q006_uparams=>step.
  is_input-zip     = zcl_zcomcp01_q006_uparams=>zip.

  DATA(lo_help) = lcl_helper=>create_instance( ).
  lo_help->change_input( CHANGING cs_input = is_input  ).
  DATA(lr_plant) = lo_help->get_plant( ).
  DATA(lo) = NEW zcl_query( is_input  ).

  DATA lr_temp  TYPE zcl_query=>tt_range_var.
  DATA lv_index TYPE syst-index.
  DATA lt_csv   TYPE zcl_query=>tt_xls.

  DATA(lv_total) = lines( lr_plant  ).
  DATA(lv_step) = is_input-step.
  WHILE lines( lr_plant ) > 0.
    IF lv_index = 0.
      lo_help->show_process( iv_index = lv_index
                             IV_total = lv_total ).
    ENDIF.
    lv_index += lv_step.
    FREE lr_temp.
    APPEND LINES OF lr_plant FROM 1 TO lv_step TO lr_temp.
    DELETE lr_plant FROM 1 TO lv_step.
*    IF lines( lt_csv ) = 0.
*      APPEND LINES OF lo->get_csv( lr_temp  ) TO lt_csv.
*    ELSE.
    APPEND LINES OF lo->get_csv( lr_temp  ) FROM 2 TO lt_csv.
*    ENDIF.
    lo_help->show_process( iv_index = lv_index
                           IV_total = lv_total ).

  ENDWHILE.

" Для participation hadoop со слов Натальи
  REPLACE ALL OCCURRENCES OF REGEX '^(\d{2})\.(\d{4})'
          IN TABLE lt_csv WITH '$2$1'
          RESPECTING CASE.

  IF is_input-zip IS NOT INITIAL.
    lo->create_zip( ).

    DATA(lt_def) = lo->get_def( ).

    lo->add_file_to_zip( iv_name    = |{ is_input-name }.csv|
                         IV_XsTRING = lo->get_xstring_tab( REF #( lt_csv ) )    ).

    lo->add_file_to_zip( iv_name    = |{ is_input-name_def }.def|
                         IV_XsTRING = lo->get_xstring_tab( REF #( lt_def ) )    ).

    lo->upload_xstring( iv_xsctring  = lo->save_zip( )
                        iv_name      = |{  is_input-name }|
                        iv_extension = 'zip'
                        iv_folder    = is_input-folder ).
  ELSE.
    IF lines( lt_csv ) <> 0.
      lo->upload( it           = lt_csv
                  iv_name      = CONV #( is_input-name )
                  iv_extension = 'csv'
                  iv_folder    = CONV #( is_input-folder ) ).
    ENDIF.

    IF lines( lo->get_def( ) ) <> 0.
      lo->upload( it           = lo->get_def( )
                  iv_name      = CONV #( is_input-name_def )
                  iv_extension = 'csv'
                  iv_folder    = CONV #( is_input-folder ) ).
    ENDIF.
  ENDIF.