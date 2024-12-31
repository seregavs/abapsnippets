MT_COMPONENTS TYPE ABAP_COMPONENT_TAB.
 
   mt_components = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( '/BIC/AZABCO102' ) )->get_components( ).

 
 METHOD set_select_script.
    DATA lv_row  TYPE ty_string.
    DATA lv_date TYPE n LENGTH 6 VALUE '202301'.

    CLEAR me->i_select_script.
    DATA(lv_start_regex) = | '"' \|\| REPLACE_REGEXPR('[\\r\\n]' IN REPLACE_REGEXPR('(["\\\\])' IN ifnull(|.
    DATA(lv_end_regex) = |,'') WITH '\\\\\\1') WITH '') \|\| '"' as|.
    DATA(lv_start_regex_p) = | '"' \|\| REPLACE_REGEXPR('[\\r\\n]' IN REPLACE_REGEXPR('(["\\\\])' IN |.
    DATA(lv_end_regex_p) = | WITH '\\\\\\1') WITH '') \|\| '"' as|.
    DATA(lv_lines) = lines( mt_components ).
    LOOP AT mt_components ASSIGNING FIELD-SYMBOL(<ls>).
      DATA(lv_name_as) = replace( val   = <ls>-name
                                  regex = '/BIC/'
                                  with  = 'BIC_' ).
      CASE sy-tabix.
        WHEN 1.
          lv_row-row = | SELECT \n|.
          lv_row-row = |{ lv_row-row }| & |{ lv_start_regex } "{ <ls>-name }" { lv_end_regex } "{ lv_name_as }", \n|.

        WHEN lv_lines.
          CASE <ls>-type->type_kind.
            WHEN 'P'.
              lv_row-row = |{ lv_row-row }| & |{ lv_start_regex_p } "{ <ls>-name }" { lv_end_regex_p } "{ lv_name_as }" \n|.
            WHEN OTHERS.
              lv_row-row = |{ lv_row-row }| & |{ lv_start_regex } "{ <ls>-name }" { lv_end_regex } "{ lv_name_as }" \n|.
          ENDCASE.
          lv_row-row = |{ lv_row-row } from "{ i_dbschema }"."/BIC/AZABCO102" \n|.
          lv_row-row = |{ lv_row-row } where "CALMONTH" >= '{ lv_date }'|.
        WHEN OTHERS.
          CASE <ls>-type->type_kind.
            WHEN 'P'.
              lv_row-row = |{ lv_row-row }| & |{ lv_start_regex_p } "{ <ls>-name }" { lv_end_regex_p } "{ lv_name_as }", \n|.
            WHEN OTHERS.
              lv_row-row = |{ lv_row-row }| & |{ lv_start_regex } "{ <ls>-name }" { lv_end_regex } "{ lv_name_as }", \n|.
          ENDCASE.

      ENDCASE.

    ENDLOOP.
    APPEND lv_row TO me->i_select_script.
  ENDMETHOD.