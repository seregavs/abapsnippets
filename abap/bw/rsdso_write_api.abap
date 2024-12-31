        l_text =
          |{ ls_data-ac_doc_no };| &&
          |{ ls_data-comp_code };| &&
          |{ ls_data-calyear };| &&
          |{ ls_data-ref_key3 };| &&
          |{ ls_data-doc_date DATE = ENVIRONMENT };| &&

https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abapcompute_string_format_options.htm#!ABAP_ADDITION_14@14@

  CALL FUNCTION 'RSDSO_WRITE_API'
    EXPORTING
      i_adsonm        = lc_adso
      i_activate_data = rs_c_false
      it_data         = lt_zgtdo19[].


  lt_zgtdo19[] = CORRESPONDING #( lt_hist[] ).

  LOOP AT lt_zgtdo19 REFERENCE INTO DATA(lref_x)
  WHERE  algo = 'X'.
    INSERT VALUE #( material = lref_x->material ) INTO TABLE lt_s_material.
  ENDLOOP.

  IF lt_s_material[] IS NOT INITIAL.
    SELECT material , val_class
      FROM /bi0/mmaterial
      FOR ALL ENTRIES IN @lt_s_material
      WHERE material = @lt_s_material-material
        AND objvers = 'A'
      INTO TABLE @lt_s_material.

    LOOP AT lt_zgtdo19 REFERENCE INTO lref_x
    WHERE  algo = 'X'.
      lref_x->val_class = lt_s_material[ material = lref_x->material ]-val_class  .
    ENDLOOP.
  ENDIF.
==============================

            CALL METHOD cl_rsar_function=>right(
              EXPORTING
                i_len    = l_arg2
                i_string = l_arg3
              IMPORTING
                e_string = l_arg1 ).
