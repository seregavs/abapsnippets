RSU5_SAPI_BADI
BADI extractor extension

  method if_ex_rsu5_sapi_badi~data_transform.
    types:
     tt_bwe_equi type table of bwe_equi. " ñòðóêòóðà ýêñòðàêöèè

    field-symbols: <lt_data> type tt_bwe_equi,
                   <ls_data> type bwe_equi.
    check i_datasource = '0EQUIPMENT_ATTR'.

    assign c_t_data[] to <lt_data>.

    loop at <lt_data> assigning <ls_data>.

    endloop.
  endmethod.