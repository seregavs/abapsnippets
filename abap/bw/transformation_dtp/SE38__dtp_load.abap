REPORT ZDTPLOAD.

TABLES: rsbkdtpt.
DATA: lcl_dtp     TYPE REF TO cl_rsbk_dtp,
      lcl_request TYPE REF TO cl_rsbk_request,
      lcl_filter  TYPE REF TO cl_rsbc_filter,
*      w_loc_dtp   TYPE rsbkdtpnm,
      lv_dtps     TYPE TABLE OF rsbkdtpnm.

*  w_loc_dtp = 'DTP_ARXABGLIEOU3NCJK1RFPINBBK'.
  PARAMETERS:
    p_dtp TYPE rsbkdtpnm.

  lcl_dtp = cl_rsbk_dtp=>factory( p_dtp ).
  cl_rspm_runtime=>remove_named_runtime( i_instance_name = if_rspm_types=>co_runtime_mode-db_read_only
                                 && p_dtp ).
  TRY.
      CALL METHOD lcl_dtp->if_rsbk_dtp_execute~create_request
        RECEIVING
          r_r_request = lcl_request.
      lcl_request->set_ctype( rsbc_c_ctype-sync ).
    CATCH cx_rs_not_found .
    CATCH cx_rs_foreign_lock .
    CATCH cx_rs_failed .
  ENDTRY.
  TRY.
      CALL METHOD lcl_request->if_rsbk_request~doit
        EXPORTING
          i_no_commit = rs_c_false.
    CATCH cx_rs_failed .
ENDTRY.