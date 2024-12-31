*&---------------------------------------------------------------------*
*& Report ZBW_CST_UNLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbw_cst_unload.

PARAMETERS: padso TYPE rsoadsonm DEFAULT 'ALCOO01'.

DATA: lv_tname TYPE char30.

DATA:
  lo_conn      TYPE REF TO cl_sql_connection,
  lo_statement TYPE REF TO cl_sql_statement,
  lx_sql       TYPE REF TO cx_sql_exception.

TRY.
    lo_conn = cl_sql_connection=>get_connection( ).
    lo_statement = lo_conn->create_statement( ).
    DO 3 TIMES.
      lv_tname = |/BIC/A{ padso }{ sy-index }|.
      DATA(l_row_cnt) = lo_statement->execute_update( |UNLOAD "{ lv_tname }";| ).
      WRITE : / |"{ lv_tname }" unloaded|.
    ENDDO.
  CATCH cx_sql_exception INTO lx_sql.
    WRITE: / lx_sql->get_text( ).
    WRITE: / lx_sql->sql_code.
    WRITE: / lx_sql->sql_message.
    RETURN.
ENDTRY.