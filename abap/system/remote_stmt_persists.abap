REPORT ZBW_M_REMOTE_STMT_PERSIST.

*--- Selection screen: Parameter to input the number of days to retain records
PARAMETERS: p_ret TYPE i DEFAULT 120. " Number of days to keep records

*--- Logging Variables
DATA: lv_message        TYPE string,
      lv_sql_error_text TYPE string.

*--- SQL connection object
DATA: lr_sql_conn TYPE REF TO cl_sql_connection,
      lr_sql_stmt TYPE REF TO cl_sql_statement,
      lr_sql_res  TYPE REF TO cl_sql_result_set.

*--- SQL statement for insertion
DATA: gc_sql_stmt TYPE string.
gc_sql_stmt =
  | UPSERT ZSDA_REMOTE_STMT  | &
  |SELECT | &
  |    HASH_SHA256( | &
  |        BINTOHEX(TO_BINARY(STATEMENT_ID)) { '|' }{ '|' } | &
  |        BINTOHEX(TO_BINARY(REMOTE_SOURCE_NAME)) { '|' }{ '|' } | &
  |        BINTOHEX(TO_BINARY(START_TIME)) { '|' }{ '|' } | &
  |        BINTOHEX(TO_BINARY(END_TIME)) { '|' }{ '|' } | &
  |        BINTOHEX(TO_BINARY(FETCHED_RECORD_COUNT)) { '|' }{ '|' } | &
  |        BINTOHEX(TO_BINARY(FETCHED_SIZE)) { '|' }{ '|' } | &
  |        BINTOHEX(TO_BINARY(REMOTE_STATEMENT_STATUS)) { '|' }{ '|' } | &
  |        BINTOHEX(TO_BINARY(REMOTE_STATEMENT_STRING))| &
  |    ) AS HASHKEY,| &
  |    STATEMENT_ID,| &
  |    REMOTE_SOURCE_NAME, | &
  |    TO_DECIMAL(TO_VARCHAR(START_TIME, 'YYYYMMDDHH24MISS')) AS START_TIME,| &
  |    TO_DECIMAL(TO_VARCHAR(END_TIME, 'YYYYMMDDHH24MISS')) AS END_TIME, | &
  |    CAST(SECONDS_BETWEEN(START_TIME, END_TIME) AS INT) AS FETCH_TIME_S,| &
  |    FETCHED_RECORD_COUNT,| &
  |    FETCHED_SIZE,| &
  |    REMOTE_STATEMENT_STATUS, | &
  |    LEFT(USER_NAME, 20), | &
  |    LEFT(REMOTE_STATEMENT_STRING, 8192) AS REMOTE_STATEMENT_STRING | &
  |FROM | &
  |    sys.M_REMOTE_STATEMENTS src | &
  |WHERE | &
  |    NOT ( src.end_time is Null ) |.


*--- Start of the program logic
START-OF-SELECTION.

  TRY.
      " Create SQL connection
      lr_sql_conn = cl_sql_connection=>get_connection( ).

      " Create SQL statement
      lr_sql_stmt = lr_sql_conn->create_statement( ).

      " Execute the SQL statement for insertion
      lr_sql_stmt->execute_update( gc_sql_stmt ).

      " Log success message
      lv_message = 'Insert OK'.
      WRITE: / lv_message.

    CATCH cx_sql_exception INTO DATA(lx_sql_exception).
      lv_sql_error_text = lx_sql_exception->get_text( ).
      lv_message = 'Insert FAILED. SQL execution failed: ' && lv_sql_error_text.
      WRITE: / lv_message.
  ENDTRY.

*--- Archiving step: delete old records from ZSDA_REMOTE_STMT based on retention period
  DATA: lv_today_date TYPE timestampl.
  GET TIME STAMP FIELD lv_today_date.

  " Calculate cutoff date for deletion
  DATA(lv_cutoff_date) = cl_abap_tstmp=>SUBTRACTSECS_TO_SHORT( tstmp = lv_today_date
                                            secs  = p_ret * 24 * 60 * 60 ).

  TRY.
      " SQL statement for archiving
      lr_sql_stmt->execute_update( |DELETE FROM ZSDA_REMOTE_STMT WHERE END_TIME < { lv_cutoff_date }| ).

      " Log success message
      lv_message = 'Archiving completed successfully. Old records deleted.'.
      WRITE: / lv_message.

    CATCH cx_sql_exception INTO lx_sql_exception.
      lv_sql_error_text = lx_sql_exception->get_text( ).
      lv_message = 'Archiving failed: ' && lv_sql_error_text.
      WRITE: / lv_message.
  ENDTRY.
================================

