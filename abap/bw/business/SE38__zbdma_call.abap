REPORT zbdma_stage.

PARAMETERS: pdbcon    TYPE dbcon_name OBLIGATORY DEFAULT 'HDBBDMA',
            pgroup(2) TYPE c OBLIGATORY DEFAULT '1'.

WRITE: / |Выгрузка данных в BDMA. Начало. Соединение: { pdbcon }. Группа { pgroup }|.

SELECT * INTO TABLE @DATA(lt_bdma_stage)
 FROM zbdma_stage WHERE active = 'X' AND groupid = @pgroup.
SORT lt_bdma_stage BY orderno ASCENDING.

TRY.
    DATA(lo_conn) = cl_sql_connection=>get_connection( con_name = pdbcon ).
    DATA(lo_statement) = lo_conn->create_statement( ).
    LOOP AT lt_bdma_stage ASSIGNING FIELD-SYMBOL(<fs_bdma_stage>).
      DATA(lv_rcnt) = lo_statement->execute_procedure( proc_name = <fs_bdma_stage>-procname ).
      WRITE: / | Процедура { <fs_bdma_stage>-procname } выполнена. { lv_rcnt } строк обработано.|.
      MESSAGE  | Процедура { <fs_bdma_stage>-procname } выполнена. { lv_rcnt } строк обработано.| TYPE 'I'.
      CLEAR: lv_rcnt.
    ENDLOOP.

    IF lo_conn->is_closed( ) = ''.
      lo_conn->close( ).
    ENDIF.
    WRITE: / |Выгрузка данных в BDMA. Окончание.|.
  CATCH cx_sql_exception INTO DATA(lx_sql).
    WRITE: / |!Ошибка выгрузки данных в BDMA. Процедура { <fs_bdma_stage>-procname }.|.
    MESSAGE  |!Ошибка выгрузки данных в BDMA. Процедура { <fs_bdma_stage>-procname }. См. spool.| TYPE 'E'.
    WRITE: / |!Код ошибки { lx_sql->sql_code }.|.
    WRITE: / |! { lx_sql->sql_message }|.
    WRITE: / |! { lx_sql->get_text( ) }|.
ENDTRY.