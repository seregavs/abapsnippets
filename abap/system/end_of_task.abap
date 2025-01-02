      data(l_employees_block_low_current) = 1.
      data(l_employees_block_high_current) = 1.
      describe table l_t_employees lines data(recs_current).

      "process data in parallel
      while l_employees_block_low_current <= recs_current.
        l_employees_block_high_current = l_employees_block_low_current + i_batch_size - 1.

        "блок табельных номеров для очередной thread
        clear l_t_employees_small.
        append lines of l_t_employees
          from l_employees_block_low_current to l_employees_block_high_current
          to l_t_employees_small.
        l_employees_block_low_current = l_employees_block_high_current + 1.

        call method get_data_parallel
          exporting
            lt_pernr = l_t_employees_small.

        if sy-subrc <> 0.
          raise no_more_data.
        endif.
      endwhile.

=========================

  method get_data_parallel.
    "получаем стаж
    data(thread) = me->handler->get_free_thread( ).
    data errmsg type char255.

    call function 'ZBW_GET_EXPERIENCE' starting new task thread
      destination in group zcl_bw_parallel_handler=>c_default_group
      calling on_end_of_action on end of task
      exporting
        lt_pernr              = lt_pernr
        lt_calmon             = lt_calmon
      exceptions
        communication_failure = 1 message errmsg
        system_failure        = 2 message errmsg
        resource_failure      = 3
        others                = 4.

    case sy-subrc.
      when 0.
      when 1 or 2. "communication_failure
        log_write( ).
        raise error_passed_to_mess_handler.
      when 3. " resource_failure
        "wait for a free task and do the same again
        me->handler->handle_resource_failure( ).
      when others.
        log_write( ).
        raise error_passed_to_mess_handler.
    endcase.
  endmethod.

==============

  method on_end_of_action.
    data:
      lt_pernr like lt_results.

    receive results from function 'ZBW_GET_EXPERIENCE'
      tables
        et_pernr =  lt_pernr
        et_mess  =  lt_mess
    .

    case sy-subrc.
      when 0.

      when others.
        raise error_passed_to_mess_handler.
    endcase.

    " Free the thread for the next thread to run
    me->handler->clear_thread( conv char8( p_task ) ).
    log_write_table( lt_mess ).
    append lines of lt_pernr to lt_results.
  endmethod.

=================

  method log_write_table.
    loop at lt_mess assigning field-symbol(<fs_mess>).
      log_write(
        exporting
          i_msgty =   'I'               " Тип сообщения, делаем информационным чтобы не останавливать общую загрузку
          i_msgid =   <fs_mess>-msgid   " Класс сообщений
          i_msgno =   <fs_mess>-msgno   " Номер сообщения
          i_msgv1 =   <fs_mess>-msgv1   " Переменная сообщения
          i_msgv2 =   <fs_mess>-msgv2   " Переменная сообщения
      ).
    endloop.
  endmethod.
=============

  method LOG_WRITE.
  DATA: rsal_save_subrc TYPE sy-subrc,
        rsal_s_logparms TYPE rslogparms.

  rsal_save_subrc = sy-subrc.

  rsal_s_logparms-msgv1 = i_msgv1.
  rsal_s_logparms-msgv2 = i_msgv2.

  CALL FUNCTION 'RSAL_LOG_WRITE'
  EXPORTING
    i_msgty = i_msgty
    i_msgid = i_msgid
    i_msgno = i_msgno
    i_msgv1 = rsal_s_logparms-msgv1
    i_msgv2 = rsal_s_logparms-msgv2.

  sy-subrc = rsal_save_subrc.
  endmethod.
================

FUNCTION ZBW_GET_EXPERIENCE.
*"----------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  IMPORTING
*"     VALUE(LT_PERNR) TYPE  ZBW_T_PERNR
*"     VALUE(LT_CALMON) TYPE  ZBW_T_CALMONTHS
*"  TABLES
*"      ET_PERNR STRUCTURE  ZBW_EMPLOYEE_EXPERIENCE
*"      ET_MESS STRUCTURE  RSPC_S_MSG
*"----------------------------------------------------------------------
  data:
    ls_duration     type psen_duration_dec,
    lt_source       type sorted table of zbw_employee_experience with non-unique key pernr calmon,
    ls_mess         like line of et_mess,
    control_date    type d.

  if lt_pernr[] is not initial.
    "данные о статусе занятости
    select pernr, endda, begda
    into table @data(lt_pa0000)
    from pa0000
    for all entries in @lt_pernr
    where pernr = @lt_pernr-pernr and
          stat2 = '0'. "уволен

    "данные об оргприсовении
    select pernr, endda, begda
    into table @data(lt_pa0001)
    from pa0001
    for all entries in @lt_pernr
    where pernr = @lt_pernr-pernr. "уволен

    loop at lt_pernr into data(ls_source).
      loop at lt_calmon assigning field-symbol(<ls_calmon>).
        call function 'RP_LAST_DAY_OF_MONTHS'
          exporting
            day_in            = conv syst_datum( <ls_calmon>-calmon && '01' )
          importing
            last_day_of_month = control_date
          exceptions
            day_in_no_date    = 1
            others            = 2.
        if sy-subrc <> 0.
*          message id sy-msgid type sy-msgty number sy-msgno
*            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        endif.
=============