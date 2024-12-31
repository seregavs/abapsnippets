class ZCL_STAGE definition
  public
  final
  create public .

public section.

  class-methods GET_REV_CALC_DATE
    returning
      value(OUT_DAT) type DATS .
  class-methods GET_PCHAIN01_READY_STATUS
    importing
      !IN_DATE type DATS
      !IN_PPROC type BYTE
    returning
      value(OUT_STATUS) type BYTE .
  class-methods GET_PCHAIN02_READY_STATUS
    importing
      !IN_DATE type DATS
      !IN_PPROC type BYTE
    returning
      value(OUT_STATUS) type BYTE .
  class-methods SET_PCHAIN_LOG
    importing
      !IN_PPROC type BYTE
      !IN_DATE type DATS
      !IN_START type CHAR1
      !IN_FINISH type CHAR1 .
  class-methods GET_PCHAIN03_READY_STATUS
    importing
      !IN_DATE type DATS
      !IN_PPROC type BYTE
    returning
      value(OUT_STATUS) type BYTE .
  class-methods GET_PCHAIN04_READY_STATUS
    importing
      !IN_DATE type DATS
      !IN_PPROC type BYTE
    returning
      value(OUT_STATUS) type BYTE .
  class-methods GET_PCHAIN05_READY_STATUS
    importing
      !IN_DATE type DATS
      !IN_PPROC type BYTE
    returning
      value(OUT_STATUS) type BYTE .
protected section.
private section.

  class-methods GET_PCHAIN_EXEC_STATUS
    importing
      !IN_PPROC type BYTE
      !IN_DATE type DATS
    returning
      value(OUT_STATUS) type BYTE .
ENDCLASS.



CLASS ZCL_STAGE IMPLEMENTATION.


  METHOD get_pchain01_ready_status.
    DATA: lv_i1     TYPE i.

    SELECT SINGLE * INTO  @DATA(ls_ra_pchain)
      FROM zra_pchain
     WHERE pprocid = @in_pproc.

    DATA(lv_t2stageset) =  ls_ra_pchain-t2stageset.

    DATA(lv_i3) = zcl_stage=>get_pchain_exec_status( in_date = in_date in_pproc = in_pproc ).
    IF lv_i3 = 0.
      EXEC SQL.
        select
        (
        select count(*) from  "SAPABAP1".ZRA_STAGE WHERE processcode = :lv_t2stageset
        )
        -
        (
        select count(*) from bdma."T2_STAGE"."S_HT_30_AUDIT_LOG" as a,
                      "SAPABAP1".TVARVC as b,
                      "SAPABAP1".ZRA_STAGE as c
         where a.report_date = to_date(b.LOW||'01')
           and a.WORKFLOW_NAME = c.WORKFLOW_NAME
           and NAME = 'REVENUE_CALC_DATE'
           and c.processcode = :lv_t2stageset
           and load_date = (select max(LOAD_DATE) from bdma."T2_STAGE"."S_HT_30_AUDIT_LOG" as d
                            where a.WORKFLOW_NAME = d.WORKFLOW_NAME
                                and a.report_date = d.report_date
                             )
         )
         into :lv_i1
         from dummy
      endexec.

      select count(*)
        into @data(lv_i2)
        from ZRA_STAGE WHERE processcode = @lv_t2stageset.

      if lv_i1 = 0 and lv_i2 <> 0.
        WRITE:/ |Процесс { in_pproc } ({ ls_ra_pchain-pchainid }) : Есть все загрузки в отчетном периоде ({ in_date }).|.
        WRITE:/ |Процесс { in_pproc } ({ ls_ra_pchain-pchainid }) : запускается.|.
        out_status = '1'. " можно выполнять
      else.
        WRITE:/ |Процесс { in_pproc } ({ ls_ra_pchain-pchainid }) : Нет подтверждения по всем таблицам Stage в периоде ({ in_date }).|.
        out_status = '0'. " нельзя выполнять
      ENDIF.
    ELSE.
      WRITE:/ |Процесс { in_pproc } ({ ls_ra_pchain-pchainid }) : Этот период ({ in_date }) уже загружен.|.
      out_status = '0'. " нельзя выполнять
    ENDIF.
  ENDMETHOD.


  METHOD get_pchain02_ready_status.
    SELECT SINGLE * INTO  @DATA(ls_ra_pchain)
      FROM zra_pchain
     WHERE pprocid = @in_pproc.

    SELECT COUNT(*) INTO @DATA(lv_cnt)
      FROM zra_pchain_log
      WHERE report_date = @in_date
        AND ( ( has_start = 'X' ) OR ( has_finish_ok = 'X' ) )
        AND pprocid = @in_pproc. "
    IF lv_cnt = 0.
      WRITE:/ |Процесс { in_pproc } ({ ls_ra_pchain-pchainid }) : Ззапускается для периода ({ in_date }).|.
      out_status = '1'. " можно выполнять
    ELSE.
      WRITE:/ |Процесс { in_pproc } ({ ls_ra_pchain-pchainid }) : Этот период ({ in_date }) загружается или уже загружен.|.
      out_status = '0'. " нельзя выполнять
    ENDIF.
  ENDMETHOD.


  METHOD GET_PCHAIN03_READY_STATUS.

    SELECT SINGLE * INTO  @DATA(ls_ra_pchain)
      FROM zra_pchain
     WHERE pprocid = @in_pproc.

    SELECT COUNT(*) INTO @DATA(lv_cnt)
      FROM zra_pchain_log
      WHERE report_date = @in_date
        AND ( ( has_start = 'X' ) OR ( has_finish_ok = 'X' ) )
        AND pprocid = 6. " D_TD_RAR_CHARGES
    IF lv_cnt = 0.
      SELECT COUNT(*) INTO @lv_cnt
        FROM zra_pchain_log
        WHERE report_date = @in_date
          AND has_finish_ok = 'X' AND has_start = 'X'
          AND pprocid IN ( 1 , 5 ). " M_TD_BI_CALC_CH, D_TD_RAR_STAGE
      IF lv_cnt = 2.
        WRITE:/ |Процесс { in_pproc } ({ ls_ra_pchain-pchainid }) : Есть все загрузки в отчетном периоде ({ in_date }).|.
        WRITE:/ |Процесс { in_pproc } ({ ls_ra_pchain-pchainid }) : запускается.|.
        out_status = '1'. " можно выполнять
      ELSE.
        WRITE:/ |Процесс { in_pproc } ({ ls_ra_pchain-pchainid }) : Нет завершены предыдущие загрузки (M_TD_BI_CALC_CH, D_TD_RAR_STAGE) в периоде ({ in_date }).|.
        out_status = '0'. " нельзя выполнять
      ENDIF.
    ELSE.
      WRITE:/ |Процесс { in_pproc } ({ ls_ra_pchain-pchainid }) : Этот период ({ in_date }) загружается или уже загружен.|.
      out_status = '0'. " нельзя выполнять
    ENDIF.
  ENDMETHOD.


  METHOD GET_PCHAIN04_READY_STATUS.

    SELECT SINGLE * INTO  @DATA(ls_ra_pchain)
      FROM zra_pchain
     WHERE pprocid = @in_pproc.

    SELECT COUNT(*) INTO @DATA(lv_cnt)
      FROM zra_pchain_log
      WHERE report_date = @in_date
        AND ( ( has_start = 'X' ) OR ( has_finish_ok = 'X' ) )
        AND pprocid = 7. " D_TD_RAR_CHARGES
    IF lv_cnt = 0.
      SELECT COUNT(*) INTO @lv_cnt
        FROM zra_pchain_log
        WHERE report_date = @in_date
          AND has_finish_ok = 'X' AND has_start = 'X'
          AND pprocid IN ( 2 , 5 ). " M_TD_BI_CALC_DC_RAR, D_TD_RAR_STAGE
      IF lv_cnt = 2.
        WRITE:/ |Процесс { in_pproc } ({ ls_ra_pchain-pchainid }) : Есть все загрузки в отчетном периоде ({ in_date }).|.
        WRITE:/ |Процесс { in_pproc } ({ ls_ra_pchain-pchainid }) : запускается.|.
        out_status = '1'. " можно выполнять
      ELSE.
        WRITE:/ |Процесс { in_pproc } ({ ls_ra_pchain-pchainid }) : Нет завершены предыдущие загрузки (M_TD_BI_CALC_DC_RAR, D_TD_RAR_STAGE) в периоде ({ in_date }).|.
        out_status = '0'. " нельзя выполнять
      ENDIF.
    ELSE.
      WRITE:/ |Процесс { in_pproc } ({ ls_ra_pchain-pchainid }) : Этот период ({ in_date }) загружается или уже загружен.|.
      out_status = '0'. " нельзя выполнять
    ENDIF.
  ENDMETHOD.


  METHOD GET_PCHAIN05_READY_STATUS.
    SELECT SINGLE * INTO  @DATA(ls_ra_pchain)
      FROM zra_pchain
     WHERE pprocid = @in_pproc.

    SELECT COUNT(*) INTO @DATA(lv_cnt)
      FROM zra_pchain_log
      WHERE report_date = @in_date
        AND ( ( has_start = 'X' ) OR ( has_finish_ok = 'X' ) )
        AND pprocid = 8. " D_TD_BDMA_PROCESS
    IF lv_cnt = 0.
      SELECT COUNT(*) INTO @lv_cnt
        FROM zra_pchain_log
        WHERE report_date = @in_date
          AND has_finish_ok = 'X' AND has_start = 'X'
          AND pprocid IN ( 3 , 4, 9 ). " M_TD_BI_CALC_CALLS, M_TD_BI_CALC_DC_BDMA, D_TD_BDMA_STAGE
      IF lv_cnt = 3.
        WRITE:/ |Процесс { in_pproc } ({ ls_ra_pchain-pchainid }) : Есть все загрузки в отчетном периоде ({ in_date }).|.
        WRITE:/ |Процесс { in_pproc } ({ ls_ra_pchain-pchainid }) : запускается.|.
        out_status = '1'. " можно выполнять
      ELSE.
        WRITE:/ |Процесс { in_pproc } ({ ls_ra_pchain-pchainid }) : Нет завершены предыдущие загрузки (M_TD_BI_CALC_CALLS, M_TD_BI_CALC_DC_BDMA, D_TD_BDMA_STAGE) в периоде ({ in_date }).|.
        out_status = '0'. " нельзя выполнять
      ENDIF.
    ELSE.
      WRITE:/ |Процесс { in_pproc } ({ ls_ra_pchain-pchainid }) : Этот период ({ in_date }) загружается или уже загружен.|.
      out_status = '0'. " нельзя выполнять
    ENDIF.
  ENDMETHOD.


  METHOD get_pchain_exec_status.
    SELECT COUNT( * ) FROM zra_pchain_log
      INTO out_status
     WHERE report_date = in_date
       AND pprocid = in_pproc
       AND has_start = 'X'.
  ENDMETHOD.


  METHOD get_rev_calc_date.
    SELECT SINGLE * FROM tvarvc
      INTO @DATA(ls_tvarvc)
     WHERE name = 'REVENUE_CALC_DATE'.
    IF sy-subrc = 0.
       CONCATENATE ls_tvarvc-low '01' INTO out_dat.
    ELSE.
      out_dat = '99991231'.
    ENDIF.
  ENDMETHOD.


  METHOD set_pchain_log.
    SELECT SINGLE * INTO @DATA(ls_pchain_log) FROM zra_pchain_log.
    ls_pchain_log-pprocid = in_pproc.
    ls_pchain_log-report_date = in_date.
    ls_pchain_log-has_start = in_start.
    ls_pchain_log-has_finish_ok = in_finish.
    UPDATE zra_pchain_log FROM @ls_pchain_log.
    IF sy-dbcnt = 0.
      INSERT INTO zra_pchain_log VALUES ls_pchain_log.
      IF sy-dbcnt NE 1.
        WRITE: / |Ошибка добавления записей в zra_pchain_log: { in_pproc }:{ in_date }|.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.