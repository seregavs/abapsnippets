*&---------------------------------------------------------------------*
*& Report ZBW_AVAIL_ST1_FILL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbw_avail_st1_fill.

PARAMETERS:
  pjc       TYPE rsint1 DEFAULT 8 OBLIGATORY, " максимальное кол-во фоновых процессов материализации
  pbegp(4)  TYPE c, " ТК из нижней границы диапазона. Если не указан, рассматриваются все действующие ТК с GLN <> '0'
  pendp(4)  TYPE c, " ТК из верхней границы диапазона. Если не указан, а pbegp - указан, то рассматривается только ТК из pbegp.
  pclear(1) TYPE c DEFAULT 'X', " флаг предварительной очистки таблицы результатов материализации ZBW_AVAIL_ST1
  ppingrp   TYPE rsint1 DEFAULT 6,
  pvari(30) TYPE c DEFAULT 'PROD'. " Вариант

DATA: lv_d TYPE /bi0/oicalday.
lv_d = sy-datum - 1.

IF sy-batch EQ 'X'.
  NEW zcl_bw_avail_st1( i_date = lv_d
                        i_jobcnt = pjc
                        i_clear = pclear
                        i_pbegp = pbegp
                        i_pendp = pendp
                        i_pingrp = ppingrp
                        i_vari = pvari )->bkgr_proc_factory( ).
ELSE.
  zcl_bw_avail_st1=>put_message( in_mess = |Run ZBW_AVAIL_ST1_FILL in background mode only| ).
ENDIF.