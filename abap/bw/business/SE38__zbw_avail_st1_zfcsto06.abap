*&---------------------------------------------------------------------*
*& Report ZBW_AVAIL_ST1_FILL_PLANT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbw_avail_st1_zfcsto06.
PARAMETERS:
  "  pbegp(4) TYPE c OBLIGATORY,  " ТК из нижней границы диапазона. Если не указан, рассматриваются все действующие ТК с GLN <> '0'
  "  pendp(4) TYPE c OBLIGATORY, " ТК из верхней границы диапазона. Если не указан, а pbegp - указан, то рассматривается только ТК из pbegp.
  pdate   TYPE /bi0/oicalday OBLIGATORY, " Дата, на которую рассчитывается товарный запас (входной параметр I_DATE в HANA CV)
  pplist  TYPE char100, " список ТК
  pplist2 TYPE char100. " список ТК

*lv_mess = |date={ pdate }, plant=({ pbegp }-{ pendp })|.
*WRITE: / lv_mess.
*MESSAGE lv_mess TYPE 'S'.

CALL METHOD zcl_bw_avail_st1=>fill_plant
  EXPORTING
    in_date   = pdate
    in_plist  = pplist
    in_plist2 = pplist2.