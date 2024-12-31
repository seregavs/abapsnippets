*&---------------------------------------------------------------------*
*& Report ZSAC_FBL3N
*&---------------------------------------------------------------------*
*&  Wrapper for SAC call of FBL3N
*&---------------------------------------------------------------------*
report zsac_fbl3n.

parameters:
  pcostel(15) type c default 'C001/253805',
  pbukrs      type bukrs default 'NL30',
  pfiscp(9)   type c.

start-of-selection.

  data: lv_saknr type saknr,
        lv_kokrs type kokrs,
        lv_begda type d,
        lv_endda type d.
  ranges: lr_budat for lv_begda.

  split pcostel at '/' into lv_kokrs lv_saknr.

  if pfiscp is initial.
    pfiscp = |K4{ sy-datum+0(4) }0{ sy-datum+4(2) }|.
  endif.
  " define 1st day of year from pfiscp[er]
  lv_begda = |{ pfiscp+2(4) }0101|.
  " define last date of month from pfiscp[er]
  lv_endda = |{ pfiscp+2(4) }{ pfiscp+7(2) }20|.
  lv_endda = lv_endda + 15.
  lv_endda = |{ lv_endda+0(6) }01|.
  lv_endda = lv_endda - 1.

  lr_budat-option = 'BT'.
  lr_budat-sign = 'I'.
  lr_budat-low = lv_begda.
  lr_budat-high = lv_endda.
  append lr_budat.

  submit rfitemgl
  with sd_saknr-low = lv_saknr
  with sd_bukrs-low = pbukrs
  with so_budat in lr_budat
  with x_opsel = '' " open items
  with x_aisel = 'X' " all items
  and return.