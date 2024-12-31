    types: begin of ty_orgunit,
             orgunit      type /bi0/oiorgunit,
             /bic/zzdeprt type /bic/oizzdeprt,
           end of ty_orgunit.

    data: lt_orgunit  type table of ty_orgunit,
          begda       type d,
          endda       type d.

    begda = '20210101'.
    endda = '99991231'.

    select employee, dateto, datefrom, orgunit
      from /bi0/qemployee
      into table @data(lt_employee)
      for all entries in @result_package
     where employee = @result_package-employee
       and not ( ( dateto < @begda ) or ( datefrom > @endda ) )
       and objvers  = 'A'.

    select * from /bi0/porgunit
      into corresponding fields of table lt_orgunit.
    sort lt_orgunit by orgunit ascending.

    loop at result_package assigning field-symbol(<fs_rp>).
      loop at lt_employee assigning field-symbol(<fs_emp>)
       where employee   = <fs_rp>-employee
         and dateto    ge <fs_rp>-calday
         and datefrom  le <fs_rp>-calday.
        <fs_rp>-orgunit = <fs_emp>-orgunit.
        exit.
      endloop.
      read table lt_orgunit assigning field-symbol(<fs_ou>)
        with key orgunit = <fs_rp>-orgunit
         binary search.
      if sy-subrc = 0.
        <fs_rp>-/bic/zzdeprt   = <fs_ou>-/bic/zzdeprt.
        <fs_rp>-/bic/zzdeprt_a = <fs_ou>-/bic/zzdeprt.
      endif.
    endloop.
=========
" current quarter (in DTP)
  data: lv_d type d.
  data: lv_begda type d.
  data: lv_endda type d.
  data: lv_m(2) type n.
  lv_d = sy-datlo.
  lv_m = lv_d+4(2).
  lv_m = ( lv_m div 3 ) * 3 + 1.
  concatenate lv_d+0(4) lv_m '01' into lv_d.
  lv_begda = lv_d.
  lv_d = lv_d + 100.
  concatenate lv_d+0(6)'01' into lv_d.
  lv_endda = lv_d - 1.

  l_t_range-fieldname = 'KDAY'.
  l_t_range-iobjnm = 'CALDAY'.
  l_t_range-sign = 'I'.
  l_t_range-option = 'BT'.
  l_t_range-low = lv_begda.
  l_t_range-high = lv_endda.
  if l_idx <> 0.
    modify l_t_range index l_idx.
  else.
    append l_t_range.
  endif.
  p_subrc = 0.
=========
" previous quarter (in DTP)
  data: lv_d type d.
  data: lv_begda type d.
  data: lv_endda type d.
  data: lv_m(2) type n.
  lv_d = sy-datlo.
  lv_m = lv_d+4(2).
  lv_m = ( lv_m div 3 ) * 3 + 1.
  concatenate lv_d+0(4) lv_m '01' into lv_d.
  lv_endda = lv_d - 1.
  lv_d = lv_d - 70.
  concatenate lv_d+0(6)'01' into lv_begda.

  l_t_range-fieldname = 'KDAY'.
  l_t_range-iobjnm = 'CALDAY'.
  l_t_range-sign = 'I'.
  l_t_range-option = 'BT'.
  l_t_range-low = lv_begda.
  l_t_range-high = lv_endda.
  if l_idx <> 0.
    modify l_t_range index l_idx.
  else.
    append l_t_range.
  endif.
  p_subrc = 0.
==========
process chain check status

REPORT zztest.
DATA(lv_logid)  = VALUE rspc_logid( ).
DATA(lv_status) = VALUE rspc_state( ).
DATA(lv_chain) = VALUE rspc_chain( ).
DATA(lv_count) = VALUE int2( ).

*  data: lv_d type d.
*  data: lv_begda type d.
*  data: lv_endda type d.
*  data: lv_m(2) type n.
*  lv_d = sy-datlo.
*  lv_m = lv_d+4(2).
*  lv_m = ( lv_m div 3 ) * 3 + 1.
*  CONCATENATE lv_d+0(4) lv_m '01' INTO lv_d.
*  lv_endda = lv_d - 1.
*  lv_d = lv_d - 70.
*  CONCATENATE lv_d+0(6)'01' INTO lv_begda.

CALL FUNCTION 'RSPC_API_CHAIN_START'
  EXPORTING
    i_chain       = 'ZCEODBBPC_VB_001'
    i_synchronous = 'X'
    i_noplan      = 'X'
    i_dont_wait   = 'X'
importing
  e_logid       = lv_logid
exceptions
  failed        = 1
  others        = 2.
BREAK-POINT.

IF sy-subrc = 0.
  CLEAR:lv_count.

  DO.
    IF lv_count > 3.
      EXIT.
    ENDIF.

    CALL FUNCTION 'RSPC_API_CHAIN_GET_STATUS'
      EXPORTING
        i_chain  = lv_chain
        i_logid  = lv_logid
*       I_DONT_UPDATE        =
*       I_DONT_POLL          =
      IMPORTING
        e_status = lv_status
*       E_MANUAL_ABORT       =
*       E_MESSAGE            =
*       E_S_MESSAGE          =
      .
    IF lv_status = 'A'.
      WAIT UP TO 10 SECONDS.

    ELSE.
      EXIT.
    ENDIF.
    lv_count = lv_count + 1.
  ENDDO.
  IF lv_status <> 'G'.

    MESSAGE e022(rspc) WITH '-1'.
  ELSE.
    MESSAGE i022(rspc) WITH '0'.
  ENDIF.
ENDIF.