  DATA: lv_mandt(3) TYPE n.
  SELECT SINGLE low INTO @DATA(lv_s)
    FROM tvarvc
   WHERE name = 'DTP_MANDT_ZLOHU_O'.
  IF sy-subrc NE 0.
    lv_mandt = '200'.
  ELSE.
    CONDENSE lv_s NO-GAPS.
    lv_mandt = lv_s+0(3).
  ENDIF.

  READ TABLE l_t_range WITH KEY
       fieldname = 'MANDT'.
  DATA(l_idx) = sy-tabix.
  l_t_range-fieldname = 'MANDT'.
  l_t_range-option = 'EQ'.
  l_t_range-sign = 'I'.
  l_t_range-low = lv_mandt.
  IF l_idx <> 0.
    MODIFY l_t_range INDEX l_idx.
  ELSE.
    APPEND l_t_range.
  ENDIF.
  p_subrc = 0.
=============================
  SELECT SINGLE * INTO @DATA(lv_tvarvc)
    FROM tvarvc
   WHERE name = 'CYM11_INCIDENT_DATE'.
  IF sy-subrc NE 0.
    lv_tvarvc-low = |{ sy-datlo+0(6) }'01'|.
    lv_tvarvc-high  = sy-datlo.
  ENDIF.

  READ TABLE l_t_range WITH KEY
       fieldname = 'CALDAY'.
  DATA(l_idx) = sy-tabix.
  l_t_range-fieldname = 'CALDAY'.
  l_t_range-option = 'BT'.
  l_t_range-sign = 'I'.
  l_t_range-low  = lv_tvarvc-low.
  l_t_range-high = lv_tvarvc-high.
  IF l_idx <> 0.
    MODIFY l_t_range INDEX l_idx.
  ELSE.
    APPEND l_t_range.
  ENDIF.
  p_subrc = 0.