*&---------------------------------------------------------------------*
*& Report  ZBI_RFREISROOC
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zbi_rfreisrooc.

DATA: ztbi_rfreisrooc TYPE ztbi_rfreisrooc.
DATA:
  lt_data_ref   TYPE REF TO data.

FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

PARAMETERS: p_vari TYPE disvariant-variant,
            p_datu TYPE datum DEFAULT sy-datum.
PARAMETERS: p_create TYPE flag RADIOBUTTON GROUP rd1,
            p_view   TYPE flag RADIOBUTTON GROUP rd1,
            p_del    TYPE flag RADIOBUTTON GROUP rd1,
            p_free   TYPE flag RADIOBUTTON GROUP rd1.

SELECT-OPTIONS: s_dat FOR ztbi_rfreisrooc-zzdataroocbi,
                s_per FOR ztbi_rfreisrooc-zzpermroocbi.

START-OF-SELECTION.

  CASE abap_true.
    WHEN p_view.

      CALL FUNCTION 'SE16N_START'
        EXPORTING
          i_tab     = 'ZTBI_RFREISROOC'
          i_display = abap_true.

    WHEN p_del.
      DELETE FROM ztbi_rfreisrooc
      WHERE zzdataroocbi IN s_dat
        AND zzpermroocbi IN s_per.
      COMMIT WORK AND WAIT.

    WHEN p_free.
      EXEC SQL.
        TRUNCATE TABLE ztbi_rfreisrooc
      ENDEXEC.
      COMMIT WORK AND WAIT.

    WHEN p_create.
      cl_salv_bs_runtime_info=>set(
        display        = abap_false
        metadata       = abap_false
        data           = abap_true ).

      IF p_vari IS INITIAL.

        SUBMIT rfreisrooc VIA SELECTION-SCREEN
          WITH p_ctd = p_datu
          WITH p_dt_fr = p_datu
          AND RETURN.
      ELSE.

        SUBMIT rfreisrooc USING SELECTION-SET p_vari
          WITH p_ctd = p_datu
          WITH p_dt_fr = p_datu
          AND RETURN.
      ENDIF.

      TRY.
          cl_salv_bs_runtime_info=>get_data_ref(
          IMPORTING
            r_data            = lt_data_ref ).
        CATCH cx_salv_bs_sc_runtime_info.
          MESSAGE 'Ошибка при получении данных'(E01) TYPE 'E'.
      ENDTRY.

      cl_salv_bs_runtime_info=>clear_all( ).

      ASSIGN lt_data_ref->* TO <lt_table>.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_table>)  .
        DATA(ls_rfreisrooc) = VALUE ztbi_rfreisrooc( ).
        MOVE-CORRESPONDING <ls_table> TO ls_rfreisrooc.
        ls_rfreisrooc-mandt        = sy-mandt.
        ls_rfreisrooc-rowuid       = cl_system_uuid=>create_uuid_x16_static( ).
        ls_rfreisrooc-zzdataroocbi = p_datu.
        DATA(lv_prev) = p_datu.
        lv_prev = |{ lv_prev+0(6) }01|.
        lv_prev = lv_prev - 1.
        ls_rfreisrooc-zzpermroocbi = lv_prev+0(6).
        INSERT ztbi_rfreisrooc FROM ls_rfreisrooc.
      ENDLOOP.

      COMMIT WORK AND WAIT.

    WHEN OTHERS.
  ENDCASE.