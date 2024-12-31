CLASS zcl_ca_uparams DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.

    INTERFACES zif_ca_uparams .

    DATA mv_packnm TYPE zca_packnm .

    CLASS-METHODS reload_class
      IMPORTING
        !iv_packnm TYPE zca_uparams-packnm .
  PROTECTED SECTION.

    METHODS constructor
      IMPORTING
        !iv_packnm TYPE zca_packnm .
  PRIVATE SECTION.

    METHODS gen_soval
      IMPORTING
        !it_parval   TYPE zca_t_uparval
        !iv_pardtype TYPE domname
      EXPORTING
        !et_soval    TYPE table .
    METHODS get_pack_by_date
      IMPORTING
        !iv_date TYPE begda OPTIONAL
      EXPORTING
        !es_pack TYPE zca_uparpack .
ENDCLASS.



CLASS ZCL_CA_UPARAMS IMPLEMENTATION.


  METHOD constructor.

    mv_packnm = iv_packnm.

  ENDMETHOD.


  METHOD gen_soval.

    "ВНИМАНИЕ!!! Автор знает об излишней трудоемкости метода решения:)
    DATA: ld_struct_type TYPE REF TO cl_abap_structdescr,
          lt_comp_tab    TYPE cl_abap_structdescr=>component_table,
          ls_comp        LIKE LINE OF lt_comp_tab,
          ld_tab         TYPE REF TO cl_abap_tabledescr.
    DATA ls_val LIKE LINE OF it_parval.
    DATA ld_out TYPE REF TO data."Pointers to new data objects
    DATA ld_so_out TYPE REF TO data.

    FIELD-SYMBOLS: <fs_wa>    TYPE any, "FS for WA
                   <fs_comp>  TYPE any, "FS for component of WA
                   <fs_it>    TYPE ANY TABLE, "FS for Range-table
                   <fs_soval> TYPE any.

    IF et_soval IS NOT SUPPLIED.
      RETURN.
    ENDIF.

    CLEAR et_soval.

    "Creating line type
    "SIGN OPTION LOW HIGH
    "C(1) C(2)   DT  DT
    ls_comp-name = 'SIGN'.
    ls_comp-type = cl_abap_elemdescr=>get_c( 1 ).
    APPEND ls_comp TO lt_comp_tab.
    ls_comp-name = 'OPTION'.
    ls_comp-type = cl_abap_elemdescr=>get_c( 2 ).
    APPEND ls_comp TO lt_comp_tab.
    ls_comp-name = 'LOW'.
    ls_comp-type ?= cl_abap_elemdescr=>describe_by_name( iv_pardtype ).
    APPEND ls_comp TO lt_comp_tab.
    ls_comp-name = 'HIGH'.
    ls_comp-type ?= cl_abap_elemdescr=>describe_by_name( iv_pardtype ).
    APPEND ls_comp TO lt_comp_tab.
    ld_struct_type = cl_abap_structdescr=>create( lt_comp_tab ).

    "Creating working area
    CREATE DATA ld_out TYPE HANDLE ld_struct_type.
    ASSIGN ld_out->* TO <fs_wa>.

    "Creating internal table
    ld_tab = cl_abap_tabledescr=>create( ld_struct_type ).
    CREATE DATA ld_so_out TYPE HANDLE ld_tab.
    ASSIGN ld_so_out->* TO <fs_it>.

    LOOP AT it_parval INTO ls_val.
      ASSIGN COMPONENT 'SIGN' OF STRUCTURE <fs_wa> TO <fs_comp>.
      <fs_comp> = ls_val-valsign.
      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <fs_wa> TO <fs_comp>.
      <fs_comp> = ls_val-valopt..
      ASSIGN COMPONENT 'LOW' OF STRUCTURE <fs_wa> TO <fs_comp>.
      <fs_comp> = ls_val-vallow.
      ASSIGN COMPONENT 'HIGH' OF STRUCTURE <fs_wa> TO <fs_comp>.
      <fs_comp> = ls_val-valhigh.

      INSERT <fs_wa> INTO TABLE <fs_it>.
    ENDLOOP.

    TRY.
        ld_tab = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( et_soval ) ).

        " Типы данных полностью совпадают
        IF ld_tab->applies_to_data( <fs_it> ).
          et_soval = <fs_it>.

        ELSE.
          " Типы данных не совпадают, но ex_soval - таблица со строкой вида struct, и хочется преобразовать
          IF ld_tab->get_table_line_type( )->kind = cl_abap_typedescr=>kind_struct.
            LOOP AT <fs_it> ASSIGNING <fs_wa>.
              APPEND INITIAL LINE TO et_soval ASSIGNING <fs_soval>.
              MOVE-CORRESPONDING <fs_wa> TO <fs_soval>.
            ENDLOOP.
          ENDIF.
        ENDIF.
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD get_pack_by_date.
*{   INSERT         URRK900031                                        1
    DATA: lt_uparpack_t TYPE SORTED TABLE OF zca_uparpack
                              WITH NON-UNIQUE KEY begda.
    DATA: ls_uparpack1 LIKE LINE OF lt_uparpack_t,
          lv_maxbegda  LIKE sy-datum.

    SELECT *
      FROM zca_uparpack
      INTO TABLE lt_uparpack_t
      WHERE packnm = mv_packnm.

    LOOP AT lt_uparpack_t  INTO ls_uparpack1
      WHERE endda >= iv_date
        AND begda <= iv_date.

    ENDLOOP.

    IF sy-subrc EQ 0.
*   Вернуть значение
      es_pack   = ls_uparpack1.
    ELSE.
*   Видимо пакета нет или задан неверный срок
    ENDIF.

    RETURN.   " ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ (!) return

    SELECT MAX( begda ) FROM zca_uparpack
      INTO lv_maxbegda
      WHERE
        packnm = mv_packnm AND
        endda >= iv_date AND
        begda <= iv_date.
    IF sy-subrc <> 0.
      "Видимо пакета нет или задан неверный срок
      RETURN.
    ENDIF.

    "Select package
    SELECT SINGLE * FROM zca_uparpack
      INTO es_pack
      WHERE packnm = mv_packnm
        AND begda = lv_maxbegda.
  ENDMETHOD.


  METHOD zif_ca_uparams~check_interval.
    "Вернуть список дат начала периодов
    SELECT begda FROM zca_uparpack
      INTO TABLE et_dats
      WHERE packnm <= mv_packnm AND
            begda <= iv_dat2 AND
            endda >= iv_dat1.

  ENDMETHOD.


  METHOD zif_ca_uparams~get_param_val.

    DATA: lt_val    TYPE zca_t_uparval,
          ls_val    TYPE zca_uparval,
          ls_pack   TYPE zca_uparpack,
          ls_par    TYPE zca_uparams,
          lv_date   LIKE iv_begda,
          lv_packnm TYPE zca_uparpack-packnm,
          lv_parnm  TYPE zca_uparams-parnm.

    "Если дата не задана явно использовать действительную
    lv_date = iv_begda.
    IF iv_begda IS INITIAL.
      lv_date = sy-datum.
    ENDIF.

    lv_packnm = mv_packnm.
    lv_parnm = iv_parnm.

    TRANSLATE lv_packnm TO UPPER CASE.
    TRANSLATE lv_parnm TO UPPER CASE.

    get_pack_by_date(
      EXPORTING
        iv_date = lv_date
      IMPORTING
        es_pack = ls_pack ).

    IF ls_pack IS INITIAL.
      "Пакет отсутствует
      RAISE no_package.
    ENDIF.

    "Select parameter properties
    SELECT SINGLE * FROM zca_uparams
      INTO ls_par
      WHERE packnm = lv_packnm AND
            begda = ls_pack-begda AND
            parnm = lv_parnm.
    IF sy-subrc <> 0.
      "Нет такого параметра
      RAISE no_param.
    ENDIF.

    "Select vals
    SELECT * FROM zca_uparval
      INTO TABLE lt_val
      WHERE packnm = lv_packnm AND
            begda = ls_pack-begda AND
            parnm = lv_parnm ORDER BY PRIMARY KEY.
    IF sy-subrc <> 0..
      "Значения не заданы
*    raise no_value.
    ENDIF.

    IF ls_par-partype = 'PA'."Одно значение
      READ TABLE lt_val INDEX 1 INTO ls_val.
      TRY .
          ev_paval = ls_val-vallow.
        CATCH cx_root.
          RAISE err_convert.
      ENDTRY.


    ELSE."SO (Range-table)
      "Сгенерируем таблицу нужного типа и заполним ее полученными значениями.
      gen_soval(
        EXPORTING
           it_parval = lt_val
           iv_pardtype = ls_par-pardtype
        IMPORTING
            et_soval = et_soval
      ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_ca_uparams~get_table.

    FIELD-SYMBOLS: <fs_tab>      TYPE ANY TABLE,
                   <fs_tab_line> TYPE any.
    FIELD-SYMBOLS: <fs_val> TYPE any.

    DATA: lv_packnm  TYPE zca_uparpack-packnm,
          lv_tblname TYPE zca_upartbl-tblname.
    DATA  lv_date TYPE zca_uparpack-begda.
    DATA: ls_params TYPE zca_uparams.
    DATA: ls_package TYPE zca_uparpack.
    DATA: lt_tblval TYPE TABLE OF zca_upartbl,
          ls_tblval TYPE zca_upartbl.
    DATA: ld_tab      TYPE REF TO data,
          ld_tab_line TYPE REF TO data.
    DATA: lv_rownr TYPE i,
          lv_colnr TYPE i.
    DATA: lv_ix TYPE i.

    lv_packnm  = mv_packnm.
    lv_tblname = iv_tblname.

    TRANSLATE lv_packnm TO UPPER CASE.
    TRANSLATE lv_tblname  TO UPPER CASE.

    "Если дата не задана явно использовать действительную
    lv_date = iv_begda.
    IF iv_begda IS INITIAL.
      lv_date = sy-datum.
    ENDIF.

    "Находим пакет
    get_pack_by_date(
      EXPORTING
        iv_date   = lv_date
      IMPORTING
        es_pack = ls_package
      ).
    IF ls_package IS INITIAL.
      RAISE no_package.
    ENDIF.

    "Строку параметра
    SELECT SINGLE * FROM zca_uparams
      INTO ls_params
      WHERE
        packnm  = ls_package-packnm AND
        begda   = ls_package-begda AND
        parnm   = lv_tblname.
    IF sy-subrc <> 0.
      RAISE no_param.
    ENDIF.

    "Табличные значения
    SELECT * FROM zca_upartbl
      INTO TABLE lt_tblval
      WHERE
        packnm = ls_package-packnm AND
        begda = ls_package-begda AND
        tblname = ls_params-parnm.
    IF lt_tblval IS INITIAL.
      RETURN.
    ENDIF.

    CREATE DATA ld_tab TYPE TABLE OF (ls_params-pardtype).
    ASSIGN ld_tab->* TO <fs_tab>.

    CREATE DATA ld_tab_line TYPE (ls_params-pardtype).
    ASSIGN ld_tab_line->* TO <fs_tab_line>.

    SORT: lt_tblval BY rownr colname ASCENDING.

    LOOP AT lt_tblval INTO ls_tblval.
      AT NEW rownr.
        "<TODO>: Обнулить <fs_tab_line>
      ENDAT.

      lv_ix = ls_tblval-colname.
      ASSIGN COMPONENT lv_ix OF STRUCTURE <fs_tab_line> TO <fs_val>.
      <fs_val> = ls_tblval-value.

      AT END OF rownr.
        INSERT <fs_tab_line> INTO TABLE <fs_tab>.
      ENDAT.
    ENDLOOP.

    et_table = <fs_tab>.

  ENDMETHOD.


  METHOD zif_ca_uparams~open_parval_dialog.

    DATA ls_pack TYPE zca_uparpack.
    DATA lt_para TYPE TABLE OF zca_uparams.

    CALL METHOD get_pack_by_date
      EXPORTING
        iv_date = sy-datum
      IMPORTING
        es_pack = ls_pack.

    IF ls_pack IS INITIAL .
      "Нет пакета
      RAISE no_package.
    ENDIF.

    SELECT * FROM zca_uparams
      INTO TABLE lt_para
      WHERE packnm = mv_packnm AND
            begda = ls_pack-begda.
    IF lt_para IS INITIAL.
      "Нет параметров
      RAISE no_param.
    ENDIF.

*    CALL METHOD zca_uparams_dialog=>pack_parval_wizard
*      EXPORTING
*        iv_packnm = mv_packnm
*        iv_begda  = ls_pack-begda
*        it_params = lt_para.

  ENDMETHOD.

  METHOD reload_class.
    TYPES ltt_range_param TYPE RANGE OF zca_uparval-parnm.
    TYPES ltt_range_value TYPE RANGE OF zca_uparval-vallow.
    DATA lt_stack TYPE abap_callstack.
    DATA lv_class TYPE seoclsname.
    DATA lo_descr TYPE REF TO cl_abap_classdescr.
    FIELD-SYMBOLS <l_attr>   TYPE any.
    FIELD-SYMBOLS <lv_value> TYPE simple.
    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING callstack = lt_stack.
    LOOP AT lt_stack INTO DATA(ls_stack)
         WHERE     ( blockname = 'CLASS_CONSTRUCTOR' or blockname = 'FORCE_RELOAD' )
               AND blocktype = 'METHOD'.
      lv_class = replace( val  = ls_stack-mainprogram(30)
                          sub  = '='
                          with = ''
                          occ  = 0 ).
      lo_descr ?= cl_abap_typedescr=>describe_by_name( lv_class ).
      EXIT.
    ENDLOOP.
    ASSERT sy-subrc = 0.

    DATA(lr_params) = VALUE ltt_range_param( FOR attr IN lo_descr->attributes
                                             WHERE ( is_class = abap_true AND is_constant = space )
                                             ( sign   = rs_c_range_sign-including
                                               option = rs_c_range_opt-equal
                                               low    = attr-name ) ).

    " Выбор значений
    SELECT par~parnm   AS name,
           par~partype AS type,
           val~num,
           val~valsign AS sign,
           val~valopt  AS option,
           val~vallow  AS low,
           val~valhigh AS high,
           tab~rownr   AS row,
           tab~colname AS col,
           tab~value
      INTO TABLE @DATA(lt_vals)
      FROM zca_uparams AS par
             LEFT JOIN
               zca_uparval AS val ON  val~packnm = par~packnm
                                  AND val~begda  = par~begda
                                  AND val~parnm  = par~parnm
                 LEFT JOIN
                   zca_upartbl AS tab ON  tab~packnm  = par~packnm
                                      AND tab~begda   = par~begda
                                      AND tab~tblname = par~parnm
      WHERE par~packnm  = @iv_packnm
        AND par~parnm  IN @lr_params
        AND par~begda   = ( SELECT MAX( begda ) FROM zca_uparpack AS pack
                              WHERE pack~packnm  = @iv_packnm
                                AND pack~begda  <= @sy-datum
                                AND pack~endda  >= @sy-datum )
      ORDER BY par~parnm,
               par~partype,
               val~num,
               tab~rownr.

    " Присвоение значений аттрибутам
    LOOP AT lt_vals ASSIGNING FIELD-SYMBOL(<ls_param>)
         GROUP BY <ls_param>-name.

      ASSIGN (lv_class)=>(<ls_param>-name) TO <l_attr>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CASE <ls_param>-type.
        WHEN 'PA'.
          ASSIGN <l_attr> TO <lv_value>.
          ASSERT sy-subrc = 0.
          <lv_value> = <ls_param>-low.

        WHEN 'SO'.
          ASSIGN <l_attr> TO <lt_table>.
          ASSERT sy-subrc = 0.
          <lt_table> = CORRESPONDING #( VALUE ltt_range_value( FOR wa IN GROUP <ls_param>  WHERE ( sign <> space OR option <> space )
                                                               ( CORRESPONDING #( wa ) ) ) ).

        WHEN 'TB'.
          ASSIGN <l_attr> TO <lt_table>.
          ASSERT sy-subrc = 0.

          LOOP AT GROUP <ls_param> ASSIGNING FIELD-SYMBOL(<ls_rows>)
               GROUP BY <ls_rows>-row.
            IF <ls_rows>-row IS NOT INITIAL.
              APPEND INITIAL LINE TO <lt_table> ASSIGNING FIELD-SYMBOL(<ls_line>).
              LOOP AT GROUP <ls_rows> ASSIGNING FIELD-SYMBOL(<ls_row>).
                ASSIGN COMPONENT CONV i( <ls_row>-col ) OF STRUCTURE <ls_line> TO <lv_value>.
                ASSERT sy-subrc = 0.
                <lv_value> = <ls_row>-value.
              ENDLOOP.
            ENDIF.
          ENDLOOP.

        WHEN OTHERS.
          MESSAGE |Unknown type of param { <ls_param>-name }| TYPE rs_c_exit.

      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.