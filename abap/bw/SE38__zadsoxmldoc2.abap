*&---------------------------------------------------------------------*
*& Report zadsoxmldoc2
*&---------------------------------------------------------------------*
*& Extract BW ADSO metadata and display it in ALV grid
*&---------------------------------------------------------------------*
REPORT zadsoxmldoc2.
PARAMETERS: p_adso TYPE rsoadsonm DEFAULT 'ZMMIM_D01' OBLIGATORY.

CLASS lcl_adsoxmldoc DEFINITION.

  PUBLIC SECTION.
    DATA:
      lv_adso   TYPE rsoadsonm.

    METHODS:
      constructor IMPORTING in_adso TYPE rsoadsonm,
      display_grid.

  PRIVATE SECTION.
    CONSTANTS:
       lc_field_delimiter(4) TYPE c VALUE  '!2F'.

    TYPES:
      BEGIN OF tt_adso_data,
        adsonm           TYPE rsoadsonm,
        adsodescr(60)    TYPE c,
        el_name(40)      TYPE c,
        field(30)        TYPE c,
        iobj(30)         TYPE c,
        descr(60)        TYPE c,
        is_key(1)        TYPE c,
        key_order        TYPE i,
        el_dimension(20) TYPE c,
      END OF tt_adso_data,

      BEGIN OF tt_colname,
        adsonm      TYPE rsoadsonm,
        ttyp        TYPE rsoadsottyp,
        colname(40) TYPE c,
        descr       TYPE rsoadsodescr,
      END OF tt_colname,

      BEGIN OF tt_rsdiobj,
        iobjnm TYPE rsiobjnm,
        txtlg  TYPE rstxtlg,
      END OF tt_rsdiobj.

    DATA:
      lt_rsdiobj   TYPE STANDARD TABLE OF tt_rsdiobj,
      lt_adso_data TYPE STANDARD TABLE OF tt_adso_data,
      lt_colname   TYPE STANDARD TABLE OF tt_colname.

    METHODS:
      make_data.

ENDCLASS.

CLASS lcl_adsoxmldoc IMPLEMENTATION.

  METHOD constructor.
    DATA:       ls_colname   TYPE tt_colname.
    lv_adso = in_adso.
    SELECT iobjnm, txtlg INTO TABLE @lt_rsdiobj
      FROM rsdiobjt WHERE objvers = 'A' AND langu = @sy-langu.

    SELECT * FROM rsoadsot INTO @DATA(ls_rsoadsot)
      WHERE objvers = 'A' AND adsonm = @lv_adso AND ttyp = 'ELEM' AND langu = @sy-langu.
      CLEAR: ls_colname.
      ls_colname-adsonm = ls_rsoadsot-adsonm.
      ls_colname-ttyp = ls_rsoadsot-ttyp.
      ls_colname-colname = substring_after(  val = ls_rsoadsot-colname sub = lc_field_delimiter occ = -1 ).
      ls_colname-descr = ls_rsoadsot-description.
      APPEND ls_colname TO lt_colname.
    ENDSELECT.

  ENDMETHOD.

  METHOD display_grid.
    DATA:
      lr_header   TYPE REF TO cl_salv_form_layout_grid,
      lr_grid     TYPE REF TO cl_salv_form_layout_grid,
      lr_salv_alv TYPE REF TO cl_salv_table.
    TRY.
        me->make_data( ).
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lr_salv_alv
          CHANGING
            t_table = lt_adso_data ).

        CREATE OBJECT lr_grid.
        CREATE OBJECT lr_header.
        lr_grid = lr_header->create_grid( row = 2  column = 1 colspan = 6 ).
        lr_grid->create_label( row = 1  column = 65 text = 'ADSO field mapping' ).
        lr_salv_alv->set_top_of_list( lr_grid ).
        lr_salv_alv->display( ).
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.

  METHOD make_data.
    DATA:
      ls_adso_data TYPE tt_adso_data,
      lo_xml_doc   TYPE REF TO cl_xml_document,
      lv_keycnt    TYPE    i.
    TRY.
        SELECT SINGLE * INTO @DATA(ls_adso)
          FROM rsoadso WHERE objvers = 'A' AND adsonm =  @lv_adso.
        IF sy-subrc = 0.
          SELECT SINGLE description
            FROM rsoadsot INTO @DATA(lv_adso_desc)
            WHERE objvers = 'A' AND adsonm = @lv_adso AND ttyp = 'EUSR' AND langu = @sy-langu.
          CREATE OBJECT lo_xml_doc.
          CHECK lo_xml_doc->parse_xstring( stream = ls_adso-xml_ui ) = 0.
          DATA(lo_iter_c) = lo_xml_doc->get_first_node( )->get_children( )->create_iterator(  ).
          DATA(lo_node) = lo_iter_c->get_next( ).
          WHILE lo_node IS NOT INITIAL.
            DATA(lo_eltype) = lo_node->get_attributes(  )->get_named_item_ns( EXPORTING name = 'type' uri = 'http://www.w3.org/2001/XMLSchema-instance' ).
            IF lo_eltype IS NOT INITIAL.
              CLEAR: ls_adso_data.
              IF lo_eltype->get_value( ) = 'adso:AdsoElement'.
                ls_adso_data-adsonm = lv_adso.
                ls_adso_data-adsodescr = lv_adso_desc.
                ls_adso_data-el_name = lo_node->get_attributes(  )->get_named_item_ns( EXPORTING name = 'name' )->get_value( ).
                DATA(lo_field) = lo_node->get_attributes(  )->get_named_item_ns( EXPORTING name = 'dimension' ).
                IF lo_field IS NOT INITIAL.
                  ls_adso_data-el_dimension = shift_left( val = lo_field->get_value( ) sub = '#///' ).
                  ls_adso_data-el_dimension = shift_right( val = ls_adso_data-el_dimension  sub = '§' ).
                ENDIF.
                lo_field = lo_node->get_attributes(  )->get_named_item_ns( EXPORTING name = 'infoObjectName' ).
                IF lo_field IS NOT INITIAL.
                  ls_adso_data-iobj = lo_field->get_value( ).
                  TRY.
                      ls_adso_data-descr = lt_rsdiobj[ iobjnm = ls_adso_data-iobj ]-txtlg.
                    CATCH cx_root.
                  ENDTRY.
                ENDIF.
                TRY.
                    ls_adso_data-descr = lt_colname[ adsonm = ls_adso_data-adsonm colname = ls_adso_data-el_name ttyp = 'ELEM' ]-descr.
                  CATCH cx_root.
                ENDTRY.
                IF ls_adso_data-descr IS INITIAL.
                  ls_adso_data-descr = ls_adso_data-el_name.
                ENDIF.
                IF ls_adso_data-iobj IS INITIAL.
                  ls_adso_data-field = ls_adso_data-el_name.
                ENDIF.
                APPEND ls_adso_data TO lt_adso_data.
              ENDIF.
            ENDIF.
            IF lo_node->get_name( ) = 'keyElement'.
              lv_keycnt = lv_keycnt + 1.
              TRY.
                  DATA(lv_elname) = substring_after(  val = lo_node->get_value( ) sub = '#///' occ = 1 ).
                  lt_adso_data[ adsonm = lv_adso el_name = lv_elname ]-is_key = 'X'.
                  lt_adso_data[ adsonm = lv_adso el_name = lv_elname ]-key_order = lv_keycnt.
                CATCH cx_root.
              ENDTRY.
            ENDIF.
            lo_node = lo_iter_c->get_next( ).
          ENDWHILE.
        ENDIF.
      CATCH cx_root INTO DATA(oref).
        DATA(lv_etext) = oref->get_text( ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  NEW lcl_adsoxmldoc( in_adso = p_adso )->display_grid( ) .

END-OF-SELECTION.