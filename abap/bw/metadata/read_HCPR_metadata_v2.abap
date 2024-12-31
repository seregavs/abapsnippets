*&---------------------------------------------------------------------*
*& Report ZHCPRXMLDOC2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhcprxmldoc2.
PARAMETERS: p_hcpr TYPE rsohcprnm DEFAULT 'ZCF_CP12' OBLIGATORY.

CLASS lcl_hcprxmldoc DEFINITION.

  PUBLIC SECTION.
    DATA:
      lv_hcpr   TYPE rsohcprnm.

    METHODS:
      constructor IMPORTING in_hcpr TYPE rsohcprnm,
      display_grid.

  PRIVATE SECTION.
    CONSTANTS:
       lc_field_delimiter(4) TYPE c VALUE  '!2F'.

    TYPES:
      BEGIN OF tt_hcpr_fields,
        hcpr  TYPE rsinfocube,
        name  TYPE char40, " fieldname
        descr TYPE char40, " field description
        dim   TYPE char40,   " hcpr dimension
        lnkpp TYPE char100, " linked partproviders
        iobj  TYPE rsdiobjnm, " infoobject
      END OF   tt_hcpr_fields,

      BEGIN OF tt_pp_mapping,
        hcpr        TYPE rsinfocube,
        pprpv       TYPE char100,
        sourcefield TYPE char100,
        targetfield TYPE char100,
      END OF tt_pp_mapping,

      BEGIN OF tt_colname,
        hcprnm      TYPE rsohcprnm,
        ttyp        TYPE rsoadsottyp,
        colname(40) TYPE c,
        descr       TYPE rsoadsodescr,
      END OF tt_colname,

      BEGIN OF tt_rsdiobj,
        iobjnm TYPE rsiobjnm,
        txtlg  TYPE rstxtlg,
      END OF tt_rsdiobj,

      BEGIN OF tt_hcpr,
        pprpv TYPE char100,
      END OF tt_hcpr.

    DATA:
      lt_rsdiobj     TYPE STANDARD TABLE OF tt_rsdiobj,
      lt_hcpr_fields TYPE STANDARD TABLE OF tt_hcpr_fields,
      lt_colname     TYPE STANDARD TABLE OF tt_colname,
      lt_hcpr        TYPE STANDARD TABLE OF tt_hcpr.

    METHODS:
      make_data.

ENDCLASS.

CLASS lcl_hcprxmldoc IMPLEMENTATION.
  METHOD constructor.
    DATA: ls_colname   TYPE tt_colname.

    lv_hcpr = in_hcpr.

    SELECT iobjnm, txtlg INTO TABLE @lt_rsdiobj
      FROM rsdiobjt WHERE objvers = 'A'  AND langu = @sy-langu.

    SELECT * FROM rsohcprt INTO @DATA(ls_rshcprt)
      WHERE objvers = 'A' AND hcprnm = @lv_hcpr AND colname <> '' AND langu = @sy-langu.
      CLEAR: ls_colname.
      ls_colname-hcprnm = ls_rshcprt-hcprnm.
      ls_colname-ttyp = 'ELEM'. " !2F
      ls_colname-colname = substring_after(  val = ls_rshcprt-colname sub = lc_field_delimiter occ = -1 ).
      IF ls_colname-colname <> 'A7'.
        ls_colname-descr = ls_rshcprt-description.
        APPEND ls_colname TO lt_colname.
      ENDIF.
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
            "t_table = lt_hcpr_fields ).
        t_table = lt_hcpr ).

        CREATE OBJECT lr_grid.
        CREATE OBJECT lr_header.
        "lr_grid = lr_header->create_grid( row = 2  column = 1 colspan = 6 ).
        "lr_grid->create_label( row = 1  column = 65 text = 'HCPR field mapping' ).
        lr_salv_alv->set_top_of_list( lr_grid ).
        lr_salv_alv->display( ).
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.

  METHOD make_data.
    DATA:
      lo_xml_doc    TYPE REF TO cl_xml_document,
      element       TYPE REF TO if_ixml_element,
      lt_pp_mapping TYPE STANDARD TABLE OF tt_pp_mapping.

    DATA: nodes          TYPE REF TO if_ixml_node_list,
          child          TYPE REF TO if_ixml_node,
          index1         TYPE i,
          index3         TYPE i,
          index4         TYPE i,
          index5         TYPE i,
          ls_hcpr_fields TYPE   tt_hcpr_fields,
          ls_pp_mapping  TYPE   tt_pp_mapping.

    SELECT SINGLE * INTO @DATA(ls_hcpr)
      FROM rsohcpr WHERE objvers = 'A' AND hcprnm =  @lv_hcpr.

    IF sy-subrc = 0.
      SELECT SINGLE description
        FROM rsohcprt INTO @DATA(lv_hcpr_desc)
        WHERE objvers = 'A' AND hcprnm = @lv_hcpr AND colname = '' AND langu = @sy-langu.

      CREATE OBJECT lo_xml_doc.
      CHECK lo_xml_doc->parse_xstring( stream = ls_hcpr-xml_ui ) = 0.
      nodes = lo_xml_doc->get_first_node( )->get_children( ).

      CLEAR: index1.
      WHILE index1 < nodes->get_length( ).
        child = nodes->get_item( index1 ).
        DATA(lv_nname) = child->get_name( ).
        index1 = index1 + 1.
        IF lv_nname = 'viewNode'.
          CLEAR: index3.
          DATA(lo_elems) = child->get_children( ).
          WHILE index3 < lo_elems->get_length( ).
            DATA(childb) = lo_elems->get_item( index3 ).
            DATA(lv_t) = childb->get_type( ).
            IF lv_t = if_ixml_node=>co_node_element.
              element   ?= childb->query_interface( ixml_iid_element ).
              DATA(lv_aname) = element->get_content_as_string( ).
              DATA(lo_elattr) = element->get_attributes( ).
              DATA(lo_eltype) = lo_elattr->get_named_item_ns( EXPORTING name = 'type' uri = 'http://www.w3.org/2001/XMLSchema-instance' ).
              IF lo_eltype IS NOT INITIAL.
                IF lo_eltype->get_value( ) = 'BwCore:BwElement'.
                  DATA(lo_field) = lo_elattr->get_named_item_ns( EXPORTING name = 'name' uri = '' ).
                  IF lo_field IS NOT INITIAL.
                    ls_hcpr_fields-hcpr = lv_hcpr.
                    ls_hcpr_fields-name = lo_field->get_value( ).
                  ENDIF.
                  lo_field = lo_elattr->get_named_item_ns( EXPORTING name = 'infoObjectName' uri = '' ).
                  IF lo_field IS NOT INITIAL.
                    ls_hcpr_fields-iobj = lo_field->get_value( ).
                  ENDIF.
                  lo_field = lo_elattr->get_named_item_ns( EXPORTING name = 'dimension' uri = '' ).
                  IF lo_field IS NOT INITIAL.
                    ls_hcpr_fields-dim = shift_left( val = lo_field->get_value( ) sub = '#///' ).
                    ls_hcpr_fields-dim = shift_right( val = ls_hcpr_fields-dim sub = '§' ).
                  ENDIF.
                  DATA(item1) = lo_elattr->get_item( 0 ).
                  DATA(value1) = item1->get_value( ).
                  TRY.
                      ls_hcpr_fields-descr = lt_rsdiobj[ iobjnm = ls_hcpr_fields-iobj ]-txtlg.
                    CATCH cx_root.
                  ENDTRY.
                  TRY.
                      ls_hcpr_fields-descr = lt_colname[ hcprnm = ls_hcpr_fields-hcpr colname = ls_hcpr_fields-name  ]-descr.
                    CATCH cx_root.
                  ENDTRY.
                  IF ls_hcpr_fields-descr IS INITIAL.
                    ls_hcpr_fields-descr = ls_hcpr_fields-name.
                  ENDIF.
                  APPEND ls_hcpr_fields TO lt_hcpr_fields.
                  CLEAR: lo_field, ls_hcpr_fields .
                ENDIF.
                CLEAR: lo_eltype.
              ENDIF.

              CLEAR: index4.
              WHILE index4 < lo_elattr->get_length( ).
                DATA(lo_elem) = lo_elattr->get_item( index4 ).
                lv_aname = substring_before( val = lv_aname sub = '.' ).
                DATA(lv_avalue) = lo_elem->get_value( ).
                IF lv_avalue = 'Composite:CompositeInput'.
                  DATA(lo_fields) = element->get_children( ).
                  CLEAR: index5.
                  WHILE index5 < lo_fields->get_length( ).
                    DATA(lo_mi) =  lo_fields->get_item( index5 ).
                    lv_t = lo_mi->get_type( ).
                    IF ( lv_t = if_ixml_node=>co_node_element ) AND ( lo_fields->get_item( index5 )->get_name( ) = 'mapping' ).
                      element   ?= lo_mi->query_interface( ixml_iid_element ).
                      ls_pp_mapping-hcpr = lv_hcpr.
                      ls_pp_mapping-pprpv = lv_aname.
                      DATA(lo_sourcefield) = element->get_attributes( )->get_named_item_ns( EXPORTING name = 'sourceName' uri = '').
                      IF lo_sourcefield IS NOT INITIAL.
                        ls_pp_mapping-sourcefield =  lo_sourcefield->get_value( ).
                      ENDIF.
                      DATA(lo_targetfield) = element->get_attributes( )->get_named_item_ns( EXPORTING name = 'targetName' uri = '').
                      IF lo_targetfield IS NOT INITIAL.
                        ls_pp_mapping-targetfield = lo_targetfield->get_value( ).
                      ENDIF.
                      APPEND ls_pp_mapping TO lt_pp_mapping.
                      CLEAR: lo_sourcefield, lo_targetfield, ls_pp_mapping.
                    ENDIF.
                    index5 = index5 + 1.
                  ENDWHILE.
                ENDIF.
                index4 = index4 + 1.
              ENDWHILE.
            ENDIF.
            index3 = index3 + 1.
          ENDWHILE.
        ENDIF.
      ENDWHILE.

      " preparation output
      SORT lt_hcpr_fields BY dim ASCENDING.
      LOOP AT lt_hcpr_fields ASSIGNING FIELD-SYMBOL(<fs_hf>).
        LOOP AT lt_pp_mapping ASSIGNING FIELD-SYMBOL(<fs_ppm>)
          WHERE targetfield = <fs_hf>-name AND hcpr = <fs_hf>-hcpr.
          IF <fs_hf>-lnkpp IS NOT INITIAL.
            <fs_hf>-lnkpp = | { <fs_hf>-lnkpp }, { <fs_ppm>-pprpv } |.
          ELSE.
            <fs_hf>-lnkpp = | { <fs_ppm>-pprpv } |.
          ENDIF.
          APPEND <fs_ppm>-pprpv TO lt_hcpr.
        ENDLOOP.
      ENDLOOP.
      SORT lt_hcpr BY pprpv ASCENDING.
      DELETE ADJACENT DUPLICATES FROM lt_hcpr.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  NEW lcl_hcprxmldoc( in_hcpr = p_hcpr )->display_grid( ) .

END-OF-SELECTION.