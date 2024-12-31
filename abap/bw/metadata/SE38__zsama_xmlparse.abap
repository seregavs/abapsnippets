*&---------------------------------------------------------------------*
*& Report ZSAMA_XMLPARSE
*&---------------------------------------------------------------------*
*& see DEMO_EXPRESSIONS for string operations
*&  https://help.sap.com/doc/saphelp_nwpi711/7.1.1/en-US/86/8280de12d511d5991b00508b6b8b11/content.htm?no_cache=true
*&---------------------------------------------------------------------*
REPORT zsama_xmlparse.

PARAMETERS: p_hcpr TYPE rsohcprnm DEFAULT 'ZCF_CP12' OBLIGATORY.

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
    pprpv       TYPE rsinfocube,
    sourcefield TYPE char100,
    targetfield TYPE char100,
  END OF tt_pp_mapping,

  BEGIN OF tt_colname,
    hcprnm      TYPE rsohcprnm,
    ttyp        TYPE rsoadsottyp,
    colname(40) TYPE c,
    descr       TYPE rsoadsodescr,
  END OF tt_colname.

DATA:
  ls_hcpr_fields TYPE   tt_hcpr_fields,
  ls_pp_mapping  TYPE   tt_pp_mapping,
  lt_hcpr_fields TYPE STANDARD TABLE OF tt_hcpr_fields,
  lt_pp_mapping  TYPE STANDARD TABLE OF tt_pp_mapping,
  ls_colname     TYPE tt_colname,
  lt_colname     TYPE STANDARD TABLE OF tt_colname.

DATA:
  lo_xml_doc TYPE REF TO cl_xml_document,
  ld_rc      TYPE i.

DATA: nodes  TYPE REF TO if_ixml_node_list,
      child  TYPE REF TO if_ixml_node,
      index1 TYPE i,
      index2 TYPE i,
      index3 TYPE i,
      index4 TYPE i,
      index5 TYPE i,
      index6 TYPE i.

DATA: attributes TYPE REF TO if_ixml_named_node_map,
      attribute  TYPE REF TO if_ixml_attribute,
      element    TYPE REF TO if_ixml_element.
DATA:
  lr_header   TYPE REF TO cl_salv_form_layout_grid,
  lr_grid     TYPE REF TO cl_salv_form_layout_grid,
  lr_salv_alv TYPE REF TO cl_salv_table.

SELECT SINGLE * INTO @DATA(ls_hcpr)
  FROM rsohcpr WHERE objvers = 'A' AND hcprnm =  @p_hcpr.
IF sy-subrc = 0.

  SELECT iobjnm, txtlg INTO TABLE @DATA(lt_rsdiobj)
    FROM rsdiobjt WHERE objvers = 'A'  AND langu = 'E'.

  SELECT * FROM rsohcprt INTO @DATA(ls_rshcprt)
    WHERE objvers = 'A' AND hcprnm = @p_hcpr AND colname <> '' AND langu = 'E'.
    CLEAR: ls_colname.
    ls_colname-hcprnm = ls_rshcprt-hcprnm.
    ls_colname-ttyp = 'ELEM'. " !2F
    ls_colname-colname = substring_after(  val = ls_rshcprt-colname sub = '!2F' occ = -1 ).
    IF ls_colname-colname <> 'A7'.
      ls_colname-descr = ls_rshcprt-description.
      APPEND ls_colname TO lt_colname.
    ENDIF.
  ENDSELECT.

  SELECT SINGLE description
    FROM rsohcprt INTO @DATA(lv_hcpr_desc)
    WHERE objvers = 'A' AND hcprnm = @p_hcpr AND colname = '' AND langu = 'E'.

  CREATE OBJECT lo_xml_doc.
  ld_rc = lo_xml_doc->parse_xstring( stream = ls_hcpr-xml_ui ).
  DATA(lo_cpnode) = lo_xml_doc->get_first_node( ).
*  lo_xml_doc->add_namespace_def(
*    EXPORTING
*      node        = lo_cpnode               " IF_IXML_NODE
*      alias       = 'xmlns:xsi'
*      uri         =  'http://www.w3.org/2001/XMLSchema-instance'
**      location    =
**      is_targetns = space
*    RECEIVING
*      retcode     =  ld_rc               " Return Value, Return Value After ABAP Statements
*  ).
*  DATA(lv_stackable) = lo_xml_doc->get_node_attribute(
*    EXPORTING
*      node  =                  lo_cpnode " Nodes
*      name  =                  'stackable'" Name of Attribute
*  ).
  nodes = lo_cpnode->get_children( ).
  CLEAR: index1.
  WHILE index1 < nodes->get_length( ).
    child = nodes->get_item( index1 ).
    DATA(lv_nname) = child->get_name( ).
    WRITE: / |node name = { lv_nname }|.
    CLEAR: attributes.
    attributes = child->get_attributes( ).
    CLEAR: index2.
    WHILE index2 < attributes->get_length( ).
      DATA(childa) = attributes->get_item( index2 ).
      attribute ?= childa->query_interface( ixml_iid_attribute ).
      DATA(lv_aname) = attribute->get_name( ).
      DATA(lv_ans) = attribute->get_namespace_prefix( ).
      DATA(lv_avalue) = attribute->get_value( ).
      WRITE: / |  Attribute: { lv_ans }.{  lv_aname } = { lv_avalue }. |.
      index2 = index2 + 1.
    ENDWHILE.
    index1 = index1 + 1.
    IF lv_nname = 'dimension'.
      DATA(lo_elems) = child->get_children( ).
      WRITE / : | dim-name: { child->get_name( ) }   |.
      IF lo_elems IS NOT INITIAL.
        "        WRITE / : | dim label = { lo_elems->get_item( 0 )->get_name( ) }|.
      ENDIF.
    ENDIF.
    IF lv_nname = 'viewNode'.
      CLEAR: index3.
      lo_elems = child->get_children( ).
      WHILE index3 < lo_elems->get_length( ).
        DATA(childb) = lo_elems->get_item( index3 ).
        DATA(lv_t) = childb->get_type( ).
        IF lv_t = if_ixml_node=>co_node_element.
          element   ?= childb->query_interface( ixml_iid_element ).
          lv_aname = element->get_content_as_string( ).
          DATA(lv_aname1) = element->get_value( ).
          WRITE: / |   Element { lv_aname } |.
          DATA(lo_elattr) = element->get_attributes( ).
          DATA(lo_eltype) = lo_elattr->get_named_item_ns( EXPORTING name = 'type' uri = 'http://www.w3.org/2001/XMLSchema-instance' ).
          IF lo_eltype IS NOT INITIAL.
            IF lo_eltype->get_value( ) = 'BwCore:BwElement'.
              DATA(lo_field) = lo_elattr->get_named_item_ns( EXPORTING name = 'name' uri = '' ).
              IF lo_field IS NOT INITIAL.
                ls_hcpr_fields-hcpr = p_hcpr.
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
              WRITE: / |    name = { ls_hcpr_fields-name }, { ls_hcpr_fields-iobj }|.
              CLEAR: lo_field, ls_hcpr_fields .
              " -- /
              DATA(lo_fname) = element->get_children( ).
              CLEAR: index4.
              WHILE index4 < lo_fname->get_length( ).
                WRITE: / |     subAttr->  { lo_fname->get_item( index4 )->get_name( ) } |.
                index4 = index4 + 1.
              ENDWHILE.
              " --/
              "-- /
              DATA(lo_attr) = element->get_attributes( ).
              IF lo_attr->get_length( ) > 0.
                DATA(lo_iter_a) = lo_attr->create_iterator( ).
                DATA(lo_attra) = lo_iter_a->get_next( ).
                WHILE lo_attra IS NOT INITIAL.
                  WRITE: / |     A.iterator-> { lo_attra->get_name( ) }|.
                  lo_attra = lo_iter_a->get_next( ).
                ENDWHILE.
              ENDIF.
              DATA(lo_iter) = element->get_children( )->create_iterator(  ).
              DATA(lo_node) = lo_iter->get_next( ).
              WHILE lo_node IS NOT INITIAL.
                WRITE: / |     iterator-> { lo_node->get_name( ) }|.
                lo_node = lo_iter->get_next( ).
              ENDWHILE.

              " -- /
            ENDIF.
            IF lo_eltype->get_value( ) = 'Composite:CompositeInput'.
              CLEAR: index5.
            ENDIF.
            CLEAR: lo_eltype.
          ENDIF.

*          IF lo_fname IS NOT INITIAL.
*             lv_t = lo_fname->get_type( ).
*             IF ( lv_t = if_ixml_node=>co_node_element ).
*                DATA(lo_label) = lo_fname->get_attributes( )->get_named_item_ns( EXPORTING name = 'label' uri = '').
*                lo_label = lo_fname->get_attributes( )->get_item( 0 ).
*                IF lo_label IS NOT INITIAL.
*                   ls_hcpr_fields-descr = lo_label->get_value( ).
*                ENDIF.
*             ENDIF.
*          ENDIF.
          CLEAR: index4.
          WHILE index4 < lo_elattr->get_length( ).
            DATA(lo_elem) = lo_elattr->get_item( index4 ).
            lv_aname1  = lo_elem->get_name( ).
            lv_aname = substring_before( val = lv_aname sub = '.' ).
            lv_avalue = lo_elem->get_value( ).
            IF lv_avalue = 'Composite:CompositeInput'.
              DATA(lo_fields) = element->get_children( ).
              CLEAR: index5.
              WHILE index5 < lo_fields->get_length( ).
                DATA(lo_mi) =  lo_fields->get_item( index5 ).
                lv_t = lo_mi->get_type( ).
                IF ( lv_t = if_ixml_node=>co_node_element ) AND ( lo_fields->get_item( index5 )->get_name( ) = 'mapping' ).
                  element   ?= lo_mi->query_interface( ixml_iid_element ).
                  ls_pp_mapping-hcpr = p_hcpr.
                  ls_pp_mapping-pprpv = lv_aname.
                  DATA(lo_sourcefield) = element->get_attributes( )->get_named_item_ns( EXPORTING name = 'sourceName' uri = '').
                  IF lo_sourcefield IS NOT INITIAL.
                    ls_pp_mapping-sourcefield =  lo_sourcefield->get_value( ).
                  ENDIF.
                  DATA(lo_targetfield) = element->get_attributes( )->get_named_item_ns( EXPORTING name = 'targetName' uri = '').
                  IF lo_targetfield IS NOT INITIAL.
                    ls_pp_mapping-targetfield = lo_targetfield->get_value( ).
                  ENDIF.
                  WRITE: / |     { ls_pp_mapping-hcpr }: { ls_pp_mapping-sourcefield } -> { ls_pp_mapping-targetfield } |.

                  APPEND ls_pp_mapping TO lt_pp_mapping.

                  CLEAR: lo_sourcefield, lo_targetfield, ls_pp_mapping.
*                  CLEAR: index6.
*                  WHILE index6 < element->get_attributes( )->get_length( ).
*                    DATA(lv_key) = element->get_attributes( )->get_item( index6 )->get_name( ).
*                    DATA(lv_value) = element->get_attributes( )->get_item( index6 )->get_value( ).
*                    DATA(lv_ns) = element->get_attributes( )->get_item( index6 )->get_namespace_uri( ).
*                    WRITE: / |       { lv_ns }.{ lv_key }: { lv_value }.|.
*                    index6 = index6 + 1.
*                  ENDWHILE.
                ENDIF.
                index5 = index5 + 1.
              ENDWHILE.
            ENDIF.
            WRITE: / |    Attribute { lv_aname1 } - { lv_avalue } |.
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
    ENDLOOP.
  ENDLOOP.

  cl_salv_table=>factory(
    IMPORTING
      r_salv_table = lr_salv_alv
    CHANGING
      t_table = lt_hcpr_fields ).

  CREATE OBJECT lr_grid.
  CREATE OBJECT lr_header.

  lr_grid = lr_header->create_grid( row = 2  column = 1 colspan = 6 ).
  lr_grid->create_label( row = 1  column = 65 text = 'HCPR field mapping' ).
  lr_salv_alv->set_top_of_list( lr_grid ).
  lr_salv_alv->display( ).
*  WRITE: / |Root node attributes|.
*  attributes = lo_cpnode->get_attributes( ).
*  CLEAR: index1.
*  IF NOT attributes IS INITIAL.
*    WHILE index1 < attributes->get_length( ).
*      child = attributes->get_item( index1 ).
*      attribute ?= child->query_interface( ixml_iid_attribute ).
*      lv_aname = attribute->get_name( ).
*      lv_avalue = attribute->get_value( ).
*      lv_ans = attribute->get_namespace_prefix( ).
*      WRITE: / | Attribute: { lv_ans }.{ lv_aname } = { lv_avalue } |.
*      index1 = index1 + 1.
*    ENDWHILE.
*
*  ENDIF.

  "  lo_xml_doc->display( ).
ENDIF.