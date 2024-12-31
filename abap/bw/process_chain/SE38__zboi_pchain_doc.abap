*&---------------------------------------------------------------------*
*& Report ZBOI_PCHAIN_DOC
*&---------------------------------------------------------------------*
*& Get process chain metadata and display in ALV Grid
*&---------------------------------------------------------------------*
REPORT zboi_pchain_doc.

TYPES:

  BEGIN OF ty_pchain,
    stream   TYPE string,
    chain_id TYPE string,
    txtlg    TYPE string,
  END OF   ty_pchain,

  BEGIN OF ty_result,
    stream(4)          TYPE c,
    chain_id(30)       TYPE c,
    period_chain(7)    TYPE c,
    chain_descr(50)    TYPE c,
    node_id(3)         TYPE c,
    parent_node_id(3)  TYPE c,
    step_type(15)      TYPE c,
    step_descr(100)    TYPE c,
    hierarchy_level(2) TYPE c,
  END OF ty_result.

DATA:lo_sql    TYPE REF TO cl_sql_statement,
     lv_sql    TYPE string,
     lx_sql    TYPE REF TO cx_sql_exception,
     lo_result TYPE REF TO cl_sql_result_set,
     lr_data   TYPE REF TO data,
     lt_result TYPE TABLE OF ty_result,
     lt_data   TYPE TABLE OF ty_result,
     lt_pchain TYPE TABLE OF ty_pchain,
     msgtxt    TYPE string,
     lv_s2(2)  TYPE c.

DATA:
  lr_header   TYPE REF TO cl_salv_form_layout_grid,
  lr_grid     TYPE REF TO cl_salv_form_layout_grid,
  lr_salv_alv TYPE REF TO cl_salv_table.

lv_s2 = |{ cl_abap_conv_in_ce=>uccpi( 124 ) }{ cl_abap_conv_in_ce=>uccpi( 124 ) }|.
TRY.
    CREATE OBJECT lo_sql .

    lv_sql =
      | SELECT substr(chain_id, 2,4) as stream, chain_id, txtlg FROM "RSPCCHAINT" | &&
      |  WHERE objvers = 'A' and langu = 'E' | &&
      |    AND ( ( chain_id LIKE 'ZFIGL%' ) OR ( chain_id LIKE 'ZMMIM%' ) OR ( chain_id LIKE 'ZFSTB%' ) | &&
      |        OR ( chain_id LIKE 'ZLOHU%' ) OR ( chain_id LIKE 'ZFSTR%' ) OR ( chain_id LIKE 'ZGN_%' )) | &&
      |  ORDER BY 1 ASC   |.
    lo_result = lo_sql->execute_query( lv_sql ).
    GET REFERENCE OF lt_pchain INTO lr_data.
    lo_result->set_param_table( lr_data ).
    lo_result->next_package( ).
    lo_result->close( ).

    LOOP AT lt_pchain ASSIGNING FIELD-SYMBOL(<fs_pchain>).

      lv_sql =
          | SELECT  | &&
          |   v1.stream, v1.chain_id, v1.period_chain, | &&
          |   v1.chain_descr, v0.node_id, v0.parent_node_id, v0.step_type,  | &&
          |   v0.step_descr, v0.hierarchy_level   | &&
          | FROM ( | &&
          | SELECT | &&
          |   v1.node_id,  | &&
          |   v1.parent_node_id,    | &&
          |   v1.chain_id, | &&
          |   v1.step_type, v2.description as step_descr, hierarchy_level FROM      | &&
          | (SELECT  | &&
          |     hierarchy_rank AS node_id,   | &&
          |     hierarchy_parent_rank AS parent_node_id, | &&
          |     chain_id, | &&
          |     "TYPE" as step_type, | &&
          |     variante, | &&
          |     concat("TYPE",variante) as gid, | &&
          |     hierarchy_level,     hierarchy_rank | &&
          |  FROM HIERARCHY (  | &&
          |  SOURCE ( SELECT chain_id, map(eventp_green,'',eventp_red, eventp_green) AS node_id, eventp_start AS parent_id, "TYPE", variante, | &&
          |        map("TYPE",'TRIGGER',0,1) as ord | &&
                  |   FROM "RSPCCHAIN" WHERE objvers = 'A' AND chain_id IN ( '{ <fs_pchain>-chain_id }' ) ) | &&
          |   START WHERE "TYPE" = 'TRIGGER' ORPHAN ROOT) | &&
          | WHERE NOT ( ( "TYPE" = 'TRIGGER' ) AND ( hierarchy_rank > 1 ) )) as v1 LEFT OUTER JOIN  | &&
          | (SELECT  | &&
          |   concat('CHAIN',chain_id) as gid, chain_id \|\| ' - ' \|\| TXTLG as description | &&
          |   FROM  "RSPCCHAINT" | &&
          |  WHERE chain_id LIKE 'Z%' | &&
          |    AND objvers = 'A' AND langu = 'E' | &&
          | UNION ALL    | &&
          | SELECT | &&
                  |   concat('DTP_LOAD',dtp) as gid,  'DTP (' \|\| updmode \|\| ') ' \|\| srctp \|\| '-' \|\| src \|\| ' -> ' \|\| tgttp \|\| '-' \|\| tgt as description | &&
          |   FROM "RSBKDTP" | &&
          |  WHERE objvers = 'A' | &&
          | UNION ALL    | &&
          | SELECT | &&
          |   concat("TYPE",variante) as gid, TXTLG as description | &&
          |   FROM "RSPCVARIANTT"    | &&
          |  WHERE objvers = 'A'  AND langu = 'E' | &&
          | UNION ALL  | &&
          | SELECT concat('TRIGGER',variante) as gid, map(meta,'X','Start metachain','Start chain') as description  | &&
          |   FROM "RSPCTRIGGER" WHERE objvers='A'  | &&
          |  ) as v2 ON v1.gid = v2.gid) as v0, | &&
          | (SELECT substr(chain_id, 2,4) as stream, chain_id,  | &&
          |         map(right(chain_id, 2),'TD','Daily','MD','Monthly',right(chain_id, 2)) as Period_chain, | &&
          |         txtlg as chain_descr FROM "RSPCCHAINT" | &&
          |   WHERE objvers = 'A' and langu = 'E') v1 | &&
          |  WHERE v0.chain_id = v1.chain_id      | &&
          | ORDER BY v1.chain_id, hierarchy_level  |.


      lo_result = lo_sql->execute_query( lv_sql ).
      GET REFERENCE OF lt_result INTO lr_data.
      lo_result->set_param_table( lr_data ).
      lo_result->next_package( ).
      lo_result->close( ).
      APPEND LINES OF lt_result TO lt_data.
      CLEAR: lt_result.
    ENDLOOP.

    cl_salv_table=>factory(
      IMPORTING
        r_salv_table = lr_salv_alv
      CHANGING
        t_table = lt_data ).

    CREATE OBJECT lr_grid.
    CREATE OBJECT lr_header.

    lr_grid = lr_header->create_grid( row = 2  column = 1 colspan = 6 ).
    lr_grid->create_label( row = 1  column = 65 text = 'Process chains' ).
    lr_salv_alv->set_top_of_list( lr_grid ).
    lr_salv_alv->display( ).

  CATCH cx_sql_exception INTO lx_sql.
    msgtxt = lx_sql->get_text( ).
ENDTRY.