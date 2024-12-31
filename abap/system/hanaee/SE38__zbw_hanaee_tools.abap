*&---------------------------------------------------------------------*
*& Report ZBW_HANAEE_TOOLS
*&---------------------------------------------------------------------*
*& The report proide some basic actions with tables in HANA EE (schema BW_REMOTE ) and local BW schema
*&---------------------------------------------------------------------*
REPORT zbw_hanaee_tools.

PARAMETERS:
  ptname  TYPE char40, " имя таблицы
  paction TYPE char1 OBLIGATORY DEFAULT '1', " 1 - MERGE, 2 - MERGE+COMPR, 3 - UNLOAD,
                                             " 4 - TRUNCATE, 5 - DELETE, 6 - FILL_ZNLSO10
                                             " 7 - DELETE from local table
  pwhere  TYPE char80. " criterias for the DELETE


START-OF-SELECTION.
  NEW zcl_bw_hanaee_utils( in_tname = ptname
                         in_action = paction
                         in_where = pwhere )->call_action( ).

END-OF-SELECTION.