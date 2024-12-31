CLASS zcl_workdaydiff DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_amdp_marker_hdb .

    CLASS-METHODS calc_wd_diff
      IMPORTING
        VALUE(i_fc)     TYPE wfcid DEFAULT 'SA'
        VALUE(i_begda)  TYPE dats
        VALUE(i_endda)  TYPE dats
      EXPORTING
        VALUE(distance) TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_workdaydiff IMPLEMENTATION.

  METHOD calc_wd_diff BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT OPTIONS READ-ONLY.
    SELECT WORKDAYS_BETWEEN(i_fc,i_begda,i_endda,'SAPBWP')+1 as distance into distance
      from dummy;
  ENDMETHOD.

ENDCLASS.