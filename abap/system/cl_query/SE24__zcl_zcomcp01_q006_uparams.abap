CLASS zcl_zcomcp01_q006_uparams DEFINITION
 PUBLIC
  ABSTRACT
  FINAL
  create public .

  PUBLIC SECTION.
    CLASS-DATA query    TYPE store_label.
    CLASS-DATA infocube TYPE store_label.
    CLASS-DATA folder   TYPE store_label.
    CLASS-DATA name     TYPE store_label.
    CLASS-DATA name_def TYPE store_label.
    CLASS-DATA flag     TYPE flag.
    CLASS-DATA year     TYPE store_label.
    CLASS-DATA step     TYPE syst_tabix.
    CLASS-DATA var_t    TYPE TABLE OF rrx_var.
    CLASS-DATA add_clm  TYPE RANGE OF rsiobjnm.
    CLASS-DATA key_clm  TYPE RANGE OF store_label.
    CLASS-DATA fil_t    TYPE TABLE OF zbw_query_filter.
    CLASS-DATA text     TYPE flag.
    CLASS-DATA zip     TYPE flag.

    TYPES tt_rrx_var          TYPE TABLE OF rrx_var WITH EMPTY KEY.
    TYPES tt_zbw_query_filter TYPE TABLE OF zbw_query_filter WITH EMPTY KEY.
    TYPES: BEGIN OF ts_input,
             query    TYPE store_label,
             infocube TYPE store_label,
             folder   TYPE store_label,
             name     TYPE store_label,
             name_def TYPE store_label,
             step     TYPE syst_tabix,
             flag     TYPE flag,
             year     TYPE store_label,
             var_t    TYPE tt_rrx_var,
             add_clm  TYPE RANGE OF rsiobjnm,
             key_clm  TYPE RANGE OF store_label,
             fil_t    TYPE tt_zbw_query_filter,
             text     TYPE flag,
             zip     TYPE flag,
           END OF ts_input.

    CLASS-METHODS class_constructor.
    CLASS-METHODS force_reload.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_zcomcp01_q006_uparams IMPLEMENTATION.
  METHOD class_constructor.
    zcl_ca_uparams=>reload_class( 'ZCOMCP01_Q006'  ).
  ENDMETHOD.

  METHOD force_reload.
    zcl_ca_uparams=>reload_class( 'ZCOMCP01_Q006'  ).
  ENDMETHOD.

ENDCLASS.