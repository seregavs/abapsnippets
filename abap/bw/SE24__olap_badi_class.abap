class ZCL_RSROA_OLAP_01 definition
  public
  create public .

public section.

*"* public components of class ZCL_RSROA_OLAP_01
*"* do not include other source files here!!!
  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_RSROA_OLAP_BADI .

  data P_KYF_ZMMIM_CP04_DS_C030 type I .
  data P_CHA_ZCMATNO type I .
  data P_KYF_0DEB_CRE_LC type I .

  class-methods GET_FIELD_POSITION_D
    importing
      !I_FIELDNM type C
      !I_S_DATA type ANY
    returning
      value(R_POSITION) type I
    raising
      CX_RS_NOT_FOUND .
protected section.
*"* protected components of class ZCL_RSROA_OLAP_01
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_RSROA_OLAP_01
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_RSROA_OLAP_01 IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_RSROA_OLAP_01=>GET_FIELD_POSITION_D
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_FIELDNM                      TYPE        C
* | [--->] I_S_DATA                       TYPE        ANY
* | [<-()] R_POSITION                     TYPE        I
* | [!CX!] CX_RS_NOT_FOUND
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD GET_FIELD_POSITION_D.
  DATA:
    l_r_tdescr      TYPE REF TO cl_abap_typedescr,
    l_r_sdescr      TYPE REF TO cl_abap_structdescr,
    l_t_components  TYPE        abap_compdescr_tab,
    l_s             TYPE        string.

  l_r_tdescr     =  cl_abap_typedescr=>describe_by_data( i_s_data ).
  l_r_sdescr     ?= l_r_tdescr.
  l_t_components =  l_r_sdescr->components.
  READ TABLE l_t_components TRANSPORTING NO FIELDS
    WITH KEY name = i_fieldnm.
  IF sy-subrc GT 0.
    CONCATENATE 'I_S_DATA' i_fieldnm INTO l_s SEPARATED BY '-'.
    RAISE EXCEPTION TYPE cx_rs_not_found
      EXPORTING
        object = 'FIELD'
        key    = l_s.
  ENDIF.
  r_position = sy-tabix.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSROA_OLAP_01->IF_EX_RSROA_OLAP_BADI~CHECK_COMPUTE_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_S_RKB1D                      TYPE        RSR_S_RKB1D
* | [--->] I_PARTCUBE                     TYPE        RSD_INFOCUBE
* | [--->] I_NCUM                         TYPE        RS_BOOL
* | [<-->] C_COMPUTE_TABLE                TYPE        RS_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD IF_EX_RSROA_OLAP_BADI~CHECK_COMPUTE_TABLE.
**  set C_COMPUTE_TABLE to TRUE only when you need the method
**  COMPUTE_TABLE to be called for your implementation
**  otherwise don't touch C_COMPUTE_TABLE. As the BADI can
**  have multi implementations an other implemantion may have
**  set C_COMPUTE_TABLE already to TRUE.

*  c_compute_table = rs_c_true.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSROA_OLAP_01->IF_EX_RSROA_OLAP_BADI~COMPUTE_SINGLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_S_RKB1D                      TYPE        RSR_S_RKB1D
* | [--->] I_PARTCUBE                     TYPE        RSD_INFOCUBE
* | [--->] I_NCUM                         TYPE        RS_BOOL
* | [<-->] C_S_DATA                       TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD IF_EX_RSROA_OLAP_BADI~COMPUTE_SINGLE.

* if P_CHA_/P_KYF... > 0.
*   assign component P_CHA_/P_KYF... of structure C_S_DATA to <l_...>.
* endif.
  FIELD-SYMBOLS:
    <fs_zmmim_cp04_ds_c030> TYPE /BIC/OIZKBALENDA,
    <fs_0deb_cre_lc>  TYPE /BI0/OIDEB_CRE_LC.

  IF p_kyf_zmmim_cp04_ds_c030 > 0.
     ASSIGN COMPONENT p_kyf_zmmim_cp04_ds_c030 of STRUCTURE c_s_data to <fs_zmmim_cp04_ds_c030>.
     CHECK <fs_zmmim_cp04_ds_c030> IS ASSIGNED.
     <fs_zmmim_cp04_ds_c030> = 101.
  ENDIF.
  IF p_kyf_0deb_cre_lc > 0.
     ASSIGN COMPONENT p_kyf_0deb_cre_lc of STRUCTURE c_s_data to <fs_0deb_cre_lc>.
     CHECK <fs_0deb_cre_lc> IS ASSIGNED.
     <fs_0deb_cre_lc> = 1001.
  ENDIF.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSROA_OLAP_01->IF_EX_RSROA_OLAP_BADI~COMPUTE_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_S_RKB1D                      TYPE        RSR_S_RKB1D
* | [--->] I_PARTCUBE                     TYPE        RSD_INFOCUBE
* | [--->] I_NCUM                         TYPE        RS_BOOL
* | [<-->] C_T_DATA                       TYPE        STANDARD TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD IF_EX_RSROA_OLAP_BADI~COMPUTE_TABLE.
** COMPUTE_TABLE is called once per PartProvider and DataManager-call
** There may be multiple DataManager-calls per part provider
**
** COMPUTE_TABLE allows more flexibility than COMPUTE_SINGLE,
** e.g. to split an entry of C_T_DATA into two entries
** or to delete an entry or to add an entry
** if you don't need this flexibility, just call COMPUTE_SINGLE
** for every entry of C_T_DATA

  FIELD-SYMBOLS:
    <l_s_data> TYPE data.

  LOOP AT c_t_data ASSIGNING <l_s_data>.
    if_ex_rsroa_olap_badi~compute_single(
      EXPORTING
        i_s_rkb1d  = i_s_rkb1d
        i_partcube = i_partcube
        i_ncum     = i_ncum
      CHANGING
        c_s_data   = <l_s_data> ).
  ENDLOOP.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_RSROA_OLAP_01=>IF_EX_RSROA_OLAP_BADI~DEFINE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_S_RKB1D                      TYPE        RSR_S_RKB1D
* | [--->] I_TH_CHANM_USED                TYPE        RRKE_TH_CHANM
* | [--->] I_TH_KYFNM_USED                TYPE        RRKE_TH_KYFNM(optional)
* | [--->] I_CHANM_ADDED                  TYPE        RS_BOOL (default =RS_C_FALSE)
* | [<-->] C_THX_KYFNM                    TYPE        RRKE_THX_KYFNM
* | [<-->] C_T_CHANM                      TYPE        RRKE_T_CHANM
* | [<-->] C_T_KYFNM                      TYPE        RSD_T_KYFNM
* | [<-->] C_PACKAGED_READ_POSSIBLE       TYPE        RS_BOOL(optional)
* | [!CX!] CX_RS_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD if_ex_rsroa_olap_badi~define.
  DATA:
    l_s_chanm  TYPE rrke_s_chanm,
    l_sx_kyfnm TYPE LINE OF rrke_thx_kyfnm,
    l_kyfnm    TYPE rsd_kyfnm.

  FIELD-SYMBOLS:
    <l_s_chanm> TYPE rrke_s_chanm.

  CASE i_s_rkb1d-infocube.

    WHEN 'ZMMIM_CP04'.

      l_s_chanm-chanm = 'ZCMATNO'.
      l_s_chanm-mode  = rrke_c_mode-read. " rrke_c_mode-read / rrke_c_mode-no_selection / rrke_c_mode-filter
      APPEND l_s_chanm TO c_t_chanm.

*      " characteristic
*      l_s_chanm-chanm = '...'.
*      l_s_chanm-mode  = ???. " rrke_c_mode-read / rrke_c_mode-no_selection / rrke_c_mode-filter
*      APPEND l_s_chanm TO c_t_chanm.
*
*      " key figure
*      " old style: use c_t_kyfnm
*      l_kyfnm = '...'.
*      APPEND l_kyfnm TO c_t_kyfnm.
*
*      " key figure
*      " new style: use c_thx_kyfnm to enable seelction of structure elements and
*      " increase performance by only reading needed keyfigures
*
*      " for each keyfigure name its input dependencies
*      l_sx_kyfnm-kyfnm = '...'. "the keyfigure that is changed inside the BADI
*      l_kyfnm = '...'. "the keyfigure that is used as input for the changed keyfigure
      l_sx_kyfnm-kyfnm = 'ZMMIM_CP04_DS_C030'.
      INSERT l_sx_kyfnm INTO TABLE c_thx_kyfnm.
      l_sx_kyfnm-kyfnm = '0DEB_CRE_LC'.
      INSERT l_sx_kyfnm INTO TABLE c_thx_kyfnm.
*
  ENDCASE.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSROA_OLAP_01->IF_EX_RSROA_OLAP_BADI~FILTER_F4_MODE_QUERY
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_CHANM                        TYPE        RSCHANM
* | [<-->] C_TS_KEY                       TYPE        RSD_TS_CHAVL
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD IF_EX_RSROA_OLAP_BADI~FILTER_F4_MODE_QUERY.

  DATA: l_chavl TYPE rschavl,
        l_n2(2) TYPE n.

  CASE i_chanm.

* filter all odd months
    WHEN '0CALMONTH'.
      LOOP AT c_ts_key INTO l_chavl.
        l_n2 = l_chavl+4(2).
        l_n2 = l_n2 MOD 2.
        IF l_n2 EQ 0.
          DELETE c_ts_key.
        ENDIF.
      ENDLOOP.

    WHEN OTHERS.
*     nothing to do
  ENDCASE.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_RSROA_OLAP_01->IF_EX_RSROA_OLAP_BADI~INITIALIZE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_S_RKB1D                      TYPE        RSR_S_RKB1D
* | [--->] I_KEY_DATE                     TYPE        RRSRDATE
* | [--->] I_TH_SFC                       TYPE        RRKG_TH_SFC
* | [--->] I_FISCVARNT                    TYPE        PERIV
* | [--->] I_TH_SFK                       TYPE        RRKG_TH_SFK
* | [--->] I_S_DATA                       TYPE        ANY
* | [!CX!] CX_RS_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD IF_EX_RSROA_OLAP_BADI~INITIALIZE.
  DATA:
    l_global_name TYPE string.

  FIELD-SYMBOLS:
    <l_global> TYPE i,
    <l_s_sfc>  TYPE rrkg_s_sfc,
    <l_s_sfk>  TYPE rrkg_s_sfk.

* there's no need to change this method
* Just create attributes for each charactersitic
* with name P_CHA_<characteristic> TYPE i.
* and constants for each key figure with name
* P_KYF_<key figure> TYPE i.

* get field postions for characteristics in structure
  LOOP AT i_th_sfc ASSIGNING <l_s_sfc>
    WHERE user_exit NE rrke_c_mode-none.
*   field name in structure is keyreturnnm
*   name of the global variable
    CONCATENATE 'P_CHA' <l_s_sfc>-chanm
        INTO l_global_name
        SEPARATED BY '_'.
*   fill the global variable
    UNASSIGN <l_global>.
    ASSIGN (l_global_name) TO <l_global>.
    CHECK <l_global> IS ASSIGNED.
    TRY.
        <l_global> = cl_exm_rsroa_olap_badi=>get_field_position_d(
                                               i_fieldnm = <l_s_sfc>-keyreturnnm
                                               i_s_data  = i_s_data ).
      CATCH cx_rs_not_found.
*       in case the charactersitic is added as free charactersitic during query
*       execution, it isn't part of the SFC at the start.
        <l_global> = 0.
    ENDTRY.
  ENDLOOP.

* get field positions for key figures in structure
  LOOP AT i_th_sfk ASSIGNING <l_s_sfk>
    WHERE value_returnnm IS NOT INITIAL.
*   name of the global variable
    CONCATENATE 'P_KYF' <l_s_sfk>-kyfnm
        INTO l_global_name
        SEPARATED BY '_'.
*   fill the global variable
    UNASSIGN <l_global>.
    ASSIGN (l_global_name) TO <l_global>.
    CHECK <l_global> IS ASSIGNED.
    <l_global> = cl_exm_rsroa_olap_badi=>get_field_position_d(
                                           i_fieldnm = <l_s_sfk>-value_returnnm
                                           i_s_data  = i_s_data ).
  ENDLOOP.
ENDMETHOD.
ENDCLASS.