*&---------------------------------------------------------------------*
*& Report ZBOI_STREAM_AFTER_PC
*&---------------------------------------------------------------------*
*& Executes pchain after streaming if data loaded from streaming was OK
*&---------------------------------------------------------------------*
REPORT zboi_stream_after_pc.

DATA: lv_spc     TYPE rspc_chain, " pchain streaming
      lv_apc     TYPE rspc_chain, " pchain after streaming
      lv_i       TYPE i,
      lv_logid_a TYPE rspc_logid.

lv_apc = 'ZMMIM_MM_E_S_TD'.
lv_spc = 'ZMMIM_MM_S_S_TD'.

SELECT * FROM rspclogchain
  INTO @DATA(ls_chainlog_a)
 WHERE chain_id = @lv_apc
   AND analyzed_status = 'G'
  ORDER BY datum DESCENDING, zeit DESCENDING.
  EXIT.
ENDSELECT.

SELECT * FROM rspclogchain
  INTO @DATA(ls_chainlog_s)
 WHERE chain_id = @lv_spc
   AND analyzed_status = 'G'
  ORDER BY datum DESCENDING, zeit DESCENDING.
  EXIT.
ENDSELECT.

IF ( ls_chainlog_s IS NOT INITIAL ). " Streaming pchain was executed
  IF  ls_chainlog_a IS NOT INITIAL.
    IF ( ( ls_chainlog_a-datum  <  ls_chainlog_s-datum ) OR
         ( ls_chainlog_a-datum  =  ls_chainlog_s-datum ) AND ( ls_chainlog_a-zeit  <  ls_chainlog_s-zeit )
      ). " After chain was executed earlier then streaming last execution
      WRITE: / |Starting { lv_apc } because of data updated |.
      CALL FUNCTION 'RSPC_CHAIN_START'
        EXPORTING
          i_chain = lv_apc
*         I_TRIGGER           =
*         I_T_VARIABLES       =
*         I_SYNCHRONOUS       =
*         I_SIMULATE          =
*         I_NOPLAN            =
*         I_DONT_WAIT         =
*         I_POLL  =
*         I_GUI   =
        IMPORTING
          e_logid = lv_logid_a.
    ENDIF.
    WRITE: / |logID: { lv_logid_a }|.
  ELSE.
    WRITE: / |Starting { lv_apc } because of no previous run |.
    CALL FUNCTION 'RSPC_CHAIN_START'
      EXPORTING
        i_chain = lv_apc
*       I_TRIGGER           =
*       I_T_VARIABLES       =
*       I_SYNCHRONOUS       =
*       I_SIMULATE          =
*       I_NOPLAN            =
*       I_DONT_WAIT         =
*       I_POLL  =
*       I_GUI   =
      IMPORTING
        e_logid = lv_logid_a.
    WRITE: / |logID: { lv_logid_a }|.
  ENDIF.
ENDIF.