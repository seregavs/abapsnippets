FUNCTION ZAPPS_DIR_DELETE.
*"----------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  IMPORTING
*"     VALUE(DIRNAME)
*"  EXCEPTIONS
*"      NOT_FOUND
*"      CANT_DELETE
*"----------------------------------------------------------------------

  DATA: COMMAND1(70),
        SRVNAME(20),
        BEGIN OF TABL OCCURS 0,
          LINE(200),
        END OF TABL.
  DATA: BEGIN OF FILE_LIST OCCURS 0.
          INCLUDE STRUCTURE MSXXLIST.
  DATA: END OF FILE_LIST.
  DATA: mcnt TYPE i.                                              " note 1673713

  CLEAR FILE_LIST.
  SRVNAME = SPACE.

  CALL FUNCTION 'RZL_READ_DIR'
    EXPORTING
      NAME           = DIRNAME
      SRVNAME        = SRVNAME
    TABLES
      FILE_TBL       = FILE_LIST
    EXCEPTIONS
      NOT_FOUND      = 1
      ARGUMENT_ERROR = 2
      SEND_ERROR     = 3.

  IF SY-SUBRC <> 0.
    RAISE NOT_FOUND.
  ENDIF.

  COMMAND1(9)    = 'rmdir -p'.
  CONCATENATE COMMAND1 DIRNAME INTO COMMAND1 SEPARATED BY space.
*  COMMAND1+9(55) = DIRNAME.

  CALL 'SYSTEM' ID 'COMMAND' FIELD COMMAND1
                ID 'TAB'     FIELD TABL-*SYS*.

  CLEAR FILE_LIST.
  SRVNAME = SPACE.

  CALL FUNCTION 'RZL_READ_DIR'
    EXPORTING
      NAME           = DIRNAME
      SRVNAME        = SRVNAME
    TABLES
      FILE_TBL       = FILE_LIST
    EXCEPTIONS
      NOT_FOUND      = 1
      ARGUMENT_ERROR = 2
      SEND_ERROR     = 3.

  IF SY-SUBRC = 0.
    RAISE CANT_DELETE.
  ENDIF.

ENDFUNCTION.