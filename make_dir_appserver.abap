FUNCTION ZAPPS_DIR_CREATE.
*"----------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  IMPORTING
*"     VALUE(DIRNAME)
*"  EXCEPTIONS
*"      ALREADY_EXISTS
*"      CANT_CREATE
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

*  FIND REGEX '[^A-Z a-z 0-9 _ \, \^ % $ # @ ! \~ \{ \} \[ \] \; \( \) \- \`]' IN DIRNAME MATCH COUNT mcnt.
  FIND REGEX '[^A-Z a-z 0-9 / \- \w]' IN DIRNAME MATCH COUNT mcnt.
  IF mcnt <> 0.
    message 'Invalid characters.' type 'E' .
  ENDIF.

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

  IF SY-SUBRC EQ 0.
    RAISE ALREADY_EXISTS.
  ENDIF.

  COMMAND1(9)    = 'mkdir -p '.
  COMMAND1+9(55) = DIRNAME.

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

  IF SY-SUBRC NE 0.
    RAISE CANT_CREATE.
  ENDIF.

ENDFUNCTION.