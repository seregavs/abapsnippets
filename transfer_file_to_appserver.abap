*&---------------------------------------------------------------------*
*& Report Z_TRANSFER_FILE_TO_APPSERV
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_transfer_file_to_appserv.


PARAMETER: fnam_pc(128) TYPE c LOWER CASE,
           fnam_ap(128) TYPE c LOWER CASE,
           binary TYPE c AS CHECKBOX.

CONSTANTS: c_width TYPE i VALUE 1000.

DATA: l_t_data(c_width) TYPE c OCCURS 0 WITH HEADER LINE.
DATA: l_filename TYPE  string.
DATA: l_t_bin(c_width) TYPE x OCCURS 0 WITH HEADER LINE,
      filelength TYPE i,
      len TYPE i,
      dat TYPE REF TO data.

FIELD-SYMBOLS: <dat> TYPE any.

l_filename = fnam_pc.

IF binary = ' '.

CALL FUNCTION 'GUI_UPLOAD'
     EXPORTING
          filename = l_filename
          filetype = 'ASC'
          has_field_separator = ''
     TABLES
          data_tab = l_t_data.

*call function 'WS_UPLOAD'
*     exporting
*          filename            = fnam_pc
*          filetype            = 'ASC'
*     tables
*          data_tab            = l_t_data
*     exceptions
*          conversion_error    = 1
*          file_open_error     = 2
*          file_read_error     = 3
*          invalid_table_width = 4
*          invalid_type        = 5
*          no_batch            = 6
*          unknown_error       = 7
*          others              = 8.

  IF fnam_ap CS '.\' OR
     fnam_ap CS './'.
    MESSAGE i051(rsar) WITH 'Filename Invalid' './ and ' '\. is not allowed'.
    EXIT.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'S_DATASET'
           ID 'PROGRAM' FIELD sy-cprog
           ID 'ACTVT' FIELD '34'
           ID 'FILENAME' FIELD fnam_ap.
  IF sy-subrc <> 0.
    MESSAGE i051(rsar) WITH 'No Authority'.
    EXIT.
  ENDIF.

* #CP-SUPPRESS: FP NOT A BUG ALL IS FINE HERE
OPEN DATASET fnam_ap FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
*default oder utf8
LOOP AT l_t_data.
* #CP-SUPPRESS: FP NOT A BUG ALL IS FINE HERE
  TRANSFER l_t_data TO fnam_ap.
ENDLOOP.
CLOSE DATASET fnam_ap.

ELSE.

*call function 'WS_UPLOAD'
*     exporting
*          filename            = fnam_pc
*          filetype            = 'BIN'
*     importing
*          filelength          = filelength
*     tables
*          data_tab            = l_t_bin
*     exceptions
*          conversion_error    = 1
*          file_open_error     = 2
*          file_read_error     = 3
*          invalid_table_width = 4
*          invalid_type        = 5
*          no_batch            = 6
*          unknown_error       = 7
*          others              = 8.
CALL FUNCTION 'GUI_UPLOAD'
     EXPORTING
          filename            = l_filename
          filetype            = 'BIN'
     IMPORTING
          filelength          = filelength
     TABLES
          data_tab            = l_t_bin
     EXCEPTIONS
          conversion_error    = 1
          file_open_error     = 2
          file_read_error     = 3
          invalid_table_width = 4
          invalid_type        = 5
          no_batch            = 6
          unknown_error       = 7
          OTHERS              = 8.

len = filelength MOD c_width.
IF len > 0.
  CREATE DATA dat TYPE x LENGTH len.
  ASSIGN dat->* TO <dat>.
ENDIF.

IF fnam_ap CS '.\' OR
   fnam_ap CS './'.
  MESSAGE i051(rsar) WITH 'Filename Invalid' './ and '
                          '\. is not allowed'.
  EXIT.
ENDIF.

AUTHORITY-CHECK OBJECT 'S_DATASET'
         ID 'PROGRAM' FIELD sy-cprog
         ID 'ACTVT' FIELD '34'
         ID 'FILENAME' FIELD fnam_ap.
IF sy-subrc <> 0.
  MESSAGE i051(rsar) WITH 'No Authority'.
  EXIT.
ENDIF.

*#CP-Supress: FP NOT A BUG ALL IS FINE HERE
OPEN DATASET fnam_ap FOR OUTPUT IN BINARY MODE.
*default oder utf8
LOOP AT l_t_bin.
  filelength = filelength - c_width.
  IF filelength < 0.
    <dat> = l_t_bin.
*#CP-Supress: FP NOT A BUG ALL IS FINE HERE
    TRANSFER <dat> TO fnam_ap.
  ELSE.
*#CP-Supress: FP NOT A BUG ALL IS FINE HERE
    TRANSFER l_t_bin TO fnam_ap.
  ENDIF.
ENDLOOP.
CLOSE DATASET fnam_ap.

ENDIF.