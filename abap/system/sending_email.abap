*&---------------------------------------------------------------------*
*& Report  ZEMAIL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zemail.

DATA l_mail_text_samp TYPE solisti1.

PARAMETERS:
  i_tplv  TYPE rsvar-variant,
  i_addr  TYPE string,
  i_name  TYPE so_obj_nam,
  i_title TYPE string LOWER CASE,
  i_text  TYPE string LOWER CASE,
  i_type  TYPE soodk-objtp DEFAULT 'RAW'.

SELECT-OPTIONS s_textxl  FOR l_mail_text_samp NO INTERVALS LOWER CASE.

IF i_type IS INITIAL.
  i_type = 'RAW'.
ENDIF.

IF i_tplv IS NOT INITIAL.
  TRANSLATE i_tplv TO UPPER CASE.

  SELECT SINGLE variant INTO i_tplv FROM varid
  WHERE report = 'ZEMAIL' AND variant = i_tplv.

  IF sy-subrc <> 0.
    WRITE: / `Вариант-шаблон не найден`.
    RETURN.
  ENDIF.

  FIELD-SYMBOLS:
     <paraml> TYPE rsparamsl.
  DATA:
    param   TYPE STANDARD TABLE OF rsparams,
    paraml  TYPE STANDARD TABLE OF rsparamsl,
    l_text  TYPE string,
    l_subst TYPE string.

  CALL FUNCTION 'RS_VARIANT_CONTENTS'
    EXPORTING
      report               = 'ZEMAIL'
      variant              = i_tplv
    TABLES
      valutab              = param[]
      valutabl             = paraml[]
    EXCEPTIONS
      variant_non_existent = 1
      variant_obsolete     = 2
      OTHERS               = 3.

  IF sy-subrc <> 0.
    WRITE: / `Вариант не найден`.
    RETURN.
  ENDIF.

  l_subst = i_text.

  LOOP AT paraml ASSIGNING <paraml>.
    CASE <paraml>-selname.
      WHEN 'I_ADDR'.
        CONCATENATE i_addr <paraml>-low INTO i_addr SEPARATED BY ';'.
      WHEN 'I_NAME'.
        IF i_name IS INITIAL.
          i_name = <paraml>-low.
        ENDIF.
      WHEN 'I_TITLE'.
        IF i_title IS INITIAL.
          i_title = <paraml>-low.
        ENDIF.
      WHEN 'I_TEXT'.
        l_text = <paraml>-low.
        REPLACE FIRST OCCURRENCE OF '$SUBSTITUTION$' IN l_text WITH l_subst.
        i_text = l_text.
      WHEN 'S_TEXTXL'.
        IF <paraml>-low IS NOT INITIAL.
          l_mail_text_samp = <paraml>-low.

          REPLACE FIRST OCCURRENCE OF '$SUBSTITUTION$' IN l_text WITH l_subst.
          APPEND VALUE #(
            sign = 'I'
            option = 'EQ'
            low = l_mail_text_samp
          ) TO s_textxl.
        ENDIF.
      WHEN 'I_TYPE'.
        IF i_type IS NOT INITIAL.
          i_type = <paraml>-low.
        ENDIF.
    ENDCASE.
  ENDLOOP.
ENDIF.

IF i_name IS INITIAL.
  WRITE: / `Параметр I_NAME не заполнен`.
  RETURN.
ELSEIF i_title IS INITIAL.
  WRITE: / 'Параметр I_TITLE не заполнен'.
  RETURN.
ELSEIF i_text IS INITIAL AND s_textxl[] IS INITIAL.
  WRITE: / `Параметр I_TEXT/S_TEXTXL не заполнен`.
  RETURN.
ENDIF.


DATA: ls_mailsubject     TYPE sodocchgi1.
DATA: lt_mailrecipients  TYPE STANDARD TABLE OF somlrec90 WITH HEADER LINE.
DATA: lt_mailtxt         TYPE STANDARD TABLE OF soli      WITH HEADER LINE.
DATA: lt_rcp TYPE STANDARD TABLE OF string.
FIELD-SYMBOLS: <l_recipient> TYPE string.

SPLIT i_addr AT ';' INTO TABLE lt_rcp.
LOOP AT lt_rcp ASSIGNING <l_recipient>.
  lt_mailrecipients-rec_type = 'U'.
  lt_mailrecipients-receiver = <l_recipient>.
  APPEND lt_mailrecipients.
ENDLOOP.

ls_mailsubject-obj_langu = 'R'.
ls_mailsubject-obj_name  = i_title.
ls_mailsubject-obj_descr = i_title.

IF i_text IS NOT INITIAL.
  lt_mailtxt = i_text.
  APPEND lt_mailtxt.
ENDIF.

LOOP AT s_textxl ASSIGNING FIELD-SYMBOL(<lr_text>).
  APPEND <lr_text>-low TO lt_mailtxt.
ENDLOOP.


CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
  EXPORTING
    document_data  = ls_mailsubject
    document_type  = i_type
  TABLES
    object_content = lt_mailtxt
    receivers      = lt_mailrecipients
  EXCEPTIONS
    OTHERS         = 8.

IF sy-subrc = 0.
  COMMIT WORK AND WAIT.
ENDIF.