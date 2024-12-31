FUNCTION zbpc_bpf_email.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_EMP_USERID) TYPE  SY-UNAME
*"     REFERENCE(I_EMP_EMAILID) TYPE  RSSTRING
*"     REFERENCE(I_APPSET_ID) TYPE  RSBPC_APPSET_ID
*"     REFERENCE(I_INSTANCE_ID) TYPE  RSBPCB_ID
*"     REFERENCE(I_TMPL_ID) TYPE  RSBPCB_TMPL_ID
*"     REFERENCE(I_DRIVER_MEMBER) TYPE  STRING
*"     REFERENCE(I_CONTEXT_MEMBER) TYPE  STRING
*"     REFERENCE(I_STEP_REGION_ID) TYPE  RSBPCB_ID
*"     REFERENCE(I_STEP) TYPE  INTEGER
*"     REFERENCE(I_STEP_ID) TYPE  RSBPCB_ID
*"     REFERENCE(I_TMPL_VERSION) TYPE  RSBPCB_TMPL_VERSION
*"----------------------------------------------------------------------

*----------------------------------------------------------------------*
* REVISION HISTORY                                                     *
*----------------------------------------------------------------------*
* Date       : 30.05.2019                                              *
* Developer  : Sule Irmak                                               *
* Requester  : 27599 - Shaik Saleem                                    *
* Change Doc.:  CEO Dashboard BPC developments                                                          *
* Description: Email notifications sends in sequence defined in custom *
* developments                                                         *
*----------------------------------------------------------------------*

* *&Get the Email id and User id Whom you want to Send  ******

  TYPES:
    BEGIN OF ty_s_notif,
      url               TYPE rsstring,
      t_recipients      TYPE rsbpc_en_t_recipients,
      payload           TYPE xstring,
      step_region_id    TYPE rsbpcb_id,
      step_rgn_context  TYPE rsstring,
      step_rgn_fullname TYPE rsstring,
    END OF ty_s_notif,
    tl_t_notif TYPE STANDARD TABLE OF ty_s_notif WITH DEFAULT KEY.
  DATA:lt_receivers       TYPE STANDARD TABLE OF  somlreci1,
       ls_it_receivers    LIKE LINE OF lt_receivers,
       lt_packing_list    TYPE STANDARD TABLE OF  sopcklsti1,
       ls_gd_doc_data     TYPE sodocchgi1,
       ls_it_packing_list LIKE LINE OF  lt_packing_list,
       l_psubject(90)     TYPE c,
       lt_message         TYPE STANDARD TABLE OF solisti1,
       ls_it_message      LIKE LINE OF lt_message,
       l_text             TYPE string,
       l_num_lines        TYPE i,
       l_protocol         TYPE string,
       l_protocol_type    TYPE string,
       l_host             TYPE string,
       l_service          TYPE string,
       l_default_lang     TYPE sylangu,
       l_laiso            TYPE laiso,
       l_url_prefix       TYPE rsstring,
       l_link             TYPE rsstring.
  DATA:  l_s_notif TYPE ty_s_notif .
  DATA: lt_tzkpi TYPE TABLE OF /bic/tzkpi,
        ls_zkpi  TYPE  /bic/tzkpi.
  DATA ls_context_member TYPE string.
  DATA l_length TYPE i .
  DATA: lt_user_name TYPE TABLE OF usr21 .
  DATA: ls_user_name TYPE usr21 .
  DATA: lt_user_name_text TYPE TABLE OF adrp .
  DATA: ls_user_name_text TYPE adrp .

  SELECT * FROM /bic/tzkpi INTO TABLE lt_tzkpi  UP TO 1 ROWS
                                WHERE /bic/zkpi = i_driver_member
                                ORDER BY PRIMARY KEY.
  CLEAR ls_zkpi.
  READ TABLE lt_tzkpi INTO ls_zkpi WITH KEY /bic/zkpi = i_driver_member.

  l_length = strlen( i_context_member ) .
  ls_context_member = i_context_member+0(9) .
  IF l_length > 34 ." if halfyearly, quarterly or monthly KPI
    IF i_context_member+18(1) <> '/' ."halfyear kpi
      ls_context_member = ls_context_member && '/' && i_context_member+9(10) .
    ELSE.
      IF i_context_member+27(1) <> '/'."quarterly kpi
        ls_context_member = ls_context_member && i_context_member+18(10) .
      ELSEIF i_context_member+34(1) <> '/' . "monthly kpi
        ls_context_member = ls_context_member && i_context_member+27(9) .
      ENDIF .
    ENDIF.
  ENDIF.

  l_protocol = 'HTTP'.

  cl_http_server=>if_http_server~get_location(  EXPORTING protocol     = l_protocol
                                                 IMPORTING host         =  l_host
                                                          port         =  l_service
                                                          out_protocol =  l_protocol
                                                          ).
  IF l_host IS INITIAL.
    l_protocol = 'HTTPS'.
    cl_http_server=>if_http_server~get_location(  EXPORTING protocol     = l_protocol
                                                IMPORTING host         =  l_host
                                                         port         =  l_service
                                                         out_protocol =  l_protocol
                                                         ).
  ENDIF.

  l_url_prefix = |{ l_protocol }://{ l_host }:{ l_service }|.
  l_link = |/sap/epm/bpc/web/?email=true&app={ i_appset_id }&|
          && |instId={ i_instance_id }&actId={ i_step_region_id }&| && |connType=pak|.

  l_link = l_link && |&role={ 'R' }|.

  FIND FIRST OCCURRENCE OF '?' IN l_url_prefix.
  IF sy-subrc = 0.
    l_s_notif-url = l_url_prefix && escape( val = l_link format = cl_abap_format=>e_html_js_html ).
  ELSE.
    l_s_notif-url = l_url_prefix && l_link.
  ENDIF.

*Get activity description .

  DATA lt_stept TYPE TABLE OF rsbpcb_stept .
  DATA ls_stept TYPE rsbpcb_stept .

  CLEAR ls_stept .
  SELECT * FROM rsbpcb_stept INTO TABLE lt_stept WHERE appset_id = i_appset_id
                                                   AND tmpl_id = i_tmpl_id
                                                   AND tmpl_version = i_tmpl_version
                                                   AND step_id = i_step_id ORDER BY PRIMARY KEY.

  CLEAR ls_stept .
  READ TABLE lt_stept INTO ls_stept WITH KEY step_id = i_step_id .


*&-- Assign the Email id and User id to  Whom you want to Send  -------------&
  FREE ls_it_receivers.
  ls_it_receivers-receiver   = i_emp_emailid. "&---- Assign Email id
  ls_it_receivers-rec_type   = 'U'.                    "&---- Send to External Email id
  ls_it_receivers-com_type   = 'INT'.
  ls_it_receivers-notif_del  = 'X'.
  ls_it_receivers-notif_ndel = 'X'.
  APPEND ls_it_receivers TO lt_receivers .
  FREE ls_it_receivers.
  ls_it_receivers-receiver   = i_emp_userid.  "&----- Assign SAP User Id
  ls_it_receivers-rec_type   = 'B'.                    "&-- Send to SAP Inbox
  ls_it_receivers-com_type   = 'INT'.
  ls_it_receivers-notif_del  = 'X'.
  ls_it_receivers-notif_ndel = 'X'.
  APPEND ls_it_receivers TO lt_receivers .
*& - END of  Assign the Email id and User id to  Whom you want to Send  --&
  "&--- Read the Number of lines in the Internal Table
  DESCRIBE TABLE lt_receivers LINES l_num_lines.
  "&--- Check the Sender Email id or SAP User id is got or not.
  IF l_num_lines IS NOT INITIAL.
*&---------------------------------------------------------------------
* Add thetext to mail text table
*&----------------------------------------------------------------------
*&-- Subject of the mail -------------&*
    IF  i_step =  1 .
      l_psubject = 'Process: ' && i_tmpl_id && '-' && '[' && ls_stept-step_name && '.' && i_driver_member && ']' && ' - To Review' .
    ELSEIF  i_step =  2 .
      l_psubject = 'Process: ' && i_tmpl_id && '-' && '[' && ls_stept-step_name && '.' && i_driver_member && ']' && ' - To Approve' .
    ELSEIF i_step = 3 .
      l_psubject = 'Process: ' && i_tmpl_id && '-' && '[' && ls_stept-step_name && '.' && i_driver_member && ']' && ' - Rejected' .
    ELSEIF i_step = 4.
      l_psubject = 'Process: ' && i_tmpl_id && '-' && '[' && ls_stept-step_name && '.' && i_driver_member && ']' && ' - Approved' .
    ENDIF.
*&--  Body  of the mail ----------------&*
    CLEAR ls_it_message.
    ls_it_message-line = 'SAP Business Planning and Consolidation'.
    l_text = 'SAP Business Planning and Consolidation' .
*    CONCATENATE '<b>' l_text '</b>:' into ls_it_message-line .
*    c2 = I_EMP_USERID.
*    CONCATENATE c1 c2 ',' INTO
*    ls_it_message-line SEPARATED BY space.
    APPEND ls_it_message TO lt_message.
******* Assign your Text  below *************************************
    CLEAR ls_it_message.
    ls_it_message-line = 'Process: ' && i_tmpl_id .
    APPEND ls_it_message TO lt_message.
    CLEAR ls_it_message.
    ls_it_message-line = 'Context: ' && ls_context_member .
    APPEND ls_it_message TO lt_message.

*** insert Blank Line{} *********************************************
    CLEAR ls_it_message.
    ls_it_message-line = '                                        '.
    APPEND ls_it_message TO lt_message.
*** insert Blank Line{} *********************************************
    CLEAR ls_it_message.
    IF i_step = 1 . "1 =  reviewer , 2 = approver
      ls_it_message-line = 'Please review the following activity' .
    ELSEIF  i_step = 2 .
      ls_it_message-line = 'Please approve the following activity' .
    ELSEIF i_step = 3 OR i_step = 4 .

      SELECT * FROM usr21 INTO TABLE lt_user_name UP TO 1 ROWS WHERE bname = sy-uname ORDER BY PRIMARY KEY.

      CLEAR ls_user_name .
      READ TABLE lt_user_name INTO ls_user_name INDEX 1 .

      SELECT * FROM adrp INTO TABLE lt_user_name_text UP TO 1 ROWS WHERE persnumber = ls_user_name-persnumber ORDER BY PRIMARY KEY.

      CLEAR ls_user_name_text.
      READ TABLE lt_user_name_text INTO ls_user_name_text WITH KEY  persnumber = ls_user_name-persnumber .

      IF i_step = 3.
        CONCATENATE 'The following activity has been rejected by - ' sy-uname '-' ls_user_name_text-name_text INTO ls_it_message-line  .
      ELSE.
        CONCATENATE 'The following activity has been approved by - ' sy-uname '-' ls_user_name_text-name_text INTO ls_it_message-line  .
      ENDIF.

    ENDIF.
    APPEND ls_it_message TO lt_message.

    CLEAR ls_it_message.
    ls_it_message-line = 'Activity: ' && '[' && ls_stept-step_name && '.' && i_driver_member && ']' .
    APPEND ls_it_message TO lt_message.

    CLEAR ls_it_message.
    ls_it_message-line = 'Context: ' && i_driver_member && ':' && ls_zkpi-txtlg.
    APPEND ls_it_message TO lt_message.

    CLEAR ls_it_message.
    CONCATENATE  '<a href="' l_s_notif-url'">''Open Activity''</a>' INTO ls_it_message-line .
    APPEND ls_it_message TO lt_message.
*** insert Blank Line{} *********************************************
    CLEAR ls_it_message.
    ls_it_message-line = '                                        '.
    APPEND ls_it_message TO lt_message.
**********************************************************************
    CLEAR ls_it_message.
    ls_it_message-line = 'If the above link does not work, copy and paste the following:' .
    APPEND ls_it_message TO lt_message.

    CLEAR ls_it_message.
    ls_it_message-line =  l_s_notif-url .
    APPEND ls_it_message TO lt_message.

    CLEAR ls_it_message.
    ls_it_message-line = 'This mail generate automatically. Please do not reply.'(003).
    APPEND ls_it_message TO lt_message.
*********************************************************************
**********& Send EMAIL MESSAGE  &*********************************
    ls_gd_doc_data-doc_size = 1.
*Populate the subject/generic message attributes
    ls_gd_doc_data-obj_langu = sy-langu.
    ls_gd_doc_data-obj_name = 'SAPRPT'.
    ls_gd_doc_data-obj_descr = l_psubject.
    ls_gd_doc_data-sensitivty = 'F'.
*Describe the body of the message
    CLEAR ls_it_packing_list.
    REFRESH lt_packing_list.
    ls_it_packing_list-transf_bin = space.
    ls_it_packing_list-head_start = 1.
    ls_it_packing_list-head_num = 0.
    ls_it_packing_list-body_start = 1.
    DESCRIBE TABLE lt_message LINES ls_it_packing_list-body_num.
    ls_it_packing_list-doc_type = 'RAW'.
    APPEND ls_it_packing_list TO lt_packing_list.
*&------ Call the Function Module to send the message to External and SAP Inbox
    CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
      EXPORTING
        document_data              = ls_gd_doc_data
        put_in_outbox              = 'X'
        commit_work                = 'X'
      TABLES
        packing_list               = lt_packing_list
        contents_txt               = lt_message
        receivers                  = lt_receivers
      EXCEPTIONS
        too_many_receivers         = 1
        document_not_sent          = 2
        document_type_not_exist    = 3
        operation_no_authorization = 4
        parameter_error            = 5
        x_error                    = 6
        enqueue_error              = 7
        OTHERS                     = 8.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF. "&---- END of Check the Sender Email id or SAP User id is got or not.

ENDFUNCTION.