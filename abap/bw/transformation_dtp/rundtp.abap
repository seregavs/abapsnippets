REPORT zrundtp.

"
PARAMETER: p_dtp TYPE rsbkdtpnm OBLIGATORY DEFAULT 'DTP_00O2TPC0G0T5KUE5DC3C0F1KC'.

DATA: r_dtp TYPE REF TO cl_rsbk_dtp.

CALL METHOD cl_rsbk_dtp=>factory
  EXPORTING
    i_dtp   = p_dtp
  RECEIVING
    r_r_dtp = r_dtp.

DATA l_r_request TYPE REF TO cl_rsbk_request.
l_r_request = r_dtp->create_request( ).
l_r_request->set_ctype( rsbc_c_ctype-sync ).
l_r_request->doit( ).