  data: i type integer.
  while i = 0.
    i = 0.
  endwhile.
  sort c_t_requests_for_del by request descending.

  loop at c_t_requests_for_del assigning <l_request> .
    " Check whether <L_REQUEST>-REQUEST shall be deleted from the
    "      inbound table of DataStore object (advanced) I_DTP-TGTADSO
    "      or not.
    " If it shall be deleted,
    "      set <L_REQUEST>-WILL_BE_DELETED to 'X' (rs_c_true).
    " If it shall not be deleted,
    "      set <L_REQUEST>-WILL_BE_DELETED to ' ' (rs_c_false).
    if sy-tabix > 1.
      <l_request>-will_be_deleted = 'X'.
    endif.
  endloop.

ZADDBVH02