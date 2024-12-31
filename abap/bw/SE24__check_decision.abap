  method z_check_decision.

    data: lv_sec type i,
          lv_ets type ccupeaka-timestamp,
          lv_now type ccupeaka-timestamp.

    get time stamp field data(lv_ts).
    lv_now = lv_ts.
    e_final_value = '1'. "1 means we don't need to activate ADSO or DSE-model
    try.
        case i_chain_name.
          when 'DOECOPA_C_D_LSA'. " for process chain DOESD_SORDERS: check if invoices are loaded
            " get last time of loading for ADSO invoices.
            select max( last_time_stamp ) into @data(lv_last_time_stamp2) from rspmrequest
              where datatarget = 'DCOAAD058' and tlogo = 'ADSO' and request_status = 'GG' and storage = 'AT'.
            if lv_last_time_stamp2 is not initial.
              lv_ets = lv_last_time_stamp2+0(14).
              data(lv_time_lag_sec) = 7200.
              call function 'CCU_TIMESTAMP_DIFFERENCE'
                exporting
                  timestamp1 = lv_now
                  timestamp2 = lv_ets
                importing
                  difference = lv_sec.
              if lv_sec le lv_time_lag_sec. " If activated less than 2 hrs, then let activate this one (code  = 0)
                e_final_value = '0'.
              endif.
              write: / |{ i_chain_name } final_value={ e_final_value }; DCOAAD058 = { lv_ets }; diff = { lv_sec };|.
            endif.
* COMMENTED 2.12.21
*            select max( last_time_stamp ) into @data(lv_last_time_stamp2) from rspmrequest
*              where datatarget = 'DSDAAD055' and tlogo = 'ADSO' and request_status = 'GG' and storage = 'AT'.
*            if lv_last_time_stamp2 is not initial.
*              lv_now = lv_last_time_stamp2+0(14).
*            endif.
*
*            select max( last_time_stamp ) into @data(lv_last_time_stamp) from rspmrequest
*              where datatarget = 'DCOCAD01'" DCOAAD033'
*                and tlogo = 'ADSO'
*                and request_status = 'GG'
*                and storage = 'AQ'
*                and last_operation_type = 'C'.
*            if lv_last_time_stamp is not initial.
*              lv_ets = lv_last_time_stamp+0(14). "20210928111443000091000
*              data(lv_time_lag_sec) = 28800. " 7200 - original value. Have to increase upto 8 hrs
*              call function 'CCU_TIMESTAMP_DIFFERENCE'
*                exporting
*                  timestamp1 = lv_now
*                  timestamp2 = lv_ets
*                importing
*                  difference = lv_sec.
*
*              if lv_sec le lv_time_lag_sec.
*                e_final_value = '0'.
*              endif.
*              write: / |{ i_chain_name } final_value={ e_final_value }; DSDAAD055 = { lv_now }; DCOCAD01 = { lv_ets }; diff = { lv_sec };|.
*            endif.
          when 'DOESD_SORDERS'. " for process chain DOECOPA_DAILY_EST_DAILY2: check if orders are loaded
            " get last time of loading for ADSO orders.
            select max( last_time_stamp ) into @lv_last_time_stamp2 from rspmrequest
              where datatarget = 'DSDAAD055' and tlogo = 'ADSO' and request_status = 'GG' and storage = 'AT'.
            if lv_last_time_stamp2 is not initial.
              lv_ets = lv_last_time_stamp2+0(14).
              lv_time_lag_sec = 7200.
              call function 'CCU_TIMESTAMP_DIFFERENCE'
                exporting
                  timestamp1 = lv_now
                  timestamp2 = lv_ets
                importing
                  difference = lv_sec.
              if lv_sec le lv_time_lag_sec. " If activated less than 2 hrs, then let activate this one (code  = 0)
                e_final_value = '0'.
              endif.
              write: / |{ i_chain_name } final_value={ e_final_value }; DSDAAD055 = { lv_ets }; diff = { lv_sec };|.
            endif.
* COMMENTED 2.12.21
*            select max( last_time_stamp ) into lv_last_time_stamp2 from rspmrequest
*              where datatarget = 'DCOCAD01'
*                and tlogo = 'ADSO'
*                and request_status = 'GG'
*                and storage = 'AQ'
*                and last_operation_type = 'C'.
*            if lv_last_time_stamp2 is not initial.
*              lv_now = lv_last_time_stamp2+0(14).
*            endif.
*            " get last time of loading for ADSO for orders
*            select max( last_time_stamp ) into @lv_last_time_stamp from rspmrequest
*              where datatarget = 'DSDAAD055' and tlogo = 'ADSO' and request_status = 'GG' and storage = 'AT'.
*            lv_ets = lv_last_time_stamp+0(14). "20210924070105000226000
*            lv_time_lag_sec = 7200.
*
*            call function 'CCU_TIMESTAMP_DIFFERENCE'
*              exporting
*                timestamp1 = lv_now
*                timestamp2 = lv_ets
*              importing
*                difference = lv_sec.
*
*            if lv_sec le lv_time_lag_sec.
*              e_final_value = '0'.
*              write: / |{ i_chain_name } final_value1={ e_final_value }; DCOCAD01 = { lv_now }; DSDAAD055 = { lv_ets }; diff = { lv_sec };|.
*            else. " check if process chain DOECOPA_MEC started/finished less than 2hrs ago
*              write: / |{ i_chain_name } final_value1=1; DCOCAD01 = { lv_now }; DSDAAD055 = { lv_ets }; diff = { lv_sec };|.
*              select datum, zeit from rspclogchain into @data(wa) "'DOECOPA_MEC'
*               where chain_id = 'DOECOPA_MEC' and draw_type = 'TRIGGER' and analyzed_status in ('G','F')
*                order by datum descending, zeit descending.
*                data(lv_ts2) = |{ wa-datum }{ wa-zeit }|.
*                exit.
*              endselect.
*              if lv_ts2 is not initial.
*                lv_ets = lv_ts2. " type conversion required
*                call function 'CCU_TIMESTAMP_DIFFERENCE'
*                  exporting
*                    timestamp1 = lv_now
*                    timestamp2 = lv_ets
*                  importing
*                    difference = lv_sec.
*                if lv_sec le lv_time_lag_sec. " all times are in UTC, incl. lt_now  + 7200. " +7200 because logs are stored in UTC. lv_now has time in CET (UTC+2)
*                  e_final_value = '0'.
*                endif.
*                write: / |{ i_chain_name } final_value2={ e_final_value }; DCOCAD01 = { lv_now }; DOECOPA_MEC = { lv_ets }; diff = { lv_sec };|.
*              endif.
*            endif.

          when 'DOEDSE'.
            " DOEDSE
            select analyzed_status, chain_id from rspclogchain into @data(wb)
             where chain_id = 'DOEDSE'
             order by datum descending, zeit descending.
              data(lv_astatus) = wb-analyzed_status.
              exit.
            endselect.
            if sy-subrc = 0.
              if lv_astatus = 'G'. " previous run is completed (Green), then allow to execute this time
                e_final_value = '0'.
              endif.
            endif.
            write: / |{ i_chain_name } DOEDSE final_value={ e_final_value };|.
            " DOEDSEP
            select analyzed_status, chain_id from rspclogchain into @wb
             where chain_id = 'DOEDSEP'
             order by datum descending, zeit descending.
              lv_astatus = wb-analyzed_status.
              exit.
            endselect.
            if sy-subrc = 0.
              if lv_astatus = 'G'. " previous run is completed (Green), then allow to execute this time
                e_final_value = '0'.
              endif.
            endif.
            write: / |{ i_chain_name } DOEDSEP final_value={ e_final_value };|.
            " DOESD_SORDERS
            select analyzed_status, chain_id from rspclogchain into @wb
             where chain_id = 'DOESD_SORDERS'
             order by datum descending, zeit descending.
              lv_astatus = wb-analyzed_status.
              exit.
            endselect.
            if sy-subrc = 0.
              if lv_astatus = 'G'. " previous run is completed (Green), then allow to execute this time
                e_final_value = '0'.
              endif.
            endif.
            write: / |{ i_chain_name } DOESD_SORDERS final_value={ e_final_value };|.
            if e_final_value = 1.
              write: / |Please execute chain DOEDSE later when all chains above will be completed successfully.|.
            endif.

                    when 'Z_DIPDAD001_TRIG'.
            " Z_DIPDAD001_TRIG
            select analyzed_status, chain_id from rspclogchain into @data(wc)
             where chain_id = 'Z_DIPDAD001_TRIG'
             order by datum descending, zeit descending.
              data(lv_cstatus) = wc-analyzed_status.
              exit.
            endselect.
            if sy-subrc = 0.
              if lv_cstatus = 'G'. " previous run is completed (Green), then allow to execute this time
                e_final_value = '0'.
              endif.
            endif.
            write: / |{ i_chain_name } Z_DIPDAD001_TRIG final_value={ e_final_value };|.


          when others.
            e_final_value = '1'.
        endcase.
      catch cx_root.
    endtry.
  endmethod.