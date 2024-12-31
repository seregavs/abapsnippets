REPORT zbw_coom_retraction  LINE-SIZE 1000..
TABLES: /bi0/scalyear, /bi0/scalmonth, /bi0/scostcenter, /bi0/scostelmnt, /bi0/splant, tvarvc.
INCLUDE <icon>.

SELECT-OPTIONS:
  s_year FOR /bi0/scalyear-calyear,
  s_per FOR /bi0/scalmonth-calmonth,
  s_kostl FOR /bi0/scostcenter-costcenter,
  s_kstar FOR /bi0/scostelmnt-costelmnt,
  s_plant FOR /bi0/splant-plant.

PARAMETERS: p_dest   TYPE c LENGTH 10 OBLIGATORY,
            p_error  AS CHECKBOX DEFAULT 'X',
            p_jobs   TYPE i OBLIGATORY DEFAULT 10,
            p_retcnt TYPE i OBLIGATORY DEFAULT 10.

INITIALIZATION.
  CASE sy-sysid.
    WHEN 'BWD'.
      p_dest = 'LRDCLNT100'.
    WHEN 'BWQ'.
      p_dest = 'LRQCLNT500'.
    WHEN 'BWP'.
      p_dest = 'LRPCLNT500'.
  ENDCASE.

** далее код из ппд ZHPKFC02 -> TFPCO_C31 Expansion
  TYPES: BEGIN OF lty_expansion_plant,
           plant TYPE /bi0/oiplant,
         END OF lty_expansion_plant.

  DATA:
    lt_plant TYPE TABLE OF lty_expansion_plant,
    lv_plant LIKE tvarvc-low.

* считать из тварва ком регионы, по которым ТК которые можем грузить

  IMPORT tab = lt_plant FROM DATABASE indx(bp) ID 'ZPLANT_EXPANSION'.

  LOOP AT lt_plant INTO DATA(ls_plant).
    s_plant = VALUE #(                   sign = 'I'
                                         option = 'EQ'
                                         low = ls_plant-plant
                                         high = '').
    APPEND s_plant.
  ENDLOOP.


** Аналог реализации OLAP-переменной CPLEXPALYEARSM
  SELECT *
  FROM   tvarvc
  WHERE  name = 'CPLEXPALYEARSM'.

    IF tvarvc-sign IS INITIAL.
      tvarvc-sign = 'I'.
    ENDIF.
    IF tvarvc-opti IS INITIAL.
      tvarvc-opti = 'EQ'.
    ENDIF.
    s_year = VALUE #( sign = tvarvc-sign
                      option = tvarvc-opti
                      low = tvarvc-low
                      high = tvarvc-high ).
    APPEND s_year.
  ENDSELECT.


START-OF-SELECTION.

  DATA: gt_outtab  TYPE TABLE OF zbw_kp06_in_s,
        gs_outtab  TYPE zbw_kp06_in_s,
        gt_messtab TYPE TABLE OF zbw_kp06_out_s,
        g_jobsret  TYPE i,
        g_iternum  TYPE i,
        g_ts1      TYPE timestampl,
        g_ts2      TYPE timestampl,
        g_cntbeg   TYPE i,
        l_date     TYPE dats,
        l_str1(15),
        l_str2(15).

  DATA: BEGIN OF gs_messaggr,
          mess TYPE zbw_kp06_out_s-mess,
          cnt  TYPE i,
        END OF gs_messaggr,
        gt_messaggr LIKE TABLE OF gs_messaggr.

  SELECT costcenter AS kostl,
         costelmnt AS kstar,
         fiscyear AS jahr,
         fiscper3 AS period,
         SUM( amount ) AS summ_curr
     FROM /bic/azibmo077
    INTO CORRESPONDING FIELDS OF TABLE @gt_outtab
    WHERE costcenter IN @s_kostl
    AND costcenter <> ''  AND costcenter <> '0000000000'
    AND costelmnt IN @s_kstar
    AND costelmnt <> '' AND costelmnt <> '0000000000'
    AND calyear IN @s_year
    AND calmonth IN @s_per
    AND plant IN @s_plant
    AND currency = 'RUB'
    GROUP BY costcenter, costelmnt, fiscyear, fiscper3
    ORDER BY costcenter, costelmnt, fiscyear, fiscper3.

  LOOP AT gt_outtab ASSIGNING FIELD-SYMBOL(<fs_line>).
    WRITE <fs_line>-summ_curr TO <fs_line>-summ LEFT-JUSTIFIED NO-GROUPING.
    CONDENSE <fs_line>-summ NO-GAPS.
  ENDLOOP.

  g_cntbeg = lines( gt_outtab ).
  GET TIME STAMP FIELD g_ts1.

  CONVERT TIME STAMP g_ts1 TIME ZONE sy-zonlo
        INTO DATE l_date
        TIME DATA(l_time).

  WRITE l_date TO l_str1.
  WRITE l_time TO l_str2.
  WRITE: / icon_information AS ICON.
  WRITE: |Выбрано { g_cntbeg } записей к проводке. Время запуска { l_str1 } { l_str2 }|.

  DO p_retcnt TIMES.
    g_iternum = sy-index.
    REFRESH gt_messtab.

    IF lines( gt_outtab ) < p_jobs.
      p_jobs = lines( gt_outtab ).
    ENDIF.

    ULINE.
    WRITE: / |Запуск итерации проводок № { g_iternum }, к проводке { lines( gt_outtab ) } записей, выполнение в { p_jobs } поток(ов/а).|.

    PERFORM taskProcessor TABLES gt_outtab gt_messtab USING p_jobs p_retcnt.
    IF lines( gt_messtab ) = 0.
      WRITE: / icon_green_light AS ICON.
      WRITE: |Итерация проводок прошла без ошибок|.
      EXIT.
    ELSE.
      REFRESH: gt_outtab, gt_messaggr.
      LOOP AT gt_messtab INTO DATA(ls_messtab).
        CLEAR: gs_outtab, gs_messaggr.
        MOVE-CORRESPONDING ls_messtab TO gs_outtab.
        MOVE-CORRESPONDING ls_messtab TO gs_messaggr.
        APPEND gs_outtab TO gt_outtab.
        gs_messaggr-cnt = 1.
        COLLECT gs_messaggr INTO gt_messaggr.
      ENDLOOP.
      SORT gt_outtab BY kostl kstar jahr period.
      DELETE ADJACENT DUPLICATES FROM gt_outtab COMPARING kostl kstar jahr period.

      IF g_iternum < p_retcnt.
        WRITE: / icon_yellow_light AS ICON.
      ELSE.
        WRITE: / icon_red_light AS ICON.
      ENDIF.
      WRITE: |В данной итерации проводок получено ошибок: { lines( gt_messtab ) } по { lines( gt_outtab ) } записям. Свод ошибок:|.
      SORT gt_messaggr BY cnt DESCENDING mess ASCENDING.
      LOOP AT gt_messaggr INTO gs_messaggr.
        WRITE: / |{ gs_messaggr-cnt } шт: { gs_messaggr-mess }|.
      ENDLOOP.
    ENDIF.
  ENDDO.

  GET TIME STAMP FIELD g_ts2.

  CONVERT TIME STAMP g_ts2 TIME ZONE sy-zonlo
        INTO DATE l_date
             TIME l_time.

  WRITE l_date TO l_str1.
  WRITE l_time TO l_str2.

  ULINE.
  WRITE: / icon_information AS ICON.
  WRITE: |Итерации проводок завершены, время завершения { l_str1 } { l_str2 }, скорость { g_cntbeg / ( g_ts2 - g_ts1 ) } записей в секунду|.

  IF lines( gt_messtab ) NE 0.
    ULINE.
  ENDIF.

  LOOP AT gt_messtab INTO ls_messtab.
    WRITE: / |МВЗ: { ls_messtab-kostl }, ВЗ: { ls_messtab-kstar }, ФГ: { ls_messtab-jahr }, Период: { ls_messtab-period }, Сумма: { ls_messtab-summ }, Ошибка: { ls_messtab-mess }|.
  ENDLOOP.

  SORT gt_messtab BY kostl kstar jahr period.
  DELETE ADJACENT DUPLICATES FROM gt_messtab COMPARING kostl kstar jahr period.

  ULINE.
  IF lines( gt_messtab ) = 0.
    WRITE: / icon_led_green AS ICON.
  ELSE.
    WRITE: / icon_led_red AS ICON.
  ENDIF.

  WRITE: |Проведено { g_cntbeg - lines( gt_messtab ) } объектов из { g_cntbeg } (ошибочных соответственно { lines( gt_messtab ) })|.

  IF lines( gt_messtab ) NE 0 AND p_error = 'X'.
    MESSAGE 'Присутствуют ошибки проводки позиций. См. спул.' TYPE 'E'.
  ENDIF.


FORM taskProcessor TABLES
                         fullTab
                         errTab
                   USING jobsNum TYPE i
                         retCount TYPE i.

  DATA: batchLines    TYPE i,
        jobID         TYPE i,
        lt_jobtab     TYPE TABLE OF zbw_kp06_in_s,
        l_jobsSend    TYPE i,
        l_mess(80),
        l_jobname(32).

  batchLines = lines( fullTab ) / jobsNum.
  jobID = 1.
  g_jobsret = 0.
  l_jobsSend = 0.

  WHILE jobID <= jobsNum.
    REFRESH lt_jobtab.
    IF jobID = jobsNum.
      LOOP AT fullTab FROM batchLines * ( jobID - 1 ) + 1 TO lines( fullTab ).
        APPEND fullTab TO lt_jobtab.
      ENDLOOP.
    ELSE.
      LOOP AT fullTab FROM batchLines * ( jobID - 1 ) + 1 TO batchLines * jobID.
        APPEND fullTab TO lt_jobtab.
      ENDLOOP.
    ENDIF.

    IF lines( lt_jobtab ) > 0.
      l_jobname = jobID.
      CALL FUNCTION 'ZBW_RFC_POST_COOM'
        DESTINATION p_dest
        STARTING NEW TASK l_jobname
        PERFORMING rfcCallback ON END OF TASK
        EXPORTING
          it_data               = lt_jobtab
        EXCEPTIONS
          system_failure        = 1 MESSAGE l_mess
          communication_failure = 2 MESSAGE l_mess
          resource_failure      = 3.
*    IMPORTING
*      et_mess = lt_messtab.
      IF sy-subrc NE 0.
        MESSAGE l_mess TYPE 'E'.
      ELSE.
        l_jobsSend = l_jobsSend + 1.
      ENDIF.
    ENDIF.
    jobID = jobID + 1.
  ENDWHILE.

  WAIT UNTIL g_jobsret >= l_jobsSend.

ENDFORM.


FORM rfcCallback USING taskname.
  DATA: lt_messtab TYPE TABLE OF zbw_kp06_out_s,
        l_mess(80).

  REFRESH lt_messtab.

  RECEIVE RESULTS FROM FUNCTION 'ZBW_RFC_POST_COOM'
  IMPORTING et_mess = lt_messtab
  EXCEPTIONS
          system_failure = 1 MESSAGE l_mess
          communication_failure = 2 MESSAGE l_mess.
  IF sy-subrc NE 0.
    MESSAGE l_mess TYPE 'E'.
  ELSE.
    APPEND LINES OF lt_messtab TO gt_messtab.
    g_jobsret = g_jobsret + 1.
  ENDIF.

ENDFORM.