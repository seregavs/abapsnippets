*&---------------------------------------------------------------------*
*& Report ZAGEST_STOCK_FILL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zagest_stock_fill.

PARAMETERS:
  p_num_pl TYPE i DEFAULT 10,
  p_pl_low TYPE /bi0/oiplant DEFAULT '',
  p_pl_hig TYPE /bi0/oiplant DEFAULT '',
  p_date   TYPE /bi0/oicalday DEFAULT '',
  p_cl_ads TYPE c,
  p_cl_sto TYPE c,
  p_cl_age TYPE c.

TYPES:
      BEGIN OF t_plant,
        plant_from TYPE /bi0/oiplant,
        plant_to TYPE /bi0/oiplant,
      END OF t_plant,
      tt_plant TYPE STANDARD TABLE OF t_plant.

DATA:
  lo_conn      TYPE REF TO cl_sql_connection,
  lo_statement TYPE REF TO cl_sql_statement,
  lx_sql       TYPE REF TO cx_sql_exception,
  lt_res       TYPE REF TO data,
  l_date       TYPE /bi0/oicalday,
  l_sprov      TYPE /bi0/oiflag,
  l_num_plant  TYPE i,
  l_count_pl   TYPE i,
  lv_ts        TYPE timestamp,
  ls_plant     TYPE t_plant,
  lt_plant     TYPE tt_plant.

CONSTANTS:
  lv_dpsize TYPE p VALUE 1000000.

START-OF-SELECTION.

  CLEAR l_sprov.
  IMPORT cspagest = l_sprov date = l_date FROM DATABASE indx(bp) ID 'ST_AGE_V2'.

* если вводной параметр даты заполнен, то берём даты расчёта из него
  IF p_date <> ''.
    l_date = p_date.
  ENDIF.

  TRY.
      lo_conn = cl_sql_connection=>get_connection( ).
      lo_statement = lo_conn->create_statement( ).
    CATCH cx_sql_exception INTO lx_sql.
      WRITE: / lx_sql->get_text( ).
      WRITE: / lx_sql->sql_code.
      WRITE: / lx_sql->sql_message.
      RETURN.
  ENDTRY.

* если установлены индикаторы очистки таблиц, то удаляем данные из таблиц
  IF p_cl_sto = 'X'.
    TRY.
        DATA(l_row_cnt) = lo_statement->execute_update( |TRUNCATE TABLE ZCALCZSTOCKOD;| ).
      CATCH cx_sql_exception INTO lx_sql.
        WRITE: / |truncation of table ZCALCZSTOCKOD got error - { lx_sql->get_text( ) }|.
        WRITE: / |truncation of table ZCALCZSTOCKOD got error - { l_date } - { lx_sql->sql_code }|.
        WRITE: / lx_sql->sql_message.
        WRITE: / |Delete started from ABAP command..'|.
        DELETE FROM zcalczstockod.
        WRITE: / |Delete ended|.
    ENDTRY.
  ENDIF.

  IF p_cl_age = 'X'.
    TRY.
        l_row_cnt = lo_statement->execute_update( |TRUNCATE TABLE ZCALCAGESTO05;| ).
      CATCH cx_sql_exception INTO lx_sql.
        WRITE: / |truncation of table ZCALCAGESTO05 got error - { lx_sql->get_text( ) }|.
        WRITE: / |truncation of table ZCALCAGESTO05 got error - { l_date } - { lx_sql->sql_code }|.
        WRITE: / lx_sql->sql_message.
        WRITE: / |Delete started from ABAP command..'|.
        DELETE FROM zcalcagesto05.
        WRITE: / |Delete ended|.
    ENDTRY.
  ENDIF.

* заполнение интервала расчёта ТК в зависимости от параметров
  IF p_pl_hig = '' AND p_pl_low = ''.
    p_pl_hig = '9999'.
  ELSEIF p_pl_hig = '' AND p_pl_low <> ''.
    p_pl_hig = p_pl_low.
  ENDIF.

  IF p_pl_low = ''.
    p_pl_low = '0001'.
  ENDIF.

* выбор ТК (по интервалам с заданным количеством в параметра ТК) согласно BEx User-Exit CRPLANTCO и ограничениям, заданным в параметрах
  DATA(lv_stmt) =
 | select distinct c2.plant_from, c2.plant_to from ( | &
 | select c1.* | &
 | ,first_value(c1.plant ) OVER (PARTITION BY grp ORDER BY ind ASC) as plant_from | &
 | ,first_value(c1.plant ) OVER (PARTITION BY grp ORDER BY ind DESC) as plant_to | &
 | from ( | &
 | SELECT  plant | &
 | , ( row_number() over (order by plant ASC) )-1 as rn | &
 | , round(ndiv0( ( row_number() over (order by plant ASC) )-1, { p_num_pl }),0, ROUND_DOWN) as grp | &
 | , mod( ( row_number() over (order by plant ASC) )-1, { p_num_pl }) as ind | &
 | FROM    "/BI0/PPLANT" | &
 | WHERE   objvers  = 'A' | &
 |   AND   plantcat  = 'A' | &
 |   AND   "/BIC/ZPLNTPROF" <> 'Z002' | &
 |   AND   gln <> '0000000000000' | &
 |   AND   plant  >= '{ p_pl_low }' | &
 |   AND   plant  <= '{ p_pl_hig }' | &
 |   AND   plant  LIKE '____') as c1 ) as c2 | &
 | ORDER BY c2.plant_from | .

 TRY.

        GET REFERENCE OF lt_plant INTO lt_res.
        DATA(l_res_ref) = lo_statement->execute_query( lv_stmt ).
        l_res_ref->set_param_table( lt_res ).
        l_res_ref->next_package( ).
        l_res_ref->close( ).
      CATCH cx_sql_exception INTO lx_sql.
        WRITE: / lx_sql->get_text( ).
        WRITE: / lx_sql->sql_code.
        WRITE: / lx_sql->sql_message.
        RETURN.
    ENDTRY.

* цикл с расчётом и записью данных в табл. по диапазону ТК согласно заданному в параметре количеству ТК для расчета
  LOOP AT lt_plant INTO ls_plant.

    GET TIME STAMP FIELD lv_ts.

    lv_stmt = |INSERT INTO "ZCALCAGESTO05" (| &
               |  ZREQTSN,ZDATAPAKID,ZRECORD,| &
               |  PLANT,MATERIAL,LAST_ISSUE,| &
               |  VOLSTOCK,VALSTOCK,BASE_UOM,LOC_CURRCY,| &
               |  ZSALES5,ZSALE_DIR,CTYPEMATR,| &
               |  ZSERVG,ZQ_DAY,| &
               |  VOLSTOCK1,VOLSTOCK2,VOLSTOCK3,| &
               |  VALSTOCK1,VALSTOCK2,VALSTOCK3,| &
               |  VOLSTOCK9,VALSTOCK9,VALSTOCKA, | &
               |  VOLSTOCKA,VALSTOCKB,VOLSTOCKB, | &
               |  RTSAEXCUSV,VENDOR) | &
               |(SELECT { lv_ts } as ZREQTSN, to_integer(row_number() over () / { lv_dpsize } + 1) as ZDATAPAKID, | &
               | mod (  row_number() over (), { lv_dpsize })+1 as ZRECORD, | &
               | IFNULL("0PLANT",'') as PLANT, | &
               | IFNULL("0MATERIAL",'') as MATERIAL,| &
               | IFNULL("0LAST_ISSUE",'') as LAST_ISSUE,| &
               | IFNULL("VOLSTOCK",(0)) as VOLSTOCK, | &
               | IFNULL("VALSTOCK",(0)) as VALSTOCK, | &
               | IFNULL("0BASE_UOM",'') as ZBASE_UOM,| &
               | IFNULL("0LOC_CURCY", '') as LOC_CURRCY,| &
               | IFNULL("ZALTCOST",(0)) as ZSALES, | &
               | IFNULL("0MATERIAL__ZSALE_DIR",'') as ZSALE_DIR,| &
               | IFNULL("0MAT_PLANT__CTYPEMATR",'') as CTYPEMATR,| &
               | IFNULL("0MAT_PLANT__ZSERVG",'') as ZSERVG,| &
               | IFNULL("CQDAY01",(0)) as ZQ_DAY, | &
               | IFNULL("VOLSTOCK1",(0)) as VOLSTOCK1, | &
               | IFNULL("VOLSTOCK2",(0)) as VOLSTOCK2, | &
               | IFNULL("VOLSTOCK3",(0)) as VOLSTOCK3, | &
               | IFNULL("VALSTOCK1",(0)) as VALSTOCK1, | &
               | IFNULL("VALSTOCK2",(0)) as VALSTOCK2, | &
               | IFNULL("VALSTOCK3",(0)) as VALSTOCK3, | &
               | IFNULL("VOLSTOCK9",(0)) as VOLSTOCK9, | &
               | IFNULL("VALSTOCK9",(0)) as VALSTOCK9, | &
               | IFNULL("VALSTOCKA",(0)) as VALSTOCKA, | &
               | IFNULL("VOLSTOCKA",(0)) as VOLSTOCKA, | &
               | IFNULL("VALSTOCKB",(0)) as VALSTOCKB, | &
               | IFNULL("VOLSTOCKB",(0)) as VOLSTOCKB, | &
               | IFNULL("RTSAEXCUSV",(0)) as RTSAEXCUSV, | &
               | IFNULL("0VENDOR", '') as VENDOR | &
               |FROM "_SYS_BIC"."lenta.bw.rt.stock/CALC_AGESTO05_CV01"('PLACEHOLDER' = ('$$I_STOCK_DATE$$', '{ l_date }')) | &
               |WHERE "0PLANT" >= '{ ls_plant-plant_from }' AND "0PLANT" <= '{ ls_plant-plant_to }');|.

    TRY.
        l_row_cnt = lo_statement->execute_update( lv_stmt ).
        DATA(lv_mess) = |date={ l_date }, plant=({ ls_plant-plant_from },{ ls_plant-plant_to }), rows={ l_row_cnt }|.
        WRITE: / lv_mess.
        MESSAGE lv_mess TYPE 'S'.
        COMMIT WORK.
      CATCH cx_sql_exception INTO lx_sql.
        WRITE: / |plant=({ ls_plant-plant_from },{ ls_plant-plant_to }), - { lx_sql->get_text( ) }|.
        WRITE: / |date={ l_date } - { lx_sql->sql_code }|.
        WRITE: / lx_sql->sql_message.
        RETURN.
    ENDTRY.
* если установлен индикатор очистки ADSO zstockod, то удаляем данные из zstockod
    IF p_cl_ads = 'X'.
      DELETE FROM /bic/azstockod2 WHERE plant BETWEEN ls_plant-plant_from AND ls_plant-plant_to.
      COMMIT WORK AND WAIT.
    ENDIF.
    IF l_sprov IS INITIAL.
      lv_stmt = |INSERT INTO "ZCALCZSTOCKOD" (| &
                 |  ZREQTSN,ZDATAPAKID,ZRECORD,| &
                 |  PLANT,MATERIAL,CALWEEK,| &
                 |  BASE_UOM,LOC_CURRCY,MAT_PLANT,| &
                 |  CTYPEMATR,ZSERVG,ZOTR_IND,| &
                 |  VALSTOCK,VOLSTOCK,| &
                 |  VALSTOCK1,VALSTOCK2,VALSTOCK9,| &
                 |  COUNTER1,VALSTOCK3,VALSTOCKB) | &
                 |(SELECT { lv_ts } as ZREQTSN, to_integer(row_number() over () / { lv_dpsize } + 1) as ZDATAPAKID, | &
                 | mod (  row_number() over (), { lv_dpsize })+1 as ZRECORD, | &
                 | IFNULL("0PLANT",'') as PLANT, | &
                 | IFNULL("0MATERIAL",'') as MATERIAL,| &
                 | IFNULL("CALWEEK",'') as CALWEEK,| &
                 | IFNULL("0BASE_UOM", '') as BASE_UOM,| &
                 | IFNULL("0LOC_CURCY", '') as LOC_CURRCY,| &
                 | IFNULL("0MAT_PLANT",'') as MAT_PLANT,| &
                 | IFNULL("0MAT_PLANT__CTYPEMATR", '') as CTYPEMATR,| &
                 | IFNULL("0MAT_PLANT__ZSERVG", '') as ZSERVG,| &
                 | IFNULL("ZOTR_IND", '') as ZOTR_IND,| &
                 | IFNULL("ALTTSTOPV",(0)) as VALSTOCK, | &
                 | IFNULL("0CPTOTSTOBU",(0)) as VOLSTOCK, | &
                 | IFNULL("VALSTOCK1",(0)) as VALSTOCK1, | &
                 | IFNULL("VALSTOCK2",(0)) as VALSTOCK2, | &
                 | IFNULL("VALSTOCK9",(0)) as VALSTOCK9, | &
                 | IFNULL("COUNTER1",(0)) as COUNTER1, | &
                 | IFNULL("VALSTOCK3",(0)) as VALSTOCK3, | &
                 | IFNULL("VALSTOCKB",(0)) as VALSTOCKB | &
                 |FROM "_SYS_BIC"."lenta.bw.rt.stock/CALC_AGE_OF_STOCK_CV01"('PLACEHOLDER' = ('$$I_STOCK_DATE$$', '{ l_date }')) | &
                 |WHERE "0PLANT" >= '{ ls_plant-plant_from }' AND "0PLANT" <= '{ ls_plant-plant_to }');|.

      TRY.
          l_row_cnt = lo_statement->execute_update( lv_stmt ).
          lv_mess = |date={ l_date }, plant=({ ls_plant-plant_from },{ ls_plant-plant_to }), rows={ l_row_cnt }|.
          WRITE: / lv_mess.
          MESSAGE lv_mess TYPE 'S'.
          COMMIT WORK.
        CATCH cx_sql_exception INTO lx_sql.
          WRITE: / |plant=({ ls_plant-plant_from },{ ls_plant-plant_to }), - { lx_sql->get_text( ) }|.
          WRITE: / |date={ l_date } - { lx_sql->sql_code }|.
          WRITE: / lx_sql->sql_message.
          RETURN.
      ENDTRY.
    ENDIF.
  ENDLOOP.