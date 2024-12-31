@EndUserText.label : 'Значения настроечных параметров'
@AbapCatalog.enhancement.category : #EXTENSIBLE_CHARACTER_NUMERIC
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #ALLOWED
define table zca_uparval {
  key mandt  : mandt not null;
  key packnm : zca_packnm not null;
  key begda  : begda not null;
  key parnm  : zca_parnm not null;
  key num    : /aif/rownr not null;
  valsign    : tvarv_sign;
  valopt     : tvarv_opti;
  vallow     : zca_vallow;
  valhigh    : zca_valhigh;

}