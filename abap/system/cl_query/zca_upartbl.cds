@EndUserText.label : 'Настроечные таблицы'
@AbapCatalog.enhancement.category : #EXTENSIBLE_CHARACTER_NUMERIC
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #ALLOWED
define table zca_upartbl {
  key mandt   : mandt not null;
  key packnm  : zca_packnm not null;
  key begda   : begda not null;
  key tblname : zca_partblname not null;
  key rownr   : /aif/rownr not null;
  key colname : /osp/dt_cjs_context not null;
  value       : zca_value;

}