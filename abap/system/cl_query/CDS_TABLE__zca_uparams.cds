@EndUserText.label : 'Настроечные параметры'
@AbapCatalog.enhancement.category : #EXTENSIBLE_CHARACTER_NUMERIC
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #ALLOWED
define table zca_uparams {
  @AbapCatalog.foreignKey.screenCheck : true
  key mandt  : mandt not null
    with foreign key t000
      where mandt = zca_uparams.mandt;
  key packnm : zca_packnm not null;
  key begda  : begda not null;
  key parnm  : zca_parnm not null;
  posnum     : zca_posnum;
  descr      : zca_descr;
  partype    : zca_partype;
  pardtype   : zca_pardtype
    with value help dd_types
      where typename = zca_uparams.pardtype;
  chusr      : chuser;
  udat       : datum;

}