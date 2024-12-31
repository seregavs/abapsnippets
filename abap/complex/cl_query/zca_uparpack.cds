@EndUserText.label : 'Настроечные пакеты'
@AbapCatalog.enhancement.category : #EXTENSIBLE_CHARACTER_NUMERIC
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #ALLOWED
define table zca_uparpack {
  key mandt  : mandt not null;
  key packnm : zca_packnm not null
    with value help zca_sh_uparpacknm
      where begda = zca_uparpack.begda
        and packnm = zca_uparpack.packnm;
  key begda  : begda not null;
  endda      : endda;
  descr      : zca_descr;
  author     : uname;
  cdat       : datum;
  fcode1     : zca_fcode;
  fcode2     : zca_fcode;
  fcode3     : zca_fcode;

}