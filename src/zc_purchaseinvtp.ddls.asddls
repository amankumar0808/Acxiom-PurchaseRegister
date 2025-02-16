@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: 'Purchase Invoice Projection View'
@ObjectModel.semanticKey: [ 'Supplierinvoice' ]
@Search.searchable: true
define root view entity ZC_purchaseinvTP
  provider contract transactional_query
  as projection on ZR_purchaseinvTP as purchaseinv
{
  @Search.defaultSearchElement: true
  @Search.fuzzinessThreshold: 0.90 
  key Companycode,
  @Search.defaultSearchElement: true
  @Search.fuzzinessThreshold: 0.90 
  key Fiscalyearvalue,
  @Search.defaultSearchElement: true
  @Search.fuzzinessThreshold: 0.90 
  key Supplierinvoice,
  Supplierinvoicewthnfiscalyear,
  Creationdatetime,
  AddressID,
  _purchaseline : redirected to composition child ZC_purchaselineTP
  
}
