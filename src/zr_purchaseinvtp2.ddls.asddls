@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: 'CDS purchaseinv(Custom Analytical Query)'
define root view entity ZR_purchaseinvTP2
  as select from zpurchinvproc as purchaseinv
  composition [0..*] of ZR_purchaselineTP2 as _purchaseline
{
  key companycode as Companycode,
  key fiscalyearvalue as Fiscalyearvalue,
  key supplierinvoice as Supplierinvoice,
  supplierinvoicewthnfiscalyear as Supplierinvoicewthnfiscalyear,
  @Semantics.systemDateTime.createdAt: true
  creationdatetime as Creationdatetime,
  addressid  as Plantadr,
  _purchaseline
  
}
