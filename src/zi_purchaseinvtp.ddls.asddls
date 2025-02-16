@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Purchase Invoice Interface View'
define root view entity ZI_purchaseinvTP
  provider contract transactional_interface
  as projection on ZR_purchaseinvTP as purchaseinv
{
  key Companycode,
  key Fiscalyearvalue,
  key Supplierinvoice,
  Supplierinvoicewthnfiscalyear,
  Creationdatetime,
  AddressID,
  _purchaseline : redirected to composition child ZI_purchaselineTP
  
}
