@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: 'Purchase Line Projection View'
@ObjectModel.semanticKey: [ 'Supplierinvoiceitem' ]
@Search.searchable: true
define view entity ZC_purchaselineTP
  as projection on ZR_purchaselineTP as purchaseline
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
  @Search.defaultSearchElement: true
  @Search.fuzzinessThreshold: 0.90 
  key Supplierinvoiceitem,
  Postingdate,
  Plantname,
  Plantadr,
  Plantcity,
  Plantgst,
  Plantpin,
  Product,
  Productname,
  Purchaseorder,
  Purchaseorderitem,
  
////////////////////////////*******************//////////////////////////////////////    
  product_trade_name,
  vendor_invoice_no,
  vendor_invoice_date,
  vendor_type,
  TypeofEnterprise,
  UdhyamAadharNo,
  UdhyamCertificateDate,
  UdhyamCertificateReceivingDate,
  RFQNO,
  RFQDate,
  SupplierQuotation,
  supplierquotationdate,
//////////////////////////////*******************/////////////////////////////////////

  Baseunit,
  Profitcenter,
  Purchaseordertype,
  Purchaseorderdate,
  Purchasingorganization,
  Purchasinggroup,
  Mrnquantityinbaseunit,
  Mrnpostingdate,
  Hsncode,
  Taxcodename,
  Originalreferencedocument,
  Igst,
  Sgst,
  Cgst,
  Rateigst,
  Ratecgst,
  Ratesgst,
  JournaldocumentrefID,
  Journaldocumentdate,
  Isreversed,
  Basicrate,
  Poqty,
  Pouom,
  Netamount,
  Taxamount,
  Roundoff,
  Manditax,
  Mandicess,
  Discount,
  Totalamount,
  Freight,
  Insurance,
  Ecs,
  Epf,
  Othercharges,
  Packaging,
  
  _purchaseinv : redirected to parent ZC_purchaseinvTP
  
}
