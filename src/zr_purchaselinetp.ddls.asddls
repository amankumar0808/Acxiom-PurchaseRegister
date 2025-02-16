@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: 'CDS View forpurchaseline'
define view entity ZR_purchaselineTP
  as select from zpurchinvlines as purchaseline
  association to parent ZR_purchaseinvTP as _purchaseinv on $projection.Companycode = _purchaseinv.Companycode and $projection.Fiscalyearvalue = _purchaseinv.Fiscalyearvalue and $projection.Supplierinvoice = _purchaseinv.Supplierinvoice
{
  key companycode as Companycode,
  key fiscalyearvalue as Fiscalyearvalue,
  key supplierinvoice as Supplierinvoice,
  key supplierinvoiceitem as Supplierinvoiceitem,
  postingdate as Postingdate,
  plantname as Plantname,
  plantadr as Plantadr,
  plantcity as Plantcity,
  plantgst as Plantgst,
  plantpin as Plantpin,
  product as Product,
  productname as Productname,
  purchaseorder as Purchaseorder,
  purchaseorderitem as Purchaseorderitem,
  
////////////////////////*******************//////////////////////////////////////  
  product_trade_name as product_trade_name,
  vendor_invoice_no as vendor_invoice_no,
  vendor_invoice_date as vendor_invoice_date,
  vendor_type as vendor_type,
  typeofenterprise as TypeofEnterprise,
  udhyamaadharno as UdhyamAadharNo,
  udhyamcertificatedate as UdhyamCertificateDate,
  udhyamcertificatereceivingdate as UdhyamCertificateReceivingDate,
  rfqno as RFQNO,
  rfqdate as RFQDate,
  supplierquotation as SupplierQuotation,
  supplierquotationdate as supplierquotationdate,
////////////////////////*******************//////////////////////////////////////  
  
  baseunit as Baseunit,
  profitcenter as Profitcenter,
  purchaseordertype as Purchaseordertype,
  purchaseorderdate as Purchaseorderdate,
  purchasingorganization as Purchasingorganization,
  purchasinggroup as Purchasinggroup,
  mrnquantityinbaseunit as Mrnquantityinbaseunit,
  mrnpostingdate as Mrnpostingdate,
  hsncode as Hsncode,
  taxcodename as Taxcodename,
  originalreferencedocument as Originalreferencedocument,
  igst as Igst,
  sgst as Sgst,
  cgst as Cgst,
  rateigst as Rateigst,
  ratecgst as Ratecgst,
  ratesgst as Ratesgst,
  journaldocumentrefid as JournaldocumentrefID,
  journaldocumentdate as Journaldocumentdate,
  isreversed as Isreversed,
  basicrate as Basicrate,
  poqty as Poqty,
  pouom as Pouom,
  netamount as Netamount,
  taxamount as Taxamount,
  roundoff as Roundoff,
  manditax as Manditax,
  mandicess as Mandicess,
  discount as Discount,
  totalamount as Totalamount,
  freight as Freight,
  insurance as Insurance,
  ecs as Ecs,
  epf as Epf,
  othercharges as Othercharges,
  packaging as Packaging,
  
  _purchaseinv
  
}
