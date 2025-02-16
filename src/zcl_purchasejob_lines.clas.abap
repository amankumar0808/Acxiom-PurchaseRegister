CLASS zcl_purchasejob_lines DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_apj_dt_exec_object .
    INTERFACES if_apj_rt_exec_object .

    INTERFACES if_oo_adt_classrun .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_PURCHASEJOB_LINES IMPLEMENTATION.


  METHOD if_apj_dt_exec_object~get_parameters.
" Return the supported selection parameters here
    et_parameter_def = VALUE #(
*      ( selname = 'S_ID'    kind = if_apj_dt_exec_object=>select_option datatype = 'C' length = 10 param_text = 'My ID'                                      changeable_ind = abap_true )
*      ( selname = 'P_DESCR' kind = if_apj_dt_exec_object=>parameter     datatype = 'C' length = 80 param_text = 'My Description'   lowercase_ind = abap_true changeable_ind = abap_true )
*      ( selname = 'P_COUNT' kind = if_apj_dt_exec_object=>parameter     datatype = 'I' length = 10 param_text = 'My Count'                                   changeable_ind = abap_true )
      ( selname = 'P_SIMUL' kind = if_apj_dt_exec_object=>parameter     datatype = 'C' length =  1 param_text = 'Full Processing' checkbox_ind = abap_true  changeable_ind = abap_true )
    ).

" Return the default parameters values here
    et_parameter_val = VALUE #(
*      ( selname = 'S_ID'    kind = if_apj_dt_exec_object=>select_option sign = 'I' option = 'EQ' low = '4711' )
*      ( selname = 'P_DESCR' kind = if_apj_dt_exec_object=>parameter     sign = 'I' option = 'EQ' low = 'My Default Description' )
*      ( selname = 'P_COUNT' kind = if_apj_dt_exec_object=>parameter     sign = 'I' option = 'EQ' low = '200' )
      ( selname = 'P_SIMUL' kind = if_apj_dt_exec_object=>parameter     sign = 'I' option = 'EQ' low = abap_false )
    ).

  ENDMETHOD.


  METHOD if_apj_rt_exec_object~execute.
    TYPES ty_id TYPE c LENGTH 10.
    DATA s_id    TYPE RANGE OF ty_id.
    DATA p_descr TYPE c LENGTH 80.
    DATA p_count TYPE i.
    DATA p_simul TYPE abap_boolean.
    DATA processfrom TYPE d.

*************************************** RATE GST ************************************
DATA: lv_gst_text  TYPE string,
      lv_igst_perc TYPE string,
      lv_cgst_perc TYPE string,
      lv_sgst_perc TYPE string,
      lv_pos_start TYPE i,
      lv_pos_end   TYPE i,
      lv_length    TYPE i.

TYPES: BEGIN OF ty_gst_data,
         SupplierInvoice TYPE I_SuplrInvcItemPurOrdRefAPI01-SupplierInvoice,
         PurchaseOrderItem TYPE I_SuplrInvcItemPurOrdRefAPI01-PurchaseOrderItem,
         Igst TYPE char10,
         Cgst TYPE char10,
         Sgst TYPE char10,
       END OF ty_gst_data.
DATA: lt_gst_data TYPE TABLE OF ty_gst_data, " Temporary table for storing GST values
      ls_gst_data TYPE ty_gst_data.  " Work area for GST extraction
************************************************************************************

    DATA: jobname   type cl_apj_rt_api=>TY_JOBNAME.
    DATA: jobcount  type cl_apj_rt_api=>TY_JOBCOUNT.
    DATA: catalog   type cl_apj_rt_api=>TY_CATALOG_NAME.
    DATA: template  type cl_apj_rt_api=>TY_TEMPLATE_NAME.

    DATA: lt_purchinvlines TYPE STANDARD TABLE OF zpurchinvlines,
          wa_purchinvlines TYPE zpurchinvlines,
          lt_purchinvprocessed TYPE STANDARD TABLE OF zpurchinvproc,
          wa_purchinvprocessed TYPE zpurchinvproc.


****************************************************************************************
   DATA maxInvoiceDate TYPE D.
    DATA deleteString TYPE C LENGTH 4.
    DATA: lv_tstamp TYPE timestamp, lv_date TYPE d, lv_time TYPE t, lv_dst TYPE abap_bool.

    GET TIME STAMP FIELD DATA(lv_timestamp).

    GET TIME STAMP FIELD lv_tstamp.
    CONVERT TIME STAMP lv_tstamp TIME ZONE sy-zonlo INTO DATE lv_date TIME lv_time DAYLIGHT SAVING TIME lv_dst.

    deleteString = |{ lv_date+6(2) }| && |{ lv_time+0(2) }|.

    IF deleteString = '2819'.
      delete from zpurchinvlines.
      delete from zpurchinvproc.
      COMMIT WORK.
    ENDIF.

    SELECT FROM zbillinglines
      FIELDS max( invoicedate )
      INTO @maxInvoiceDate .
    IF maxInvoiceDate is INITIAL.
      maxInvoiceDate = 20010101.
    ELSE.
      maxInvoiceDate = maxInvoiceDate - 30.
    ENDIF.
****************************************************************************************


    " Getting the actual parameter values
    LOOP AT it_parameters INTO DATA(ls_parameter).
      CASE ls_parameter-selname.
        WHEN 'S_ID'.
          APPEND VALUE #( sign   = ls_parameter-sign
                          option = ls_parameter-option
                          low    = ls_parameter-low
                          high   = ls_parameter-high ) TO s_id.
        WHEN 'P_DESCR'. p_descr = ls_parameter-low.
        WHEN 'P_COUNT'. p_count = ls_parameter-low.
        WHEN 'P_SIMUL'. p_simul = ls_parameter-low.
      ENDCASE.
    ENDLOOP.

    try.
*      read own runtime info catalog
       cl_apj_rt_api=>GET_JOB_RUNTIME_INFO(
                        importing
                          ev_jobname        = jobname
                          ev_jobcount       = jobcount
                          ev_catalog_name   = catalog
                          ev_template_name  = template ).

       catch cx_apj_rt.

    endtry.

    processfrom = sy-datum - 30.
    IF p_simul = abap_true.
       processfrom = sy-datum - 2000.
    ENDIF.


******************************************************* HEADER **********************************
    SELECT FROM I_SupplierInvoiceAPI01 AS c
        LEFT JOIN i_supplier AS b ON b~supplier = c~InvoicingParty
***********************************
        LEFT JOIN I_PurchaseOrderItemAPI01 AS hdr ON c~BusinessPlace = hdr~Plant
        LEFT JOIN I_BusPartAddress AS HDR1 ON HDR~Plant = HDR1~BusinessPartner
***********************************
        FIELDS
            b~Supplier , b~PostalCode , b~BPAddrCityName , b~BPAddrStreetName , b~TaxNumber3,
            b~SupplierFullName, b~region, c~ReverseDocument , c~ReverseDocumentFiscalYear,
            c~CompanyCode , c~PaymentTerms , c~CreatedByUser , c~CreationDate , c~InvoicingParty , c~InvoiceGrossAmount,
            c~DocumentCurrency , c~SupplierInvoiceIDByInvcgParty, c~FiscalYear, c~SupplierInvoice, c~SupplierInvoiceWthnFiscalYear,
            c~DocumentDate, c~PostingDate,
**************************************
            HDR1~AddressID
**************************************
        WHERE c~PostingDate >= @processfrom
            AND NOT EXISTS (
               SELECT supplierinvoice FROM zpurchinvproc
               WHERE c~supplierinvoice = zpurchinvproc~supplierinvoice AND
                 c~CompanyCode = zpurchinvproc~companycode AND
                 c~FiscalYear = zpurchinvproc~fiscalyearvalue )
            INTO TABLE @DATA(ltheader).





******************************************************* LINE ITEM **********************************
    LOOP AT ltheader INTO DATA(waheader).
      lv_timestamp = cl_abap_tstmp=>add_to_short( tstmp = lv_timestamp secs = 11111 ).

* Delete already processed sales line
      delete from zpurchinvlines
        Where zpurchinvlines~companycode = @waheader-CompanyCode AND
        zpurchinvlines~fiscalyearvalue = @waheader-FiscalYear AND
        zpurchinvlines~supplierinvoice = @waheader-SupplierInvoice.



      SELECT FROM I_SuplrInvcItemPurOrdRefAPI01 AS a
************************************
        left join I_PurchaseOrderItemAPI01 as li on a~PurchaseOrder = li~PurchaseOrder
        LEFT JOIN zmaterial_table AS li4 on li~Material = li4~mat
        left join I_DELIVERYDOCUMENTITEM as li2 on li~PurchaseOrder = li2~ReferenceSDDocument and li~PurchaseOrderItem = li2~ReferenceSDDocumentItem
        left join I_DELIVERYDOCUMENT as li3 on li2~DeliveryDocument = li3~DeliveryDocument
        left join I_PURCHASEORDERHISTORYAPI01 as li5 on li~PurchaseOrder = li5~PurchaseOrder and li~PurchaseOrderItem = li5~PurchaseOrderItem and li5~GoodsMovementType = '101'
        left join I_PURCHASEORDERAPI01 as li6 on li~PurchaseOrder = li6~PurchaseOrder
        left join I_BusinessPartner as li7 on li6~Supplier = li7~BusinessPartner
        left join I_BusinessPartnerLegalFormText as li8 on li7~LegalForm = li8~LegalForm
        left join I_PURCHASEORDERITEMTP_2 as li9 on li~PurchaseOrder = li9~PurchaseOrder
        left join I_Requestforquotation_Api01 as li10 on li9~SupplierQuotation = li10~RequestForQuotation
        left join I_SupplierQuotation_Api01 as li11 on li9~SupplierQuotation = li11~SupplierQuotation
        left join I_ACCOUNTINGDOCUMENTJOURNAL as li12 on li5~PurchasingHistoryDocument = li12~DocumentReferenceID
        left join I_PURORDITMPRICINGELEMENTAPI01 as li13 on li~PurchaseOrder = li13~PurchaseOrder and li~PurchaseOrderItem = li13~PurchaseOrderItem
        left join I_taxcodetext as li14 on li13~TaxCode = li14~TaxCode
************************************

        FIELDS
            a~PurchaseOrderItem, a~SupplierInvoiceItem,
            a~PurchaseOrder, a~SupplierInvoiceItemAmount AS tax_amt, a~SupplierInvoiceItemAmount, a~taxcode,
            a~FreightSupplier , a~SupplierInvoice , a~FiscalYear , a~TaxJurisdiction AS SInvwithFYear, a~plant,
            a~PurchaseOrderItemMaterial AS material, a~QuantityInPurchaseOrderUnit, a~QtyInPurchaseOrderPriceUnit,
            a~PurchaseOrderQuantityUnit, PurchaseOrderPriceUnit, a~ReferenceDocument , a~ReferenceDocumentFiscalYear,
************************************
            li~Plant as plantcity, li~Plant as plantpin, li3~DeliveryDocumentBySupplier, li4~trade_name, li5~DocumentDate,
            li8~LegalFormDescription, li9~SupplierQuotation, li10~RFQPublishingDate, li11~SupplierQuotation as SQ,
            li11~QuotationSubmissionDate, li5~PostingDate, li12~DocumentReferenceID,
            li14~TaxCodeName as Igst, li14~TaxCodeName as Cgst, li14~TaxCodeName as Sgst
************************************
        WHERE a~SupplierInvoice = @waheader-SupplierInvoice
          AND a~FiscalYear = @waheader-FiscalYear
          INTO TABLE @DATA(ltlines).


************************Additional LOOP For "Mrnpostingdate"***************************
            LOOP AT ltlines INTO DATA(WA_NEW).
            clear: wa_new-PostingDate.
            SELECT single postingdate FROM I_PurchaseOrderItemAPI01 as a
                    left join I_PURCHASEORDERHISTORYAPI01 as b on a~PurchaseOrder = b~PurchaseOrder and a~PurchaseOrderItem = b~PurchaseOrderItem
                WHERE b~PURCHASINGHISTORYCATEGORY EQ 'Q' and a~purchaseorder = @wa_new-PurchaseOrder and a~purchaseorderitem = @wa_new-PurchaseOrderItem
                INTO @WA_NEW-PostingDate.

****************************** FOR HSN CODE ****************************
            SELECT single FROM I_PurchaseOrderItemAPI01 as a
                  left join I_PURCHASEORDERHISTORYAPI01 as b on a~PurchaseOrder = b~PurchaseOrder and a~PurchaseOrderItem = b~PurchaseOrderItem and b~GoodsMovementType EQ '101'
                  left join I_ACCOUNTINGDOCUMENTJOURNAL as c on b~PurchasingHistoryDocument = c~DocumentReferenceID
                fields c~DOCUMENTREFERENCEID
                    WHERE a~purchaseorder = @wa_new-PurchaseOrder and a~purchaseorderitem = @wa_new-PurchaseOrderItem
                    INTO @WA_NEW-DOCUMENTREFERENCEID .

            ENDLOOP.
***************************************************************************************


********************************************** RATE GST LOOP *******************************************
LOOP AT ltlines INTO DATA(ls_line).
  CLEAR: ls_gst_data, lv_cgst_perc, lv_sgst_perc, lv_igst_perc.

  " Store Invoice and PO Item
  ls_gst_data-SupplierInvoice = ls_line-SupplierInvoice.
  ls_gst_data-PurchaseOrderItem = ls_line-PurchaseOrderItem.

  " Extract CGST percentage
  FIND FIRST OCCURRENCE OF REGEX '\d+' IN ls_line-Cgst SUBMATCHES lv_cgst_perc.
  IF lv_cgst_perc IS NOT INITIAL.
    ls_gst_data-Cgst = lv_cgst_perc.
  ENDIF.

  " Extract SGST percentage
  FIND FIRST OCCURRENCE OF REGEX '\d+' IN ls_line-Sgst SUbMATCHES lv_sgst_perc.
  IF lv_sgst_perc IS NOT INITIAL.
    ls_gst_data-Sgst = lv_sgst_perc.
  ENDIF.

  " Extract IGST percentage
  FIND FIRST OCCURRENCE OF REGEX '\d+' IN ls_line-Igst SUbMATCHES lv_igst_perc.
  IF lv_igst_perc IS NOT INITIAL.
    ls_gst_data-Igst = lv_igst_perc.
  ENDIF.

  " Append only if values are found
  APPEND ls_gst_data TO lt_gst_data.
ENDLOOP.

" Now, assign extracted GST values back to the original ltlines table
LOOP AT ltlines INTO ls_line.
  READ TABLE lt_gst_data INTO ls_gst_data
    WITH KEY SupplierInvoice = ls_line-SupplierInvoice
             PurchaseOrderItem = ls_line-PurchaseOrderItem.

  IF sy-subrc = 0.
    IF ls_gst_data-Igst IS NOT INITIAL.
      ls_line-Igst = ls_gst_data-Igst.
    ENDIF.
    IF ls_gst_data-Cgst IS NOT INITIAL.
      ls_line-Cgst = ls_gst_data-Cgst.
    ENDIF.
    IF ls_gst_data-Sgst IS NOT INITIAL.
      ls_line-Sgst = ls_gst_data-Sgst.
    ENDIF.
  ENDIF.

  MODIFY ltlines FROM ls_line.
ENDLOOP.

*LOOP AT ltlines INTO DATA(ls_line).
*  CLEAR: ls_gst_data, lv_gst_text, lv_igst_perc, lv_cgst_perc, lv_sgst_perc, lv_pos_start, lv_pos_end, lv_length.
*
*  ls_gst_data-SupplierInvoice = ls_line-SupplierInvoice.
*  ls_gst_data-PurchaseOrderItem = ls_line-PurchaseOrderItem.
*
*  " Process IGST
*  lv_gst_text = ls_line-Igst.
*  IF lv_gst_text CS 'IGST'.
*    FIND FIRST OCCURRENCE OF REGEX 'IGST\s*(\d+)%' IN lv_gst_text MATCH OFFSET lv_pos_start.
*    IF sy-subrc = 0.
*      lv_pos_start = lv_pos_start + 5.  " Skip 'IGST '
*      FIND FIRST OCCURRENCE OF '%' IN lv_gst_text+lv_pos_start MATCH OFFSET lv_pos_end.
*      lv_length = lv_pos_end.
*      lv_igst_perc = lv_gst_text+lv_pos_start(lv_length).
*      ls_gst_data-Igst = lv_igst_perc.
*    ENDIF.
*  ENDIF.
*
*  " Process CGST
*  lv_gst_text = ls_line-Cgst.
*  IF lv_gst_text CS 'CGST'.
*    FIND FIRST OCCURRENCE OF REGEX 'CGST\s*(\d+)%' IN lv_gst_text MATCH OFFSET lv_pos_start.
*    IF sy-subrc = 0.
*      lv_pos_start = lv_pos_start + 5.  " Skip 'CGST '
*      FIND FIRST OCCURRENCE OF '%' IN lv_gst_text+lv_pos_start MATCH OFFSET lv_pos_end.
*      lv_length = lv_pos_end.
*      lv_cgst_perc = lv_gst_text+lv_pos_start(lv_length).
*      ls_gst_data-Cgst = lv_cgst_perc.
*    ENDIF.
*  ENDIF.
*
*  " Process SGST
*  lv_gst_text = ls_line-Sgst.
*  IF lv_gst_text CS 'SGST'.
*    FIND FIRST OCCURRENCE OF REGEX 'SGST\s*(\d+)%' IN lv_gst_text MATCH OFFSET lv_pos_start.
*    IF sy-subrc = 0.
*      lv_pos_start = lv_pos_start + 5.  " Skip 'SGST '
*      FIND FIRST OCCURRENCE OF '%' IN lv_gst_text+lv_pos_start MATCH OFFSET lv_pos_end.
*      lv_length = lv_pos_end.
*      lv_sgst_perc = lv_gst_text+lv_pos_start(lv_length).
*      ls_gst_data-Sgst = lv_sgst_perc.
*    ENDIF.
*  ENDIF.
*
*  " Store extracted GST values for this SupplierInvoice and PurchaseOrderItem
*  APPEND ls_gst_data TO lt_gst_data.
*ENDLOOP.
*
*" Assign stored GST values back to corresponding invoice items
*LOOP AT ltlines INTO ls_line.
*  READ TABLE lt_gst_data INTO ls_gst_data
*    WITH KEY SupplierInvoice = ls_line-SupplierInvoice
*             PurchaseOrderItem = ls_line-PurchaseOrderItem.
*IF sy-subrc = 0.
*    " Update only if there is a valid GST percentage
*    IF ls_gst_data-Igst IS NOT INITIAL.
*      ls_line-Igst = ls_gst_data-Igst.
*    ENDIF.
*    IF ls_gst_data-Cgst IS NOT INITIAL.
*      ls_line-Cgst = ls_gst_data-Cgst.
*    ENDIF.
*    IF ls_gst_data-Sgst IS NOT INITIAL.
*      ls_line-Sgst = ls_gst_data-Sgst.
*    ENDIF.
*  ENDIF.
*  MODIFY ltlines FROM ls_line.
*ENDLOOP.
******************************************************************************************************


*      SELECT FROM I_BillingDocItemPrcgElmntBasic FIELDS BillingDocument , BillingDocumentItem, ConditionRateValue, ConditionAmount, ConditionType
*        WHERE BillingDocument = @waheader-BillingDocument
*        INTO TABLE @DATA(it_price).

        SELECT FROM I_Producttext as a FIELDS
            a~ProductName, a~Product
        FOR ALL ENTRIES IN @ltlines
        WHERE a~Product = @ltlines-material AND a~Language = 'E'
            INTO TABLE @DATA(it_product).

        SELECT FROM I_PurchaseOrderItemAPI01 AS a
            LEFT JOIN I_PurchaseOrderAPI01 AS b ON a~PurchaseOrdeR = b~PurchaseOrder
            FIELDS a~BaseUnit , b~PurchaseOrderType , b~PurchasingGroup , b~PurchasingOrganization ,
            b~PurchaseOrderDate , a~PurchaseOrder , a~PurchaseOrderItem , a~ProfitCenter
        FOR ALL ENTRIES IN @ltlines
        WHERE a~PurchaseOrder = @ltlines-PurchaseOrder AND a~PurchaseOrderItem = @ltlines-PurchaseOrderItem
            INTO TABLE @DATA(it_po).

        SELECT FROM I_MaterialDocumentItem_2
            FIELDS MaterialDocument , PurchaseOrder , PurchaseOrderItem , QuantityInBaseUnit , PostingDate
        FOR ALL ENTRIES IN @ltlines
        WHERE MaterialDocument  = @ltlines-ReferenceDocument
            INTO TABLE @DATA(it_grn).

        SELECT FROM I_ProductPlantIntlTrd FIELDS
            product , plant  , ConsumptionTaxCtrlCode
            FOR ALL ENTRIES IN @ltlines
        WHERE product = @ltlines-Material  AND plant = @ltlines-Plant
            INTO TABLE @DATA(it_hsn).

        SELECT FROM I_taxcodetext
            FIELDS TaxCode , TaxCodeName
        FOR ALL ENTRIES IN @ltlines
        WHERE Language = 'E' AND taxcode = @ltlines-TaxCode
            INTO TABLE @DATA(it_tax).

        LOOP AT ltlines INTO DATA(walines).

            wa_purchinvlines-client = SY-MANDT.
            wa_purchinvlines-companycode = waheader-CompanyCode.
            wa_purchinvlines-fiscalyearvalue = waheader-FiscalYear.
            wa_purchinvlines-supplierinvoice = waheader-SupplierInvoice.
            wa_purchinvlines-supplierinvoiceitem = walines-SupplierInvoiceItem.
            wa_purchinvlines-postingdate = waheader-PostingDate.
************************************** Item Level Fields Added ****************************
            wa_purchinvlines-plantcity = WALINES-plantcity.
            wa_purchinvlines-plantpin = walines-plantpin.
            wa_purchinvlines-product_trade_name = walines-trade_name.
            wa_purchinvlines-vendor_invoice_no = walines-DeliveryDocumentBySupplier.
            wa_purchinvlines-vendor_invoice_date = walines-DocumentDate.
            wa_purchinvlines-vendor_type = walines-LegalFormDescription.
            wa_purchinvlines-rfqno = walines-SupplierQuotation.
            wa_purchinvlines-rfqno = walines-SupplierQuotation.
            wa_purchinvlines-rfqdate = walines-RFQPublishingDate.
            wa_purchinvlines-supplierquotation = walines-sq.
            wa_purchinvlines-supplierquotationdate = walines-QuotationSubmissionDate.
            wa_purchinvlines-mrnquantityinbaseunit = walines-PostingDate.
            wa_purchinvlines-hsncode = walines-DocumentReferenceID.
            wa_purchinvlines-rateigst = ls_line-Igst.
            wa_purchinvlines-ratecgst = ls_line-cgst.
            wa_purchinvlines-ratesgst = ls_line-sgst.


            SELECT SINGLE FROM I_IN_BusinessPlaceTaxDetail AS a
                LEFT JOIN  I_Address_2  AS b ON a~AddressID = b~AddressID
                FIELDS
                a~BusinessPlaceDescription,
                a~IN_GSTIdentificationNumber,
                b~Street, b~PostalCode , b~CityName
            WHERE a~CompanyCode = @waheader-CompanyCode AND a~BusinessPlace = @walines-Plant
            INTO ( @wa_purchinvlines-plantname, @wa_purchinvlines-plantgst, @wa_purchinvlines-plantadr, @wa_purchinvlines-plantpin,
                @wa_purchinvlines-plantcity ).

            wa_purchinvlines-product                   = walines-material.
            READ TABLE it_product INTO DATA(wa_product) WITH KEY product = walines-material.
            wa_purchinvlines-productname = wa_product-ProductName.

            wa_purchinvlines-purchaseorder             = walines-PurchaseOrder.
            wa_purchinvlines-purchaseorderitem         = walines-PurchaseOrderItem.

            READ TABLE it_po INTO DATA(wa_po) WITH KEY PurchaseOrder = walines-PurchaseOrder
                                                    PurchaseOrderItem = walines-PurchaseOrderItem.

            wa_purchinvlines-baseunit                  = wa_po-BaseUnit.
            wa_purchinvlines-profitcenter              = wa_po-ProfitCenter.
            wa_purchinvlines-purchaseordertype         = wa_po-PurchaseOrderType.
*            wa_purchinvlines-purchaseorderdate         : wa_po-PurchaseOrderDate.
            wa_purchinvlines-purchasingorganization    = wa_po-PurchasingOrganization.
            wa_purchinvlines-purchasinggroup           = wa_po-PurchasingGroup.

            READ TABLE it_grn INTO DATA(wa_grn) WITH KEY MaterialDocument = walines-ReferenceDocument.
            wa_purchinvlines-mrnquantityinbaseunit     = wa_grn-QuantityInBaseUnit.
*            wa_purchinvlines-mrnpostingdate            = wa_grn-PostingDate;
*            ls_response-grn = wa_it-ReferenceDocument.
*        ls_response-grnyear = wa_it-ReferenceDocumentFiscalYear.

            READ TABLE it_hsn INTO DATA(wa_hsn) WITH KEY plant = walines-Plant Product = walines-Material.
            wa_purchinvlines-hsncode                    = wa_hsn-ConsumptionTaxCtrlCode.
            CLEAR wa_hsn.

            READ TABLE it_tax INTO DATA(wa_tax) WITH KEY TaxCode = walines-TaxCode.
            wa_purchinvlines-taxcodename                = wa_tax-TaxCodeName.

*        wa_purchinvlines-originalreferencedocument : abap.char(20);

            SELECT SINGLE TaxItemAcctgDocItemRef FROM i_operationalacctgdocitem
                WHERE OriginalReferenceDocument = @walines-sinvwithfyear AND TaxItemAcctgDocItemRef IS NOT INITIAL
                AND AccountingDocumentItemType <> 'T'
                AND FiscalYear = @walines-FiscalYear
                AND CompanyCode = @waheader-CompanyCode
                AND AccountingDocumentType = 'RE'
            INTO  @DATA(lv_TaxItemAcctgDocItemRef).






************************************************ OLD GST CODE ***********************************************************
*            SELECT  SINGLE AmountInCompanyCodeCurrency FROM i_operationalacctgdocitem
*                WHERE OriginalReferenceDocument = @walines-sinvwithfyear
*                    AND TaxItemAcctgDocItemRef = @lv_TaxItemAcctgDocItemRef
*                    AND AccountingDocumentItemType = 'T'
*                    AND FiscalYear = @walines-FiscalYear
*                    AND CompanyCode = @waheader-CompanyCode
*                    AND TransactionTypeDetermination = 'JII'
*            INTO  @wa_purchinvlines-igst.
*
*            IF wa_purchinvlines-igst IS INITIAL.
*                SELECT  SINGLE AmountInCompanyCodeCurrency FROM i_operationalacctgdocitem
*                    WHERE OriginalReferenceDocument = @walines-sinvwithfyear
*                        AND TaxItemAcctgDocItemRef = @lv_TaxItemAcctgDocItemRef
*                        AND AccountingDocumentItemType = 'T'
*                        AND FiscalYear = @walines-FiscalYear
*                        AND CompanyCode = @waheader-CompanyCode
*                        AND TransactionTypeDetermination = 'JIC'
*                INTO  @wa_purchinvlines-cgst.
*
*                SELECT  SINGLE AmountInCompanyCodeCurrency FROM i_operationalacctgdocitem
*                    WHERE OriginalReferenceDocument = @walines-sinvwithfyear
*                        AND TaxItemAcctgDocItemRef = @lv_TaxItemAcctgDocItemRef
*                        AND AccountingDocumentItemType = 'T'
*                        AND FiscalYear = @walines-FiscalYear
*                        AND CompanyCode = @waheader-CompanyCode
*                        AND TransactionTypeDetermination = 'JIS'
*                INTO  @wa_purchinvlines-sgst.
*            ENDIF.
****************************************************************************************************************






*            wa_purchinvlines-igst = ABS( wa_purchinvlines-igst ).
*            wa_purchinvlines-cgst = ABS( wa_purchinvlines-cgst ).
*            wa_purchinvlines-sgst = ABS( wa_purchinvlines-sgst ).

*        wa_purchinvlines-rateigst                  : abap.dec(13,2);
*        wa_purchinvlines-ratecgst                  : abap.dec(13,2);
*        wa_purchinvlines-ratesgst                  : abap.dec(13,2);

            SELECT SINGLE FROM I_JournalEntry
                FIELDS DocumentDate ,
                    DocumentReferenceID ,
                    IsReversed
            WHERE OriginalReferenceDocument = @walines-SupplierInvoice
            INTO (  @wa_purchinvlines-journaldocumentdate , @wa_purchinvlines-journaldocumentrefid, @wa_purchinvlines-isreversed ).

            wa_purchinvlines-pouom                      = walines-PurchaseOrderPriceUnit.
            wa_purchinvlines-poqty                      = walines-QuantityInPurchaseOrderUnit.
            wa_purchinvlines-netamount                  = walines-SupplierInvoiceItemAmount.
            wa_purchinvlines-basicrate                  = ROUND( val = wa_purchinvlines-netamount / wa_purchinvlines-poqty dec = 2 ).

            wa_purchinvlines-taxamount                  = wa_purchinvlines-igst + wa_purchinvlines-sgst +
                                                          wa_purchinvlines-cgst.
            wa_purchinvlines-totalamount                = wa_purchinvlines-taxamount + wa_purchinvlines-netamount.

*        wa_purchinvlines-roundoff                  : abap.dec(13,2);
*        wa_purchinvlines-manditax                  : abap.dec(13,2);
*        wa_purchinvlines-mandicess                 : abap.dec(13,2);
*        wa_purchinvlines-discount                  : abap.dec(13,2);

*       CLEAR wa_price.


            APPEND wa_purchinvlines TO lt_purchinvlines.
            CLEAR : wa_purchinvlines.
            CLEAR : wa_po, wa_grn, wa_hsn, wa_tax, lv_taxitemacctgdocitemref.
        ENDLOOP.

        INSERT zpurchinvlines FROM TABLE @lt_purchinvlines.
        wa_purchinvprocessed-client = SY-MANDT.
        wa_purchinvprocessed-supplierinvoice = waheader-SupplierInvoice.
        wa_purchinvprocessed-companycode = waheader-CompanyCode.
        wa_purchinvprocessed-fiscalyearvalue = waheader-FiscalYear.
        wa_purchinvprocessed-supplierinvoicewthnfiscalyear = waheader-SupplierInvoiceWthnFiscalYear.
        wa_purchinvprocessed-creationdatetime = lv_timestamp.
***************************************** Header Level Fields Added *******************************
        wa_purchinvlines-plantadr = waheader-AddressID.

        APPEND wa_purchinvprocessed TO lt_purchinvprocessed.
        INSERT zpurchinvproc FROM TABLE @lt_purchinvprocessed.
        COMMIT WORK.

        CLEAR :  wa_purchinvprocessed, lt_purchinvprocessed, lt_purchinvlines.
        CLEAR : ltlines, it_product, it_po, it_grn, it_hsn, it_tax.

    ENDLOOP.


  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
      DATA processfrom TYPE d.
      DATA p_simul TYPE abap_boolean.
      DATA assignmentreference TYPE string.

************************************* RATE GST **********************************
DATA: lv_gst_text  TYPE string,
      lv_igst_perc TYPE string,
      lv_cgst_perc TYPE string,
      lv_sgst_perc TYPE string,
      lv_pos_start TYPE i,
      lv_pos_end   TYPE i,
      lv_length    TYPE i.

TYPES: BEGIN OF ty_gst_data,
         SupplierInvoice TYPE I_SuplrInvcItemPurOrdRefAPI01-SupplierInvoice,
         PurchaseOrderItem TYPE I_SuplrInvcItemPurOrdRefAPI01-PurchaseOrderItem,
         Igst TYPE char10,
         Cgst TYPE char10,
         Sgst TYPE char10,
       END OF ty_gst_data.
DATA: lt_gst_data TYPE TABLE OF ty_gst_data, " Temporary table for storing GST values
      ls_gst_data TYPE ty_gst_data.          " Work area for GST extraction
*********************************************************************************


      DATA: lt_purchinvlines TYPE STANDARD TABLE OF zpurchinvlines,
          wa_purchinvlines TYPE zpurchinvlines,
          lt_purchinvprocessed TYPE STANDARD TABLE OF zpurchinvproc,
          wa_purchinvprocessed TYPE zpurchinvproc.

    GET TIME STAMP FIELD DATA(lv_timestamp).

*delete from zpurchinvproc.
*delete from zpurchinvlines.
*COMMIT WORK.

    p_simul = abap_true.
    processfrom = sy-datum - 30.
    IF p_simul = abap_true.
       processfrom = sy-datum - 2000.
    ENDIF.


***************************************************** HEADER *****************************************
    SELECT FROM I_SupplierInvoiceAPI01 AS c
        LEFT JOIN i_supplier AS b ON b~supplier = c~InvoicingParty
***********************************
        LEFT JOIN I_PurchaseOrderItemAPI01 AS hdr ON c~BusinessPlace = hdr~Plant
        LEFT JOIN I_BusPartAddress AS HDR1 ON HDR~Plant = HDR1~BusinessPartner
***********************************
        FIELDS
            b~Supplier , b~PostalCode , b~BPAddrCityName , b~BPAddrStreetName , b~TaxNumber3,
            b~SupplierFullName, b~region, c~ReverseDocument , c~ReverseDocumentFiscalYear,
            c~CompanyCode , c~PaymentTerms , c~CreatedByUser , c~CreationDate , c~InvoicingParty , c~InvoiceGrossAmount,
            c~DocumentCurrency , c~SupplierInvoiceIDByInvcgParty, c~FiscalYear, c~SupplierInvoice, c~SupplierInvoiceWthnFiscalYear,
            c~DocumentDate, c~PostingDate,
**************************************
            HDR1~AddressID
**************************************
        WHERE c~PostingDate >= @processfrom
            AND NOT EXISTS (
               SELECT supplierinvoice FROM zpurchinvproc
               WHERE c~supplierinvoice = zpurchinvproc~supplierinvoice AND
                 c~CompanyCode = zpurchinvproc~companycode AND
                 c~FiscalYear = zpurchinvproc~fiscalyearvalue )
*            AND c~supplierinvoice = '5105600237'
            INTO TABLE @DATA(ltheader).




********************************************************* LINE ITEM ***************************************
    LOOP AT ltheader INTO DATA(waheader).
*      lv_timestamp = cl_abap_tstmp=>add_to_short( tstmp = lv_timestamp secs = 11111 ).
      get TIME STAMP FIELD lv_timestamp.

* Delete already processed sales line
      delete from zpurchinvlines
        Where zpurchinvlines~companycode = @waheader-CompanyCode AND
        zpurchinvlines~fiscalyearvalue = @waheader-FiscalYear AND
        zpurchinvlines~supplierinvoice = @waheader-SupplierInvoice.



      SELECT FROM I_SuplrInvcItemPurOrdRefAPI01 AS a
**********************************************
        left join I_PurchaseOrderItemAPI01 as li on a~PurchaseOrder = li~PurchaseOrder
        LEFT JOIN zmaterial_table AS li4 on li~Material = li4~mat
        left join I_DELIVERYDOCUMENTITEM as li2 on li~PurchaseOrder = li2~ReferenceSDDocument and li~PurchaseOrderItem = li2~ReferenceSDDocumentItem
        left join I_DELIVERYDOCUMENT as li3 on li2~DeliveryDocument = li3~DeliveryDocument
        left join I_PURCHASEORDERHISTORYAPI01 as li5 on li~PurchaseOrder = li5~PurchaseOrder and li~PurchaseOrderItem = li5~PurchaseOrderItem and li5~GoodsMovementType = '101'
        left join I_PURCHASEORDERAPI01 as li6 on li~PurchaseOrder = li6~PurchaseOrder
        left join I_BusinessPartner as li7 on li6~Supplier = li7~BusinessPartner
        left join I_BusinessPartnerLegalFormText as li8 on li7~LegalForm = li8~LegalForm
        left join I_PURCHASEORDERITEMTP_2 as li9 on li~PurchaseOrder = li9~PurchaseOrder
        left join I_Requestforquotation_Api01 as li10 on li9~SupplierQuotation = li10~RequestForQuotation
        left join I_SupplierQuotation_Api01 as li11 on li9~SupplierQuotation = li11~SupplierQuotation
        left join I_ACCOUNTINGDOCUMENTJOURNAL as li12 on li5~PurchasingHistoryDocument = li12~DocumentReferenceID
        left join I_PURORDITMPRICINGELEMENTAPI01 as li13 on li~PurchaseOrder = li13~PurchaseOrder and li~PurchaseOrderItem = li13~PurchaseOrderItem
        left join I_taxcodetext as li14 on li13~TaxCode = li14~TaxCode
***********************************************
        FIELDS
            a~PurchaseOrderItem, a~SupplierInvoiceItem,
            a~PurchaseOrder, a~SupplierInvoiceItemAmount AS tax_amt, a~SupplierInvoiceItemAmount, a~taxcode,
            a~FreightSupplier , a~SupplierInvoice , a~FiscalYear , a~TaxJurisdiction, a~plant,
            a~PurchaseOrderItemMaterial AS material, a~QuantityInPurchaseOrderUnit, a~QtyInPurchaseOrderPriceUnit,
            a~PurchaseOrderQuantityUnit, PurchaseOrderPriceUnit, a~ReferenceDocument , a~ReferenceDocumentFiscalYear,
************************************************
            li~Plant as plantcity, li~Plant as plantpin, li3~DeliveryDocumentBySupplier, li4~trade_name, li5~DocumentDate,
            li8~LegalFormDescription, li9~SupplierQuotation, li10~RFQPublishingDate, li11~SupplierQuotation as SQ,
            li11~QuotationSubmissionDate, li5~PostingDate, li12~DocumentReferenceID, li14~TaxCodeName as Igst, li14~TaxCodeName as Cgst, li14~TaxCodeName as Sgst
***********************************************
        WHERE a~SupplierInvoice = @waheader-SupplierInvoice
          AND a~FiscalYear = @waheader-FiscalYear
          AND a~SuplrInvcDeliveryCostCndnType = ''
        ORDER BY a~PurchaseOrderItem, a~SupplierInvoiceItem
          INTO TABLE @DATA(ltlines).


************************Additional LOOP For "Mrnpostingdate"***************************
            LOOP AT ltlines INTO DATA(WA_NEW).
            clear: wa_new-PostingDate.

            SELECT single    postingdate FROM I_PurchaseOrderItemAPI01 as a
                    left join I_PURCHASEORDERHISTORYAPI01 as b on a~PurchaseOrder = b~PurchaseOrder and a~PurchaseOrderItem = b~PurchaseOrderItem
                WHERE b~PURCHASINGHISTORYCATEGORY EQ 'Q' and a~purchaseorder = @wa_new-PurchaseOrder and a~purchaseorderitem = @wa_new-PurchaseOrderItem
                INTO @WA_NEW-PostingDate.

****************************** FOR HSN CODE ****************************
            SELECT single FROM I_PurchaseOrderItemAPI01 as a
                  left join I_PURCHASEORDERHISTORYAPI01 as b on a~PurchaseOrder = b~PurchaseOrder and a~PurchaseOrderItem = b~PurchaseOrderItem and b~GoodsMovementType EQ '101'
                  left join I_ACCOUNTINGDOCUMENTJOURNAL as c on b~PurchasingHistoryDocument = c~DocumentReferenceID
                fields c~DOCUMENTREFERENCEID
                    WHERE a~purchaseorder = @wa_new-PurchaseOrder and a~purchaseorderitem = @wa_new-PurchaseOrderItem
                    INTO @WA_NEW-DOCUMENTREFERENCEID.

            ENDLOOP.
***************************************************************************************


********************************************** RATE GST LOOP *******************************************
*LOOP AT ltlines INTO DATA(ls_line).
*  CLEAR: ls_gst_data, lv_cgst_perc, lv_sgst_perc, lv_igst_perc.
*
*  " Store Invoice and PO Item
*  ls_gst_data-SupplierInvoice = ls_line-SupplierInvoice.
*  ls_gst_data-PurchaseOrderItem = ls_line-PurchaseOrderItem.
*
*  " Extract CGST percentage
*  FIND FIRST OCCURRENCE OF REGEX 'CGST\s*(\d+)%' IN ls_line-Cgst SUBMATCHES lv_cgst_perc.
*  IF lv_cgst_perc IS NOT INITIAL.
**    ls_gst_data-Cgst = lv_cgst_perc.
*     wa_purchinvlines-ratecgst = lv_cgst_perc.
*  ENDIF.
*
*  " Extract SGST percentage
*  FIND FIRST OCCURRENCE OF REGEX 'SGST\s*(\d+)%' IN ls_line-Sgst SUBMATCHES lv_sgst_perc.
*  IF lv_sgst_perc IS NOT INITIAL.
**    ls_gst_data-Sgst = lv_sgst_perc.
*      wa_purchinvlines-ratesgst = lv_sgst_perc.
*  ENDIF.
*
*  " Extract IGST percentage
*  FIND FIRST OCCURRENCE OF REGEX 'IGST\s*(\d+)%' IN ls_line-Igst SUBMATCHES lv_igst_perc.
*  IF lv_igst_perc IS NOT INITIAL.
**    ls_gst_data-Igst = lv_igst_perc.
*      wa_purchinvlines-rateigst = lv_igst_perc.
*  ENDIF.
*
**  APPEND ls_gst_data TO lt_gst_data.
*   append wa_purchinvlines to  lt_purchinvlines.
*   clear: wa_purchinvlines-ratecgst, wa_purchinvlines-ratesgst, wa_purchinvlines-rateigst,ls_line,wa_purchinvlines.
*ENDLOOP.
******************************************************************************************************




*      SELECT FROM I_BillingDocItemPrcgElmntBasic FIELDS BillingDocument , BillingDocumentItem, ConditionRateValue, ConditionAmount, ConditionType
*        WHERE BillingDocument = @waheader-BillingDocument
*        INTO TABLE @DATA(it_price).

        SELECT FROM I_Producttext as a FIELDS
            a~ProductName, a~Product
        FOR ALL ENTRIES IN @ltlines
        WHERE a~Product = @ltlines-material AND a~Language = 'E'
            INTO TABLE @DATA(it_product).

        SELECT FROM I_PurchaseOrderItemAPI01 AS a
            LEFT JOIN I_PurchaseOrderAPI01 AS b ON a~PurchaseOrdeR = b~PurchaseOrder
            FIELDS a~BaseUnit , b~PurchaseOrderType , b~PurchasingGroup , b~PurchasingOrganization ,
            b~PurchaseOrderDate , a~PurchaseOrder , a~PurchaseOrderItem , a~ProfitCenter
        FOR ALL ENTRIES IN @ltlines
        WHERE a~PurchaseOrder = @ltlines-PurchaseOrder AND a~PurchaseOrderItem = @ltlines-PurchaseOrderItem
            INTO TABLE @DATA(it_po).

        SELECT FROM I_MaterialDocumentItem_2
            FIELDS MaterialDocument , PurchaseOrder , PurchaseOrderItem , QuantityInBaseUnit , PostingDate
        FOR ALL ENTRIES IN @ltlines
        WHERE MaterialDocument  = @ltlines-ReferenceDocument
            INTO TABLE @DATA(it_grn).

        SELECT FROM I_taxcodetext
            FIELDS TaxCode , TaxCodeName
        FOR ALL ENTRIES IN @ltlines
        WHERE Language = 'E' AND taxcode = @ltlines-TaxCode
            INTO TABLE @DATA(it_tax).




             LOOP AT ltlines INTO DATA(walines).

*CLEAR: ls_gst_data, lv_cgst_perc, lv_sgst_perc, lv_igst_perc.
**  ls_gst_data-SupplierInvoice = walines-SupplierInvoice.
**  ls_gst_data-PurchaseOrderItem = walines-PurchaseOrderItem.
*
*  FIND FIRST OCCURRENCE OF REGEX 'CGST\s*(\d+)%' IN walines-Cgst SUBMATCHES lv_cgst_perc.
*  IF lv_cgst_perc IS NOT INITIAL.
**    ls_gst_data-Cgst = lv_cgst_perc.
*     wa_purchinvlines-ratecgst = lv_cgst_perc.
*  ENDIF.
*
*  FIND FIRST OCCURRENCE OF REGEX 'SGST\s*(\d+)%' IN walines-Sgst SUBMATCHES lv_sgst_perc.
*  IF lv_sgst_perc IS NOT INITIAL.
**    ls_gst_data-Sgst = lv_sgst_perc.
*      wa_purchinvlines-ratesgst = lv_sgst_perc.
*  ENDIF.
*
*  FIND FIRST OCCURRENCE OF REGEX 'IGST\s*(\d+)%' IN walines-Igst SUBMATCHES lv_igst_perc.
*  IF lv_igst_perc IS NOT INITIAL.
**    ls_gst_data-Igst = lv_igst_perc.
*      wa_purchinvlines-rateigst = lv_igst_perc.
*  ENDIF.

*  APPEND ls_gst_data TO lt_gst_data.
*   append wa_purchinvlines to  lt_purchinvlines.



            wa_purchinvlines-client = SY-MANDT.
            wa_purchinvlines-companycode = waheader-CompanyCode.
            wa_purchinvlines-fiscalyearvalue = waheader-FiscalYear.
            wa_purchinvlines-supplierinvoice = waheader-SupplierInvoice.
            wa_purchinvlines-supplierinvoiceitem = walines-SupplierInvoiceItem.
            wa_purchinvlines-postingdate = waheader-PostingDate.
********************************** Item Level Fields Added ****************************
             wa_purchinvlines-plantcity = WALINES-plantcity.
            wa_purchinvlines-plantpin = walines-plantpin.
            wa_purchinvlines-product_trade_name = walines-trade_name.
            wa_purchinvlines-vendor_invoice_no = walines-DeliveryDocumentBySupplier.
            wa_purchinvlines-vendor_invoice_date = walines-DocumentDate.
            wa_purchinvlines-vendor_type = walines-LegalFormDescription.
            wa_purchinvlines-rfqno = walines-SupplierQuotation.
            wa_purchinvlines-rfqno = walines-SupplierQuotation.
            wa_purchinvlines-rfqdate = walines-RFQPublishingDate.
            wa_purchinvlines-supplierquotation = walines-sq.
            wa_purchinvlines-supplierquotationdate = walines-QuotationSubmissionDate.
            wa_purchinvlines-mrnquantityinbaseunit = walines-PostingDate.
            wa_purchinvlines-hsncode = walines-DocumentReferenceID.
*            wa_purchinvlines-rateigst = ls_line-Igst.
*            wa_purchinvlines-ratecgst = ls_line-cgst.
*            wa_purchinvlines-ratesgst = ls_line-sgst.


            SELECT SINGLE FROM I_IN_BusinessPlaceTaxDetail AS a
                LEFT JOIN  I_Address_2  AS b ON a~AddressID = b~AddressID
                FIELDS
                a~BusinessPlaceDescription,
                a~IN_GSTIdentificationNumber,
                b~Street, b~PostalCode , b~CityName
            WHERE a~CompanyCode = @waheader-CompanyCode AND a~BusinessPlace = @walines-Plant
            INTO ( @wa_purchinvlines-plantname, @wa_purchinvlines-plantgst, @wa_purchinvlines-plantadr, @wa_purchinvlines-plantpin,
                @wa_purchinvlines-plantcity ).

            wa_purchinvlines-product                   = walines-material.
            READ TABLE it_product INTO DATA(wa_product) WITH KEY product = walines-material.
            wa_purchinvlines-productname = wa_product-ProductName.

            wa_purchinvlines-purchaseorder             = walines-PurchaseOrder.
            wa_purchinvlines-purchaseorderitem         = walines-PurchaseOrderItem.
            CONCATENATE walines-SupplierInvoice walines-FiscalYear INTO wa_purchinvlines-originalreferencedocument.

            READ TABLE it_po INTO DATA(wa_po) WITH KEY PurchaseOrder = walines-PurchaseOrder
                                                    PurchaseOrderItem = walines-PurchaseOrderItem.

            wa_purchinvlines-baseunit                  = wa_po-BaseUnit.
            wa_purchinvlines-profitcenter              = wa_po-ProfitCenter.
            wa_purchinvlines-purchaseordertype         = wa_po-PurchaseOrderType.
            wa_purchinvlines-purchaseorderdate         = wa_po-PurchaseOrderDate.
            wa_purchinvlines-purchasingorganization    = wa_po-PurchasingOrganization.
            wa_purchinvlines-purchasinggroup           = wa_po-PurchasingGroup.

            READ TABLE it_grn INTO DATA(wa_grn) WITH KEY MaterialDocument = walines-ReferenceDocument.
            wa_purchinvlines-mrnquantityinbaseunit     = wa_grn-QuantityInBaseUnit.
*            wa_purchinvlines-mrnpostingdate            = wa_grn-PostingDate;
*            ls_response-grn = wa_it-ReferenceDocument.
*        ls_response-grnyear = wa_it-ReferenceDocumentFiscalYear.

            READ TABLE it_tax INTO DATA(wa_tax) WITH KEY TaxCode = walines-TaxCode.
            wa_purchinvlines-taxcodename                = wa_tax-TaxCodeName.


            CONCATENATE walines-PurchaseOrder walines-PurchaseOrderItem INTO assignmentreference.

            SELECT SINGLE TaxItemAcctgDocItemRef, IN_HSNOrSACCode FROM i_operationalacctgdocitem
                WHERE OriginalReferenceDocument = @waheader-SupplierInvoiceWthnFiscalYear AND TaxItemAcctgDocItemRef is not INITIAL
                AND AccountingDocumentItemType <> 'T'
                AND FiscalYear = @walines-FiscalYear
                AND CompanyCode = @waheader-CompanyCode
                AND AccountingDocumentType = 'RE'
                AND AssignmentReference = @assignmentreference
                AND Material = @walines-material
            INTO  (  @DATA(lv_TaxItemAcctgDocItemRef), @DATA(lv_HSNCode) ).
            wa_purchinvlines-hsncode = lv_HSNCode.






************************************************ OLD GST CODE ***********************************************************
            SELECT  SINGLE AmountInCompanyCodeCurrency FROM i_operationalacctgdocitem
                WHERE OriginalReferenceDocument = @waheader-SupplierInvoiceWthnFiscalYear
                    AND TaxItemAcctgDocItemRef = @lv_TaxItemAcctgDocItemRef
                    AND AccountingDocumentItemType = 'T'
                    AND FiscalYear = @walines-FiscalYear
                    AND CompanyCode = @waheader-CompanyCode
                    AND TransactionTypeDetermination = 'JII'
            INTO  @wa_purchinvlines-igst.

            IF wa_purchinvlines-igst IS INITIAL.
                SELECT  SINGLE AmountInCompanyCodeCurrency FROM i_operationalacctgdocitem
                    WHERE OriginalReferenceDocument = @waheader-SupplierInvoiceWthnFiscalYear
                        AND TaxItemAcctgDocItemRef = @lv_TaxItemAcctgDocItemRef
                        AND AccountingDocumentItemType = 'T'
                        AND FiscalYear = @walines-FiscalYear
                        AND CompanyCode = @waheader-CompanyCode
                        AND TransactionTypeDetermination = 'JIC'
                INTO  @wa_purchinvlines-cgst.

                SELECT  SINGLE AmountInCompanyCodeCurrency FROM i_operationalacctgdocitem
                    WHERE OriginalReferenceDocument = @waheader-SupplierInvoiceWthnFiscalYear
                        AND TaxItemAcctgDocItemRef = @lv_TaxItemAcctgDocItemRef
                        AND AccountingDocumentItemType = 'T'
                        AND FiscalYear = @walines-FiscalYear
                        AND CompanyCode = @waheader-CompanyCode
                        AND TransactionTypeDetermination = 'JIS'
                INTO  @wa_purchinvlines-sgst.
            ENDIF.

""""""""""""""""""""""""""""""""""""""""""""""""for rate percent.
*            wa_purchinvlines-rateigst   = 0.
*            wa_purchinvlines-ratecgst   = 0.
*            wa_purchinvlines-ratesgst   = 0.

            IF walines-TaxCode = 'L0'.
                wa_purchinvlines-ratecgst   = 3.
                wa_purchinvlines-ratesgst   = 3.
            ELSEIF walines-TaxCode = 'I0'.
                wa_purchinvlines-rateigst   = 3.
            ELSEIF walines-TaxCode = 'L5'.
                wa_purchinvlines-ratecgst   = 5.
                wa_purchinvlines-ratesgst   = 5.
            ELSEIF walines-TaxCode = 'I1'.
                wa_purchinvlines-rateigst   = 5.
            ELSEIF walines-TaxCode = 'L2'.
                wa_purchinvlines-ratecgst   = 6.
                wa_purchinvlines-ratesgst   = 6.
            ELSEIF walines-TaxCode = 'I2'.
                wa_purchinvlines-rateigst   = 12.
            ELSEIF walines-TaxCode = 'L3'.
                wa_purchinvlines-ratecgst   = 9.
                wa_purchinvlines-ratesgst   = 9.
            ELSEIF walines-TaxCode = 'I3'.
                wa_purchinvlines-rateigst   = 18.
            ELSEIF walines-TaxCode = 'L4'.
                wa_purchinvlines-ratecgst   = 14.
                wa_purchinvlines-ratesgst   = 14.
            ELSEIF walines-TaxCode = 'I4'.
                wa_purchinvlines-rateigst   = 28.
            ELSEIF walines-TaxCode = 'F5'.
                wa_purchinvlines-ratecgst   = 9.
                wa_purchinvlines-ratesgst   = 9.
            ELSEIF walines-TaxCode = 'H5'.
                wa_purchinvlines-ratecgst   = 9.
                wa_purchinvlines-ratesgst   = 9.
                wa_purchinvlines-rateigst   = 18.
            ELSEIF walines-TaxCode = 'H6'.
                wa_purchinvlines-ratecgst   = 9.
*               ls_response-Ugstrate = '9'.
*               wa_purchinvlines-CESSRate = '18'.
            ELSEIF walines-TaxCode = 'H4'.
                wa_purchinvlines-rateigst   = 18.
*               ls_response-Ugstrate = '9'.
*               ls_response-CESSRate = '18'.
            ELSEIF walines-TaxCode = 'H3'.
                wa_purchinvlines-ratecgst   = 9.
*               ls_response-Ugstrate = '9'.
*               LS_RESPONSE-CESSRate = '18'.
            ELSEIF walines-TaxCode = 'J3'.
                wa_purchinvlines-ratecgst   = 9.
*               ls_response-Ugstrate = '9'.
*               LS_RESPONSE-CESSRate = '18'.
            ELSEIF walines-TaxCode = 'G6'.
                wa_purchinvlines-rateigst   = 18.
*               ls_response-Ugstrate = '9'.
*               ls_response-CESSRate = '18'.
            ELSEIF walines-TaxCode = 'G7'.
                wa_purchinvlines-ratecgst   = 9.
                wa_purchinvlines-ratesgst   = 9.
*               ls_response-CESSRate = '18'.
            ENDIF.
*****************************************************************************************************************






            SELECT SINGLE FROM I_JournalEntry
                FIELDS DocumentDate ,
                    DocumentReferenceID ,
                    IsReversed
            WHERE OriginalReferenceDocument = @walines-SupplierInvoice
            INTO (  @wa_purchinvlines-journaldocumentdate , @wa_purchinvlines-journaldocumentrefid, @wa_purchinvlines-isreversed ).

            wa_purchinvlines-pouom                      = walines-PurchaseOrderPriceUnit.
            wa_purchinvlines-poqty                      = walines-QuantityInPurchaseOrderUnit.
            wa_purchinvlines-netamount                  = walines-SupplierInvoiceItemAmount.
            wa_purchinvlines-basicrate                  = ROUND( val = wa_purchinvlines-netamount / wa_purchinvlines-poqty dec = 2 ).

            wa_purchinvlines-taxamount                  = wa_purchinvlines-igst + wa_purchinvlines-sgst +
                                                          wa_purchinvlines-cgst.
            wa_purchinvlines-totalamount                = wa_purchinvlines-taxamount + wa_purchinvlines-netamount.

            SELECT FROM I_SuplrInvcItemPurOrdRefAPI01 AS a
            FIELDS
                a~PurchaseOrderItem, a~SupplierInvoiceItem,a~SuplrInvcDeliveryCostCndnType,
                a~PurchaseOrder, a~SupplierInvoiceItemAmount, a~taxcode,
                a~FreightSupplier
            WHERE a~SupplierInvoice = @waheader-SupplierInvoice
              AND a~FiscalYear = @waheader-FiscalYear
              AND a~PurchaseOrderItem = @walines-PurchaseOrderItem
              AND a~SuplrInvcDeliveryCostCndnType <> ''
              INTO TABLE @DATA(ltsublines).

            wa_purchinvlines-discount       = 0.
            wa_purchinvlines-freight        = 0.
            wa_purchinvlines-insurance      = 0.
            wa_purchinvlines-ecs            = 0.
            wa_purchinvlines-epf            = 0.
            wa_purchinvlines-othercharges   = 0.
            wa_purchinvlines-packaging      = 0.
            LOOP AT ltsublines INTO DATA(wasublines).
                if wasublines-SuplrInvcDeliveryCostCndnType = 'FGW1'.
*                   Freight
                    wa_purchinvlines-freight += wasublines-SupplierInvoiceItemAmount.
                ELSEIF wasublines-SuplrInvcDeliveryCostCndnType = 'FQU1'.
*                   Freight
                    wa_purchinvlines-freight += wasublines-SupplierInvoiceItemAmount.
                ELSEIF wasublines-SuplrInvcDeliveryCostCndnType = 'FVA1'.
*                   Freight
                    wa_purchinvlines-freight += wasublines-SupplierInvoiceItemAmount.
                ELSEIF wasublines-SuplrInvcDeliveryCostCndnType = 'ZDIN'.
*                   Insurance Value
                    wa_purchinvlines-insurance += wasublines-SupplierInvoiceItemAmount.
                ELSEIF wasublines-SuplrInvcDeliveryCostCndnType = 'ZINS'.
*                   Insurance Value
                    wa_purchinvlines-insurance += wasublines-SupplierInvoiceItemAmount.
                ELSEIF wasublines-SuplrInvcDeliveryCostCndnType = 'ZECS'.
*                   ECS
                    wa_purchinvlines-ecs += wasublines-SupplierInvoiceItemAmount.
                ELSEIF wasublines-SuplrInvcDeliveryCostCndnType = 'ZEPF'.
*                   EPF
                    wa_purchinvlines-epf += wasublines-SupplierInvoiceItemAmount.
                ELSEIF wasublines-SuplrInvcDeliveryCostCndnType = 'ZOTH'.
*                   Other Charges
                    wa_purchinvlines-othercharges += wasublines-SupplierInvoiceItemAmount.
                ELSEIF wasublines-SuplrInvcDeliveryCostCndnType = 'ZPKG'.
*                   Packaging & Forwarding Charges
                    wa_purchinvlines-packaging += wasublines-SupplierInvoiceItemAmount.
                ELSE.
                    wa_purchinvlines-othercharges += wasublines-SupplierInvoiceItemAmount.
                ENDIF.
            ENDLOOP.

            wa_purchinvlines-totalamount    = wa_purchinvlines-taxamount + wa_purchinvlines-netamount + wa_purchinvlines-freight +
                                              wa_purchinvlines-insurance + wa_purchinvlines-ecs +
                                              wa_purchinvlines-epf + wa_purchinvlines-othercharges +
                                              wa_purchinvlines-packaging.

*       CLEAR wa_price.


            APPEND wa_purchinvlines TO lt_purchinvlines.
*            modify lt_purchinvlines from .
            CLEAR : wa_purchinvlines,walines.
            CLEAR : wa_po, wa_grn, wa_tax, lv_taxitemacctgdocitemref.
        ENDLOOP.

        INSERT zpurchinvlines FROM TABLE @lt_purchinvlines.


        wa_purchinvprocessed-client = SY-MANDT.
        wa_purchinvprocessed-supplierinvoice = waheader-SupplierInvoice.
        wa_purchinvprocessed-companycode = waheader-CompanyCode.
        wa_purchinvprocessed-fiscalyearvalue = waheader-FiscalYear.
        wa_purchinvprocessed-supplierinvoicewthnfiscalyear = waheader-SupplierInvoiceWthnFiscalYear.
        wa_purchinvprocessed-creationdatetime = lv_timestamp.
************************************** Header Level Fields Added *******************************
        wa_purchinvlines-plantadr = waheader-AddressID.


        APPEND wa_purchinvprocessed TO lt_purchinvprocessed.
        INSERT zpurchinvproc FROM TABLE @lt_purchinvprocessed.
        COMMIT WORK.

        CLEAR :  wa_purchinvprocessed, lt_purchinvprocessed, lt_purchinvlines.
        CLEAR : ltlines, it_product, it_po, it_grn, it_tax.

    ENDLOOP.

*    SELECT * FROM zbillinglines
*               INTO TABLE @DATA(it).
*    LOOP AT it INTO DATA(wa1).
*      out->write( data = 'Data : client -' ) .
*      out->write( data = wa1-client ) .
*      out->write( data = '- bukrs-' ) .
*      out->write( data = wa1-materialdescription ) .
*      out->write( data = '- doc-' ) .
*      out->write( data = wa1-billingdocument ) .
*      out->write( data = '- item -' ) .
*      out->write( data = wa1-billingdocumentitem ) .
*    endloop.


  ENDMETHOD.
ENDCLASS.
