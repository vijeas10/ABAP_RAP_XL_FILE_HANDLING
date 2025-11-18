@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection View XL  Data'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity zvije_c_xl_data

  as projection on zvije__xl_data
{
  key EndUser,
  key FileId,
  key LineId,
  key LineNumber,
      PoNumber,
      PoItem,
      GrQuantity,
      UnitOfMeasure,
      SiteId,
      HeaderText,
      /* Associations */
      _XLUser : redirected to parent zvije_c_xl_user
}
