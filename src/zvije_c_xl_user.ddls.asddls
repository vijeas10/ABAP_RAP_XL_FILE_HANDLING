@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection View XL  User'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity zvije_c_xl_user
  provider contract transactional_query
  as projection on zvije_i_xl_user
{
  key     EndUser,
  key     FileId,
          FileStatus,
          @Semantics.largeObject: {
              mimeType: 'Mimetype',
              fileName: 'Filename',
          //          acceptableMimeTypes: [ 'text/csv' ],
              contentDispositionPreference: #INLINE
          }
          Attachment,
          Mimetype,
          Filename,
          LocalCreatedBy,
          LocalCreatedAt,
          LocalLastChangedBy,
          LocalLastChangedAt,
          LastChangedAt,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_CAL_FILE_STATUS'
  virtual Status_Criticality : abap.char( 1 ),
          /* Associations */
          _XLData : redirected to composition child zvije_c_xl_data
}
