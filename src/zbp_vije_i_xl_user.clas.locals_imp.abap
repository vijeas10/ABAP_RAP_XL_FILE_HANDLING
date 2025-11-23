CLASS lhc_XLHead DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR XLHead RESULT result.

    METHODS earlynumbering_create FOR NUMBERING
      IMPORTING entities FOR CREATE XLHead.

    METHODS uplaodExcelData FOR MODIFY
      IMPORTING keys FOR ACTION XLHead~uplaodExcelData RESULT result.

    METHODS FillFileStatus FOR DETERMINE ON MODIFY
      IMPORTING keys FOR XLHead~FillFileStatus.

    METHODS FillSelectedStatus FOR DETERMINE ON MODIFY
      IMPORTING keys FOR XLHead~FillSelectedStatus.
    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR XLHead RESULT result.
*    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
*      IMPORTING REQUEST requested_authorizations FOR XLHead RESULT result.

ENDCLASS.


CLASS lhc_XLHead IMPLEMENTATION.
  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD earlynumbering_create.
    DATA(wl_user) = cl_abap_context_info=>get_user_technical_name( ).
    LOOP AT entities ASSIGNING FIELD-SYMBOL(<fs_entity>).
      APPEND CORRESPONDING #( <fs_entity> ) TO mapped-xlhead ASSIGNING FIELD-SYMBOL(<fs_mapped>).
      IF <fs_mapped> IS ASSIGNED.
        <fs_mapped>-EndUser = wl_user.
        IF <fs_mapped>-FileId IS INITIAL.
          TRY.
              <fs_mapped>-FileId = cl_system_uuid=>create_uuid_x16_static( ).
            CATCH cx_uuid_error.
              " Update Error Message
              APPEND VALUE #( %cid = <fs_entity>-%cid

                              %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error
                                                            text     = 'Issue in Unique File ID Creation' ) )

                     TO reported-xlhead.
          ENDTRY.
        ENDIF.

      ELSE. " Update Error Message
        APPEND VALUE #( %cid = <fs_entity>-%cid

                        %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error
                                                      text     = 'Issue in Unique File ID Creation' ) )

               TO reported-xlhead.

      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD uplaodExcelData.
    DATA lo_table_descr  TYPE REF TO cl_abap_tabledescr.
    DATA lo_struct_descr TYPE REF TO cl_abap_structdescr.
    DATA wtl_excel       TYPE STANDARD TABLE OF zbp_vije_i_xl_user=>t_s_excel.
    DATA wtl_data        TYPE TABLE FOR CREATE zvije_i_xl_user\_XLData.
    DATA wl_index        TYPE sy-index.

    FIELD-SYMBOLS <fs_col_header> TYPE string.

    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(wl_user) = cl_abap_context_info=>get_user_technical_name( ).

    " Read current Entity's instance attachment
    READ ENTITIES OF zvije_i_xl_user IN LOCAL MODE
         ENTITY XLHead
         ALL FIELDS WITH CORRESPONDING #( keys )
         RESULT DATA(wtl_file_entity).
    DATA(wl_attachment) = wtl_file_entity[ 1 ]-Attachment.

    IF wl_attachment IS INITIAL.
      RETURN.
    ENDIF.
    "
    " Logic to fetch EXCEL data to internal table
    DATA(lo_xlsx) = xco_cp_xlsx=>document->for_file_content( iv_file_content = wl_attachment )->read_access( ).
    DATA(lo_worksheet) = lo_xlsx->get_workbook( )->worksheet->at_position( iv_position = 1 ).
    DATA(lo_selection_pattern) = xco_cp_xlsx_selection=>pattern_builder->simple_from_to( )->get_pattern( ).
    DATA(lo_execute) = lo_worksheet->select( io_pattern = lo_selection_pattern  )->row_stream( )->operation->write_to(
                                                                                    REF #( wtl_excel )   ).
    lo_execute->set_value_transformation( xco_cp_xlsx_read_access=>value_transformation->string_value  )->if_xco_xlsx_ra_operation~execute( ).

    " Get Number of Columns in EXCEL file

    TRY.
        lo_table_descr ?= cl_abap_tabledescr=>describe_by_data( p_data = wtl_excel ).
        lo_struct_descr ?= lo_table_descr->get_table_line_type( ).
        DATA(wl_no_of_cols) = lines( lo_struct_descr->components ).

      CATCH cx_sy_move_cast_error.

    ENDTRY.
    " Validate Excel Column Header
    DATA(wel_header) = wtl_excel[ 1 ].
    IF wel_header IS NOT INITIAL.

      DO wl_no_of_cols TIMES.

        wl_index = sy-index.
        ASSIGN COMPONENT wl_index OF STRUCTURE wel_header TO <fs_col_header>.
        IF <fs_col_header> IS NOT ASSIGNED.
          CONTINUE.
        ENDIF.

        DATA(wl_value) = to_upper( <fs_col_header> ).
        DATA(wl_error) = abap_false.
        CASE wl_index.
          WHEN 1.
            wl_error = COND #( WHEN wl_value <> 'PO NUMBER' THEN abap_true ELSE wl_error ).

          WHEN 2.
            wl_error = COND #( WHEN wl_value <> 'PO ITEM' THEN abap_true ELSE wl_error ).
          WHEN 3.
            wl_error = COND #( WHEN wl_value <> 'GR QUANTITY' THEN abap_true ELSE wl_error ).
          WHEN 4.
            wl_error = COND #( WHEN wl_value <> 'UOM' THEN abap_true ELSE wl_error ).
          WHEN 5.
            wl_error = COND #( WHEN wl_value <> 'SITE' THEN abap_true ELSE wl_error ).
          WHEN 6.
            wl_error = COND #( WHEN wl_value <> 'HEADER TEXT' THEN abap_true ELSE wl_error ).
          WHEN 9.
            wl_error = abap_true.
        ENDCASE.

        IF wl_error IS NOT INITIAL.
          APPEND VALUE #( %tky = wtl_file_entity[ 1 ]-%tky ) TO failed-xlhead.
          APPEND VALUE #( %tky = wtl_file_entity[ 1 ]-%tky
                          %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error
                                                        text     = 'Wrong File Format!!' ) )

                 TO reported-xlhead.
          UNASSIGN <fs_col_header>.
          EXIT.

        ENDIF.

        UNASSIGN <fs_col_header>.

      ENDDO.

    ENDIF.
*    CHECK wl_error IS INITIAL.
    DELETE wtl_excel INDEX 1.
    DELETE wtl_excel WHERE po_number IS INITIAL.
    TRY.
        DATA(wl_uuid) = cl_system_uuid=>create_uuid_x16_static( ).
        LOOP AT wtl_excel ASSIGNING FIELD-SYMBOL(<fs_excel>).
          <fs_excel>-line_id     = wl_uuid.
          <fs_excel>-line_number = sy-tabix.
        ENDLOOP.

      CATCH cx_uuid_error.

    ENDTRY.
    " Child entity data preparation for creation
    wtl_data = VALUE #( ( %cid_ref  = keys[ 1 ]-%cid_ref
                          %is_draft = keys[ 1 ]-%is_draft
                          FileId    = keys[ 1 ]-FileId
                          EndUser   = keys[ 1 ]-EndUser
                          %target   = VALUE #(  FOR wel_excel IN wtl_excel
                                               ( %cid      = keys[ 1 ]-%cid_ref
                                                 %is_draft = keys[ 1 ]-%is_draft
                                                 %data     = VALUE #( EndUser       = keys[ 1 ]-EndUser
                                                                      FileId        = keys[ 1 ]-FileId
                                                                      GrQuantity    = wel_excel-gr_quantity
                                                                      HeaderText    = wel_excel-header_text
                                                                      LineId        = wel_excel-line_id
                                                                      LineNumber    = wel_excel-po_number
                                                                      PoItem        = wel_excel-po_item
                                                                      PoNumber      = wel_excel-po_number
                                                                      SiteId        = wel_excel-site_id
                                                                      UnitOfMeasure = wel_excel-unit_of_measure  )
                                                 %control  = VALUE #( EndUser    = if_abap_behv=>mk-on
                                                                      FileId     = if_abap_behv=>mk-on
                                                                      GrQuantity = if_abap_behv=>mk-on
                                                                      HeaderText = if_abap_behv=>mk-on
                                                                      LineId     = if_abap_behv=>mk-on
                                                                      LineNumber = if_abap_behv=>mk-on
                                                                      PoItem     = if_abap_behv=>mk-on
                                                                      PoNumber   = if_abap_behv=>mk-on
                                                                      SiteId     = if_abap_behv=>mk-on   ) ) )  ) ).
    " Read Child entity existing data data
    READ ENTITIES OF zvije_i_xl_user IN LOCAL MODE
         ENTITY XLHead BY \_XLData
*    ENTITY xldata
         ALL FIELDS WITH CORRESPONDING #( keys )
         RESULT DATA(wtl_child).
    " Delete Existing entry if exist for child
    IF wtl_child IS NOT INITIAL.
      MODIFY ENTITIES OF zvije_i_xl_user IN LOCAL MODE
             ENTITY xldata
             DELETE FROM VALUE #( FOR wel_child IN wtl_child
                                  ( %key      = wel_child-%key
                                    %is_draft = wel_child-%is_draft ) )
     " TODO: variable is assigned but never used (ABAP cleaner)
             MAPPED DATA(wtl_del_mapped)
    " TODO: variable is assigned but never used (ABAP cleaner)
             FAILED DATA(wtl_del_failed)
             " TODO: variable is assigned but never used (ABAP cleaner)
             REPORTED DATA(wtl_del_reported).

    ENDIF.

    " Create Child entry data(Association data)
    MODIFY ENTITIES OF zvije_i_xl_user IN LOCAL MODE
           ENTITY XLHead CREATE BY \_XLData
           AUTO FILL CID WITH wtl_data.

    " Modify Status
    MODIFY ENTITIES OF zvije_i_xl_user IN LOCAL MODE
           ENTITY XLHead
           UPDATE FROM VALUE #( ( %tky                = wtl_file_entity[ 1 ]-%tky
                                  FileStatus          = 'File Uploaded'
                                  %control-FileStatus = if_abap_behv=>mk-on ) )
              " TODO: variable is assigned but never used (ABAP cleaner)
           MAPPED DATA(wtl_upd_mapped)
           " TODO: variable is assigned but never used (ABAP cleaner)
           FAILED DATA(wtl_upd_failed)
           " TODO: variable is assigned but never used (ABAP cleaner)
           REPORTED DATA(wtl_upd_reported).

    " Read updated Entry

    READ ENTITIES OF zvije_i_xl_user
         IN LOCAL MODE
         ENTITY XLHead ALL FIELDS WITH CORRESPONDING #( keys )
         RESULT DATA(wtl_updated_data).

    " Send the response back to front end
    result = VALUE #( FOR wel_result IN wtl_updated_data
                      ( %tky   = wel_result-%tky
*                          %is_draft = wel_result-%is_draft
                        %param = wel_result ) ).

*Provide Success Message
    CHECK wtl_del_failed IS INITIAL AND wtl_upd_failed IS INITIAL.
    APPEND VALUE #( %tky = wtl_file_entity[ 1 ]-%tky
                     %msg = new_message_with_text( severity = if_abap_behv_message=>severity-success
                                                               text     = 'File Uploaded Successfully'
                     ) ) TO reported-xlhead.



  ENDMETHOD.

  METHOD FillFileStatus.
    " Updating file Status while file creation through determination
    READ ENTITIES OF zvije_i_xl_user IN LOCAL MODE
         ENTITY XLHead
         FIELDS ( EndUser FileStatus )
         WITH CORRESPONDING #( keys )
         RESULT DATA(wtl_filestatus).

    LOOP AT wtl_filestatus ASSIGNING FIELD-SYMBOL(<fs_filestatus>).

      MODIFY ENTITIES OF zvije_i_xl_user IN LOCAL MODE
             ENTITY XLHead
             UPDATE FIELDS ( FileStatus )
             WITH VALUE #( ( %tky                = <fs_filestatus>-%tky
                             %data-FileStatus    = 'File Not Selected'
                             %control-FileStatus = if_abap_behv=>mk-on ) ).

    ENDLOOP.
  ENDMETHOD.

  METHOD FillSelectedStatus.
    " Updating file Status while file Selection through determination
    READ ENTITIES OF zvije_i_xl_user IN LOCAL MODE
         ENTITY XLHead
         FIELDS ( EndUser FileStatus )
         WITH CORRESPONDING #( keys )
         RESULT DATA(wtl_filestatus).

    LOOP AT wtl_filestatus ASSIGNING FIELD-SYMBOL(<fs_filestatus>).

      MODIFY ENTITIES OF zvije_i_xl_user IN LOCAL MODE
             ENTITY XLHead
             UPDATE FIELDS ( FileStatus )
             WITH VALUE #( ( %tky                = <fs_filestatus>-%tky
                             %data-FileStatus    = COND #( WHEN <fs_filestatus>-Attachment IS INITIAL
                                                           THEN 'File Not Selected'
                                                           ELSE 'File Selected'  )
                             %control-FileStatus = if_abap_behv=>mk-on ) ).

    ENDLOOP.
  ENDMETHOD.
*  METHOD get_global_authorizations.
*  ENDMETHOD.

  METHOD get_instance_features.

    READ ENTITIES OF zvije_i_xl_user IN LOCAL MODE
    ENTITY XLHead
    ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(wtl_result).

    LOOP AT wtl_result ASSIGNING FIELD-SYMBOL(<fs_result>).

      APPEND VALUE #(
                              %tky  = <fs_result>-%tky
                              %action-uplaodexceldata = COND #( WHEN <fs_result>-FileStatus = 'File Uploaded'
                                                                THEN if_abap_behv=>fc-o-disabled
                                                                WHEN <fs_result>-%is_draft = if_abap_behv=>mk-on
                                                                THEN if_abap_behv=>fc-o-disabled
                                                                )
                     ) TO result.

    ENDLOOP.



  ENDMETHOD.

ENDCLASS.
