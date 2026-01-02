CLASS lhc_xldata DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS Download_data FOR MODIFY
      IMPORTING keys FOR ACTION xldata~Download_data RESULT result.

ENDCLASS.

CLASS lhc_xldata IMPLEMENTATION.

  METHOD Download_data.
    DATA: wl_xstring TYPE xstring,
          wtl_string TYPE TABLE OF string.
*  Method to download template

    wtl_string = VALUE #( ( |PO Number,PO Item,GR Quantity,UoM SITE,Header Text | ) ).
*    Convert to XSTRING
*    DATA(wl_xstring) = cl_bcs_convert=>string_to_xstring(
*                         iv_string     = wl_header
**                     iv_convert_cp = 'X'
*                         iv_codepage   = '4103'
*                         iv_add_bom    =  abap_false
*                       ).

*
*    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
*     EXPORTING
*      text = wl_header
*      IMPORTING
*       buffer = wl_xstring .
      ENDMETHOD.


ENDCLASS.

*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
