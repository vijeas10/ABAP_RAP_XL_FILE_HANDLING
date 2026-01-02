CLASS zcl_string_to_xstring DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:convert_to_xstring
      IMPORTING iv_data           TYPE string
      RETURNING VALUE(rv_xstring) TYPE xstring.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_string_to_xstring IMPLEMENTATION.
  METHOD convert_to_xstring.
*         DATA(wl_xstring) = cl_bcs_convert=>string_to_xstring(
*                         iv_string     = iv_data
**                     iv_convert_cp = 'X'
*                         iv_codepage   = '4103'
*                         iv_add_bom    =  abap_false
*                       ).
*    CALL FUNCTION 'scms_string_to_xstring'

  ENDMETHOD.

ENDCLASS.
