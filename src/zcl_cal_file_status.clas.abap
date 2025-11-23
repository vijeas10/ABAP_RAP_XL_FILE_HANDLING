CLASS zcl_cal_file_status DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_sadl_exit .
    INTERFACES if_sadl_exit_calc_element_read .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_cal_file_status IMPLEMENTATION.


  METHOD if_sadl_exit_calc_element_read~calculate.

    DATA:wtl_user TYPE STANDARD TABLE OF zvije_c_xl_user.

    wtl_user = CORRESPONDING #( it_original_data ).
    LOOP AT wtl_user ASSIGNING FIELD-SYMBOL(<fs_user>).

      <fs_user>-Status_Criticality =  COND #(
                                              WHEN <fs_user>-FileStatus = 'File Not Selected' THEN '1'
                                              WHEN <fs_user>-FileStatus = 'File Selected'     THEN '2'
                                              WHEN <fs_user>-FileStatus = 'File Uploaded'     THEN '3'

                                              ).
    ENDLOOP.
*Send response back to Front End
    ct_calculated_data = CORRESPONDING #( wtl_user ).

  ENDMETHOD.


  METHOD if_sadl_exit_calc_element_read~get_calculation_info.
  ENDMETHOD.
ENDCLASS.
