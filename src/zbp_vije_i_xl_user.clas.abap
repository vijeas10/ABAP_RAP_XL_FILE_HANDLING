CLASS zbp_vije_i_xl_user DEFINITION PUBLIC ABSTRACT FINAL FOR BEHAVIOR OF zvije_i_xl_user.
  PUBLIC SECTION.

    TYPES : BEGIN OF t_s_excel,
              po_number       TYPE string,
              po_item         TYPE string,
              gr_quantity     TYPE string,
              unit_of_measure TYPE string,
              site_id         TYPE string,
              header_text     TYPE string,
              line_number     TYPE string,
              line_id         TYPE string,
            END OF t_s_excel.



ENDCLASS.



CLASS zbp_vije_i_xl_user IMPLEMENTATION.
ENDCLASS.
