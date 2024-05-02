*&---------------------------------------------------------------------*
*& Report /PM11/AUTOPOSTING
*&---------------------------------------------------------------------*
*&Created by: BLESSY
*&Date: 07-09-2022
*&Program Description: Automatic Posting of Data
*&---------------------------------------------------------------------*
REPORT /pm11/autoposting.

INCLUDE /pm11/autoposting_top.
INCLUDE /pm11/autoposting_sel.
INCLUDE /pm11/autoposting_cl.
INCLUDE /pm11/autoposting_impl.

START-OF-SELECTION.

  DATA(o_getdata)    = NEW lcl_auto_post( ).

* Create Measurement docs
  IF p_mdoc IS NOT INITIAL.
    o_getdata->create_measurement_docs( im_plant = p_plant ).
  ENDIF.

* Error message if either release and time confirm is not selected
*  IF  ( p_orders IS NOT INITIAL
*  AND   p_conf IS INITIAL ) OR
*      ( p_conf IS NOT INITIAL
*  AND  p_orders IS INITIAL ).
*    MESSAGE TEXT-e01 TYPE 'S' DISPLAY LIKE 'E'.
*    LEAVE LIST-PROCESSING.
*    LEAVE SCREEN.
*  ENDIF.

* Process WO for Release and Final Confirmation
*  IF  p_orders IS NOT INITIAL
*  AND p_conf IS NOT INITIAL.
*    g_found = abap_true.
*    o_getdata->release_work_orders( EXPORTING im_plant = p_plant ).
*    o_getdata->time_confirm_order( ).
*  ENDIF.

  IF  p_orders IS NOT INITIAL.
    o_getdata->release_work_orders( EXPORTING im_plant = p_plant ).
  ENDIF.

  IF p_conf IS NOT INITIAL.
    o_getdata->time_confirm_order( im_plant = p_plant ).
    g_found = abap_true.
  ENDIF.


* Process orders for Teco
  IF p_teco IS NOT INITIAL.
    o_getdata->create_teco_orders( im_plant = p_plant ).
  ENDIF.

  o_getdata->display_data( ).
