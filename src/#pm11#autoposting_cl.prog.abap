*&---------------------------------------------------------------------*
*& Include          /PM11/AUTOPOSTING_CL
*&---------------------------------------------------------------------*

CLASS lcl_auto_post DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS: create_measurement_docs
      IMPORTING
        im_plant TYPE iwerk.

    CLASS-METHODS: release_work_orders
      IMPORTING
        im_plant TYPE werks_d.

    CLASS-METHODS: time_confirm_order
      IMPORTING
        im_plant TYPE werks_d.

    CLASS-METHODS: create_teco_orders
      IMPORTING
        im_plant TYPE werks_d.

    CLASS-METHODS: display_data.

ENDCLASS.

CLASS lcl_rfc_call DEFINITION.

  PUBLIC SECTION.

    METHODS: call_rfc_mdoc
      IMPORTING
                im_value                 TYPE rimr0-recdc
      RETURNING VALUE(out_measuring_doc) TYPE imrg-mdocm.


    DATA: l_mdocm1     TYPE imrg-mdocm.

ENDCLASS.

CLASS lcl_get_orders DEFINITION.

  PUBLIC SECTION.

    METHODS: read_order_tab
      IMPORTING
        im_werks       TYPE werks_d
        im_stat        TYPE j_istat.

ENDCLASS.
