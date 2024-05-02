*&---------------------------------------------------------------------*
*& Include          /PM11/AUTOPOSTING_IMPL
*&---------------------------------------------------------------------*
CLASS lcl_auto_post IMPLEMENTATION.

  METHOD create_measurement_docs.

    DATA: lt_excl_point TYPE RANGE OF imrc_point.
    DATA: lcl_random  TYPE REF TO cl_abap_random_int.
    DATA: lv_begda      TYPE begda,
          lv_est        TYPE /pm11/s_ltf_point,
          lv_error      TYPE bapiret2_t,
          lv_pyear_char TYPE i,
          lv_pyear_val  TYPE crmt_mpk_value_visible_ui,
          lv_value      TYPE f,
          lv_unit       TYPE crmt_mpk_unit,
          lv_anzdz      TYPE crmt_mpk_decimals,
          lv_rec_val    TYPE rimr0-recdc,
          lv_no_value   TYPE rimr0-recdc,
          lv_min        TYPE i,
          lv_max        TYPE i,
          lv_random     TYPE i,
          lv_ops        TYPE p LENGTH 16 DECIMALS 2,
          lv_mread      TYPE p LENGTH 16 DECIMALS 2,
          lv_mread_2    TYPE p LENGTH 16 DECIMALS 2,
          lv_val        TYPE p LENGTH 16 DECIMALS 2,
          lv_int_min    TYPE i,
          lv_int_max    TYPE i,
          lv_ran_int    TYPE qf00-ran_int,
          lv_int_read   TYPE i.

    DATA: r_plant TYPE RANGE OF werks_d,
          r_wkctr TYPE RANGE OF arbpl.

    SELECT a~tplnr,
           a~iwerk,
           a~objnr,
           a~lgwid,
           b~objid,
           b~arbpl,
           b~werks
      FROM iflot AS a
      INNER JOIN crhd AS b ON a~lgwid = b~objid
      INTO TABLE @DATA(lt_iflot)
      WHERE iwerk = @im_plant.

    IF sy-subrc EQ 0.

      SORT lt_iflot BY tplnr iwerk.

      IF p_wkctr IS NOT INITIAL.
        DELETE lt_iflot WHERE arbpl NE p_wkctr.
      ENDIF.
      IF p_werks IS NOT INITIAL.
        DELETE lt_iflot WHERE werks NE p_werks.
      ENDIF.

    ENDIF.

    IF lt_iflot IS NOT INITIAL.

      SELECT FROM imptt
      FIELDS point, mpobj, inact, indct
         FOR ALL ENTRIES IN @lt_iflot
       WHERE mpobj = @lt_iflot-objnr
         AND inact = @space
         AND indct = @abap_true
        INTO TABLE @DATA(lt_imptt).

      IF sy-subrc EQ 0.
        SORT lt_imptt BY point.
      ENDIF.

    ENDIF.

    IF lt_imptt IS NOT INITIAL.

      SELECT FROM imrg
      FIELDS mdocm, point, idate, recdu
         FOR ALL ENTRIES IN @lt_imptt
       WHERE point = @lt_imptt-point
        INTO TABLE @DATA(lt_imrg).

      IF sy-subrc EQ 0.
        SORT lt_imrg BY point idate DESCENDING.
      ENDIF.

    ENDIF.


*   "Check if Measurement Document already exist for current date
*   "Do not process if found
    lt_excl_point =  VALUE #( FOR ls_imrg IN lt_imrg
                              WHERE ( idate = sy-datum )
                              ( sign   = 'I'
                                option = 'EQ'
                                low    = ls_imrg-point ) ).
    IF lt_excl_point IS NOT INITIAL.
      DELETE ADJACENT DUPLICATES FROM lt_excl_point.
      DELETE lt_imrg WHERE point IN lt_excl_point.
    ENDIF.
    DELETE ADJACENT DUPLICATES FROM lt_imrg COMPARING point.

    LOOP AT lt_imrg ASSIGNING FIELD-SYMBOL(<fs_imrg>).

      "Generate random number for interval reading where
      "1 = 20@ chance, 2 = 40% annual estimate, 3 = 50% - 150%
      CLEAR: lv_int_max, lv_int_min, lv_int_read.

      lv_int_max = 3.
      lv_int_min = 1.

      CALL FUNCTION 'QF05_RANDOM_INTEGER'
        EXPORTING
          ran_int_max   = lv_int_max
          ran_int_min   = lv_int_min
        IMPORTING
          ran_int       = lv_int_read
        EXCEPTIONS
          invalid_input = 1
          OTHERS        = 2.
      IF sy-subrc EQ 0.
      ENDIF.

      gv_point = <fs_imrg>-point.
      lv_begda = <fs_imrg>-idate.

*     "Get annual estimate and Last Measurement Date
      /pm11/cl_ltf_estimates=>read_annual_estimate(
        EXPORTING
          iv_point  = gv_point
          iv_begda  = lv_begda
        IMPORTING
          es_est    = lv_est
          mt_errors = lv_error
          ) .

      IF NOT lv_est IS INITIAL.

        DATA(lv_date_diff) = sy-datum - lv_est-idate.

*       "Convert FLTP value to character
        lv_value = lv_est-pyear.
        lv_unit  = lv_est-mrngu.
        lv_anzdz = lv_est-anzdz.

        CALL FUNCTION 'CONVERT_VALUE_FLTP_TO_CHAR'
          EXPORTING
            iv_value       = lv_est-pyear
            iv_unit        = lv_est-mrngu
            iv_decimals    = lv_est-anzdz
          IMPORTING
            ev_value       = lv_pyear_val
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.
        IF sy-subrc EQ 0.
          lv_pyear_char = lv_pyear_val.
        ENDIF.

        DATA(o_rfc_call) = NEW lcl_rfc_call( ).

        "Create Measurement Document
        "20% Chance - Zero
        IF lv_int_read EQ 1.
          lv_no_value = '0'.
          o_rfc_call->call_rfc_mdoc( im_value = lv_no_value ).
        ENDIF.

*       "40% Chance
*       "Equal to the Annual Estimate / # of days since last MDOC
        IF lv_int_read EQ 2.
          TRY.
              lv_mread = lv_pyear_char * lv_date_diff.
            CATCH cx_sy_zerodivide.
          ENDTRY.
          CLEAR: lv_rec_val.
          lv_rec_val = lv_mread.

          o_rfc_call->call_rfc_mdoc( im_value = lv_rec_val ).
        ENDIF.


        "40% Chance
        IF lv_int_read EQ 3.

*          lv_min = 1. "Assign 1 if -50%
*          lv_max = 2. "Assign 2 if +50%
*
*        TRY.
*            lcl_random = cl_abap_random_int=>create( min = lv_min
*                                        max = lv_max ).
*          CATCH cx_abap_random.
*        ENDTRY.
*        lv_random = lcl_random->get_next( ).
*
**       "Equal to the Annual Estimate +/- 50% / # of days since last MDOC
*        lv_ops = COND string( WHEN lv_random = 1 THEN - ( 50 / 100 )
*                              WHEN lv_random = 2 THEN ( 50 / 100 ) ).

          "Get random int between 50 - 150
          CLEAR: lv_min, lv_max, lv_ran_int.

          lv_min = 50.
          lv_max = 150.

          CALL FUNCTION 'QF05_RANDOM_INTEGER'
            EXPORTING
              ran_int_max   = lv_max
              ran_int_min   = lv_min
            IMPORTING
              ran_int       = lv_ran_int
            EXCEPTIONS
              invalid_input = 1
              OTHERS        = 2.
          IF sy-subrc EQ 0.
            lv_ops = lv_ran_int / 100.
          ENDIF.

          TRY.
              lv_val     = lv_pyear_char * lv_ops.
*              lv_mread_2 = ( lv_pyear_char + lv_val ) / lv_date_diff.
              lv_mread_2 = lv_val / lv_date_diff.
            CATCH cx_sy_zerodivide.
          ENDTRY.
          CLEAR: lv_rec_val.
          lv_rec_val = lv_mread_2.

          o_rfc_call->call_rfc_mdoc( im_value = lv_rec_val ).

        ENDIF.
      ENDIF.

      CLEAR: gv_point, lv_begda, lv_val, lv_mread_2, lv_rec_val.
    ENDLOOP.

    TRY.
        i_displ_mdoc = CORRESPONDING #( i_mdoc_final MAPPING mdoc = mdocm ).
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.


  METHOD release_work_orders.

    DATA: bapi_msg TYPE bapiret2-message.

    DATA: lt_methods TYPE TABLE OF bapi_alm_order_method,
          lt_return  TYPE TABLE OF bapiret2.
    DATA: lt_jest    TYPE RANGE OF objnr.
    DATA: lt_release_orders TYPE STANDARD TABLE OF ty_out_operation.

    DATA(lv_counter) = 1.
    DATA(lv_created) = 'I0001'.
    DATA(o_get_created_orders) = NEW lcl_get_orders( ).

*   "Retrieve from Order master data using Plant with status = CRTD
    o_get_created_orders->read_order_tab( im_werks       = im_plant
                                          im_stat        = lv_created ).

*   "Remove all entries with release rejected
    lt_jest = VALUE #( FOR out_jest IN g_out_jest ( sign = 'I' option = 'EQ' low = out_jest-objnr ) ).
    lt_release_orders[] = g_out_operation[].

    SORT lt_release_orders BY aufnr vornr DESCENDING.

    IF lt_jest IS NOT INITIAL.
      DELETE lt_release_orders WHERE objnr IN lt_jest.
    ENDIF.

    "Prepare table for time confirmed orders.
    i_confirmed_orders[] = VALUE #( FOR line_conf IN lt_release_orders ( aufnr = line_conf-aufnr
                                                                         arbei = line_conf-arbei
                                                                         vornr = line_conf-vornr ) ).

    DELETE ADJACENT DUPLICATES FROM lt_release_orders COMPARING aufnr.

*   Prepare table for BAPI call
    LOOP AT lt_release_orders ASSIGNING FIELD-SYMBOL(<fs_release_ord>).

      DATA(lv_aufnr) = <fs_release_ord>-aufnr.

      FREE: lt_methods, lt_return.

      APPEND VALUE #( refnumber  = lv_counter
                      objecttype = 'HEADER'
                      method     = 'RELEASE'
                      objectkey  = lv_aufnr )
                TO lt_methods.

      APPEND VALUE #( refnumber  = lv_counter
                      objecttype = space
                      method     = 'SAVE'
                      objectkey  = space )
                TO lt_methods.

*        APPEND VALUE #( aufnr = <fs_release_ord>-aufnr
*                        arbei = <fs_release_ord>-arbei
*                        vornr = <fs_release_ord>-vornr ) TO i_released.

      lv_counter = lv_counter + 1.

      "Call BAPI to release order
      CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
        TABLES
          it_methods = lt_methods
          return     = lt_return.

      "Commit Bapi if successful; Rollback if Error.
      IF line_exists( lt_return[ type = 'E' ] ).
        i_bapi_work_order = VALUE #( FOR ls_return_err IN lt_return WHERE ( id = 'C2' )
                                  ( message = ls_return_err-message ) ).
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ELSE.
        i_bapi_work_order = VALUE #( FOR ls_return_success IN lt_return WHERE ( id = 'CO' )
                                  ( message = ls_return_success-message ) ).
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.
      ENDIF.

      "Move return message to i_work_order for report display
      APPEND LINES OF i_bapi_work_order TO i_work_order.

      FREE: i_bapi_work_order.

    ENDLOOP.

  ENDMETHOD.

  METHOD time_confirm_order.

    DATA: lt_timeconfirmation TYPE TABLE OF bapi_alm_timeconfirmation,
          lt_return_conf_msg  TYPE TABLE OF bapi_alm_return.
    DATA: lv_ran_int_conf TYPE qf00-ran_int.

    DATA: lv_work TYPE i.

    IF g_out_operation IS NOT INITIAL.

      DATA(lv_min_op) = 1.
      DATA(lv_max_op) = 5.

      TRY.
          DATA(lcl_random) = cl_abap_random_int=>create( min = lv_min_op
                                                         max = lv_max_op ).
        CATCH cx_abap_random.
      ENDTRY.

      SORT i_confirmed_orders BY aufnr arbei vornr.
      DELETE ADJACENT DUPLICATES FROM i_confirmed_orders COMPARING ALL FIELDS.
*     "Process only orders that were successfuly released
*      LOOP AT i_released INTO DATA(ls_released).
      LOOP AT i_confirmed_orders INTO DATA(ls_released).

        DATA(lv_random_op) = lcl_random->get_next( ).
        DATA(lv_activity)  = ls_released-arbei.
        lv_work            = lv_activity + lv_random_op - 2.
        IF lv_work LT 0.
          lv_work = 1.
        ENDIF.

        "Time confirmation probabilities for Start Date
        DATA(lv_min_conf) = 1.
        DATA(lv_max_conf) = 4.

        CALL FUNCTION 'QF05_RANDOM_INTEGER'
          EXPORTING
            ran_int_max   = lv_max_conf
            ran_int_min   = lv_min_conf
          IMPORTING
            ran_int       = lv_ran_int_conf
          EXCEPTIONS
            invalid_input = 1
            OTHERS        = 2.
        IF sy-subrc EQ 0.
          CASE lv_ran_int_conf.
            WHEN 1. "basic start date = today's date
              DATA(lv_basic_start_date) = sy-datum.
            WHEN 2. "Basic Start Date equals yesterday
              lv_basic_start_date = sy-datum - 1.
            WHEN 3. "Basic Start Date equals 2 days ago.
              lv_basic_start_date = sy-datum - 2.
            WHEN 4. "Basic Start Date equals 3 days ago.
              lv_basic_start_date = sy-datum - 3.
          ENDCASE.
        ENDIF.

        FREE: lt_timeconfirmation, lt_return_conf_msg.
*      "Prepare table for BAPI call
        APPEND VALUE #( orderid         = ls_released-aufnr
                        operation       = ls_released-vornr
                        fin_conf        = abap_true
                        postg_date      = sy-datum
                        exec_start_date = lv_basic_start_date
                        exec_fin_date   = lv_basic_start_date
                        act_work_2      = lv_work ) TO lt_timeconfirmation.

        CALL FUNCTION 'BAPI_ALM_CONF_CREATE'
          TABLES
            timetickets   = lt_timeconfirmation
            detail_return = lt_return_conf_msg.

        IF line_exists( lt_return_conf_msg[ type = 'I' ] ).
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ENDIF.

*     "Move return message to i_final_order for report display
        i_bapi_timeconf  = VALUE #( FOR line IN lt_return_conf_msg
*                                ( conf_msg = |{ line-message_v1 }| & | : | & |{  line-message }| ) ).
                                ( conf_msg = |{ ls_released-aufnr }| & | | & | { ls_released-vornr }| & | : | & |{  line-message }| ) ).
        i_timeconf = VALUE #( FOR order_line IN lt_return_conf_msg WHERE ( type = 'I' )
                                     ( aufnr = order_line-message_v1 ) ).

        APPEND LINES OF i_bapi_timeconf TO i_conf_msg.
        APPEND LINES OF i_timeconf TO i_final_order.

        FREE: i_bapi_timeconf, i_timeconf.
        CLEAR: lv_min_conf, lv_max_conf, lv_ran_int_conf, lv_basic_start_date.

      ENDLOOP.

    ELSE.

*   "Retrieve from Order master data using Plant with status = REL
      DATA: lt_teco TYPE RANGE OF aufnr.
      DATA: lt_cnf  TYPE RANGE OF aufnr.
      DATA: lt_rel  TYPE RANGE OF aufnr.

      DATA(lv_werks) = im_plant.
      DATA(lv_rel) = 'I0002'.
      g_tc_flag = abap_true.
      DATA(o_get_rel_orders) = NEW lcl_get_orders( ).
      o_get_rel_orders->read_order_tab( im_werks       = im_plant
                                        im_stat        = lv_rel ).

*     "Remove TECO orders
      lt_teco = VALUE #( FOR ls_ops IN g_out_operation WHERE ( stat = 'I0045' )
                                      ( sign   = 'I'
                                        option = 'EQ'
                                        low    = ls_ops-aufnr ) ).
      IF lt_teco IS NOT INITIAL.
        DELETE ADJACENT DUPLICATES FROM lt_teco.
        DELETE g_out_operation WHERE aufnr IN lt_teco.
      ENDIF.

*     "Remove CNF orders
      lt_cnf = VALUE #( FOR ls_ops IN g_out_operation WHERE ( stat = 'I0009' )
                                      ( sign   = 'I'
                                        option = 'EQ'
                                        low    = ls_ops-aufnr ) ).
      IF lt_cnf IS NOT INITIAL.
        DELETE ADJACENT DUPLICATES FROM lt_cnf.
        DELETE g_out_operation WHERE aufnr IN lt_cnf.
      ENDIF.

      "Retain REL orders only
      lt_rel = VALUE #( FOR ls_ops IN g_out_operation WHERE ( stat = lv_rel )
                                      ( sign   = 'I'
                                        option = 'EQ'
                                        low    = ls_ops-aufnr ) ).
      IF lt_rel IS NOT INITIAL.
        DELETE ADJACENT DUPLICATES FROM lt_rel.
        DELETE g_out_operation WHERE aufnr NOT IN lt_rel.
      ENDIF.

      DELETE g_out_operation WHERE stat NE lv_rel.

      "Prepare table for time confirmed orders.
      i_confirmed_orders[] = VALUE #( FOR line_conf IN g_out_operation ( aufnr = line_conf-aufnr
                                                                         arbei = line_conf-arbei
                                                                         vornr = line_conf-vornr ) ).
      SORT i_confirmed_orders BY aufnr vornr.

      lv_min_op = 1.
      lv_max_op = 5.

      TRY.
          lcl_random = cl_abap_random_int=>create( min = lv_min_op
                                                   max = lv_max_op ).
        CATCH cx_abap_random.
      ENDTRY.

      LOOP AT i_confirmed_orders INTO ls_released.

        lv_random_op = lcl_random->get_next( ).
        lv_activity  = ls_released-arbei.
        lv_work            = lv_activity + lv_random_op - 2.
        IF lv_work LT 0.
          lv_work = 1.
        ENDIF.

        "Time confirmation probabilities for Start Date
        lv_min_conf = 1.
        lv_max_conf = 4.

        CALL FUNCTION 'QF05_RANDOM_INTEGER'
          EXPORTING
            ran_int_max   = lv_max_conf
            ran_int_min   = lv_min_conf
          IMPORTING
            ran_int       = lv_ran_int_conf
          EXCEPTIONS
            invalid_input = 1
            OTHERS        = 2.
        IF sy-subrc EQ 0.
          CASE lv_ran_int_conf.
            WHEN 1. "basic start date = today's date
              lv_basic_start_date = sy-datum.
            WHEN 2. "Basic Start Date equals yesterday
              lv_basic_start_date = sy-datum - 1.
            WHEN 3. "Basic Start Date equals 2 days ago.
              lv_basic_start_date = sy-datum - 2.
            WHEN 4. "Basic Start Date equals 3 days ago.
              lv_basic_start_date = sy-datum - 3.
          ENDCASE.
        ENDIF.

        FREE: lt_timeconfirmation, lt_return_conf_msg.
*      "Prepare table for BAPI call
        APPEND VALUE #( orderid         = ls_released-aufnr
                        operation       = ls_released-vornr
                        fin_conf        = abap_true
                        postg_date      = sy-datum
                        exec_start_date = lv_basic_start_date
                        exec_fin_date   = lv_basic_start_date
                        act_work_2      = lv_work ) TO lt_timeconfirmation.

        CALL FUNCTION 'BAPI_ALM_CONF_CREATE'
          TABLES
            timetickets   = lt_timeconfirmation
            detail_return = lt_return_conf_msg.

        IF line_exists( lt_return_conf_msg[ type = 'I' ] ).
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ENDIF.

*     "Move return message to i_final_order for report display
        i_bapi_timeconf  = VALUE #( FOR line IN lt_return_conf_msg
*                                ( conf_msg = |{ line-message_v1 }| & | : | & |{  line-message }| ) ).
                                ( conf_msg = |{ ls_released-aufnr }| & | | & | { ls_released-vornr }| & | : | & |{  line-message }| ) ).
        i_timeconf = VALUE #( FOR order_line IN lt_return_conf_msg WHERE ( type = 'I' )
                                     ( aufnr = order_line-message_v1 ) ).

        APPEND LINES OF i_bapi_timeconf TO i_conf_msg.
        APPEND LINES OF i_timeconf TO i_final_order.

        FREE: i_bapi_timeconf, i_timeconf.
        CLEAR: lv_min_conf, lv_max_conf, lv_ran_int_conf, lv_basic_start_date.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD create_teco_orders.

    DATA: lt_teco         TYPE RANGE OF aufnr,
          lt_cnf          TYPE RANGE OF aufnr,
          lt_methods_teco TYPE TABLE OF bapi_alm_order_method,
          lt_header       TYPE TABLE OF bapi_alm_order_headers_i,
          lt_return_teco  TYPE TABLE OF bapiret2.

*   "Process final confimation orders
    IF g_found IS NOT INITIAL.

      LOOP AT i_final_order ASSIGNING FIELD-SYMBOL(<fs_final_order>).

        FREE: lt_methods_teco, lt_header, lt_return_teco.

        APPEND VALUE #( refnumber  = 1
                        objecttype = 'HEADER'
                        method     = 'TECHNICALCOMPLETE'
                        objectkey  = <fs_final_order>-aufnr )
                  TO lt_methods_teco.

        APPEND VALUE #( refnumber  = 1
                        objecttype = space
                        method     = 'SAVE'
                        objectkey  = space )
                  TO lt_methods_teco.

        APPEND VALUE #( orderid  = <fs_final_order>-aufnr ) TO lt_header.

        CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
          TABLES
            it_methods = lt_methods_teco
            it_header  = lt_header
            return     = lt_return_teco.

        IF line_exists( lt_return_teco[ id = 'IW' ] ).
          i_bapi_teco = VALUE #( FOR line IN lt_return_teco WHERE ( id = 'IW')
                       ( teco_msg = line-message ) ).
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ENDIF.

        APPEND LINES OF i_bapi_teco TO i_teco_msg.

        FREE i_bapi_teco.

      ENDLOOP.

    ELSE.

*     "If only Teco Work Orders is selected in the sel.screen,
*     "search all order with CNF status
      DATA(lv_werks) = im_plant.
      DATA(lv_cnf)  = 'I0009'.
      DATA(lv_teco) = 'I0045'.

      DATA(o_get_conf_ord) = NEW lcl_get_orders( ).
      o_get_conf_ord->read_order_tab( im_werks       = lv_werks
                                      im_stat        = lv_cnf ).

*     "Remove TECO orders
      lt_teco = VALUE #( FOR ls_ops IN g_out_operation WHERE ( stat = lv_teco )
                                      ( sign   = 'I'
                                        option = 'EQ'
                                        low    = ls_ops-aufnr ) ).

      IF lt_teco IS NOT INITIAL.
        DELETE ADJACENT DUPLICATES FROM lt_teco.
        DELETE g_out_operation WHERE aufnr IN lt_teco.
      ENDIF.

*     "Retain CNF orders only
      lt_cnf = VALUE #( FOR ls_ops IN g_out_operation WHERE ( stat = lv_cnf )
                                      ( sign   = 'I'
                                        option = 'EQ'
                                        low    = ls_ops-aufnr ) ).

      IF lt_cnf IS NOT INITIAL.
        DELETE ADJACENT DUPLICATES FROM lt_cnf.
        DELETE g_out_operation WHERE aufnr NOT IN lt_cnf.
      ENDIF.

      DELETE ADJACENT DUPLICATES FROM g_out_operation COMPARING aufnr.

      LOOP AT g_out_operation ASSIGNING FIELD-SYMBOL(<fs_out_teco>).

        FREE: lt_methods_teco, lt_header, lt_return_teco.

        APPEND VALUE #( refnumber  = 1
                        objecttype = 'HEADER'
                        method     = 'TECHNICALCOMPLETE'
                        objectkey  = <fs_out_teco>-aufnr )
                  TO lt_methods_teco.

        APPEND VALUE #( refnumber  = 1
                        objecttype = space
                        method     = 'SAVE'
                        objectkey  = space )
                  TO lt_methods_teco.

        APPEND VALUE #( orderid  = <fs_out_teco>-aufnr ) TO lt_header.

        CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
          TABLES
            it_methods = lt_methods_teco
            it_header  = lt_header
            return     = lt_return_teco.

        IF line_exists( lt_return_teco[ id = 'IW' ] ).
          i_bapi_teco = VALUE #( FOR line IN lt_return_teco WHERE ( id = 'IW')
                                           ( teco_msg = line-message ) ).
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ENDIF.

        APPEND LINES OF i_bapi_teco TO i_teco_msg.
      ENDLOOP.

    ENDIF.
  ENDMETHOD.

  METHOD display_data.

    IF p_mdoc IS NOT INITIAL.
      SKIP 1.
      WRITE: / TEXT-m01.
      WRITE: / sy-uline(30).
      LOOP AT i_displ_mdoc INTO DATA(ls_displ_mdoc).
        WRITE: / ls_displ_mdoc-mdoc.
      ENDLOOP.
    ENDIF.

    IF  p_orders IS NOT INITIAL.
*    AND p_conf   IS NOT INITIAL.
      SKIP 1.
      WRITE: / TEXT-m02.
      WRITE: / sy-uline(21).
      LOOP AT i_work_order INTO DATA(ls_work_order).
        WRITE: / ls_work_order-message.  "aufnr.
      ENDLOOP.
    ENDIF.

    IF p_conf IS NOT INITIAL.
      SKIP 1.
      WRITE: / TEXT-m03.
      WRITE: / sy-uline(25).
      LOOP AT i_conf_msg INTO DATA(ls_conf_msg).
        WRITE: / ls_conf_msg-conf_msg.
      ENDLOOP.
    ENDIF.

    IF p_teco IS NOT INITIAL.
      SKIP 1.
      WRITE: / TEXT-m04.
      WRITE: / sy-uline(23).
      LOOP AT i_teco_msg INTO DATA(ls_teco_msg).
        WRITE: / ls_teco_msg-teco_msg.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_rfc_call IMPLEMENTATION.

  METHOD call_rfc_mdoc.

    CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001'
      EXPORTING
        measurement_point    = gv_point
        reading_date         = sy-datum
        reading_time         = sy-uzeit
        reader               = sy-uname
        recorded_value       = im_value
        difference_reading   = abap_true
        prepare_update       = 'X'
        commit_work          = 'X'
        wait_after_commit    = 'X'
      IMPORTING
        measurement_document = l_mdocm1
      EXCEPTIONS
        no_authority         = 1
        point_not_found      = 2
        index_not_unique     = 3
        type_not_found       = 4
        point_locked         = 5
        point_inactive       = 6
        timestamp_in_future  = 7
        timestamp_duprec     = 8
        unit_unfit           = 9
        value_not_fltp       = 10
        value_overflow       = 11
        value_unfit          = 12
        value_missing        = 13
        code_not_found       = 14
        notif_type_not_found = 15
        notif_prio_not_found = 16
        notif_gener_problem  = 17
        update_failed        = 18
        invalid_time         = 19
        invalid_date         = 20
        OTHERS               = 21.
    IF sy-subrc EQ 0.
      APPEND l_mdocm1 TO i_mdoc_final.
      CLEAR: l_mdocm1.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_get_orders IMPLEMENTATION.

  METHOD read_order_tab.

    IF g_found IS NOT INITIAL. "time confirmation
      SELECT aufk~aufnr,
             aufk~objnr,
             aufk~werks,
             aufk~vaplz,
             aufk~wawrk,
             afvc~vornr,
             afvv~arbei,
             afvv~ismnw,
             afvv~ofmnw,
             afvv~arbeh,
             afvv~fsavd,
             afvv~fsedd,
             jest~stat,
             jest~inact
        FROM aufk
       INNER JOIN afko ON afko~aufnr = aufk~aufnr
       INNER JOIN afvc ON afvc~aufpl = afko~aufpl
       INNER JOIN afvv ON afvc~aufpl = afvv~aufpl AND afvc~aplzl = afvv~aplzl
       INNER JOIN jest ON jest~objnr = aufk~objnr
        INTO TABLE @DATA(lt_operation)
       WHERE jest~stat  = @im_stat
         AND jest~inact = @space
         AND aufk~werks = @im_werks.

      IF sy-subrc EQ 0.
        SORT lt_operation BY aufnr.

        SELECT FROM jest
        FIELDS objnr, stat, inact
           FOR ALL ENTRIES IN @lt_operation
         WHERE objnr = @lt_operation-objnr
           AND stat  = 'I0055'
          INTO TABLE @DATA(lt_jest).
        IF sy-subrc EQ 0.
          SORT lt_jest BY objnr.
          g_out_jest[] = lt_jest[].
        ENDIF.

      ENDIF.

    ELSE. "teco work order; release WO

      SELECT aufk~aufnr
             aufk~objnr
             aufk~werks
             aufk~vaplz
             aufk~wawrk
             afvc~vornr
             afvv~arbei
             afvv~ismnw
             afvv~ofmnw
             afvv~arbeh
             afvv~fsavd
             afvv~fsedd
             jest~stat
             jest~inact
        FROM aufk
       INNER JOIN afko ON afko~aufnr = aufk~aufnr
       INNER JOIN afvc ON afvc~aufpl = afko~aufpl
       INNER JOIN afvv ON afvc~aufpl = afvv~aufpl AND afvc~aplzl = afvv~aplzl
       INNER JOIN jest ON jest~objnr = aufk~objnr
        INTO TABLE lt_operation
       WHERE jest~inact = space
         AND aufk~werks = im_werks.

      IF sy-subrc EQ 0.
        SORT lt_operation BY aufnr.
      ENDIF.

    ENDIF.


    IF p_wkctr IS NOT INITIAL.
      DELETE lt_operation WHERE vaplz NE p_wkctr.
    ENDIF.
    IF p_werks IS NOT INITIAL.
      DELETE lt_operation WHERE wawrk NE p_werks.
    ENDIF.

    g_out_operation[] = lt_operation[].

  ENDMETHOD.

ENDCLASS.
