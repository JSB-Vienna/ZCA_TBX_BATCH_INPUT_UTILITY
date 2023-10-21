"! <p class="shorttext synchronized" lang="en">CA-TBX: Batch input utility</p>
CLASS zcl_ca_batch_input_utility DEFINITION PUBLIC
                                 CREATE PROTECTED.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Constants and value checks for batch input utility</p>
      mo_bi_options TYPE REF TO zcl_ca_c_batch_input_utility READ-ONLY,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">X = OPEN_GROUP executed for a new BI session</p>
      mv_is_session TYPE abap_bool READ-ONLY.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Factory for a new BI session, incl. open of the session</p>
      "!
      "! @parameter iv_group    | <p class="shorttext synchronized" lang="en">Name of Batch input session</p>
      "! @parameter iv_user     | <p class="shorttext synchronized" lang="en">User id for authority check and history</p>
      "! @parameter iv_holddate | <p class="shorttext synchronized" lang="en">Start date of session / locked until (see descr. data elem.)</p>
      "! @parameter iv_keep     | <p class="shorttext synchronized" lang="en">X = Keep session after execution</p>
      "! @parameter result      | <p class="shorttext synchronized" lang="en">Common object: Batch input utility</p>
      "! @raising   zcx_ca_batch_input_utility  | <p class="shorttext synchronized" lang="en">Common exception: Error in batch input utility</p>
      factory_for_session
        IMPORTING
          iv_group      TYPE apq_grpn
          iv_user       TYPE apq_mapn  DEFAULT sy-uname
          iv_holddate   TYPE apq_stda  OPTIONAL
          iv_keep       TYPE abap_bool DEFAULT abap_false
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_batch_input_utility
        RAISING
          zcx_ca_batch_input_utility,

      "! <p class="shorttext synchronized" lang="en">Factory for data collection for a single transaction</p>
      "!
      "! @parameter result     | <p class="shorttext synchronized" lang="en">Common object: Batch input utility</p>
      "! @raising   zcx_ca_batch_input_utility | <p class="shorttext synchronized" lang="en">Common exception: Error in batch input utility</p>
      factory_for_transaction
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_batch_input_utility
        RAISING
          zcx_ca_batch_input_utility.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      constructor,

      "! <p class="shorttext synchronized" lang="en">Call transaction / append transaction data to session</p>
      "!
      "! @parameter iv_transaction_code | <p class="shorttext synchronized" lang="en">Transactions code to be processed</p>
      "! @parameter iv_authority_check  | <p class="shorttext synchronized" lang="en">X = With authority check on transaction code</p>
      "! @parameter iv_init_data        | <p class="shorttext synchronized" lang="en">X = Initialize BI data</p>
      "! @parameter et_messages         | <p class="shorttext synchronized" lang="en">Table with collected messages during CALL ta USING ...</p>
      "! @parameter ev_subrc_call       | <p class="shorttext synchronized" lang="en">Return code of transaction call</p>
      "! @raising   zcx_ca_batch_input_utility    | <p class="shorttext synchronized" lang="en">Common exception: Error in batch input utility</p>
      call_transaction
        IMPORTING
          iv_transaction_code  TYPE syst_tcode
          iv_authority_check   TYPE abap_bool DEFAULT abap_true
          iv_init_data         TYPE abap_bool DEFAULT abap_true
        EXPORTING
          et_messages          TYPE srct_ctu_messtab
          VALUE(ev_subrc_call) TYPE sysubrc
        RAISING
          zcx_ca_batch_input_utility,

      "! <p class="shorttext synchronized" lang="en">Closing batch input session</p>
      close_group,

      "! <p class="shorttext synchronized" lang="en">Returns the current batch input data to change them</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Current batch input data</p>
      get_data
        RETURNING
          VALUE(result) TYPE hrtb_bdcdata,

      "! <p class="shorttext synchronized" lang="en">Returns current controlling options to change them</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Current BI controlling options</p>
      get_options
        RETURNING
          VALUE(result) TYPE ctu_params,

      "! <p class="shorttext synchronized" lang="en">Reset current batch input data, e. g. for a new transaction</p>
      reset_data,

      "! <p class="shorttext synchronized" lang="en">Replacement (!) of current batch input data</p>
      "!
      "! @parameter it_bi_data | <p class="shorttext synchronized" lang="en">New or changed batch input data</p>
      set_data
        IMPORTING
          it_bi_data TYPE hrtb_bdcdata,

      "! <p class="shorttext synchronized" lang="en">Set program name and next screen number</p>
      "!
      "! @parameter iv_program | <p class="shorttext synchronized" lang="en">Batch input program name</p>
      "! @parameter iv_dynpro  | <p class="shorttext synchronized" lang="en">Batch input screen number</p>
      set_dynpro
        IMPORTING
          iv_program TYPE bdc_prog
          iv_dynpro  TYPE bdc_dynr,

      "! <p class="shorttext synchronized" lang="en">Set field name and value, inclusive the value conversion</p>
      "!
      "! @parameter iv_field_name     | <p class="shorttext synchronized" lang="en">Batch input field name</p>
      "! @parameter iv_value          | <p class="shorttext synchronized" lang="en">Value in SAP internal format, automatically converted</p>
      "! @parameter iv_currency       | <p class="shorttext synchronized" lang="en">Currency for an amount</p>
      "! @parameter iv_unit           | <p class="shorttext synchronized" lang="en">Unit of measure for a quantity</p>
      "! @parameter iv_no_conversion  | <p class="shorttext synchronized" lang="en">X = No conversion into external format</p>
      "! @parameter iv_ignore_initial | <p class="shorttext synchronized" lang="en">X = Ignore initial value</p>
      "! @parameter iv_is_bapi_amount | <p class="shorttext synchronized" lang="en">X = Use special conversion for BAPI amount</p>
      "! @raising   zcx_ca_conv       | <p class="shorttext synchronized" lang="en">Common exception: Conversion failed</p>
      set_field
        IMPORTING
          iv_field_name     TYPE bdc_fnam
          iv_value          TYPE data      OPTIONAL
          iv_currency       TYPE waers     OPTIONAL
          iv_unit           TYPE meins     OPTIONAL
          iv_no_conversion  TYPE abap_bool DEFAULT abap_false
          iv_ignore_initial TYPE abap_bool DEFAULT abap_true
          iv_is_bapi_amount TYPE abap_bool DEFAULT abap_false
        RAISING
          zcx_ca_conv,

      "! <p class="shorttext synchronized" lang="en">Replacement (!) of current controlling options</p>
      "!
      "! @parameter is_options | <p class="shorttext synchronized" lang="en">New or changed controlling options</p>
      "! @raising   zcx_ca_batch_input_utility | <p class="shorttext synchronized" lang="en">Common exception: Error in batch input utility</p>
      set_options
        IMPORTING
          is_options TYPE ctu_params
        RAISING
          zcx_ca_batch_input_utility,

      "! <p class="shorttext synchronized" lang="en">Set standard controlling options for background processing</p>
      set_options_for_bgr,

      "! <p class="shorttext synchronized" lang="en">Set standard controlling options for dialog processing</p>
      set_options_for_dia.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Batch Input data</p>
      mt_bi_data TYPE hrtb_bdcdata,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Controlling options for batch input processing</p>
      ms_options TYPE ctu_params.



* P R I V A T E   S E C T I O N
  PRIVATE SECTION.


ENDCLASS.



CLASS ZCL_CA_BATCH_INPUT_UTILITY IMPLEMENTATION.


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constuctor
    "-----------------------------------------------------------------*
    mo_bi_options = zcl_ca_c_batch_input_utility=>get_instance( ).
  ENDMETHOD.                    "constructor


  METHOD call_transaction.
    "-----------------------------------------------------------------*
    "   Either insert session data into queue or call transaction
    "   for immediate execution
    "-----------------------------------------------------------------*
    CLEAR et_messages.
    CASE mv_is_session.
      WHEN abap_true.
        "C r e a t e   b a t c h   i n p u t   s e s s i o n
        "Insert batch input transactions in batch input session
        CALL FUNCTION 'BDC_INSERT'
          EXPORTING
            tcode            = iv_transaction_code
            ctuparams        = ms_options
          TABLES
            dynprotab        = mt_bi_data
          EXCEPTIONS
            not_open         = 1
            tcode_invalid    = 2
            internal_error   = 3
            queue_error      = 4
            printing_invalid = 5
            posting_invalid  = 6
            OTHERS           = 7.
        IF sy-subrc EQ 0.
          "Everything was fine - continue.

        ELSEIF sy-subrc BETWEEN 1 AND 2.
          DATA(lx_param) =
                CAST zcx_ca_batch_input_utility(
                         zcx_ca_error=>create_exception(
                                        iv_excp_cls = zcx_ca_batch_input_utility=>c_zcx_ca_batch_input_utility
                                        iv_function = 'BDC_OPEN_GROUP'
                                        iv_subrc    = sy-subrc ) ) ##no_text.
          IF lx_param IS BOUND.
            RAISE EXCEPTION lx_param.
          ENDIF.

        ELSE.
          DATA(lx_intern) =
                CAST zcx_ca_intern(
                         zcx_ca_intern=>create_exception(
                                        iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                        iv_function = 'BDC_OPEN_GROUP'
                                        iv_subrc    = sy-subrc ) ) ##no_text.
          IF lx_intern IS BOUND.
            RAISE EXCEPTION lx_intern.
          ENDIF.
        ENDIF.

      WHEN abap_false.
        "C a l l   t r a n s a c t i o n
        "Call requested transaction
        CASE iv_authority_check.
          WHEN abap_true.
            "WITH authority check of the transaction code
            CALL TRANSACTION iv_transaction_code WITH AUTHORITY-CHECK
                                                 USING         mt_bi_data "#EC CI_CALLTA
                                                 OPTIONS FROM  ms_options
                                                 MESSAGES INTO et_messages.

          WHEN OTHERS.
            "W I T H O U T   authority check of the transaction code
            CALL TRANSACTION iv_transaction_code WITHOUT AUTHORITY-CHECK
                                                 USING         mt_bi_data "#EC CI_CALLTA
                                                 OPTIONS FROM  ms_options
                                                 MESSAGES INTO et_messages.
        ENDCASE.

        ev_subrc_call = sy-subrc.
    ENDCASE.

    "Initialize batch input data table
    IF iv_init_data EQ abap_true.
      reset_data( ).
    ENDIF.
  ENDMETHOD.                    "call_transaction


  METHOD close_group.
    "-----------------------------------------------------------------*
    "   Close of session
    "-----------------------------------------------------------------*
    "Close batch input session
    CALL FUNCTION 'BDC_CLOSE_GROUP'
      EXCEPTIONS
        not_open    = 1
        queue_error = 2
        OTHERS      = 3.
    CASE sy-subrc.
      WHEN 0 OR 1.
        "Everything was fine - continue.
        mv_is_session = abap_false.

      WHEN OTHERS.
        DATA(lx_intern) =
              CAST zcx_ca_intern(
                       zcx_ca_intern=>create_exception(
                                      iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                      iv_function = 'BDC_CLOSE_GROUP'
                                      iv_subrc    = sy-subrc ) ) ##no_text.
        IF lx_intern IS BOUND.
          RAISE EXCEPTION lx_intern.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "close_group


  METHOD factory_for_session.
    "-----------------------------------------------------------------*
    "   Create instance for execution as batch input session
    "-----------------------------------------------------------------*
    "Since it is essential to work with created instance check if it is requested
    IF result IS NOT SUPPLIED.
      "Parameter &1 is not supplied / has no receiving field, but is needed
      RAISE EXCEPTION TYPE zcx_ca_batch_input_utility
        EXPORTING
          textid   = zcx_ca_batch_input_utility=>param_not_supplied
          mv_msgv1 = 'RO_BDC' ##no_text.
    ENDIF.

    "Open batch input session for adding transactions
    DATA(lv_keep) = CONV apq_qdel( zcl_ca_conv=>boolean_2_flag( iv_keep ) ).
    CALL FUNCTION 'BDC_OPEN_GROUP'
      EXPORTING
        group               = iv_group
        user                = iv_user
        holddate            = iv_holddate
        keep                = lv_keep
      EXCEPTIONS
        user_invalid        = 1
        group_invalid       = 2
        group_is_locked     = 3
        holddate_invalid    = 4
        running             = 5
        client_invalid      = 6
        destination_invalid = 7
        internal_error      = 8
        queue_error         = 9
        system_lock_error   = 10
        OTHERS              = 11 ##number_ok.
    IF sy-subrc EQ 0.
      "Everything was fine - continue.

    ELSEIF sy-subrc BETWEEN 1 AND 5.
      DATA(lx_param) =
            CAST zcx_ca_batch_input_utility(
                     zcx_ca_error=>create_exception(
                                    iv_excp_cls = zcx_ca_batch_input_utility=>c_zcx_ca_batch_input_utility
                                    iv_function = 'BDC_OPEN_GROUP'
                                    iv_subrc    = sy-subrc ) ) ##no_text.
      IF lx_param IS BOUND.
        RAISE EXCEPTION lx_param.
      ENDIF.

    ELSE.
      DATA(lx_intern) =
            CAST zcx_ca_intern(
                     zcx_ca_intern=>create_exception(
                                    iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                    iv_function = 'BDC_OPEN_GROUP'
                                    iv_subrc    = sy-subrc ) ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.

    "Create singleton instance
    result = NEW #( ).

    "Prepare batch input options for background
    result->set_options_for_bgr( ).
    "Set session type
    result->mv_is_session = abap_true.
  ENDMETHOD.                    "factory_for_session


  METHOD factory_for_transaction.
    "-----------------------------------------------------------------*
    "   Create instance for transaction execution
    "-----------------------------------------------------------------*
    "Since it is essential to work with created instance check if it is requested
    IF result IS NOT SUPPLIED.
      "Parameter &1 is not supplied / has no receiving field, but is needed
      RAISE EXCEPTION TYPE zcx_ca_batch_input_utility
        EXPORTING
          textid   = zcx_ca_batch_input_utility=>param_not_supplied
          mv_msgv1 = 'RO_BDC' ##no_text.
    ENDIF.

    "Create singleton instance
    result = NEW #( ).

    "Prepare batch input options for dialog
    result->set_options_for_dia( ).
    "Set session type
    result->mv_is_session = abap_false.
  ENDMETHOD.                    "factory_for_transaction


  METHOD get_data.
    "-----------------------------------------------------------------*
    "   Get batch input data
    "-----------------------------------------------------------------*
    result = mt_bi_data.
  ENDMETHOD.                    "get_data


  METHOD get_options.
    "-----------------------------------------------------------------*
    "   Get controlling settings
    "-----------------------------------------------------------------*
    result = ms_options.
  ENDMETHOD.                    "get_options


  METHOD reset_data.
    "-----------------------------------------------------------------*
    "   Reset batch input data
    "-----------------------------------------------------------------*
    CLEAR mt_bi_data.
  ENDMETHOD.                    "reset_data


  METHOD set_data.
    "-----------------------------------------------------------------*
    "   Set batch input data (e. g. after changing by user)
    "-----------------------------------------------------------------*
    mt_bi_data = it_bi_data.
  ENDMETHOD.                    "set_data


  METHOD set_dynpro.
    "-----------------------------------------------------------------*
    "   Set program name and screen number
    "-----------------------------------------------------------------*
    APPEND VALUE #( program  = iv_program
                    dynpro   = iv_dynpro
                    dynbegin = abap_true ) TO mt_bi_data.
  ENDMETHOD.                    "set_dynpro


  METHOD set_field.
    "-----------------------------------------------------------------*
    "   Set and convert field name and value
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_data              TYPE bdcdata.

    "Should initial values be ignored?
    IF iv_ignore_initial EQ abap_true AND
       iv_value          IS INITIAL.
      RETURN.
    ENDIF.

    "Should the value be used as passed?
    IF iv_no_conversion EQ abap_true.
      ls_data-fval = iv_value.

    ELSE.
      "Convert internal value into external format
      zcl_ca_conv=>internal_2_external(
                                  EXPORTING
                                    internal_value        = iv_value
                                    currency              = iv_currency
                                    unit_of_measure       = iv_unit
                                    return_in_bapi_format = iv_is_bapi_amount
                                  IMPORTING
                                    external_value        = ls_data-fval ).
    ENDIF.

    "Set dynpro field name and collect in BDC data table
    ls_data-fnam = iv_field_name.
    APPEND ls_data TO mt_bi_data.
  ENDMETHOD.                    "set_field


  METHOD set_options.
    "-----------------------------------------------------------------*
    "   Set and check user controlling settings
    "-----------------------------------------------------------------*
    "Check allowed values of all options
    mo_bi_options->is_processing_mode_valid( is_options-dismode ).
    mo_bi_options->is_update_mode_valid( is_options-updmode ).
    mo_bi_options->is_catt_mode_valid( is_options-cattmode ).
    mo_bi_options->is_use_screen_size_valid( is_options-defsize ).
    mo_bi_options->is_quit_at_commit_valid( is_options-racommit ).
    mo_bi_options->is_bi_active_valid( is_options-nobinpt ).
    mo_bi_options->is_no_bi_end_valid( is_options-nobiend ).

    ms_options = is_options.
  ENDMETHOD.                    "set_options


  METHOD set_options_for_bgr.
    "-----------------------------------------------------------------*
    "   Set default controlling values for execution in background
    "-----------------------------------------------------------------*
    ms_options-dismode  = mo_bi_options->processing_mode-no_screens.      " = N.
    ms_options-updmode  = mo_bi_options->update_mode-asynchronous.        " = A.
    ms_options-cattmode = mo_bi_options->catt_mode-no_catt.               " = SPACE
    ms_options-defsize  = mo_bi_options->use_screen_size-standard.        " = X
    ms_options-racommit = mo_bi_options->quit_at_commit-no_termination.   " = X
    ms_options-nobinpt  = mo_bi_options->bi_active-active.                " = SPACE
    ms_options-nobiend  = mo_bi_options->no_bi_end-inactive.              " = X
  ENDMETHOD.                    "set_options_for_bgr


  METHOD set_options_for_dia.
    "-----------------------------------------------------------------*
    "   Set default controlling values for execution in dialog.
    "   Optimized for calling specific functions / position at a
    "   specific place within a transaction.
    "-----------------------------------------------------------------*
    ms_options-dismode  = mo_bi_options->processing_mode-at_error_only.  " = E.
    ms_options-updmode  = mo_bi_options->update_mode-synchronous.        " = S.
    ms_options-cattmode = mo_bi_options->catt_mode-no_catt.              " = SPACE
    ms_options-defsize  = mo_bi_options->use_screen_size-current.        " = SPACE
    ms_options-racommit = mo_bi_options->quit_at_commit-no_termination.  " = X.
    ms_options-nobinpt  = mo_bi_options->bi_active-inactive.             " = X.
    ms_options-nobiend  = mo_bi_options->no_bi_end-inactive.             " = X.
  ENDMETHOD.                    "set_options_for_dia
ENDCLASS.
