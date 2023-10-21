"! <p class="shorttext synchronized" lang="en">CA-TBX: Constants and value checks for batch input utility</p>
CLASS zcl_ca_c_batch_input_utility DEFINITION PUBLIC
                                   CREATE PROTECTED.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">CATT mode</p>
      BEGIN OF catt_mode,
        "! <p class="shorttext synchronized" lang="en">CATT mode: CATT with individual screen control</p>
        display    TYPE ctu_catt   VALUE 'A' ##no_text,
        "! <p class="shorttext synchronized" lang="en">CATT mode: No CATT</p>
        no_catt    TYPE ctu_catt   VALUE space ##no_text,
        "! <p class="shorttext synchronized" lang="en">CATT mode: CATT without individual screen control </p>
        no_screens TYPE ctu_catt   VALUE 'N' ##no_text,
      END OF catt_mode,

      "! <p class="shorttext synchronized" lang="en">Used screen size for batch input execution</p>
      BEGIN OF use_screen_size,
        "! <p class="shorttext synchronized" lang="en">Standard screen size: Current size</p>
        current  TYPE ctu_defsze VALUE space ##no_text,
        "! <p class="shorttext synchronized" lang="en">Standard screen size: Standard size</p>
        standard TYPE ctu_defsze VALUE 'X' ##no_text,
      END OF use_screen_size,

      "! <p class="shorttext synchronized" lang="en">Processing modes for CALL TRANSACTION</p>
      BEGIN OF processing_mode,
        "! <p class="shorttext synchronized" lang="en">Processing mode for CALL TA: Process all screens</p>
        display       TYPE ctu_mode   VALUE 'A' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Processing mode for CALL TA: Screen only at error</p>
        at_error_only TYPE ctu_mode   VALUE 'E' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Processing mode for CALL TA: Dark processing - no screens</p>
        no_screens    TYPE ctu_mode   VALUE 'N' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Processing mode for CALL TA: Like dark mode, but stop at BP</p>
        debugger      TYPE ctu_mode   VALUE 'P' ##no_text,
      END OF processing_mode,

      "! <p class="shorttext synchronized" lang="en">Options for SY-BINPT after batch input completion</p>
      BEGIN OF no_bi_end,
        "! <p class="shorttext synchronized" lang="en">Value of SY-BINPT after BI completion: Set to X</p>
        active   TYPE ctu_noben  VALUE space ##no_text,
        "! <p class="shorttext synchronized" lang="en">Value of SY-BINPT after BI completion: Set to space</p>
        inactive TYPE ctu_noben  VALUE 'X' ##no_text,
      END OF no_bi_end,

      "! <p class="shorttext synchronized" lang="en">Value of SY-BINPT during BI processing</p>
      BEGIN OF bi_active,
        "! <p class="shorttext synchronized" lang="en">Value of SY-BINPT during BI processing: Set to X</p>
        active   TYPE ctu_nobim  VALUE space ##no_text,
        "! <p class="shorttext synchronized" lang="en">Value of SY-BINPT during BI processing: Set to space</p>
        inactive TYPE ctu_nobim  VALUE 'X' ##no_text,
      END OF bi_active,

      "! <p class="shorttext synchronized" lang="en">CALL TRANSACTION USING... is not completed by COMMIT</p>
      BEGIN OF quit_at_commit,
        "! <p class="shorttext synchronized" lang="en">Quit at COMMIT WORK: Don't quit</p>
        no_termination TYPE ctu_rafc   VALUE 'X' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Quit at COMMIT WORK: Quit</p>
        termination    TYPE ctu_rafc   VALUE space ##no_text,
      END OF quit_at_commit,

      "! <p class="shorttext synchronized" lang="en">Update mode</p>
      BEGIN OF update_mode,
        "! <p class="shorttext synchronized" lang="en">Update mode: Asynchronous</p>
        asynchronous TYPE ctu_update VALUE 'A' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Update mode: Local posting -&gt; SET UPDATE TASK LOCAL</p>
        local        TYPE ctu_update VALUE 'L' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Update mode: Synchronous</p>
        synchronous  TYPE ctu_update VALUE 'S' ##no_text,
      END OF update_mode.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get instance</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Class instance</p>
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_c_batch_input_utility.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Valid SY-BINPT activation value passed?</p>
      "!
      "! @parameter bi_active                  | <p class="shorttext synchronized" lang="en">SY-BINPT activation value</p>
      "! @raising   zcx_ca_batch_input_utility | <p class="shorttext synchronized" lang="en">Common exception: Error in batch input utility</p>
      is_bi_active_valid FINAL
        IMPORTING
          bi_active TYPE ctu_nobim
        RAISING
          zcx_ca_batch_input_utility,

      "! <p class="shorttext synchronized" lang="en">Valid CATT mode passed?</p>
      "!
      "! @parameter catt_mode                  | <p class="shorttext synchronized" lang="en">CATT mode</p>
      "! @raising   zcx_ca_batch_input_utility | <p class="shorttext synchronized" lang="en">Common exception: Error in batch input utility</p>
      is_catt_mode_valid FINAL
        IMPORTING
          catt_mode TYPE ctu_catt
        RAISING
          zcx_ca_batch_input_utility,

      "! <p class="shorttext synchronized" lang="en">Valid batch input ending option passed?</p>
      "!
      "! @parameter no_bi_end                  | <p class="shorttext synchronized" lang="en">Batch input ending option</p>
      "! @raising   zcx_ca_batch_input_utility | <p class="shorttext synchronized" lang="en">Common exception: Error in batch input utility</p>
      is_no_bi_end_valid FINAL
        IMPORTING
          no_bi_end TYPE ctu_noben
        RAISING
          zcx_ca_batch_input_utility,

      "! <p class="shorttext synchronized" lang="en">Valid processing mode passed?</p>
      "!
      "! @parameter processing_mode            | <p class="shorttext synchronized" lang="en">Processing mode</p>
      "! @raising   zcx_ca_batch_input_utility | <p class="shorttext synchronized" lang="en">Common exception: Error in batch input utility</p>
      is_processing_mode_valid FINAL
        IMPORTING
          processing_mode TYPE ctu_mode
        RAISING
          zcx_ca_batch_input_utility,

      "! <p class="shorttext synchronized" lang="en">Valid commit termination option passed?</p>
      "!
      "! @parameter quit_at_commit             | <p class="shorttext synchronized" lang="en">Commit termination option</p>
      "! @raising   zcx_ca_batch_input_utility | <p class="shorttext synchronized" lang="en">Common exception: Error in batch input utility</p>
      is_quit_at_commit_valid FINAL
        IMPORTING
          quit_at_commit TYPE ctu_rafc
        RAISING
          zcx_ca_batch_input_utility,

      "! <p class="shorttext synchronized" lang="en">Valid update mode passed?</p>
      "!
      "! @parameter update_mode                | <p class="shorttext synchronized" lang="en">Update mode</p>
      "! @raising   zcx_ca_batch_input_utility | <p class="shorttext synchronized" lang="en">Common exception: Error in batch input utility</p>
      is_update_mode_valid FINAL
        IMPORTING
          update_mode TYPE ctu_update
        RAISING
          zcx_ca_batch_input_utility,

      "! <p class="shorttext synchronized" lang="en">Valid screen size passed?</p>
      "!
      "! @parameter use_screen_size            | <p class="shorttext synchronized" lang="en">Screen size for execution</p>
      "! @raising   zcx_ca_batch_input_utility | <p class="shorttext synchronized" lang="en">Common exception: Error in batch input utility</p>
      is_use_screen_size_valid FINAL
        IMPORTING
          use_screen_size TYPE ctu_defsze
        RAISING
          zcx_ca_batch_input_utility.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Check value against fixed_values</p>
      "!
      "! @parameter value                      | <p class="shorttext synchronized" lang="en">Value under test</p>
      "! @parameter param_name                 | <p class="shorttext synchronized" lang="en">Name of field/parameter for output in error message</p>
      "! @raising   zcx_ca_batch_input_utility | <p class="shorttext synchronized" lang="en">Common exception: File handling errors</p>
      check_against_fixed_values FINAL
        IMPORTING
          value      TYPE simple
          param_name TYPE csequence
        RAISING
          zcx_ca_batch_input_utility.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Instance of the class itself</p>
      singleton_instance     TYPE REF TO zcl_ca_c_batch_input_utility.

ENDCLASS.



CLASS ZCL_CA_C_BATCH_INPUT_UTILITY IMPLEMENTATION.


  METHOD check_against_fixed_values.
    "-----------------------------------------------------------------*
    "   Check value against fixed_values
    "-----------------------------------------------------------------*
    TRY.
        NEW zcl_ca_ddic( iv_data       = value
                         iv_param_name = param_name )->check_fixed_values( iv_value       = value
                                                                           iv_raise_excep = abap_true ).

      CATCH zcx_ca_param INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_batch_input_utility( lx_catched ).
        RAISE EXCEPTION lx_error.
    ENDTRY.
  ENDMETHOD.                    "check_against_fixed_values


  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance
    "-----------------------------------------------------------------*
    IF zcl_ca_c_batch_input_utility=>singleton_instance IS NOT BOUND.
      zcl_ca_c_batch_input_utility=>singleton_instance = NEW #( ).
    ENDIF.

    result = zcl_ca_c_batch_input_utility=>singleton_instance.
  ENDMETHOD.                    "get_instance


  METHOD is_bi_active_valid.
    "-----------------------------------------------------------------*
    "   Valid SY-BINPT activation value passed?
    "-----------------------------------------------------------------*
    check_against_fixed_values( value      = bi_active
                                param_name = 'BI_ACTIVE' ) ##no_text.
  ENDMETHOD.                    "is_bi_active_valid


  METHOD is_catt_mode_valid.
    "-----------------------------------------------------------------*
    "   Valid CATT mode passed?
    "-----------------------------------------------------------------*
    check_against_fixed_values( value      = catt_mode
                                param_name = 'CATT_MODE' ) ##no_text.
  ENDMETHOD.                    "is_catt_mode_valid


  METHOD is_no_bi_end_valid.
    "-----------------------------------------------------------------*
    "   Valid batch input ending option passed?
    "-----------------------------------------------------------------*
    check_against_fixed_values( value      = no_bi_end
                                param_name = 'NO_BI_END' ) ##no_text.
  ENDMETHOD.                    "is_no_bi_end_valid


  METHOD is_processing_mode_valid.
    "-----------------------------------------------------------------*
    "   Valid processing mode passed?
    "-----------------------------------------------------------------*
    check_against_fixed_values( value      = processing_mode
                                param_name = 'PROCESSING_MODE' ) ##no_text.
  ENDMETHOD.                    "is_processing_mode_valid


  METHOD is_quit_at_commit_valid.
    "-----------------------------------------------------------------*
    "   Valid commit termination option passed?
    "-----------------------------------------------------------------*
    check_against_fixed_values( value      = quit_at_commit
                                param_name = 'QUIT_AT_COMMIT' ) ##no_text.
  ENDMETHOD.                    "is_quit_at_commit_valid


  METHOD is_update_mode_valid.
    "-----------------------------------------------------------------*
    "   Valid update mode passed?
    "-----------------------------------------------------------------*
    check_against_fixed_values( value      = update_mode
                                param_name = 'UPDATE_MODE' ) ##no_text.
  ENDMETHOD.                    "is_update_mode


  METHOD is_use_screen_size_valid.
    "-----------------------------------------------------------------*
    "   Valid screen size passed?
    "-----------------------------------------------------------------*
    check_against_fixed_values( value      = use_screen_size
                                param_name = 'USE_SCREEN_SIZE' ) ##no_text.
  ENDMETHOD.                    "is_use_screen_size_valid
ENDCLASS.
