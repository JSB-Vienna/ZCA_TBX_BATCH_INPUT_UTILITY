"! <p class="shorttext synchronized" lang="en">CA-TBX exception: Batch input utility error occured</p>
CLASS zcx_ca_batch_input_utility DEFINITION
  PUBLIC
  INHERITING FROM zcx_ca_param
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF zcx_ca_batch_input_utility,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '087',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ca_batch_input_utility .
    "! <p class="shorttext synchronized" lang="en">My own name</p>
    CONSTANTS c_zcx_ca_batch_input_utility TYPE seoclsname VALUE 'ZCX_CA_BATCH_INPUT_UTILITY' ##NO_TEXT.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        !textid      LIKE if_t100_message=>t100key OPTIONAL
        !previous    LIKE previous OPTIONAL
        !mt_return   TYPE bapiret2_t OPTIONAL
        !mv_subrc    TYPE syst_subrc OPTIONAL
        !mv_msgty    TYPE symsgty OPTIONAL
        !mv_msgv1    TYPE symsgv OPTIONAL
        !mv_msgv2    TYPE symsgv OPTIONAL
        !mv_msgv3    TYPE symsgv OPTIONAL
        !mv_msgv4    TYPE symsgv OPTIONAL
        !mv_severity TYPE t_severity OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_CA_BATCH_INPUT_UTILITY IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous    = previous
        mt_return   = mt_return
        mv_subrc    = mv_subrc
        mv_msgty    = mv_msgty
        mv_msgv1    = mv_msgv1
        mv_msgv2    = mv_msgv2
        mv_msgv3    = mv_msgv3
        mv_msgv4    = mv_msgv4
        mv_severity = mv_severity.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = zcx_ca_batch_input_utility .
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
