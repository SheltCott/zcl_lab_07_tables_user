CLASS zcl_lab_07_tables_t100437 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun .

    TYPES: BEGIN OF ty_employee,
             id     TYPE i,
             email  TYPE string,
             ape1   TYPE string,
             ape2   TYPE string,
             name   TYPE string,
             fechan TYPE d,
             fechaa TYPE d,
           END OF ty_employee.

    DATA: mt_employees   TYPE TABLE OF ty_employee,
          mt_employees_2 TYPE TABLE OF ty_employee,
          ms_employee    TYPE ty_employee,
          mt_spfli       TYPE TABLE OF spfli,
          ms_spfli       TYPE spfli,
          ms_spfli_2     TYPE spfli.

    METHODS add_records IMPORTING ir_out TYPE REF TO if_oo_adt_classrun_out.
    METHODS insert_record IMPORTING ir_out TYPE REF TO if_oo_adt_classrun_out.
    METHODS append_records IMPORTING ir_out TYPE REF TO if_oo_adt_classrun_out.
    METHODS corresponding_example IMPORTING ir_out TYPE REF TO if_oo_adt_classrun_out.
    METHODS read_table_example IMPORTING ir_out TYPE REF TO if_oo_adt_classrun_out.
    METHODS read_table_with_key IMPORTING ir_out TYPE REF TO if_oo_adt_classrun_out.
    METHODS check_records IMPORTING ir_out TYPE REF TO if_oo_adt_classrun_out.
    METHODS get_record_index IMPORTING ir_out TYPE REF TO if_oo_adt_classrun_out.
    METHODS loop_example IMPORTING ir_out TYPE REF TO if_oo_adt_classrun_out.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_lab_07_tables_t100437 IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
*    Add records
    add_records( out ).

*    Insert records
    insert_record( out ).

*    Adding records with append
    append_records( out ).

*    Corresponding
    corresponding_example( out ).

*    Read table with index
    read_table_example( out ).

*    Read table with key
    read_table_with_key( out ).

*    Checking records
    check_records( out ).

*    Index of a record
    get_record_index( out ).

*    Loop statement
    loop_example( out ).
  ENDMETHOD.

  METHOD add_records.
*    Adding records to internal tables
    me->mt_employees = VALUE #( ( id = 1
                                  email = 'emp1@logali.com'
                                  ape1 = 'perez'
                                  ape2 = 'gomez'
                                  name = 'juan'
                                  fechan = '19900101'
                                  fechaa = '20220101' ) ).

    me->ms_employee-email = 'emp2@logali.com'.
    me->ms_employee-ape1 = 'Lopez'.
    me->ms_employee-ape2 = 'Martinez'.
    me->ms_employee-name = 'Ana'.
    me->ms_employee-fechan = '19920202'.
    me->ms_employee-fechaa ='20220202'.
    APPEND me->ms_employee TO me->mt_employees.

    ir_out->write(  data = me->mt_employees name = 'Add records'  ).
  ENDMETHOD.

  METHOD insert_record.
*     Insert missing 3rd record
    me->ms_employee-email = 'emp2@logali.com'.
    me->ms_employee-ape1 = 'Lopez'.
    me->ms_employee-ape2 = 'Martinez'.
    me->ms_employee-name = 'Ana'.
    me->ms_employee-fechan = '19920202'.
    me->ms_employee-fechaa ='20220202'.

    INSERT me->ms_employee INTO TABLE me->mt_employees.

    ir_out->write(  data = me->mt_employees name = 'Insert records'  ).
  ENDMETHOD.

  METHOD append_records.
*    Add a record a me->mt_employees_2 usando append con estructura
    me->ms_employee = VALUE #( id = 5
                               email = 'emp5@logali.com'
                               ape1 = 'torres'
                               ape2 = 'ruiz'
                               name = 'carlos'
                               fechan = '19950505'
                               fechaa = '20220505' ).

    APPEND me->ms_employee TO me->mt_employees_2.

*     Adding a record a me->mt_employees_2 using append value
    APPEND VALUE #( id = 6
                    email = 'emp6@logali.com'
                    ape1 = 'hernandez'
                    ape2 = 'jimenez'
                    name = 'laura'
                    fechan = '19960606'
                    fechaa = '20220606' ) TO me->mt_employees_2.

*     Adding lines from me->mt_employees a me->mt_employees_2
    APPEND LINES OF me->mt_employees FROM 2 TO 3 TO me->mt_employees_2.

    ir_out->write(  data = me->mt_employees_2 name = 'Add records with APPEND LINES' ).
  ENDMETHOD.

  METHOD corresponding_example.
*     Using move-corresponding to move data between structures
    SELECT * FROM spfli
        WHERE carrid EQ 'LH'
        INTO TABLE @mt_spfli.

    MOVE-CORRESPONDING me->ms_spfli TO me->ms_spfli_2.

    ir_out->write( data = me->ms_spfli_2 name = 'Add records using MOVE-CORRESPONDING' ).
  ENDMETHOD.

  METHOD read_table_example.
*     Reading a table with index
    READ TABLE me->mt_spfli INTO me->ms_spfli INDEX 1.
    ir_out->write( data = me->ms_spfli name = 'Read Table' ).
  ENDMETHOD.

  METHOD read_table_with_key.
*     Read table with key to display departure city for destination airport 'fra'
    READ TABLE me->mt_spfli INTO me->ms_spfli WITH KEY airpto = 'fra'.
    ir_out->write( |Departure city for fra: { me->ms_spfli-airpfrom }| ).
  ENDMETHOD.

  METHOD check_records.
*     Flight consultation with CONNID older 0400
    SELECT * FROM spfli WHERE connid GT '0400' INTO TABLE @me->mt_spfli.
    ir_out->write( data = me->mt_spfli name = 'Flight consultation with CONNID older 0400' ).

*     Check if the flight exists 0407
    IF line_exists( me->mt_spfli[ connid = '0407' ] ).
      ir_out->write( |Flight 0407 exists| ).
    ELSE.
      ir_out->write( |Flight 0407 does not exist| ).
    ENDIF.
  ENDMETHOD.

  METHOD get_record_index.
*     Get index of flight 0407

*    READ TABLE me->mt_spfli WITH KEY connid = '0407' TRANSPORTING NO FIELDS.
    DATA(lv_index) = line_index( me->mt_spfli[ connid = '0407'  ]  ).
    IF sy-subrc = 0.
      ir_out->write( |Flight Index 0407: { lv_index }| ).
    ELSE.
      ir_out->write( 'Flight 0407 not found' ).
    ENDIF.
  ENDMETHOD.

  METHOD loop_example.
*     Loop through records with Loop = 'km'
    LOOP AT me->mt_spfli INTO me->ms_spfli WHERE distid = 'KM'.
      ir_out->write( me->ms_spfli ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.