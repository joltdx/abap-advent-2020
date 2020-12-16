CLASS zcl_advent2020_day16_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_ticket_rules,
        field TYPE string,
        low_1 TYPE i,
        high_1 TYPE i,
        low_2 TYPE i,
        high_2 TYPE i,
      END OF ty_ticket_rules.

    TYPES:
      BEGIN OF ty_ticket,
        values TYPE STANDARD TABLE OF i,
      END OF ty_ticket.

    DATA mt_ticket_rules TYPE STANDARD TABLE OF ty_ticket_rules.
    DATA ms_my_ticket TYPE ty_ticket.
    DATA mt_nearby_tickets TYPE STANDARD TABLE OF ty_ticket.

    METHODS parse_notes
      IMPORTING
        input TYPE string.

    METHODS part_1
      RETURNING
        VALUE(result) TYPE string.

    METHODS part_2
      RETURNING
        VALUE(result) TYPE string.

    METHODS is_completely_invalid
      IMPORTING
        value TYPE i
      RETURNING
        VALUE(result) TYPE abap_bool.
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY16_joltdx IMPLEMENTATION.


  METHOD zif_advent2020_joltdx~solve.

    REPLACE ALL OCCURRENCES OF |\r| IN input WITH ||.
    parse_notes( input ).

    output = |Part 1: { part_1( ) }\nPart 2: { part_2( ) }|.

  ENDMETHOD.

  METHOD parse_notes.
    DATA ticket_rules TYPE ty_ticket_rules.
    DATA ticket TYPE ty_ticket.
    DATA this_is_an_integer TYPE i.

    SPLIT input AT |\n\n| INTO DATA(input_fields) DATA(input_my_ticket) DATA(input_nearby).

    SPLIT input_fields AT |\n| INTO TABLE DATA(fields).
    LOOP AT fields INTO DATA(field).
      FIND REGEX '^([^:]+): (\d+)-(\d+) or (\d+)-(\d+)'
        IN field
        SUBMATCHES ticket_rules-field
                   ticket_rules-low_1
                   ticket_rules-high_1
                   ticket_rules-low_2
                   ticket_rules-high_2.
      INSERT ticket_rules INTO TABLE mt_ticket_rules.
    ENDLOOP.

    SPLIT input_my_ticket AT |\n| INTO DATA(headline) DATA(my_ticket).
    SPLIT my_ticket AT |,| INTO TABLE DATA(values).
    LOOP AT values INTO DATA(value).
      this_is_an_integer = value.
      INSERT this_is_an_integer INTO TABLE ms_my_ticket-values.
    ENDLOOP.

    SPLIT input_nearby AT |\n| INTO TABLE DATA(nearby_tickets).
    LOOP AT nearby_tickets INTO DATA(nearby).
      IF sy-tabix = 1.
        CONTINUE.
      ENDIF.
      CLEAR ticket-values[].
      SPLIT nearby AT |,| INTO TABLE values.
      LOOP AT values INTO value.
        this_is_an_integer = value.
        INSERT this_is_an_integer INTO TABLE ticket-values.
      ENDLOOP.
      INSERT ticket INTO TABLE mt_nearby_tickets.
    ENDLOOP.

  ENDMETHOD.

  METHOD is_completely_invalid.
    result = abap_false.
    LOOP AT mt_ticket_rules INTO DATA(ticket_rules).
      IF value BETWEEN ticket_rules-low_1 AND ticket_rules-high_1 OR
          value BETWEEN ticket_rules-low_2 AND ticket_rules-high_2.
        RETURN.
      ENDIF.
    ENDLOOP.
    result = abap_true.
  ENDMETHOD.

  METHOD part_1.
    DATA error_rate TYPE i.

    LOOP AT mt_nearby_tickets INTO DATA(nearby).
      LOOP AT nearby-values INTO DATA(nearby_value).
        IF is_completely_invalid( nearby_value ) = abap_true.
          error_rate = error_rate + nearby_value.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    result = error_rate.

  ENDMETHOD.

  METHOD part_2.

    result = 'todo'.

  ENDMETHOD.
ENDCLASS.
