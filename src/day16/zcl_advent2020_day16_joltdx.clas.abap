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
        can_be_in_position TYPE string,
        is_position TYPE i,
      END OF ty_ticket_rules.

    TYPES:
      BEGIN OF ty_ticket,
        values TYPE STANDARD TABLE OF i,
      END OF ty_ticket.

    DATA mt_ticket_rules TYPE STANDARD TABLE OF ty_ticket_rules.
    DATA ms_my_ticket TYPE ty_ticket.
    DATA mt_nearby_tickets TYPE STANDARD TABLE OF ty_ticket.
    DATA mt_valid_tickets TYPE STANDARD TABLE OF ty_ticket.

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



CLASS zcl_advent2020_day16_joltdx IMPLEMENTATION.


  METHOD zif_advent2020_joltdx~solve.

    REPLACE ALL OCCURRENCES OF |\r| IN input WITH ||.
    parse_notes( input ).

    output = |Part 1: { part_1( ) }\nPart 2: { part_2( ) }|.

  ENDMETHOD.

  METHOD parse_notes.
    DATA ticket_rules TYPE ty_ticket_rules.
    DATA ticket TYPE ty_ticket.
    DATA this_is_an_integer TYPE i.
    DATA init_can_be_in_position TYPE string.

    SPLIT input AT |\n\n| INTO DATA(input_fields) DATA(input_my_ticket) DATA(input_nearby).

    SPLIT input_fields AT |\n| INTO TABLE DATA(fields).
    DO lines( fields ) TIMES.
      init_can_be_in_position = |{ init_can_be_in_position }\|{ sy-index }\||.
    ENDDO.
    ticket_rules-can_be_in_position = init_can_be_in_position.
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
    DATA valid_ticket TYPE abap_bool.

    LOOP AT mt_nearby_tickets INTO DATA(nearby).
      valid_ticket = abap_true.
      LOOP AT nearby-values INTO DATA(nearby_value).
        IF is_completely_invalid( nearby_value ) = abap_true.
          error_rate = error_rate + nearby_value.
          valid_ticket = abap_false.
        ENDIF.
      ENDLOOP.
      IF valid_ticket = abap_true.
        INSERT nearby INTO TABLE mt_valid_tickets.
      ENDIF.
    ENDLOOP.

    result = error_rate.

  ENDMETHOD.

  METHOD part_2.
    DATA valuepipe TYPE string.
    DATA only_position TYPE string.
    DATA pos TYPE i.
    DATA match_count TYPE i.
    DATA identified_unique TYPE abap_bool.
    DATA result_product TYPE i VALUE 1.

    LOOP AT mt_ticket_rules ASSIGNING FIELD-SYMBOL(<ticket_rules>).
      LOOP AT mt_valid_tickets INTO DATA(ticket).
        IF <ticket_rules>-is_position > 0.
          CONTINUE.
        ENDIF.
        SPLIT <ticket_rules>-can_be_in_position AT |\|| INTO TABLE DATA(positions).
        DELETE positions WHERE table_line = ''.
        LOOP AT positions INTO DATA(position).
          pos = position.
          READ TABLE ticket-values INDEX pos INTO DATA(value).
          IF sy-subrc = 0 AND
              value NOT BETWEEN <ticket_rules>-low_1 AND <ticket_rules>-high_1 AND
              value NOT BETWEEN <ticket_rules>-low_2 AND <ticket_rules>-high_2.
            valuepipe = |\|{ pos }\||.
            REPLACE ALL OCCURRENCES OF valuepipe IN <ticket_rules>-can_be_in_position WITH ||.
            FIND REGEX '\|\|' IN <ticket_rules>-can_be_in_position MATCH COUNT match_count.
            IF match_count = 0.
              only_position = <ticket_rules>-can_be_in_position.
              REPLACE ALL OCCURRENCES OF |\|| IN only_position WITH ||.
              <ticket_rules>-is_position = only_position.
              CLEAR <ticket_rules>-can_be_in_position.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    identified_unique = abap_true.
    WHILE identified_unique = abap_true.
      identified_unique = abap_false.
      LOOP AT mt_ticket_rules INTO DATA(ticket_rules) WHERE is_position > 0.
        valuepipe = |\|{ ticket_rules-is_position }\||.
        LOOP AT mt_ticket_rules ASSIGNING <ticket_rules> WHERE can_be_in_position CS valuepipe.
          REPLACE ALL OCCURRENCES OF valuepipe IN <ticket_rules>-can_be_in_position WITH ||.
          FIND REGEX '\|\|' IN <ticket_rules>-can_be_in_position MATCH COUNT match_count.
          IF match_count = 0.
            only_position = <ticket_rules>-can_be_in_position.
            REPLACE ALL OCCURRENCES OF |\|| IN only_position WITH ||.
            <ticket_rules>-is_position = only_position.
            CLEAR <ticket_rules>-can_be_in_position.
            identified_unique = abap_true.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDWHILE.

    LOOP AT mt_ticket_rules INTO ticket_rules.
      FIND REGEX '^departure' IN ticket_rules-field MATCH COUNT match_count.
      IF match_count > 0.
        READ TABLE ms_my_ticket-values INDEX ticket_rules-is_position INTO DATA(position_value).
        IF sy-subrc = 0.
          result_product = result_product * position_value.
        ENDIF.
      ENDIF.
    ENDLOOP.

    result = result_product.

  ENDMETHOD.
ENDCLASS.