CLASS zcl_advent2020_day13_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_bus_offset,
        bus TYPE p,
        offset TYPE p,
      END OF ty_bus_offset.
    DATA mv_earliest_timestamp TYPE p.
    DATA mt_buses_in_service TYPE STANDARD TABLE OF p.
    DATA mt_bus_and_offset TYPE STANDARD TABLE OF ty_bus_offset.
    DATA mv_max_bus TYPE p.
    DATA mv_max_bus_offset TYPE p.

    METHODS part_1
      RETURNING
        VALUE(result) TYPE string.

    METHODS part_2_oh_crap_no
      RETURNING
        VALUE(result) TYPE string.

    METHODS part_2
      RETURNING
        VALUE(result) TYPE string.

    METHODS get_next_departure_after
      IMPORTING
        after_when TYPE p
        bus_number TYPE p
      RETURNING
        VALUE(result) TYPE p.
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY13_joltdx IMPLEMENTATION.


  METHOD zif_advent2020_joltdx~solve.

    DATA this_is_an_integer TYPE p.
    DATA bus_offset TYPE ty_bus_offset.
    SPLIT input AT |\n| INTO DATA(timestamp) DATA(buses).
    mv_earliest_timestamp = timestamp.
    SPLIT buses AT |,| INTO TABLE DATA(lt_buses).
    LOOP AT lt_buses INTO DATA(bus).
      IF bus <> 'x'.
        this_is_an_integer = bus.
        APPEND this_is_an_integer TO mt_buses_in_service.

        bus_offset-bus = this_is_an_integer.
        APPEND bus_offset TO mt_bus_and_offset.
        IF bus_offset-bus > mv_max_bus.
          mv_max_bus = bus_offset-bus.
          mv_max_bus_offset = bus_offset-offset.
        ENDIF.
      ENDIF.
      bus_offset-offset = bus_offset-offset + 1.
    ENDLOOP.

    output = |Part 1: { part_1( ) }\nPart 2: { part_2( ) }|.

  ENDMETHOD.

  METHOD part_1.
    DATA earliest_time TYPE p VALUE 9999999999.
    DATA earliest_bus TYPE p.
    DATA bus_last_start TYPE p.
    DATA bus_next_start TYPE p.

    LOOP AT mt_buses_in_service INTO DATA(bus).
      bus_last_start = ( mv_earliest_timestamp DIV bus ) * bus.
      bus_next_start = bus_last_start + bus.
      IF bus_next_start < earliest_time.
        earliest_time = bus_next_start.
        earliest_bus = bus.
      ENDIF.
    ENDLOOP.

    result = |Bus { earliest_bus }, time { earliest_time }.| &&
             | Answer = { ( earliest_time - mv_earliest_timestamp ) * earliest_bus }|.

    IF 1 = 2.
      part_2_oh_crap_no( ).
    ENDIF.
  ENDMETHOD.

  METHOD part_2.
    " I had to google this to find the Chinese Remainder Theorem, so here goes...
    " (I kept my "oh crap no" solution timed out, or rather, I timed out waiting...)
    " (Also, I might have broken it before I left it for good... ) )

    " This gave me a results that's 22 off. Wat.
    " Running in ABAP Steampunk instead (with minor adjustments of datatypes) gave
    " me the correct answer.

    " Wat.

    " So I've learnd that JavaScript uses IEEE754 and can be "imprecise at large numbers".
    " And we're talking large numbers in this one...
    " So. Well. I'll just leave it at that.

    DATA entire_product TYPE p VALUE 1.
    DATA m TYPE p.
    DATA x TYPE p.
    DATA y TYPE p.
    DATA sum_product TYPE p.

    LOOP AT mt_bus_and_offset INTO DATA(bus_and_offset).
      entire_product = entire_product * bus_and_offset-bus.
    ENDLOOP.

    LOOP AT mt_bus_and_offset INTO bus_and_offset.
      m = entire_product DIV bus_and_offset-bus.
      x = m - ( ( m DIV bus_and_offset-bus ) * bus_and_offset-bus ).
      DO bus_and_offset-bus TIMES.
        IF ( ( x * sy-index ) + 1 ) MOD bus_and_offset-bus = 0.
          y = sy-index.
          EXIT.
        ENDIF.
      ENDDO.
      sum_product = sum_product + ( ( bus_and_offset-offset * y ) * m ).
      WHILE sum_product > entire_product.
        sum_product = sum_product - entire_product.
      ENDWHILE.
    ENDLOOP.

    result = sum_product - ( ( sum_product DIV entire_product ) * entire_product ).
  ENDMETHOD.


  METHOD part_2_oh_crap_no.
    DATA departure TYPE p.
    DATA timestamp TYPE p.
    DATA after_timestamp TYPE p.
    DATA success TYPE abap_bool.
    DATA next_max_bus_timestamp TYPE p.

    READ TABLE mt_bus_and_offset INDEX 1 INTO DATA(first_bus).
    DELETE mt_bus_and_offset INDEX 1.

    timestamp = get_next_departure_after( after_when = 100000000000000
                                          bus_number = first_bus-bus ).
    next_max_bus_timestamp = get_next_departure_after( after_when = 702970661700000
                                                       bus_number = mv_max_bus ).
    timestamp = get_next_departure_after( after_when = ( next_max_bus_timestamp - mv_max_bus_offset ) - 1
                                          bus_number = first_bus-bus ).
    DO.
      success = abap_true.
      after_timestamp = timestamp.
      LOOP AT mt_bus_and_offset INTO DATA(bus_and_offset).
        departure = get_next_departure_after( after_when = after_timestamp
                                              bus_number = bus_and_offset-bus ).
        IF departure <> ( timestamp + bus_and_offset-offset ).
          success = abap_false.
          EXIT.
        ENDIF.
        after_timestamp = timestamp + bus_and_offset-offset.
      ENDLOOP.
      IF success = abap_true.
        result = timestamp.
        RETURN.
      ENDIF.
      next_max_bus_timestamp = get_next_departure_after( after_when = next_max_bus_timestamp
                                                         bus_number = mv_max_bus ).
      timestamp = get_next_departure_after( after_when = ( next_max_bus_timestamp - mv_max_bus_offset ) - 1
                                            bus_number = first_bus-bus ).
      IF timestamp > 702970661767766.
        EXIT.
      ENDIF.
    ENDDO.

    result = 'No such timestamp found'.
  ENDMETHOD.

  METHOD get_next_departure_after.
    result = ( ( after_when DIV bus_number ) * bus_number ) + bus_number.
  ENDMETHOD.
ENDCLASS.
