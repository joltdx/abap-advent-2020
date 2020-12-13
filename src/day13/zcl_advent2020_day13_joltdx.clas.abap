CLASS zcl_advent2020_day13_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_earliest_timestamp TYPE i.
    DATA mt_buses_in_service TYPE STANDARD TABLE OF i.

    METHODS part_1
      RETURNING
        VALUE(result) TYPE string.

    METHODS part_2
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.



CLASS ZCL_ADVENT2020_DAY13_joltdx IMPLEMENTATION.


  METHOD zif_advent2020_joltdx~solve.

    DATA this_is_an_integer TYPE i.

    SPLIT input AT |\n| INTO DATA(timestamp) DATA(buses).
    mv_earliest_timestamp = timestamp.
    SPLIT buses AT |,| INTO TABLE DATA(lt_buses).
    LOOP AT lt_buses INTO DATA(bus).
      IF bus <> 'x'.
        this_is_an_integer = bus.
        APPEND this_is_an_integer TO mt_buses_in_service.
      ENDIF.
    ENDLOOP.

    output = |Part 1: { part_1( ) }\nPart 2: { part_2( ) }|.

  ENDMETHOD.

  METHOD part_1.
    DATA earliest_time TYPE i VALUE 999999999999.
    DATA earliest_bus TYPE i.
    DATA bus_last_start TYPE i.
    DATA bus_next_start TYPE i.

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
  ENDMETHOD.

  METHOD part_2.

    READ TABLE mt_buses_in_service INDEX 3 INTO result.

  ENDMETHOD.
ENDCLASS.
