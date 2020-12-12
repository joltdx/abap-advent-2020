CLASS zcl_advent2020_day12_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_navigation,
        action TYPE c,
        value TYPE i,
      END OF ty_navigation.
    DATA mt_navigation TYPE STANDARD TABLE OF ty_navigation WITH EMPTY KEY.

    METHODS part_1
      RETURNING
        VALUE(result) TYPE string.

    METHODS part_2
      RETURNING
        VALUE(result) TYPE string.

    METHODS parse_input
      IMPORTING
        input TYPE string.

    METHODS get_manhattan
      IMPORTING
        start_facing TYPE c
      RETURNING
        VALUE(result) TYPE i.

    METHODS get_manhattan_waypoint
      IMPORTING
        start_facing TYPE c
        start_east TYPE i
        start_north TYPE i
      RETURNING
        VALUE(result) TYPE i.

    METHODS rotate
      IMPORTING
        facing TYPE c
        navigation TYPE ty_navigation
      RETURNING
        VALUE(result) TYPE c.

ENDCLASS.



CLASS ZCL_ADVENT2020_DAY12_joltdx IMPLEMENTATION.

  METHOD zif_advent2020_joltdx~solve.

    parse_input( input ).

    output = |Part 1: { part_1( ) }\nPart 2: { part_2( ) }|.

  ENDMETHOD.

  METHOD part_1.

    result = get_manhattan( 'E' ).

  ENDMETHOD.

  METHOD part_2.

    result = get_manhattan_waypoint( start_facing = 'E'
                                     start_east = 10
                                     start_north = 1 ).

  ENDMETHOD.

  METHOD parse_input.
    DATA navigation TYPE ty_navigation.

    SPLIT input AT |\n| INTO TABLE DATA(instructions).
    LOOP AT instructions INTO DATA(instruction).
      navigation-action = instruction(1).
      navigation-value = instruction+1.
      APPEND navigation TO mt_navigation.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_manhattan.
    DATA facing TYPE c.
    DATA east TYPE i.
    DATA north TYPE i.

    facing = start_facing.

    LOOP AT mt_navigation INTO DATA(navigation).
      IF navigation-action = 'L' OR
          navigation-action = 'R'.
        facing = rotate( facing = facing
                         navigation = navigation ).
        CONTINUE.
      ELSEIF navigation-action = 'F'.
        navigation-action = facing.
      ENDIF.

      CASE navigation-action.
        WHEN 'N'.
          north = north + navigation-value.
        WHEN 'E'.
          east = east + navigation-value.
        WHEN 'S'.
          north = north - navigation-value.
        WHEN 'W'.
          east = east - navigation-value.
      ENDCASE.
    ENDLOOP.
    IF east < 0.
      east = 0 - east.
    ENDIF.
    IF north < 0.
      north = 0 - north.
    ENDIF.
    result = east + north.
  ENDMETHOD.

  METHOD get_manhattan_waypoint.
    DATA facing TYPE c.
    DATA waypoint_east TYPE i.
    DATA waypoint_north TYPE i.
    DATA temp_dammit TYPE i.
    DATA east TYPE i.
    DATA north TYPE i.
    DATA rotating TYPE i.

    facing = start_facing.
    waypoint_east = start_east.
    waypoint_north = start_north.

    LOOP AT mt_navigation INTO DATA(navigation).
      CASE navigation-action.
        WHEN 'L'.
          rotating = navigation-value DIV 90.
          DO rotating TIMES.
            temp_dammit = waypoint_east.
            waypoint_east = 0 - waypoint_north.
            waypoint_north = temp_dammit.
          ENDDO.
        WHEN 'R'.
          rotating = navigation-value DIV 90.
          DO rotating TIMES.
            temp_dammit = waypoint_east.
            waypoint_east = waypoint_north.
            waypoint_north = 0 - temp_dammit.
          ENDDO.
        WHEN 'F'.
          east = east + ( navigation-value * waypoint_east ).
          north = north + ( navigation-value * waypoint_north ).
        WHEN 'N'.
          waypoint_north = waypoint_north + navigation-value.
        WHEN 'E'.
          waypoint_east = waypoint_east + navigation-value.
        WHEN 'S'.
          waypoint_north = waypoint_north - navigation-value.
        WHEN 'W'.
          waypoint_east = waypoint_east - navigation-value.
      ENDCASE.
    ENDLOOP.
    IF east < 0.
      east = 0 - east.
    ENDIF.
    IF north < 0.
      north = 0 - north.
    ENDIF.
    result = east + north.
  ENDMETHOD.

  METHOD rotate.
    DATA direction TYPE i.
    DATA turning TYPE i.
    DATA compass TYPE c LENGTH 4 VALUE 'NESW'.

    FIND facing IN compass MATCH OFFSET direction.

    turning = navigation-value DIV 90.
    IF navigation-action = 'L'.
      direction = direction - turning.
    ELSE.
      direction = direction + turning.
    ENDIF.

    IF direction > 3.
      direction = direction - 4.
    ELSEIF direction < 0.
      direction = 4 + direction.
    ENDIF.

    result = compass+direction(1).
  ENDMETHOD.
ENDCLASS.
