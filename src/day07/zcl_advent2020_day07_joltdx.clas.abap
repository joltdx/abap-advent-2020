CLASS zcl_advent2020_day07_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES ty_string_table TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    DATA mt_input TYPE ty_string_table.

    METHODS can_contain
      IMPORTING
        inner_color TYPE string
      RETURNING
        VALUE(outer_colors) TYPE ty_string_table.

    METHODS count_containers
      IMPORTING
        outer_color TYPE string
      RETURNING
        VALUE(result) TYPE i.

    METHODS part_1
      RETURNING
        VALUE(result) TYPE string.

    METHODS part_2
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.



CLASS ZCL_ADVENT2020_DAY07_joltdx IMPLEMENTATION.


  METHOD zif_advent2020_joltdx~solve.

    SPLIT input AT |\n| INTO TABLE mt_input.

    output = |Part 1: { part_1( ) }. Part 2: { part_2( ) }|.

  ENDMETHOD.

  METHOD can_contain.

    DATA container TYPE string.
    DATA bags TYPE string.
    DATA counter TYPE i.

    LOOP AT mt_input INTO DATA(input).
      SPLIT input AT | bags contain | INTO container bags.
      FIND inner_color IN bags MATCH COUNT counter.
      IF counter > 0.
        APPEND container TO outer_colors.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD count_containers.

    DATA container TYPE string.
    DATA bags TYPE string.
    DATA counter TYPE i.
    DATA next_bag_color TYPE string.
    DATA next_bag_count TYPE i.

    LOOP AT mt_input INTO DATA(input).
      SPLIT input AT | bags contain | INTO container bags.
      IF container = outer_color.
        IF bags = |no other bags.|.
          result = 0.
          RETURN.
        ENDIF.

        REPLACE ALL OCCURRENCES OF |.| IN bags WITH ||.
        SPLIT bags AT |, | INTO TABLE DATA(next_bags).
        IF lines( next_bags ) = 0.
          APPEND bags TO next_bags.
        ENDIF.
        LOOP AT next_bags INTO DATA(next_bag).
          FIND REGEX '(\d) (\D+) (bags?)$' IN next_bag SUBMATCHES next_bag_count next_bag_color.
          counter = counter + next_bag_count + ( next_bag_count * count_containers( next_bag_color ) ).
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    result = counter.
  ENDMETHOD.

  METHOD part_1.

    DATA(containers) = can_contain( |shiny gold| ).
    LOOP AT containers INTO DATA(container).
      DATA(set_of_containers) = can_contain( container ).
      LOOP AT set_of_containers INTO DATA(new_container).
        APPEND new_container TO containers.
      ENDLOOP.
    ENDLOOP.

    SORT containers.
    DELETE ADJACENT DUPLICATES FROM containers.

    result = lines( containers ).

  ENDMETHOD.

  METHOD part_2.

    result = count_containers( |shiny gold| ).

  ENDMETHOD.

ENDCLASS.
