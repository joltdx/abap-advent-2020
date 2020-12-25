CLASS zcl_advent2020_day25_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_door_pub_key TYPE i.
    DATA mv_card_pub_key TYPE i.
    METHODS part_1
      RETURNING
        VALUE(result) TYPE string.

    METHODS part_2
      RETURNING
        VALUE(result) TYPE string.

    METHODS find_loop_size
      IMPORTING
        pub_key TYPE i
      RETURNING
        VALUE(result) TYPE i.
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY25_joltdx IMPLEMENTATION.

  METHOD zif_advent2020_joltdx~solve.

    SPLIT input AT |\n| INTO mv_card_pub_key mv_door_pub_key.

    output = |Part 1:\n{ part_1( ) }\n\nPart 2:\n{ part_2( ) }|.

  ENDMETHOD.

  METHOD part_1.

    DATA(door_loop_size) = find_loop_size( mv_door_pub_key ).
    DATA(card_loop_size) = find_loop_size( mv_card_pub_key ).

    DATA(card_encryption_key) = 1.
    DO card_loop_size TIMES.
      card_encryption_key = ( card_encryption_key * mv_door_pub_key ) MOD 20201227.
    ENDDO.

    DATA(door_encryption_key) = 1.
    DO door_loop_size TIMES.
      door_encryption_key = ( door_encryption_key * mv_card_pub_key ) MOD 20201227.
    ENDDO.

    result = |Door public key: { mv_door_pub_key }\n| &&
             |Card public key: { mv_card_pub_key }\n| &&
             |Door loop size: { door_loop_size }\n| &&
             |Card loop size: { card_loop_size }\n| &&
             |Door encryption key: { door_encryption_key }\n| &&
             |Card encryption key: { card_encryption_key }|.

  ENDMETHOD.

  METHOD part_2.

    result = |Done, did it. There was no part 2 o_O|.

  ENDMETHOD.

  METHOD find_loop_size.
    DATA value TYPE i.
    value = 1.
    WHILE value <> pub_key.
      value = ( value * 7 ) MOD 20201227.
      result = result + 1.
    ENDWHILE.
  ENDMETHOD.
ENDCLASS.
