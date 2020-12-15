CLASS zcl_advent2020_day15_joltdx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_advent2020_joltdx .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_number_game,
        number TYPE i,
        turn TYPE i,
        turn_diff TYPE i,
      END OF ty_number_game.

    " This needs to be a hashed table for part 2 to work in reasonable time... :)
    DATA mt_number_game TYPE STANDARD TABLE OF ty_number_game WITH EMPTY KEY.
    DATA mv_last_starting_number TYPE i.

    METHODS part_1
      RETURNING
        VALUE(result) TYPE string.

    METHODS part_2
      RETURNING
        VALUE(result) TYPE string.

    METHODS run_game_until
      IMPORTING
        final_turn_number TYPE i
      RETURNING
        VALUE(result) TYPE i.
ENDCLASS.



CLASS ZCL_ADVENT2020_DAY15_joltdx IMPLEMENTATION.


  METHOD zif_advent2020_joltdx~solve.

    SPLIT input AT |,| INTO TABLE DATA(starting_numbers).
    LOOP AT starting_numbers INTO DATA(number).
      APPEND VALUE #( number = number
                      turn   = sy-tabix ) TO mt_number_game.
      mv_last_starting_number = number.
    ENDLOOP.

    output = |Part 1: { part_1( ) }\nPart 2:{ part_2( ) }|.

  ENDMETHOD.

  METHOD part_1.

    result = run_game_until( 2020 ).

  ENDMETHOD.

  METHOD part_2.

    result = run_game_until( 30000000 ).

  ENDMETHOD.

  METHOD run_game_until.
    DATA current_turn TYPE i.
    DATA last_spoken TYPE i.
    DATA number_spoken TYPE i.
    DATA number_of_turns_to_do TYPE i.

    " This needs to be a hashed table for part 2 to work in reasonable time... :)
    DATA lt_number_game TYPE STANDARD TABLE OF ty_number_game WITH EMPTY KEY.

    lt_number_game = mt_number_game.

    current_turn = lines( lt_number_game ).
    last_spoken = mv_last_starting_number.

    number_of_turns_to_do = final_turn_number - current_turn.
    DO number_of_turns_to_do TIMES.
      current_turn = current_turn + 1.
      " This needs to be a hashed table for part 2 to work in reasonable time... :)
      READ TABLE lt_number_game WITH KEY number = last_spoken ASSIGNING FIELD-SYMBOL(<previously_spoken>).
      IF sy-subrc = 0.
        number_spoken = <previously_spoken>-turn_diff.
      ELSE.
        number_spoken = 0.
      ENDIF.
      READ TABLE lt_number_game WITH KEY number = number_spoken ASSIGNING FIELD-SYMBOL(<game_turn>).
      IF sy-subrc = 0.
        <game_turn>-turn_diff = current_turn - <game_turn>-turn.
        <game_turn>-turn = current_turn.
      ELSE.
        APPEND VALUE #( number = number_spoken
                        turn_diff = 0
                        turn   = current_turn ) TO lt_number_game.
      ENDIF.
      last_spoken = number_spoken.
    ENDDO.

    result = number_spoken.

  ENDMETHOD.
ENDCLASS.
