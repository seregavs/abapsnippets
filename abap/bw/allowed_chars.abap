

    DATA: i_str TYPE string,
          e_str TYPE string.
    DATA l_rskc_pattern TYPE string.
    DATA l_subrc TYPE sy-subrc.

    i_str = input.
    e_str = i_str.

    CALL FUNCTION 'RSKC_CHAVL_CHECK'
      EXPORTING
        i_chavl     = i_str
      IMPORTING
        e_err_subrc = l_subrc.

    IF l_subrc IS INITIAL.
      " ok
    ELSE.
      " подразумевается, что не используются ALL_CAPITAL*
      CALL FUNCTION 'RSKC_ALLOWED_CHAR_GET'
        IMPORTING
          e_allowed_char = l_rskc_pattern.

      "
      " Экранирование '\' специальных символов внутри класса: ]\^-
      "
      REPLACE ALL OCCURRENCES OF REGEX `[\]\\\^\-]`
        IN l_rskc_pattern WITH `\\$0`.

      "
      " Паттерн для удаления символов - первый символ '!' или '#'
      " и все, не входящие в разрешенные символы
      "
      CONCATENATE '(^([!#])+)|([^' l_rskc_pattern '])' INTO l_rskc_pattern.

      "
      " Удалить все символы, которые не входят в разрешенные
      "
      REPLACE ALL OCCURRENCES OF REGEX l_rskc_pattern
        IN e_str WITH ' ' IGNORING CASE.

    ENDIF.

    output = e_str.