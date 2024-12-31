Start routine in AMDP transformation

METHOD GLOBAL_START BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT OPTIONS READ-ONLY.
-- *** Begin of routine - insert your code only below this line ***

-- Note the _M class are not considered for DTP execution.
-- AMDP Breakpoints must be set in the _A class instead.

outTab = SELECT * FROM :inTab where TO_DATE(left(RUNTS,8), 'YYYYMMDD')  >= add_days (CURRENT_DATE,-2);

-- *** End of routine - insert your code only before this line ***
ENDMETHOD.