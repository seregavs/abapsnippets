-- как можно вытащить бинарные данные из БД со стороны оракла:
-- Create a directory the database can use to export the data:

CREATE DIRECTORY export_dir AS '/path/to/your/directory';

-- Then use a PL/SQL script to export all the BLOBs to that directory:

DECLARE
  v_start          NUMBER(38,0);
  v_size  CONSTANT NUMBER( 5,0) := 32000;
  v_len            NUMBER(38,0);
  v_buffer         RAW(32000);
  v_file           UTL_FILE.FILE_TYPE;
BEGIN
  FOR r IN ( SELECT filename, полеBLOB FROM your_table )
  LOOP
    v_file  := UTL_FILE.FOPEN('EXPORT_DIR', r.filename, 'wb', 32760 );
    v_start := 1;
    v_len   := DBMS_LOB.GETLENGTH( r.полеBLOB );
    WHILE v_start <= v_len LOOP
      DBMS_LOB.READ(
        r.полеBLOB,
        LEAST( v_len - v_start + 1, v_size ),
        v_start,
        v_buffer
      );
      UTL_FILE.PUT_RAW( v_file, v_buffer );
      UTL_FILE.FFLUSH( v_file ); 
      v_start := v_start + v_size;
    END LOOP;
    UTL_FILE.FCLOSE( v_file );
  END LOOP;
END;
/

select blob into blob_variable from table where filter condition
open file using UTL_FILE
loop
  read a raw chunk from the blob_variable into a raw buffer using DBMS_LOB
  write the raw buffer to disk using UTL_FILE
  exit when there are no more data to read from blob_variable
end loop
close file