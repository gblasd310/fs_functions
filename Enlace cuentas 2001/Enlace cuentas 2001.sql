-- (c) Servicios de Informática Colegiada, S.A. de C.V.
-- Extensión de OpenFIN: enlace_cuentas_2001 
-- Enlace cuentas 2001
-- 09/11/2022

-- ----------------------------------------------------------------------------
-- 09/11/2022 
-- Crea tablas realcionadas con esta extensión
CREATE OR REPLACE FUNCTION enlace_cuentas_2001___db ()
  RETURNS INTEGER AS $$
DECLARE
  -- Variables  
  _id       TEXT;
  _desc     TEXT;
  _fecha    TEXT;
  _version  TEXT := 'enlace_cuentas_2001.1';
BEGIN -- (c) 2011 Servicios de Informática Colegiada, S.A. de C.V.
  -- 0 ------------------------------------------------------------------------
  -- --------------------------------------------------------------------------
  --_id    := _version |+ '.0';
  --_desc  := '....';
  --_fecha := '09/11/2022';
  --IF (of_updatedb_ofx(_id,_desc,_fecha,md5(_id|+_desc|+_fecha))) THEN
  --  
  --END IF;





  RETURN 0;
END;$$
LANGUAGE plpgsql;


-- ----------------------------------------------------------------------------
-- 09/11/2022 
-- Inicialización
CREATE OR REPLACE FUNCTION enlace_cuentas_2001___ini ()
  RETURNS BOOLEAN AS $$
DECLARE
  -- Variables
  --f     DATE := ofpsd('global','fecha',now()::DATE);
BEGIN -- (c) 2011 Servicios de Informática Colegiada, S.A. de C.V.
  -- Revisando versiones
  --IF (NOT of_ofx_check_version('1.14.1')) THEN
  --  RETURN FALSE;
  --END IF;
  
  --PERFORM of_ofx_set('de_fecha','set='||of_fecha_dpm(f)::TEXT);
  --PERFORM of_ofx_set('a_fecha','set='||of_fecha_dum(f)::TEXT);
  PERFORM of_ofx_notice('popup','Recuerda guardar tus archivos como TXT delimitados por tabulaciones');
  /* IF NOT EXISTS (SELECT * FROM auxiliares_ref_masivo) THEN
    CREATE TABLE auxiliares_ref_masivo(
      idsucaux,
      idproducto,
      idauxiliar,
      idsucauxref,
      idproductoref,
      idauxiliarref,
    );
  END IF; */

  PERFORM of_ofx_set('bt_aceptar','sensitive=FALSE');
  PERFORM of_ofx_set('bt_imprimir','sensitive=FALSE');

  RETURN TRUE;
END;$$
LANGUAGE plpgsql;

SELECT of_db_drop_type('ofx_enlace_cuentas_2001_masivo','CASCADE');
CREATE TYPE ofx_enlace_cuentas_2001_masivo AS (
  idsucaux        INTEGER,
  idproducto      INTEGER,
  idauxiliar      INTEGER,
  idsucauxref     INTEGER,
  idproductoref   INTEGER,
  idauxiliarref   INTEGER
);

CREATE OR REPLACE FUNCTION enlace_cuentas_2001 ()
  RETURNS VOID AS $$
DECLARE
  data_report     ofx_enlace_cuentas_2001_masivo%ROWTYPE;
  rlin            RECORD;
  fname           TEXT:= 'vsr_vars_archivo';
  _arr            TEXT[];
  idsucaux1       INTEGER;
  idproducto1     INTEGER;
  idauxiliar1     INTEGER;
  idsucauxref1    INTEGER;
  idproductoref1  INTEGER;
  idauxiliarref1  INTEGER;
  optionQuery     BOOLEAN:= of_ofx_get('chk_desvincular');


BEGIN


  IF (of_ofx_get('archivo') IS NULL) THEN
    PERFORM of_ofx_notice('popup','No se ha seleccionado ningún archivo.');
  ELSE
    -- ACTIVA OPCION DESVINCULAR
    --PERFORM of_ofx_notice('info', optionQuery::TEXT);
    IF (optionQuery) THEN
      FOR rlin IN SELECT * FROM of_archivo_txt_read(fname) AS linea LOOP
      _arr := string_to_array(replace(rlin.linea,E'\t',','),',');
      idsucaux1       := _arr[1]::INTEGER;
      idproducto1     := _arr[2]::INTEGER;
      idauxiliar1     := _arr[3]::INTEGER;
      idsucauxref1    := _arr[4]::INTEGER;
      idproductoref1  := _arr[5]::INTEGER;
      idauxiliarref1  := _arr[6]::INTEGER;
      IF EXISTS (SELECT * FROM auxiliares_ref WHERE (idsucaux, idproducto, idauxiliar, idsucauxref, idproductoref, idauxiliarref) = (idsucaux1, idproducto1, idauxiliar1, idsucauxref1, idproductoref1, idauxiliarref1)) THEN
        --INSERT INTO auxiliares_ref(idsucaux,idproducto,idauxiliar,tiporef,idsucauxref,idproductoref,idauxiliarref,montoref) VALUES (idsucaux1,idproducto1,idauxiliar1,0,idsucauxref1,idproductoref1,idauxiliarref1,0);
        DELETE FROM auxiliares_ref WHERE (idsucaux, idproducto, idauxiliar, idsucauxref, idproducto, idauxiliarref) = (idsucaux1, idproducto1, idauxiliar1, idsucauxref1, idproducto1, idauxiliarref1); 
        PERFORM of_ofx_notice('info','Cuenta desvinculada ' |+ idsucaux1::TEXT |+ '-' |+ idproducto1::TEXT |+ '-' |+ idauxiliar1::TEXT |+ ' >>> ' |+ idsucauxref1::TEXT |+ '-' |+ idproductoref1::TEXT |+ '-' |+ idauxiliarref1::TEXT);
      ELSE
        PERFORM of_ofx_notice('info','VINCULO (' |+ idsucaux1::TEXT |+ '-' |+ idproducto1::TEXT |+ '-' |+ idauxiliar1::TEXT |+ ') -> (' |+ idsucauxref1::TEXT |+ '-' |+ idproductoref1::TEXT |+ '-' |+ idauxiliarref1::TEXT |+ ') NO EXISTENTE');
      END IF;
    END LOOP;
    ELSE
      FOR rlin IN SELECT * FROM of_archivo_txt_read(fname) AS linea LOOP
      _arr := string_to_array(replace(rlin.linea,E'\t',','),',');
      idsucaux1       := _arr[1]::INTEGER;
      idproducto1     := _arr[2]::INTEGER;
      idauxiliar1     := _arr[3]::INTEGER;
      idsucauxref1    := _arr[4]::INTEGER;
      idproductoref1  := _arr[5]::INTEGER;
      idauxiliarref1  := _arr[6]::INTEGER;
      IF NOT EXISTS (SELECT * FROM auxiliares_ref WHERE (idsucaux, idproducto, idauxiliar, idsucauxref, idproductoref, idauxiliarref) = (idsucaux1, idproducto1, idauxiliar1, idsucauxref1, idproductoref1, idauxiliarref1)) THEN
        --IF NOT EXISTS (SELECT * FROM auxiliares_ref WHERE (idsucaux,idproducto,idauxiliar)=(idsucaux1,idproducto1,idauxiliar1))THEN
          --IF NOT EXISTS (SELECT * FROM auxiliares_ref WHERE (idsucauxref,idproductoref,idauxiliarref)=(idsucauxref1,idproductoref1,idauxiliarref1)) THEN
            INSERT INTO auxiliares_ref(idsucaux,idproducto,idauxiliar,tiporef,idsucauxref,idproductoref,idauxiliarref,montoref) VALUES (idsucaux1,idproducto1,idauxiliar1,0,idsucauxref1,idproductoref1,idauxiliarref1,0);
            PERFORM of_ofx_notice('info','Cuenta vinculada ' |+ idsucaux1::TEXT |+ '-' |+ idproducto1::TEXT |+ '-' |+ idauxiliar1::TEXT |+ ' >>> ' |+ idsucauxref1::TEXT |+ '-' |+ idproductoref1::TEXT |+ '-' |+ idauxiliarref1::TEXT);
            data_report.idsucaux := idsucaux1;
            data_report.idproducto := idproducto1;
            data_report.idauxiliar := idauxiliar1;
            data_report.idsucauxref := idsucauxref1;
            data_report.idproductoref := idproductoref1;
            data_report.idauxiliarref := idauxiliarref1;          
          --ELSE
          --  PERFORM of_ofx_notice('info', idsucauxref1::TEXT |+ '-' |+ idproductoref1::TEXT |+ '-' |+ idauxiliarref1::TEXT |+ ' YA SE ENCUENTRA ENLAZADA A UNA CUENTA');
          --END IF;
        --ELSE
          --PERFORM of_ofx_notice('info', idsucaux1::TEXT |+ '-' |+ idproducto1::TEXT |+ '-' |+ idauxiliar1::TEXT |+ ' YA SE ENCUENTRA ENLAZADA A UNA CUENTA');
        --END IF;
      ELSE
        --DELETE FROM auxiliares_ref WHERE (idsucaux, idproducto, idauxiliar, idsucauxref, idproducto, idauxiliarref) = (idsucaux1, idproducto1, idauxiliar1, idsucauxref1, idproducto1, idauxiliarref1); 
        PERFORM of_ofx_notice('info','VINCULO (' |+ idsucaux1::TEXT |+ '-' |+ idproducto1::TEXT |+ '-' |+ idauxiliar1::TEXT |+ ') -> (' |+ idsucauxref1::TEXT |+ '-' |+ idproductoref1::TEXT |+ '-' |+ idauxiliarref1::TEXT |+ ') EXISTENTE');
      END IF;
    END LOOP;

    END IF;
    
  END IF;
  PERFORM of_ofx_set('bt_imprimir','sensitive=TRUE');
  RETURN;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION ofx_enlace_cuentas_2001_masivo ()
  RETURNS SETOF ofx_enlace_cuentas_2001_masivo AS $$
  DECLARE
    r       ofx_enlace_cuentas_2001_masivo%ROWTYPE;
    rlin    RECORD;
    _arr    TEXT[];
    fname           TEXT:= 'vsr_vars_archivo';
    
  BEGIN

    r.idsucaux        := 0;
    r.idproducto      := 0;
    r.idauxiliar      := 0;
    r.idsucauxref     := 0;
    r.idproductoref   := 0;
    r.idauxiliarref   := 0;

    IF (of_ofx_get('archivo') IS NULL) THEN

      PERFORM of_ofx_notice('popup','No se ha seleccionado ningún archivo.');

    ELSE

      FOR rlin IN SELECT * FROM of_archivo_txt_read(fname) AS linea LOOP

        _arr := string_to_array(TRIM(replace(rlin.linea,E'\t',',')),',');
        
        r.idsucaux        := _arr[1];
        r.idproducto      := _arr[2];
        r.idauxiliar      := _arr[3];
        r.idsucauxref     := _arr[4];
        r.idproductoref   := _arr[5];
        r.idauxiliarref   := _arr[6];
          
        RETURN NEXT r;

      END LOOP;

      PERFORM of_ofx_set('bt_aceptar','sensitive=TRUE');

    END IF;

  RETURN;
  END;
$$ LANGUAGE plpgsql;




-- ----------------------------------------------------------------------------
-- 09/11/2022 
-- Finalización
CREATE OR REPLACE FUNCTION enlace_cuentas_2001___fin ()
  RETURNS BOOLEAN AS $$
DECLARE
  -- Variables
BEGIN -- (c) 2011 Servicios de Informática Colegiada, S.A. de C.V.
  RETURN TRUE;
END;$$
LANGUAGE plpgsql;


-- ----------------------------------------------------------------------------
-- 09/11/2022 
-- Validciones
CREATE OR REPLACE FUNCTION enlace_cuentas_2001___val (p_variable TEXT, p_valor TEXT)
  RETURNS BOOLEAN AS $$
DECLARE
  -- Variables
BEGIN -- (c) 2011 Servicios de Informática Colegiada, S.A. de C.V.
  --PERFORM of_param_sesion_raise(NULL); -- Mostrar los parámetros de sesión disponibles 
  --IF (p_variable = 'mi_variable') THEN 
  --  x := of_ofx_get('mi_variable');
  --  y := of_ofx_get_integer('mi_integer');
  --  PERFORM of_ofx_notice('error','Este es un mensaje de error');
  --  RETURN FALSE;
  --END IF;
  RETURN TRUE;
END;$$
LANGUAGE plpgsql;


-- ----------------------------------------------------------------------------
-- 09/11/2022 
-- Procesa el "click" de un boton que no tiene definida una función específica
CREATE OR REPLACE FUNCTION enlace_cuentas_2001___on_click (p_button TEXT, p_data TEXT) 
  RETURNS INTEGER AS $$
DECLARE
  -- Variables
BEGIN -- (c) 2011 Servicios de Informática Colegiada, S.A. de C.V.
  IF (p_button = 'bt_aceptar') THEN
    
  END IF;
  RETURN 0;
END;$$
LANGUAGE plpgsql;


-- ----------------------------------------------------------------------------
-- 09/11/2022
-- Es llamada cuando se selecciona un renglón de un TreeView 
--CREATE OR REPLACE FUNCTION enlace_cuentas_2001___cursor_changed (p_widget TEXT, p_path TEXT, p_row_info TEXT[])
--  RETURNS INTEGER AS $$
--DECLARE
--  -- Variables
--BEGIN -- (c) 2011 Servicios de Informática Colegiada, S.A. de C.V.
--  --PERFORM of_ofx_notice('info','widget: '||p_widget||', path: '||p_path||', row_info:'||p_row_info::TEXT);
--  
--  --IF (p_widget = 'un_widget_x_tv') THEN
--  --  
--  --END IF;
--  RETURN 0;
--END;$$
--LANGUAGE plpgsql;


-- ----------------------------------------------------------------------------
-- 09/11/2022
-- Es llamada cuando se da un "doble-click" a un renglón de un TreeView 
--CREATE OR REPLACE FUNCTION enlace_cuentas_2001___row_activated (p_widget TEXT, p_path TEXT, p_row_info TEXT[])
--  RETURNS INTEGER AS $$
--DECLARE
--  -- Variables
--BEGIN -- (c) 2011 Servicios de Informática Colegiada, S.A. de C.V.
--  --PERFORM of_ofx_notice('info','widget: '||p_widget||', path: '||p_path||', row_info:'||p_row_info::TEXT);
--  
--  --IF (p_widget = 'un_widget_x_tv') THEN
--  --  
--  --END IF;
--  RETURN 0;
--END;$$
--LANGUAGE plpgsql;
