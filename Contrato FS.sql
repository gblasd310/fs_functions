
CREATE TYPE ofx_contrato_fs AS (
    idsucaux    INTEGER,
    idproducto  INTEGER,
    idauxiliar  INTEGER,

    idsucursal  INTEGER,
    idrol       INTEGER,
    idasociado  INTEGER,

    nombre_completo     TEXT,
    referencia          TEXT,
    pais                TEXT,
    direccion_completa  TEXT,
    ruta                TEXT,
    cuenta_2001         TEXT,
    correo              TEXT,
    telefono_fijo       TEXT,
    pais_nacimiento     TEXT,
    entidad_nacimiento  TEXT,
    nacionalidad        TEXT,
    edad                TEXT,
    sexo                TEXT,
    curp                TEXT,
    rfc                 TEXT,
    ocupacion           TEXT,
    monto_max_formato   TEXT,

    -- Datos de la unidad
    vin                 TEXT,
    motor               TEXT,
    marca               TEXT,
    modelo              TEXT,
    color               TEXT,

    -- Datos del pagare
    montomensualidad    TEXT,
    nmensualidades      TEXT,
    bullet_texto        TEXT,
    bullet_numero       TEXT,
    fe_ultimopago       TEXT,
    condonacion_numero  TEXT,
    condonacion_texto   TEXT,

)



CREATE OR REPLACE FUNCTION ofx_contrato_fs()
    RETURNS SETOF ofx_contrato_fs AS 
$$
    DECLARE

        t               ofx_contrato_fs%ROWTYPE;

        idsucaux1       INTEGER:= of_ofx_get('idsucaux1');
        idproducto1     INTEGER:= of_ofx_get('idproducto1');
        idauxiliar1     INTEGER:= of_ofx_get('idauxiliar1')

    BEGIN
        


        -- Obtenemos los datos del pagare o tabla de amortización
        SELECT
            INTO
                t.nmensualidades, t.fe_ultimopago, t.bullet_numero, t.bullet_texto
                idpago, vence, abono, of_numero_letra(aboro)
        FROM
            ofx_pp_monto_sugerido
        WHERE
            (idsucaux, idproducto, idauxiliar) = (idsucaux1, idproducto1, idauxiliar1);
        ORDER BY idpago DESC LIMIT 1;

        --Obtenemos la mensualidad
        SELECT
            INTO
                t.montomensualidad
        FROM
            ofx_pp_monto_sugerido_planpago
        WHERE
            (idsucaux, idproducto, idauxiliar) = (idsucaux1, idproducto1, idauxiliar1);
        ORDER BY idpago ASC LIMIT 1;

        --Obtenemos la condonacion
        IF (abono > mensualidad) THEN 
            t.condonacion_numero  := abono - mensualidad;
            t.condonacion_texto   := of_numero_letra(condonacion_numero)
        END IF;


        --Obtenemos los datos de la unidad
        SELECT
            INTO
                t.vin, t.motor, t.modelo, t.marca, t.descripcion, t.color
                vin, motor, modelo, marca, descripcion, color
        FROM
            ofx_multicampos_sustentable.auxiliar_masdatos
        INNER JOIN deudores USING(kauxiliar)
        INNER JOIN unidades USING(vin)
        WHERE
            (idsucaux, idproducto, idauxiliar) = (idsucaux1, idproducto1, idauxiliar1);

    END;

$$ LANGUAGE plpgsql;
















