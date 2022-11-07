CREATE OR REPLACE FUNCTION actualizar_fe(idsucaux1 INTEGER, idproducto1 INTEGER, idauxiliar1 INTEGER) RETURNS VOID AS $$
DECLARE
    nueva_fe DATE:=of_ofx_get('nueva_fe');
BEGIN

IF (
    SELECT 
        estatus
    FROM
        deudores
    WHERE
        (
            idsucaux, idproducto, idauxiliar) = (idsucaux1, idproducto1, idauxiliar1)
    ) < 3 THEN
    UPDATE
        deudores
    SET
        (
            fechaape, 
            fechasdoini, 
            fechaultima, 
            fechaactivacion
        ) = (
            nueva_fe::DATE, 
            nueva_fe::DATE, 
            nueva_fe::DATE, 
            nueva_fe::DATE
        )
    WHERE
        (idsucaux, idproducto, idauxiliar) = (idsucaux1, idproducto1, idauxiliar1);
END IF;
END;
$$ LANGUAGE plpgsql;