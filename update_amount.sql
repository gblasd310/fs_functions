CREATE
OR REPLACE FUNCTION updateAmounts(idsucaux1 INTEGER, idproducto1 INTEGER, idauxiliar1 INTEGER) RETURNS VOID AS $$ 

DECLARE
    nuevo_monto NUMERIC:=of_ofx_get('nuevo_monto');

BEGIN

IF (
    SELECT 
        estatus
    FROM
        deudores
    WHERE
        (idsucaux, idproducto, idauxiliar) = (idsucaux1, idproducto1, idauxiliar1)
    ) < 3 THEN
    UPDATE
        deudores
    SET
        (montosolicitado, montoautorizado, montoentregado) = (nuevo_monto, nuevo_monto, nuevo_monto)
    WHERE
        (idsucaux, idproducto, idauxiliar) = (idsucaux1, idproducto1, idauxiliar1);
END IF;

    RAISE NOTICE '%' ,nuevo_monto;
END;

$$ LANGUAGE plpgsql