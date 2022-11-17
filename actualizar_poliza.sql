CREATE OR REPLACE FUNCTION actualizar_poliza(
    eidsucpol INTEGER,
    eperiodo INTEGER,
    etipopol INTEGER,
    eidpoliza INTEGER,
    econcepto TEXT
) RETURNS VOID AS 
$$

BEGIN

  IF EXISTS 
  (
    SELECT concepto FROM polizas WHERE 
    (idsucpol, periodo, tipopol, idpoliza) = 
    (eidsucpol, eperiodo, etipopol, eidpoliza)
  ) THEN

    UPDATE 
      polizas 
    SET 
      concepto = econcepto
    WHERE 
      (idsucpol, periodo, tipopol, idpoliza) = 
      (eidsucpol, eperiodo, etipopol, eidpoliza);
  
  END IF;
  
END;

$$ LANGUAGE plpgsql;