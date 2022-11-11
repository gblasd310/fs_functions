CREATE OR REPLACE FUNCTION actualizar_poliza(
    eidsucpol INTEGER,
    eperiodo INTEGER,
    etipopol INTEGER,
    eidpoliza INTEGER,
    econcepto TEXT
) RETURNS VOID AS 
$$

begin
  update polizas
     set concepto=econcepto
   where (idsucpol, periodo, tipopol, idpoliza) = (eidsucpol, eperiodo, etipopol, eidpoliza);
end;

$$ LANGUAGE plpgsql;