  SELECT   kontakt.basic.get_alirattipusid_alirattipus (d.f_alirattipusid)
              AS tev,
           e.f_lean_tip AS lean_tip,
           a.f_sapi AS sapi,
           TRUNC (a.f_idopont, 'mi') AS idopont,
           a.f_oka AS leosztas_ok,
           NVL (
              kontakt.basic.ivk_afctermcsop (a.f_ivk),
              basic.get_irat_tipusid_irat_tipus (
                 basic.ivk_tipus_il (a.f_ivk, 'AFC')
              )
           )
              AS tipus,
           CASE
              WHEN f_grp1 < 0
              THEN
                 'KRITIKUS'
              WHEN f_grp1 >= 0 AND f_grp2 >= 9
              THEN
                 'NAP:' || ' ' || '9+'
              WHEN f_grp1 >= 0 AND f_grp2 < 9
              THEN
                 'NAP:' || ' ' || TO_CHAR (f_grp2)
              ELSE
                 'Z_NA'
           END
              AS prioritas,
           COUNT (DISTINCT a.f_ivk) AS darab
    FROM   kontakt.t_lean_snapshot a,
           (  SELECT   f_sapi, TRUNC (MAX (f_idopont), 'mi') AS f_idopont
                FROM   kontakt.t_lean_snapshot
            GROUP BY   f_sapi) b,
           (SELECT   *
              FROM   kontakt.t_sapi
             WHERE   f_lean_procedure = 'al2' AND f_sapi = 'Alil') c,
           kontakt.t_lean d,
           kontakt.t_lean_alirattipus e
   WHERE       a.f_sapi = b.f_sapi
           AND TRUNC (a.f_idopont, 'mi') = b.f_idopont
           AND a.f_sapi = c.f_sapi
           AND c.f_szervezet = 'AFC'
           AND a.f_ivk = d.f_ivk
           AND d.f_alirattipusid = e.f_alirattipusid
GROUP BY   kontakt.basic.get_alirattipusid_alirattipus (d.f_alirattipusid),
           e.f_lean_tip,
           a.f_sapi,
           TRUNC (a.f_idopont, 'mi'),
           a.f_oka,
           NVL (
              kontakt.basic.ivk_afctermcsop (a.f_ivk),
              basic.get_irat_tipusid_irat_tipus (
                 basic.ivk_tipus_il (a.f_ivk, 'AFC')
              )
           ),
           CASE
              WHEN f_grp1 < 0
              THEN
                 'KRITIKUS'
              WHEN f_grp1 >= 0 AND f_grp2 >= 9
              THEN
                 'NAP:' || ' ' || '9+'
              WHEN f_grp1 >= 0 AND f_grp2 < 9
              THEN
                 'NAP:' || ' ' || TO_CHAR (f_grp2)
              ELSE
                 'Z_NA'
           END
           